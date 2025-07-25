#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <pthread.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <time.h>
#include <errno.h>
#include "core_protection.h"

#define HTTP_PORT 8080
#define METRICS_PORT 9090
#define BUFFER_SIZE 4096

// Global protection instance
static core_protection_t* global_protection = NULL;
static pthread_mutex_t protection_mutex = PTHREAD_MUTEX_INITIALIZER;

// Metrics
static uint64_t total_requests = 0;
static uint64_t approved_requests = 0;
static uint64_t rejected_requests = 0;
static uint64_t total_response_time_us = 0;

// Server control
static volatile int keep_running = 1;

// Signal handler
void signal_handler(int sig) {
    if (sig == SIGINT || sig == SIGTERM) {
        keep_running = 0;
        printf("\nShutting down protection server...\n");
    }
}

// Parse JSON request (simple parser for demo)
int parse_trade_request(const char* json, trade_request_t* trade) {
    // Simple parsing - in production use proper JSON library
    char* pos;
    
    // Parse symbol
    pos = strstr(json, "\"symbol\":\"");
    if (pos) {
        sscanf(pos + 10, "%15[^\"]", trade->symbol);
    }
    
    // Parse position_size
    pos = strstr(json, "\"position_size\":");
    if (pos) {
        trade->position_size = atof(pos + 16);
    }
    
    // Parse entry_price
    pos = strstr(json, "\"entry_price\":");
    if (pos) {
        trade->entry_price = atof(pos + 14);
    }
    
    // Parse stop_loss
    pos = strstr(json, "\"stop_loss\":");
    if (pos) {
        trade->stop_loss = atof(pos + 12);
    }
    
    // Parse account_balance
    pos = strstr(json, "\"account_balance\":");
    if (pos) {
        trade->account_balance = atof(pos + 18);
    }
    
    trade->timestamp = time(NULL);
    return 1;
}

// Handle HTTP request
void handle_http_request(int client_socket, const char* request) {
    char response[BUFFER_SIZE];
    char body[BUFFER_SIZE];
    
    // Check request type
    if (strstr(request, "GET /health")) {
        // Health check
        snprintf(response, sizeof(response),
                "HTTP/1.1 200 OK\r\n"
                "Content-Type: application/json\r\n"
                "Content-Length: 17\r\n"
                "\r\n"
                "{\"status\":\"ok\"}");
        send(client_socket, response, strlen(response), 0);
        return;
    }
    
    if (strstr(request, "GET /ready")) {
        // Readiness check
        snprintf(response, sizeof(response),
                "HTTP/1.1 200 OK\r\n"
                "Content-Type: application/json\r\n"
                "Content-Length: 19\r\n"
                "\r\n"
                "{\"ready\":\"true\"}");
        send(client_socket, response, strlen(response), 0);
        return;
    }
    
    if (strstr(request, "POST /validate")) {
        // Extract body
        char* body_start = strstr(request, "\r\n\r\n");
        if (!body_start) {
            snprintf(response, sizeof(response),
                    "HTTP/1.1 400 Bad Request\r\n"
                    "Content-Length: 0\r\n\r\n");
            send(client_socket, response, strlen(response), 0);
            return;
        }
        
        strcpy(body, body_start + 4);
        
        // Parse trade request
        trade_request_t trade;
        if (!parse_trade_request(body, &trade)) {
            snprintf(response, sizeof(response),
                    "HTTP/1.1 400 Bad Request\r\n"
                    "Content-Length: 0\r\n\r\n");
            send(client_socket, response, strlen(response), 0);
            return;
        }
        
        // Validate trade
        pthread_mutex_lock(&protection_mutex);
        uint64_t start_time = time(NULL) * 1000000;
        protection_result_t result = validate_trade_protection(global_protection, &trade);
        uint64_t end_time = time(NULL) * 1000000;
        uint64_t response_time = end_time - start_time;
        
        // Update metrics
        total_requests++;
        total_response_time_us += response_time;
        if (result.approved) {
            approved_requests++;
        } else {
            rejected_requests++;
        }
        pthread_mutex_unlock(&protection_mutex);
        
        // Build response
        snprintf(body, sizeof(body),
                "{\"approved\":%s,\"adjusted_size\":%.2f,\"rejection_reason\":\"%s\"}",
                result.approved ? "true" : "false",
                result.adjusted_size,
                result.rejection_reason ? result.rejection_reason : "");
        
        snprintf(response, sizeof(response),
                "HTTP/1.1 200 OK\r\n"
                "Content-Type: application/json\r\n"
                "Content-Length: %zu\r\n"
                "\r\n%s",
                strlen(body), body);
        
        send(client_socket, response, strlen(response), 0);
        return;
    }
    
    // 404 for other requests
    snprintf(response, sizeof(response),
            "HTTP/1.1 404 Not Found\r\n"
            "Content-Length: 0\r\n\r\n");
    send(client_socket, response, strlen(response), 0);
}

// Handle metrics request
void handle_metrics_request(int client_socket) {
    char metrics[BUFFER_SIZE];
    char response[BUFFER_SIZE];
    
    pthread_mutex_lock(&protection_mutex);
    double avg_response_time = total_requests > 0 ? 
        (double)total_response_time_us / total_requests : 0;
    
    snprintf(metrics, sizeof(metrics),
            "# HELP protection_requests_total Total number of validation requests\n"
            "# TYPE protection_requests_total counter\n"
            "protection_requests_total %lu\n"
            "# HELP protection_approved_total Total approved trades\n"
            "# TYPE protection_approved_total counter\n"
            "protection_approved_total %lu\n"
            "# HELP protection_rejected_total Total rejected trades\n"
            "# TYPE protection_rejected_total counter\n"
            "protection_rejected_total %lu\n"
            "# HELP protection_response_time_microseconds Average response time\n"
            "# TYPE protection_response_time_microseconds gauge\n"
            "protection_response_time_microseconds %.2f\n"
            "# HELP protection_circuit_breaker_triggered Circuit breaker status\n"
            "# TYPE protection_circuit_breaker_triggered gauge\n"
            "protection_circuit_breaker_triggered %d\n"
            "# HELP protection_kill_switch_active Kill switch status\n"
            "# TYPE protection_kill_switch_active gauge\n"
            "protection_kill_switch_active %d\n",
            total_requests,
            approved_requests,
            rejected_requests,
            avg_response_time,
            global_protection->trading_halted ? 1 : 0,
            global_protection->kill_switch_enabled ? 1 : 0);
    pthread_mutex_unlock(&protection_mutex);
    
    snprintf(response, sizeof(response),
            "HTTP/1.1 200 OK\r\n"
            "Content-Type: text/plain; version=0.0.4\r\n"
            "Content-Length: %zu\r\n"
            "\r\n%s",
            strlen(metrics), metrics);
    
    send(client_socket, response, strlen(response), 0);
}

// HTTP server thread
void* http_server_thread(void* arg) {
    int server_socket, client_socket;
    struct sockaddr_in server_addr, client_addr;
    socklen_t client_len = sizeof(client_addr);
    char buffer[BUFFER_SIZE];
    
    // Create socket
    server_socket = socket(AF_INET, SOCK_STREAM, 0);
    if (server_socket < 0) {
        perror("Socket creation failed");
        return NULL;
    }
    
    // Allow socket reuse
    int opt = 1;
    setsockopt(server_socket, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
    
    // Bind socket
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = INADDR_ANY;
    server_addr.sin_port = htons(HTTP_PORT);
    
    if (bind(server_socket, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0) {
        perror("Bind failed");
        close(server_socket);
        return NULL;
    }
    
    // Listen
    if (listen(server_socket, 5) < 0) {
        perror("Listen failed");
        close(server_socket);
        return NULL;
    }
    
    printf("HTTP server listening on port %d\n", HTTP_PORT);
    
    while (keep_running) {
        // Accept connection
        client_socket = accept(server_socket, (struct sockaddr*)&client_addr, &client_len);
        if (client_socket < 0) {
            if (errno == EINTR) continue;
            perror("Accept failed");
            continue;
        }
        
        // Read request
        memset(buffer, 0, sizeof(buffer));
        recv(client_socket, buffer, sizeof(buffer) - 1, 0);
        
        // Handle request
        handle_http_request(client_socket, buffer);
        
        // Close connection
        close(client_socket);
    }
    
    close(server_socket);
    return NULL;
}

// Metrics server thread
void* metrics_server_thread(void* arg) {
    int server_socket, client_socket;
    struct sockaddr_in server_addr, client_addr;
    socklen_t client_len = sizeof(client_addr);
    char buffer[BUFFER_SIZE];
    
    // Create socket
    server_socket = socket(AF_INET, SOCK_STREAM, 0);
    if (server_socket < 0) {
        perror("Metrics socket creation failed");
        return NULL;
    }
    
    // Allow socket reuse
    int opt = 1;
    setsockopt(server_socket, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
    
    // Bind socket
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = INADDR_ANY;
    server_addr.sin_port = htons(METRICS_PORT);
    
    if (bind(server_socket, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0) {
        perror("Metrics bind failed");
        close(server_socket);
        return NULL;
    }
    
    // Listen
    if (listen(server_socket, 5) < 0) {
        perror("Metrics listen failed");
        close(server_socket);
        return NULL;
    }
    
    printf("Metrics server listening on port %d\n", METRICS_PORT);
    
    while (keep_running) {
        // Accept connection
        client_socket = accept(server_socket, (struct sockaddr*)&client_addr, &client_len);
        if (client_socket < 0) {
            if (errno == EINTR) continue;
            perror("Metrics accept failed");
            continue;
        }
        
        // Read request
        memset(buffer, 0, sizeof(buffer));
        recv(client_socket, buffer, sizeof(buffer) - 1, 0);
        
        // Handle metrics request
        if (strstr(buffer, "GET /metrics")) {
            handle_metrics_request(client_socket);
        }
        
        // Close connection
        close(client_socket);
    }
    
    close(server_socket);
    return NULL;
}

// Main function
int main(int argc, char* argv[]) {
    // Set up signal handlers
    signal(SIGINT, signal_handler);
    signal(SIGTERM, signal_handler);
    
    // Initialize protection system
    global_protection = malloc(sizeof(core_protection_t));
    global_protection->max_position_risk_percent = atof(getenv("MAX_POSITION_RISK_PERCENT") ?: "0.01");
    global_protection->daily_loss_limit_percent = atof(getenv("DAILY_LOSS_LIMIT_PERCENT") ?: "0.02");
    global_protection->current_exposure = 0;
    global_protection->daily_pnl = 0;
    global_protection->trading_halted = false;
    global_protection->require_stop_loss = true;
    global_protection->default_stop_percent = atof(getenv("DEFAULT_STOP_PERCENT") ?: "0.02");
    global_protection->kill_switch_enabled = false;
    global_protection->kill_switch_timestamp = 0;
    global_protection->max_response_time_ms = atoi(getenv("MAX_RESPONSE_TIME_MS") ?: "100");
    
    printf("CNS Protection Server Starting...\n");
    printf("Configuration:\n");
    printf("  Max Position Risk: %.2f%%\n", global_protection->max_position_risk_percent * 100);
    printf("  Daily Loss Limit: %.2f%%\n", global_protection->daily_loss_limit_percent * 100);
    printf("  Default Stop Loss: %.2f%%\n", global_protection->default_stop_percent * 100);
    printf("  Max Response Time: %ums\n", global_protection->max_response_time_ms);
    
    // Start server threads
    pthread_t http_thread, metrics_thread;
    pthread_create(&http_thread, NULL, http_server_thread, NULL);
    pthread_create(&metrics_thread, NULL, metrics_server_thread, NULL);
    
    // Wait for threads
    pthread_join(http_thread, NULL);
    pthread_join(metrics_thread, NULL);
    
    // Cleanup
    free(global_protection);
    
    printf("Protection server shutdown complete.\n");
    return 0;
}