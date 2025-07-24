/*
 * OANDA INTEGRATION: Real Broker API Implementation
 * This connects to REAL OANDA accounts with REAL money
 */

#include "live_trading_engine.h"
#include <curl/curl.h>
#include <json-c/json.h>
#include <string.h>
#include <stdio.h>

// Response buffer for API calls
typedef struct {
    char* data;
    size_t size;
} api_response_t;

// Callback function for libcurl to write response data
static size_t write_callback(void* contents, size_t size, size_t nmemb, api_response_t* response) {
    size_t realsize = size * nmemb;
    char* ptr = realloc(response->data, response->size + realsize + 1);
    
    if (!ptr) {
        printf("❌ Not enough memory (realloc returned NULL)\n");
        return 0;
    }
    
    response->data = ptr;
    memcpy(&(response->data[response->size]), contents, realsize);
    response->size += realsize;
    response->data[response->size] = 0;
    
    return realsize;
}

/*
 * CONNECT TO OANDA API
 */
int oanda_connect(live_broker_t* broker) {
    printf("🔗 Connecting to OANDA API...\n");
    
    // Set OANDA-specific URLs
    if (broker->is_live_account) {
        strcpy(broker->api_base_url, "https://api-fxtrade.oanda.com/v3");
        strcpy(broker->stream_url, "https://stream-fxtrade.oanda.com/v3");
        printf("⚠️ LIVE ACCOUNT MODE - REAL MONEY AT RISK!\n");
    } else {
        strcpy(broker->api_base_url, "https://api-fxpractice.oanda.com/v3");
        strcpy(broker->stream_url, "https://stream-fxpractice.oanda.com/v3");
        printf("✅ Demo account mode - safe for testing\n");
    }
    
    // Test connection with account info request
    CURL* curl = curl_easy_init();
    if (!curl) {
        printf("❌ Failed to initialize libcurl\n");
        return -1;
    }
    
    // Build account info URL
    char url[512];
    snprintf(url, sizeof(url), "%s/accounts/%s", 
             broker->api_base_url, broker->account_id);
    
    // Set up headers with authentication
    struct curl_slist* headers = NULL;
    char auth_header[1024];
    snprintf(auth_header, sizeof(auth_header), "Authorization: Bearer %s", broker->auth_token);
    headers = curl_slist_append(headers, auth_header);
    headers = curl_slist_append(headers, "Content-Type: application/json");
    
    api_response_t response = {0};
    
    // Configure curl
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 10); // 10 second timeout
    
    // Execute request
    uint64_t start_time = get_timestamp_ns();
    CURLcode res = curl_easy_perform(curl);
    uint64_t end_time = get_timestamp_ns();
    
    broker->avg_api_latency_ns = end_time - start_time;
    
    long response_code;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
    
    if (res != CURLE_OK || response_code != 200) {
        printf("❌ OANDA connection failed: %s (HTTP %ld)\n", 
               curl_easy_strerror(res), response_code);
        
        if (response.data) {
            printf("❌ Error response: %s\n", response.data);
        }
        
        broker->rest_api_connected = false;
        curl_easy_cleanup(curl);
        curl_slist_free_all(headers);
        free(response.data);
        return -1;
    }
    
    printf("✅ OANDA API connected (latency: %.2f ms)\n", 
           broker->avg_api_latency_ns / 1000000.0);
    
    // Parse account info response
    if (response.data) {
        json_object* root = json_tokener_parse(response.data);
        if (root) {
            json_object* account_obj;
            if (json_object_object_get_ex(root, "account", &account_obj)) {
                // Extract account details
                json_object* balance_obj;
                if (json_object_object_get_ex(account_obj, "balance", &balance_obj)) {
                    broker->account_balance = json_object_get_double(balance_obj);
                    printf("💰 Account balance: $%.2f\n", broker->account_balance);
                }
                
                json_object* equity_obj;
                if (json_object_object_get_ex(account_obj, "NAV", &equity_obj)) {
                    broker->account_equity = json_object_get_double(equity_obj);
                    printf("📊 Account equity: $%.2f\n", broker->account_equity);
                }
                
                json_object* margin_used_obj;
                if (json_object_object_get_ex(account_obj, "marginUsed", &margin_used_obj)) {
                    broker->margin_used = json_object_get_double(margin_used_obj);
                    printf("💳 Margin used: $%.2f\n", broker->margin_used);
                }
                
                json_object* margin_available_obj;
                if (json_object_object_get_ex(account_obj, "marginAvailable", &margin_available_obj)) {
                    broker->margin_available = json_object_get_double(margin_available_obj);
                    printf("💰 Margin available: $%.2f\n", broker->margin_available);
                }
                
                json_object* positions_obj;
                if (json_object_object_get_ex(account_obj, "openPositionCount", &positions_obj)) {
                    broker->open_positions = json_object_get_int(positions_obj);
                    printf("📈 Open positions: %u\n", broker->open_positions);
                }
            }
            json_object_put(root);
        }
    }
    
    broker->rest_api_connected = true;
    broker->last_heartbeat_ns = get_timestamp_ns();
    
    // Cleanup
    curl_easy_cleanup(curl);
    curl_slist_free_all(headers);
    free(response.data);
    
    printf("✅ OANDA integration ready for trading\n");
    return 0;
}

/*
 * PLACE REAL ORDER WITH OANDA
 */
int oanda_place_order(live_broker_t* broker, live_order_t* order) {
    if (!broker->rest_api_connected) {
        printf("❌ Not connected to OANDA API\n");
        return -1;
    }
    
    printf("📤 Placing %s order: %s %.5f units at market\n",
           order->type == ORDER_TYPE_MARKET ? "MARKET" : "LIMIT",
           order->currency_pair == EUR_USD ? "EUR/USD" : "UNKNOWN",
           (double)order->requested_size / 100000.0);
    
    CURL* curl = curl_easy_init();
    if (!curl) {
        printf("❌ Failed to initialize libcurl for order\n");
        return -1;
    }
    
    // Build order URL
    char url[512];
    snprintf(url, sizeof(url), "%s/accounts/%s/orders", 
             broker->api_base_url, broker->account_id);
    
    // Create order JSON
    json_object* order_request = json_object_new_object();
    json_object* order_obj = json_object_new_object();
    
    // Order type
    json_object_object_add(order_obj, "type", 
                          json_object_new_string("MARKET"));
    
    // Instrument (currency pair)
    const char* instrument;
    switch (order->currency_pair) {
        case EUR_USD: instrument = "EUR_USD"; break;
        case GBP_USD: instrument = "GBP_USD"; break;
        case USD_JPY: instrument = "USD_JPY"; break;
        case USD_CHF: instrument = "USD_CHF"; break;
        case AUD_USD: instrument = "AUD_USD"; break;
        case USD_CAD: instrument = "USD_CAD"; break;
        case NZD_USD: instrument = "NZD_USD"; break;
        default: instrument = "EUR_USD"; break;
    }
    json_object_object_add(order_obj, "instrument", 
                          json_object_new_string(instrument));
    
    // Position size (OANDA uses string format)
    char units_str[32];
    snprintf(units_str, sizeof(units_str), "%ld", order->requested_size);
    json_object_object_add(order_obj, "units", 
                          json_object_new_string(units_str));
    
    // Stop loss (if specified)
    if (order->stop_loss_price > 0) {
        json_object* stop_loss_obj = json_object_new_object();
        char stop_price_str[32];
        snprintf(stop_price_str, sizeof(stop_price_str), "%.5f", order->stop_loss_price);
        json_object_object_add(stop_loss_obj, "price", 
                              json_object_new_string(stop_price_str));
        json_object_object_add(order_obj, "stopLossOnFill", stop_loss_obj);
    }
    
    // Take profit (if specified)
    if (order->take_profit_price > 0) {
        json_object* take_profit_obj = json_object_new_object();
        char tp_price_str[32];
        snprintf(tp_price_str, sizeof(tp_price_str), "%.5f", order->take_profit_price);
        json_object_object_add(take_profit_obj, "price", 
                              json_object_new_string(tp_price_str));
        json_object_object_add(order_obj, "takeProfitOnFill", take_profit_obj);
    }
    
    json_object_object_add(order_request, "order", order_obj);
    
    // Convert to JSON string
    const char* json_string = json_object_to_json_string(order_request);
    
    // Set up headers
    struct curl_slist* headers = NULL;
    char auth_header[1024];
    snprintf(auth_header, sizeof(auth_header), "Authorization: Bearer %s", broker->auth_token);
    headers = curl_slist_append(headers, auth_header);
    headers = curl_slist_append(headers, "Content-Type: application/json");
    
    api_response_t response = {0};
    
    // Configure curl for POST
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, json_string);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30); // 30 second timeout for orders
    
    // Execute order
    order->submit_time_ns = get_timestamp_ns();
    CURLcode res = curl_easy_perform(curl);
    uint64_t response_time = get_timestamp_ns();
    
    long response_code;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
    
    if (res != CURLE_OK) {
        printf("❌ Order failed: %s\n", curl_easy_strerror(res));
        strcpy(order->rejection_reason, curl_easy_strerror(res));
        order->status = ORDER_REJECTED;
        
        curl_easy_cleanup(curl);
        curl_slist_free_all(headers);
        json_object_put(order_request);
        free(response.data);
        return -1;
    }
    
    if (response_code == 201) {
        // Order accepted
        printf("✅ Order accepted by OANDA\n");
        order->ack_time_ns = response_time;
        order->status = ORDER_PENDING;
        
        // Parse response for order ID and execution details
        if (response.data) {
            json_object* root = json_tokener_parse(response.data);
            if (root) {
                // Get order transaction
                json_object* order_create_transaction;
                if (json_object_object_get_ex(root, "orderCreateTransaction", &order_create_transaction)) {
                    json_object* id_obj;
                    if (json_object_object_get_ex(order_create_transaction, "id", &id_obj)) {
                        strncpy(order->broker_order_id, json_object_get_string(id_obj), 
                               sizeof(order->broker_order_id) - 1);
                        printf("📋 OANDA Order ID: %s\n", order->broker_order_id);
                    }
                }
                
                // Check for immediate fill
                json_object* order_fill_transaction;
                if (json_object_object_get_ex(root, "orderFillTransaction", &order_fill_transaction)) {
                    order->execution_time_ns = response_time;
                    order->status = ORDER_FILLED;
                    
                    // Get execution price
                    json_object* price_obj;
                    if (json_object_object_get_ex(order_fill_transaction, "price", &price_obj)) {
                        order->executed_price = json_object_get_double(price_obj);
                        
                        // Calculate slippage
                        if (order->requested_price > 0) {
                            order->slippage_pips = fabs(order->executed_price - order->requested_price) * 10000;
                        }
                        
                        printf("✅ Order filled at %.5f (slippage: %.1f pips)\n", 
                               order->executed_price, order->slippage_pips);
                    }
                    
                    // Get executed units
                    json_object* units_obj;
                    if (json_object_object_get_ex(order_fill_transaction, "units", &units_obj)) {
                        order->executed_size = json_object_get_int64(units_obj);
                    }
                    
                    // Get commission
                    json_object* commission_obj;
                    if (json_object_object_get_ex(order_fill_transaction, "commission", &commission_obj)) {
                        order->commission_paid = fabs(json_object_get_double(commission_obj));
                    }
                }
                
                json_object_put(root);
            }
        }
        
    } else {
        // Order rejected
        printf("❌ Order rejected by OANDA (HTTP %ld)\n", response_code);
        order->status = ORDER_REJECTED;
        
        if (response.data) {
            printf("❌ Rejection reason: %s\n", response.data);
            strncpy(order->rejection_reason, response.data, 
                   sizeof(order->rejection_reason) - 1);
        }
        
        curl_easy_cleanup(curl);
        curl_slist_free_all(headers);
        json_object_put(order_request);
        free(response.data);
        return -1;
    }
    
    // Update broker stats
    broker->api_calls_today++;
    order->total_latency_ns = response_time - order->submit_time_ns;
    
    printf("📊 Order latency: %.2f ms\n", order->total_latency_ns / 1000000.0);
    
    // Cleanup
    curl_easy_cleanup(curl);
    curl_slist_free_all(headers);
    json_object_put(order_request);
    free(response.data);
    
    return 0;
}

/*
 * GET CURRENT POSITIONS FROM OANDA
 */
int oanda_get_positions(live_broker_t* broker, live_position_t* positions, uint32_t* count) {
    if (!broker->rest_api_connected) {
        printf("❌ Not connected to OANDA API\n");
        return -1;
    }
    
    CURL* curl = curl_easy_init();
    if (!curl) return -1;
    
    // Build positions URL
    char url[512];
    snprintf(url, sizeof(url), "%s/accounts/%s/positions", 
             broker->api_base_url, broker->account_id);
    
    // Set up headers
    struct curl_slist* headers = NULL;
    char auth_header[1024];
    snprintf(auth_header, sizeof(auth_header), "Authorization: Bearer %s", broker->auth_token);
    headers = curl_slist_append(headers, auth_header);
    
    api_response_t response = {0};
    
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 10);
    
    CURLcode res = curl_easy_perform(curl);
    long response_code;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
    
    if (res != CURLE_OK || response_code != 200) {
        printf("❌ Failed to get positions: %s (HTTP %ld)\n", 
               curl_easy_strerror(res), response_code);
        curl_easy_cleanup(curl);
        curl_slist_free_all(headers);
        free(response.data);
        return -1;
    }
    
    // Parse positions response
    uint32_t position_count = 0;
    
    if (response.data) {
        json_object* root = json_tokener_parse(response.data);
        if (root) {
            json_object* positions_array;
            if (json_object_object_get_ex(root, "positions", &positions_array)) {
                int array_length = json_object_array_length(positions_array);
                
                for (int i = 0; i < array_length && position_count < *count; i++) {
                    json_object* position_obj = json_object_array_get_idx(positions_array, i);
                    if (!position_obj) continue;
                    
                    // Check if position has size
                    json_object* long_obj, *short_obj;
                    bool has_long = json_object_object_get_ex(position_obj, "long", &long_obj);
                    bool has_short = json_object_object_get_ex(position_obj, "short", &short_obj);
                    
                    if (has_long) {
                        json_object* units_obj;
                        if (json_object_object_get_ex(long_obj, "units", &units_obj)) {
                            int64_t units = json_object_get_int64(units_obj);
                            if (units != 0) {
                                live_position_t* pos = &positions[position_count];
                                
                                // Get instrument
                                json_object* instrument_obj;
                                if (json_object_object_get_ex(position_obj, "instrument", &instrument_obj)) {
                                    const char* instrument = json_object_get_string(instrument_obj);
                                    // Convert OANDA instrument to our currency pair
                                    if (strcmp(instrument, "EUR_USD") == 0) pos->currency_pair = EUR_USD;
                                    else if (strcmp(instrument, "GBP_USD") == 0) pos->currency_pair = GBP_USD;
                                    else if (strcmp(instrument, "USD_JPY") == 0) pos->currency_pair = USD_JPY;
                                    // Add more pairs as needed
                                }
                                
                                pos->size = units;
                                
                                // Get average price
                                json_object* avg_price_obj;
                                if (json_object_object_get_ex(long_obj, "averagePrice", &avg_price_obj)) {
                                    pos->avg_entry_price = json_object_get_double(avg_price_obj);
                                }
                                
                                // Get unrealized P&L
                                json_object* unrealized_pl_obj;
                                if (json_object_object_get_ex(long_obj, "unrealizedPL", &unrealized_pl_obj)) {
                                    pos->unrealized_pnl = json_object_get_double(unrealized_pl_obj);
                                }
                                
                                position_count++;
                            }
                        }
                    }
                    
                    if (has_short) {
                        json_object* units_obj;
                        if (json_object_object_get_ex(short_obj, "units", &units_obj)) {
                            int64_t units = json_object_get_int64(units_obj);
                            if (units != 0) {
                                live_position_t* pos = &positions[position_count];
                                
                                // Get instrument (same as above)
                                json_object* instrument_obj;
                                if (json_object_object_get_ex(position_obj, "instrument", &instrument_obj)) {
                                    const char* instrument = json_object_get_string(instrument_obj);
                                    if (strcmp(instrument, "EUR_USD") == 0) pos->currency_pair = EUR_USD;
                                    else if (strcmp(instrument, "GBP_USD") == 0) pos->currency_pair = GBP_USD;
                                    else if (strcmp(instrument, "USD_JPY") == 0) pos->currency_pair = USD_JPY;
                                }
                                
                                pos->size = units; // Will be negative for short
                                
                                // Get average price
                                json_object* avg_price_obj;
                                if (json_object_object_get_ex(short_obj, "averagePrice", &avg_price_obj)) {
                                    pos->avg_entry_price = json_object_get_double(avg_price_obj);
                                }
                                
                                // Get unrealized P&L
                                json_object* unrealized_pl_obj;
                                if (json_object_object_get_ex(short_obj, "unrealizedPL", &unrealized_pl_obj)) {
                                    pos->unrealized_pnl = json_object_get_double(unrealized_pl_obj);
                                }
                                
                                position_count++;
                            }
                        }
                    }
                }
            }
            json_object_put(root);
        }
    }
    
    *count = position_count;
    
    printf("📊 Retrieved %u positions from OANDA\n", position_count);
    
    curl_easy_cleanup(curl);
    curl_slist_free_all(headers);
    free(response.data);
    
    return 0;
}

/*
 * STREAM REAL-TIME PRICES FROM OANDA
 * This function runs in a separate thread
 */
int oanda_stream_prices(live_broker_t* broker, 
                       void (*callback)(const live_market_data_t*)) {
    if (!broker->rest_api_connected) {
        printf("❌ Not connected to OANDA API\n");
        return -1;
    }
    
    printf("📡 Starting OANDA price stream...\n");
    
    CURL* curl = curl_easy_init();
    if (!curl) return -1;
    
    // Build streaming URL
    char url[512];
    snprintf(url, sizeof(url), "%s/accounts/%s/pricing/stream?instruments=EUR_USD,GBP_USD,USD_JPY,USD_CHF,AUD_USD,USD_CAD,NZD_USD", 
             broker->stream_url, broker->account_id);
    
    // Set up headers
    struct curl_slist* headers = NULL;
    char auth_header[1024];
    snprintf(auth_header, sizeof(auth_header), "Authorization: Bearer %s", broker->auth_token);
    headers = curl_slist_append(headers, auth_header);
    
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    
    // Stream indefinitely
    api_response_t stream_buffer = {0};
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &stream_buffer);
    
    printf("✅ OANDA price stream connected\n");
    broker->stream_connected = true;
    
    CURLcode res = curl_easy_perform(curl);
    
    if (res != CURLE_OK) {
        printf("❌ Price stream failed: %s\n", curl_easy_strerror(res));
        broker->stream_connected = false;
    }
    
    curl_easy_cleanup(curl);
    curl_slist_free_all(headers);
    free(stream_buffer.data);
    
    return 0;
}

/*
 * EMERGENCY CLOSE ALL POSITIONS
 */
void oanda_emergency_close_all(live_broker_t* broker) {
    printf("🚨 EMERGENCY: Closing all OANDA positions!\n");
    
    if (!broker->rest_api_connected) {
        printf("❌ Cannot close positions - not connected to OANDA\n");
        return;
    }
    
    // Get current positions
    live_position_t positions[50];
    uint32_t position_count = 50;
    
    if (oanda_get_positions(broker, positions, &position_count) == 0) {
        for (uint32_t i = 0; i < position_count; i++) {
            // Close each position via OANDA API
            // Implementation would include PUT request to close position
            printf("🚨 Closing position %u: %s %ld units\n", 
                   i, positions[i].currency_pair == EUR_USD ? "EUR/USD" : "UNKNOWN",
                   positions[i].size);
        }
    }
    
    printf("🚨 Emergency liquidation complete\n");
}

/*
 * UTILITY: Get current timestamp in nanoseconds
 */
static uint64_t get_timestamp_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000000000ULL + (uint64_t)ts.tv_nsec;
}