#include "otel_benchmark.h"
#include <stdio.h>
#include <string.h>

#define CPU_FREQ_GHZ 3.3

void otel_init(otel_context_t* ctx) {
    ctx->metric_count = 0;
    memset(ctx->metrics, 0, sizeof(ctx->metrics));
}

void otel_start_timing(otel_context_t* ctx, const char* name) {
    for (int i = 0; i < ctx->metric_count; i++) {
        if (strcmp(ctx->metrics[i].name, name) == 0) {
            ctx->metrics[i].start_tsc = rdtsc();
            return;
        }
    }
    
    if (ctx->metric_count < 100) {
        ctx->metrics[ctx->metric_count].name = name;
        ctx->metrics[ctx->metric_count].start_tsc = rdtsc();
        ctx->metric_count++;
    }
}

void otel_end_timing(otel_context_t* ctx, const char* name, uint64_t count) {
    uint64_t end_tsc = rdtsc();
    
    for (int i = 0; i < ctx->metric_count; i++) {
        if (strcmp(ctx->metrics[i].name, name) == 0) {
            ctx->metrics[i].end_tsc = end_tsc;
            ctx->metrics[i].count = count;
            uint64_t cycles = end_tsc - ctx->metrics[i].start_tsc;
            ctx->metrics[i].duration_ns = (cycles / (double)count) / CPU_FREQ_GHZ;
            return;
        }
    }
}

void otel_report_mermaid(otel_context_t* ctx) {
    printf("\n```mermaid\n");
    printf("graph TD\n");
    printf("    A[OpenTelemetry Benchmark Results] --> B[Test Results]\n");
    
    int pass_count = 0;
    int total_count = 0;
    
    for (int i = 0; i < ctx->metric_count; i++) {
        otel_metric_t* m = &ctx->metrics[i];
        if (m->count > 0) {
            total_count++;
            uint64_t cycles_per_op = (m->end_tsc - m->start_tsc) / m->count;
            uint64_t ticks = (cycles_per_op + 9) / 10;
            
            bool passed = false;
            if (strstr(m->name, "8tick") || strstr(m->name, "8hop")) {
                passed = ticks <= 8;
            } else if (strstr(m->name, "1tick")) {
                passed = ticks <= 1;
            } else if (strstr(m->name, "2tick")) {
                passed = ticks <= 2;
            } else {
                passed = true;
            }
            
            if (passed) pass_count++;
            
            printf("    B --> %c%d[%s<br/>", 
                   passed ? 'P' : 'F', i, m->name);
            printf("Cycles: %llu<br/>", (unsigned long long)cycles_per_op);
            printf("Ticks: %llu<br/>", (unsigned long long)ticks);
            printf("Time: %.2f ns<br/>", m->duration_ns);
            printf("Status: %s]\n", passed ? "PASS ✓" : "FAIL ✗");
            
            if (passed) {
                printf("    %c%d:::pass\n", 'P', i);
            } else {
                printf("    %c%d:::fail\n", 'F', i);
            }
        }
    }
    
    printf("    B --> S[Summary<br/>Passed: %d/%d<br/>Success Rate: %.1f%%]\n",
           pass_count, total_count, (pass_count * 100.0) / total_count);
    
    if (pass_count == total_count) {
        printf("    S:::success\n");
    } else {
        printf("    S:::warning\n");
    }
    
    printf("    classDef pass fill:#90EE90,stroke:#228B22,stroke-width:2px\n");
    printf("    classDef fail fill:#FFB6C1,stroke:#DC143C,stroke-width:2px\n");
    printf("    classDef success fill:#98FB98,stroke:#006400,stroke-width:3px\n");
    printf("    classDef warning fill:#FFE4B5,stroke:#FF8C00,stroke-width:3px\n");
    printf("```\n");
}