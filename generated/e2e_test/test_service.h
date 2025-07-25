#ifndef TEST_SERVICE_H
#define TEST_SERVICE_H

#include <stdint.h>
#include <stdbool.h>

typedef enum {
    TEST_SERVICE_SIGNAL_START = 1,
    TEST_SERVICE_SIGNAL_PROCESS = 2,
    TEST_SERVICE_SIGNAL_COMPLETE = 3
} test_service_signal_t;

typedef struct {
    uint32_t state;
    uint64_t tick_count;
} test_service_bitactor_t;

bool test_service_init(test_service_bitactor_t* actor);
bool test_service_tick(test_service_bitactor_t* actor);

#endif