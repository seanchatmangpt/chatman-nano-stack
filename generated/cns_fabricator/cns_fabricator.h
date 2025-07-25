#ifndef CNS_FABRICATOR_H
#define CNS_FABRICATOR_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/* Signal definitions for cns_fabricator */
typedef enum {
    CNS_FABRICATOR_SIGNAL_SENSOR_DATA = 1,
    CNS_FABRICATOR_SIGNAL_ANOMALY_DETECTED = 2,
    CNS_FABRICATOR_SIGNAL_MAINTENANCE_PREDICTED = 3,
    CNS_FABRICATOR_SIGNAL_RESOURCE_OPTIMIZED = 4,
    CNS_FABRICATOR_SIGNAL_QUALITY_CHECKED = 5
} cns_fabricator_signal_t;

/* BitActor structure */
typedef struct {
    uint32_t state;
    uint64_t tick_count;
    uint64_t signal_count;
    uint8_t scratch[4096];
} cns_fabricator_bitactor_t;

/* API functions */
bool cns_fabricator_init(cns_fabricator_bitactor_t* actor);
bool cns_fabricator_tick(cns_fabricator_bitactor_t* actor);
bool cns_fabricator_emit(cns_fabricator_bitactor_t* actor, cns_fabricator_signal_t signal);

#endif /* CNS_FABRICATOR_H */
