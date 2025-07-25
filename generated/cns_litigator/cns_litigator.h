#ifndef CNS_LITIGATOR_H
#define CNS_LITIGATOR_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/* Signal definitions for cns_litigator */
typedef enum {
    CNS_LITIGATOR_SIGNAL_CASE_CREATED = 1,
    CNS_LITIGATOR_SIGNAL_DOCUMENT_UPLOADED = 2,
    CNS_LITIGATOR_SIGNAL_HEARING_SCHEDULED = 3,
    CNS_LITIGATOR_SIGNAL_BILLING_ACTIVITY = 4,
    CNS_LITIGATOR_SIGNAL_DEADLINE_ALERT = 5
} cns_litigator_signal_t;

/* BitActor structure */
typedef struct {
    uint32_t state;
    uint64_t tick_count;
    uint64_t signal_count;
    uint8_t scratch[4096];
} cns_litigator_bitactor_t;

/* API functions */
bool cns_litigator_init(cns_litigator_bitactor_t* actor);
bool cns_litigator_tick(cns_litigator_bitactor_t* actor);
bool cns_litigator_emit(cns_litigator_bitactor_t* actor, cns_litigator_signal_t signal);

#endif /* CNS_LITIGATOR_H */
