#ifndef CNS_CLINICIAN_H
#define CNS_CLINICIAN_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/* Signal definitions for cns_clinician */
typedef enum {
    CNS_CLINICIAN_SIGNAL_PATIENT_REGISTERED = 1,
    CNS_CLINICIAN_SIGNAL_DIAGNOSIS_RECORDED = 2,
    CNS_CLINICIAN_SIGNAL_TREATMENT_PRESCRIBED = 3,
    CNS_CLINICIAN_SIGNAL_APPOINTMENT_SCHEDULED = 4,
    CNS_CLINICIAN_SIGNAL_INSURANCE_VERIFIED = 5
} cns_clinician_signal_t;

/* BitActor structure */
typedef struct {
    uint32_t state;
    uint64_t tick_count;
    uint64_t signal_count;
    uint8_t scratch[4096];
} cns_clinician_bitactor_t;

/* API functions */
bool cns_clinician_init(cns_clinician_bitactor_t* actor);
bool cns_clinician_tick(cns_clinician_bitactor_t* actor);
bool cns_clinician_emit(cns_clinician_bitactor_t* actor, cns_clinician_signal_t signal);

#endif /* CNS_CLINICIAN_H */
