/*
 * Generated BitActor Header - Semantic Generation
 * Project: semantic_legal_test
 * Generated from TTL classes: 25
 * Tick Budget: 8
 * Classes: LegalCase, LegalActor, Attorney, Client, Judge, Witness, LegalDocument, Motion, Brief, Contract, Evidence, CaseEvent, Hearing, Deposition, Trial, Settlement, BillableActivity, LegalResearch, CaseLaw, Statute, Regulation, Deadline, CaseAnalytics, AccessControl, AuditEntry
 */

#ifndef SEMANTIC_LEGAL_TEST_H
#define SEMANTIC_LEGAL_TEST_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#define TICK_BUDGET 8
#define TTL_CLASSES 25

// TTL-derived type definitions
#define TTL_CLASS_LEGALCASE 0
#define TTL_CLASS_LEGALACTOR 1
#define TTL_CLASS_ATTORNEY 2
#define TTL_CLASS_CLIENT 3
#define TTL_CLASS_JUDGE 4
#define TTL_CLASS_WITNESS 5
#define TTL_CLASS_LEGALDOCUMENT 6
#define TTL_CLASS_MOTION 7
#define TTL_CLASS_BRIEF 8
#define TTL_CLASS_CONTRACT 9
#define TTL_CLASS_EVIDENCE 10
#define TTL_CLASS_CASEEVENT 11
#define TTL_CLASS_HEARING 12
#define TTL_CLASS_DEPOSITION 13
#define TTL_CLASS_TRIAL 14
#define TTL_CLASS_SETTLEMENT 15
#define TTL_CLASS_BILLABLEACTIVITY 16
#define TTL_CLASS_LEGALRESEARCH 17
#define TTL_CLASS_CASELAW 18
#define TTL_CLASS_STATUTE 19
#define TTL_CLASS_REGULATION 20
#define TTL_CLASS_DEADLINE 21
#define TTL_CLASS_CASEANALYTICS 22
#define TTL_CLASS_ACCESSCONTROL 23
#define TTL_CLASS_AUDITENTRY 24

// BitActor interface
typedef struct {
    uint32_t state;
    uint32_t tick_count;
    uint32_t signal_count;
    uint32_t reserved; // Cache alignment
} semantic_legal_test_bitactor_t;

// Core functions
bool semantic_legal_test_init(semantic_legal_test_bitactor_t* actor);
bool semantic_legal_test_tick(semantic_legal_test_bitactor_t* actor);
bool test_8tick_compliance(semantic_legal_test_bitactor_t* actor, int iterations);

#endif // SEMANTIC_LEGAL_TEST_H
