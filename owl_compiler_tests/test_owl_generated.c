/*
 * Test program for OWL compiler generated code
 * Tests basic functionality of the generated C files
 */

#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "generated_code/basic/basic_ontology.h"

void test_person_creation() {
    printf("Testing Person creation...\n");
    
    Person_t* person = person_create();
    assert(person != NULL);
    assert(person->base.uri != NULL);
    assert(strcmp(person->base.uri, "http://example.org/ontology#Person") == 0);
    assert(person->base.label != NULL);
    assert(strcmp(person->base.label, "Person") == 0);
    
    // Test property assignment
    person->has_name = strdup("John Doe");
    person->has_age = 30;
    
    assert(person_validate(person));
    
    person_destroy(person);
    printf("✓ Person creation test passed\n");
}

void test_organization_creation() {
    printf("Testing Organization creation...\n");
    
    Organization_t* org = organization_create();
    assert(org != NULL);
    assert(org->base.uri != NULL);
    assert(strcmp(org->base.uri, "http://example.org/ontology#Organization") == 0);
    
    assert(organization_validate(org));
    
    organization_destroy(org);
    printf("✓ Organization creation test passed\n");
}

void test_employee_creation() {
    printf("Testing Employee creation...\n");
    
    Employee_t* emp = employee_create();
    assert(emp != NULL);
    assert(emp->base.uri != NULL);
    assert(strcmp(emp->base.uri, "http://example.org/ontology#Employee") == 0);
    
    // Employee should have optimization hints
    assert(emp->optimization.memory_layout_enabled == true);
    
    assert(employee_validate(emp));
    
    employee_destroy(emp);
    printf("✓ Employee creation test passed\n");
}

void test_api_functions() {
    printf("Testing API functions...\n");
    
    // Test instance creation via API
    owl_object_t* obj = owl_create_instance("http://example.org/ontology#Person");
    assert(obj != NULL);
    assert(obj->uri != NULL);
    
    // Test property lookup
    const property_descriptor_t* prop = owl_get_property("http://example.org/ontology#hasName");
    assert(prop != NULL);
    assert(prop->type == PROPERTY_TYPE_DATATYPE);
    assert(prop->domain_count == 1);
    assert(prop->range_count == 1);
    
    // Test class lookup
    const class_descriptor_t* cls = owl_get_class("http://example.org/ontology#Employee");
    assert(cls != NULL);
    assert(cls->instance_size == sizeof(Employee_t));
    assert(cls->parent_count == 1);
    
    owl_destroy_instance(obj);
    printf("✓ API functions test passed\n");
}

void test_eightfold_integration() {
    printf("Testing Eightfold Path integration...\n");
    
    eightfold_context_t* ctx = eightfold_create_context();
    assert(ctx != NULL);
    assert(ctx->capacity > 0);
    assert(ctx->instances != NULL);
    assert(ctx->instance_count == 0);
    
    // Add some instances
    Person_t* person = person_create();
    assert(eightfold_add_instance(ctx, &person->base));
    assert(ctx->instance_count == 1);
    
    eightfold_destroy_context(ctx);
    printf("✓ Eightfold Path integration test passed\n");
}

int main() {
    printf("=== OWL Compiler Generated Code Test Suite ===\n\n");
    
    test_person_creation();
    test_organization_creation();
    test_employee_creation();
    test_api_functions();
    test_eightfold_integration();
    
    printf("\n✅ All tests passed successfully!\n");
    printf("The OWL compiler successfully generates working C code.\n");
    
    return 0;
}