#!/bin/bash
# Compare C performance with different optimization levels

echo "🔬 C Performance Comparison - Different Optimization Levels"
echo "=========================================================="

# Create a simple benchmark that's easier to measure
cat > micro_benchmark.c << 'EOF'
#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#include "generated_code/basic/basic_ontology.h"

#define ITERATIONS 10000000

double get_time_ms() {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_sec * 1000.0 + tv.tv_usec / 1000.0;
}

int main() {
    // Benchmark 1: Object allocation
    double start = get_time_ms();
    for (int i = 0; i < ITERATIONS; i++) {
        Person_t* p = person_create();
        person_destroy(p);
    }
    double alloc_time = get_time_ms() - start;
    
    // Benchmark 2: Property operations
    Person_t* person = person_create();
    person->has_age = 0;
    
    start = get_time_ms();
    for (int i = 0; i < ITERATIONS; i++) {
        person->has_age++;
    }
    double prop_time = get_time_ms() - start;
    
    // Benchmark 3: API lookup
    start = get_time_ms();
    for (int i = 0; i < ITERATIONS; i++) {
        const class_descriptor_t* cls = owl_get_class("http://example.org/ontology#Person");
        (void)cls;
    }
    double lookup_time = get_time_ms() - start;
    
    printf("Allocation: %.2f ms (%.2f ns/op)\n", 
           alloc_time, alloc_time * 1000000.0 / ITERATIONS);
    printf("Property:   %.2f ms (%.2f ns/op)\n", 
           prop_time, prop_time * 1000000.0 / ITERATIONS);
    printf("Lookup:     %.2f ms (%.2f ns/op)\n", 
           lookup_time, lookup_time * 1000000.0 / ITERATIONS);
    
    person_destroy(person);
    return 0;
}
EOF

echo -e "\n📊 Testing with -O0 (no optimization):"
gcc -O0 -o bench_O0 micro_benchmark.c generated_code/basic/basic_ontology.c -I.
./bench_O0

echo -e "\n📊 Testing with -O1 (basic optimization):"
gcc -O1 -o bench_O1 micro_benchmark.c generated_code/basic/basic_ontology.c -I.
./bench_O1

echo -e "\n📊 Testing with -O2 (standard optimization):"
gcc -O2 -o bench_O2 micro_benchmark.c generated_code/basic/basic_ontology.c -I.
./bench_O2

echo -e "\n📊 Testing with -O3 (aggressive optimization):"
gcc -O3 -o bench_O3 micro_benchmark.c generated_code/basic/basic_ontology.c -I.
./bench_O3

echo -e "\n📊 Testing with -Os (size optimization):"
gcc -Os -o bench_Os micro_benchmark.c generated_code/basic/basic_ontology.c -I.
./bench_Os

# Check binary sizes
echo -e "\n📦 Binary sizes:"
ls -lh bench_* | awk '{print $9 ": " $5}'

# Cleanup
rm -f bench_O* micro_benchmark.c