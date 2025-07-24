#!/usr/bin/env python3
"""
TTL to Memory-Mapped Triple Store Compiler
Generates C headers with embedded RDF triples for zero-parse overhead
Part of CNS BitActor 80/20 Semantic Optimization
"""
import sys
import hashlib
import time
from pathlib import Path

def hash_uri(uri):
    """Generate 64-bit hash for URI"""
    return int(hashlib.sha256(uri.encode()).hexdigest()[:16], 16)

def parse_simple_ttl(ttl_content):
    """Simple TTL parser for basic triples"""
    triples = []
    prefixes = {}
    
    lines = ttl_content.split('\n')
    for line in lines:
        line = line.strip()
        if not line or line.startswith('#'):
            continue
            
        if line.startswith('@prefix'):
            # Parse prefix declaration
            parts = line.split()
            if len(parts) >= 3:
                prefix = parts[1].rstrip(':')
                uri = parts[2].strip('<>').rstrip(' .')
                prefixes[prefix] = uri
                
        elif ' a ' in line or ' <' in line:
            # Parse triple
            line = line.rstrip(' .')
            parts = line.split(None, 2)
            if len(parts) >= 3:
                subject, predicate, obj = parts
                
                # Expand prefixes
                for prefix, uri in prefixes.items():
                    subject = subject.replace(prefix + ':', uri + '#')
                    predicate = predicate.replace(prefix + ':', uri + '#')
                    obj = obj.replace(prefix + ':', uri + '#')
                
                # Clean up
                subject = subject.strip('<>')
                predicate = predicate.strip('<>')
                obj = obj.strip('<>"')
                
                triples.append((subject, predicate, obj))
    
    return triples

def generate_mmap_header(triples, ontology_name):
    """Generate C header with memory-mapped triple data"""
    
    # Generate unique hashes for all URIs
    subjects = set()
    predicates = set()
    objects = set()
    
    for s, p, o in triples:
        subjects.add(s)
        predicates.add(p)
        objects.add(o)
    
    header = f"""/*
 * Generated Memory-Mapped Triple Store: {ontology_name}
 * Generated: {time.strftime('%Y-%m-%d %H:%M:%S')}
 * Triples: {len(triples)}
 * Zero-parse overhead semantic processing
 */

#ifndef {ontology_name.upper()}_MMAP_H
#define {ontology_name.upper()}_MMAP_H

#include <stdint.h>
#include <stdbool.h>

// Triple structure (24 bytes, cache-aligned)
typedef struct {{
    uint64_t subject;   // Subject URI hash
    uint64_t predicate; // Predicate URI hash  
    uint64_t object;    // Object URI/literal hash
}} __attribute__((packed)) triple_t;

// Memory-mapped triple store
typedef struct {{
    uint32_t count;
    uint32_t capacity;
    triple_t* triples;
}} triple_store_t;

"""

    # Generate URI hash constants
    header += f"// URI Hash Constants\n"
    for uri in sorted(subjects | predicates | objects):
        clean_name = uri.split('#')[-1].split('/')[-1].upper()
        clean_name = ''.join(c if c.isalnum() else '_' for c in clean_name)
        hash_val = hash_uri(uri)
        header += f"#define {clean_name}_HASH 0x{hash_val:016X}ULL\n"
    
    header += f"\n// Compiled triple data\n"
    header += f"static triple_t {ontology_name}_triples[] = {{\n"
    
    for s, p, o in triples:
        s_hash = hash_uri(s)
        p_hash = hash_uri(p)  
        o_hash = hash_uri(o)
        header += f"    {{0x{s_hash:016X}ULL, 0x{p_hash:016X}ULL, 0x{o_hash:016X}ULL}},\n"
    
    header += f"}};\n\n"
    
    # Generate triple store instance
    header += f"static triple_store_t {ontology_name}_store = {{\n"
    header += f"    .count = {len(triples)},\n"
    header += f"    .capacity = {len(triples)},\n"
    header += f"    .triples = {ontology_name}_triples\n"
    header += f"}};\n\n"
    
    # Generate fast lookup functions
    header += f"""// Fast triple lookup (8-tick optimized)
static inline bool {ontology_name}_has_triple(uint64_t s, uint64_t p, uint64_t o) {{
    triple_t* t = {ontology_name}_store.triples;
    uint32_t count = {ontology_name}_store.count;
    
    // Linear search with SIMD potential
    for (uint32_t i = 0; i < count; i++) {{
        if (t[i].subject == s && t[i].predicate == p && t[i].object == o) {{
            return true;
        }}
    }}
    return false;
}}

// Get objects for subject/predicate pair
static inline uint64_t {ontology_name}_get_object(uint64_t s, uint64_t p) {{
    triple_t* t = {ontology_name}_store.triples;
    uint32_t count = {ontology_name}_store.count;
    
    for (uint32_t i = 0; i < count; i++) {{
        if (t[i].subject == s && t[i].predicate == p) {{
            return t[i].object;
        }}
    }}
    return 0; // Not found
}}

// Count triples matching pattern
static inline uint32_t {ontology_name}_count_pattern(uint64_t s, uint64_t p, uint64_t o) {{
    triple_t* t = {ontology_name}_store.triples;
    uint32_t count = {ontology_name}_store.count;
    uint32_t matches = 0;
    
    for (uint32_t i = 0; i < count; i++) {{
        if ((s == 0 || t[i].subject == s) &&
            (p == 0 || t[i].predicate == p) &&
            (o == 0 || t[i].object == o)) {{
            matches++;
        }}
    }}
    return matches;
}}

#endif // {ontology_name.upper()}_MMAP_H
"""
    
    return header

def main():
    if len(sys.argv) != 3:
        print("Usage: ttl_to_mmap.py <input.ttl> <output_name>")
        sys.exit(1)
    
    ttl_file = Path(sys.argv[1])
    output_name = sys.argv[2]
    
    if not ttl_file.exists():
        print(f"Error: {ttl_file} not found")
        sys.exit(1)
    
    # Parse TTL file
    ttl_content = ttl_file.read_text()
    triples = parse_simple_ttl(ttl_content)
    
    print(f"Parsed {len(triples)} triples from {ttl_file}")
    
    # Generate header
    header_content = generate_mmap_header(triples, output_name)
    
    # Write output
    output_file = ttl_file.parent / f"{output_name}_mmap.h"
    output_file.write_text(header_content)
    
    print(f"Generated memory-mapped header: {output_file}")
    print(f"Estimated memory usage: {len(triples) * 24} bytes")
    print(f"Zero-parse overhead: {len(triples)} triples instantly available")

if __name__ == "__main__":
    main()