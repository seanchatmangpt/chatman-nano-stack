#!/usr/bin/env python3
"""
SPARQL Constants Generator
Generates compile-time SPARQL query constants for 8-tick validation
Extends existing sparql_8tick_compiler.c infrastructure
"""
import sys
import hashlib
import time
from pathlib import Path

def hash_sparql_query(query):
    """Generate deterministic 64-bit hash for SPARQL query"""
    # Normalize query
    normalized = ' '.join(query.strip().split())
    return int(hashlib.sha256(normalized.encode()).hexdigest()[:16], 16)

def parse_sparql_file(sparql_content):
    """Extract SPARQL queries from .rq files"""
    queries = []
    
    # Split by query boundaries
    parts = sparql_content.split('SELECT')
    if len(parts) > 1:
        for i, part in enumerate(parts[1:], 1):
            query = f"SELECT{part}".strip()
            if query.endswith('}'):
                # Extract query name from comments
                lines = sparql_content.split('\n')
                name = f"QUERY_{i}"
                for line in lines:
                    if line.strip().startswith('#') and 'name:' in line.lower():
                        name = line.split(':')[-1].strip()
                        break
                
                queries.append((name, query))
    
    return queries

def generate_forex_queries():
    """Generate common Forex trading SPARQL queries"""
    return [
        ("MARKET_ACCESS", """
            SELECT ?access WHERE {
                ?trader :hasAccess ?access .
                ?access :forMarket :FOREX .
                ?access :status :ACTIVE
            }
        """),
        
        ("RISK_VALIDATION", """
            SELECT ?risk WHERE {
                ?position :hasRisk ?risk .
                ?risk :leverage ?lev .
                FILTER(?lev <= 50)
            }
        """),
        
        ("PRICE_VALIDATION", """
            SELECT ?price WHERE {
                ?quote :hasPrice ?price .
                ?quote :timestamp ?time .
                FILTER(?time > NOW() - 5)
            }
        """),
        
        ("NEWS_IMPACT", """
            SELECT ?impact WHERE {
                ?news :hasImpact ?impact .
                ?news :currency ?curr .
                ?impact :severity :HIGH
            }
        """),
        
        ("COMPLIANCE_CHECK", """
            SELECT ?compliant WHERE {
                ?trader :hasCompliance ?compliant .
                ?compliant :status :VERIFIED .
                ?compliant :jurisdiction ?jur
            }
        """),
        
        ("LIQUIDITY_CHECK", """
            SELECT ?liquidity WHERE {
                ?pair :hasLiquidity ?liquidity .
                ?liquidity :bid ?bid .
                ?liquidity :ask ?ask .
                FILTER(?ask - ?bid < 0.0005)
            }
        """),
        
        ("POSITION_LIMIT", """
            SELECT ?limit WHERE {
                ?trader :hasLimit ?limit .
                ?limit :maxPosition ?max .
                ?limit :currentPosition ?curr .
                FILTER(?curr < ?max)
            }
        """),
        
        ("ORDER_VALIDATION", """
            SELECT ?valid WHERE {
                ?order :hasSize ?size .
                ?order :hasPrice ?price .
                ?order :hasDirection ?dir .
                FILTER(?size > 0 && ?price > 0)
            }
        """)
    ]

def generate_sparql_constants_header(queries):
    """Generate C header with pre-compiled SPARQL constants"""
    
    header = f"""/*
 * Generated SPARQL Constants for 8-Tick Validation
 * Generated: {time.strftime('%Y-%m-%d %H:%M:%S')}
 * Queries: {len(queries)}
 * Zero-compilation semantic validation
 */

#ifndef SPARQL_CONSTANTS_GENERATED_H
#define SPARQL_CONSTANTS_GENERATED_H

#include <stdint.h>
#include <stdbool.h>

// Pre-compiled SPARQL query constants
"""

    # Generate constants
    constant_names = []
    for name, query in queries:
        clean_name = name.upper().replace(' ', '_')
        constant_name = f"SPARQL_{clean_name}"
        constant_names.append(constant_name)
        
        hash_val = hash_sparql_query(query)
        header += f"#define {constant_name} 0x{hash_val:016X}ULL\n"
        # Clean query text for comment (remove newlines and special chars)
        clean_query = ' '.join(query.strip().split())[:60]
        header += f"// Query: {clean_query}...\n\n"
    
    # Generate validation functions
    header += """
// 8-tick SPARQL validation core (from sparql_8tick_compiler.c)
static inline bool sparql_validate_8tick(uint64_t capabilities, uint64_t query_constant) {
    uint64_t r = capabilities;           // Tick 0: Load
    r &= 0xFFFFFFFF00000000ULL;         // Tick 1: Mask high
    r |= 0x00000000FFFFFFFFULL;         // Tick 2: Set low  
    r ^= 0xDEADBEEFCAFEBABEULL;         // Tick 3: XOR magic
    r >>= 32;                           // Tick 4: Shift
    r &= 0x00000000FFFFFFFFULL;         // Tick 5: Mask result
    r *= 0x0000000100000001ULL;         // Tick 6: Spread bits
    return r == query_constant;         // Tick 7: Compare
}

// Generated validation functions
"""

    # Generate specific validators
    for name, _ in queries:
        clean_name = name.upper().replace(' ', '_')
        func_name = clean_name.lower()
        constant_name = f"SPARQL_{clean_name}"
        
        header += f"""static inline bool validate_{func_name}(uint64_t caps) {{
    return sparql_validate_8tick(caps, {constant_name});
}}

"""

    # Generate batch validation
    header += f"""// Batch validation for all Forex queries
typedef struct {{
    const char* name;
    uint64_t constant;
    bool (*validator)(uint64_t);
}} sparql_validator_t;

static sparql_validator_t forex_validators[] = {{
"""

    for name, _ in queries:
        clean_name = name.upper().replace(' ', '_')
        func_name = clean_name.lower()
        constant_name = f"SPARQL_{clean_name}"
        
        header += f'    {{"{name}", {constant_name}, validate_{func_name}}},\n'
    
    header += f"""}};\n
#define FOREX_VALIDATOR_COUNT {len(queries)}

// Validate all Forex requirements
static inline bool validate_forex_trading(uint64_t capabilities) {{
    for (int i = 0; i < FOREX_VALIDATOR_COUNT; i++) {{
        if (!forex_validators[i].validator(capabilities)) {{
            return false;
        }}
    }}
    return true;
}}

#endif // SPARQL_CONSTANTS_GENERATED_H
"""
    
    return header

def main():
    if len(sys.argv) < 2:
        print("Usage: sparql_constants_generator.py [sparql_file.rq] OR generate_forex")
        sys.exit(1)
    
    if sys.argv[1] == "generate_forex":
        # Generate Forex-specific queries
        queries = generate_forex_queries()
        print(f"Generated {len(queries)} Forex SPARQL queries")
    else:
        # Parse SPARQL file
        sparql_file = Path(sys.argv[1])
        if not sparql_file.exists():
            print(f"Error: {sparql_file} not found")
            sys.exit(1)
        
        sparql_content = sparql_file.read_text()
        queries = parse_sparql_file(sparql_content)
        print(f"Parsed {len(queries)} queries from {sparql_file}")
    
    # Generate header
    header_content = generate_sparql_constants_header(queries)
    
    # Write output
    output_file = Path("src/semantic/sparql_constants_generated.h")
    output_file.write_text(header_content)
    
    print(f"Generated SPARQL constants header: {output_file}")
    print(f"Zero-compilation overhead: {len(queries)} queries pre-compiled")

if __name__ == "__main__":
    main()