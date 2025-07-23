#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// SPARQL Compile-Time Constant Generator
// This tool converts SPARQL queries to 64-bit constants at build time

// FNV-1a hash for deterministic constant generation
static uint64_t fnv1a_hash(const char* str) {
    uint64_t hash = 14695981039346656037ULL;
    while (*str) {
        hash ^= (uint8_t)*str++;
        hash *= 1099511628211ULL;
    }
    return hash;
}

// Parse SPARQL query and generate constant
static uint64_t sparql_to_constant(const char* query_file) {
    FILE* f = fopen(query_file, "r");
    if (!f) {
        fprintf(stderr, "Error: Cannot open %s\n", query_file);
        return 0;
    }
    
    // Read entire query
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    
    char* content = malloc(size + 1);
    fread(content, 1, size, f);
    content[size] = '\0';
    fclose(f);
    
    // Extract key patterns for deterministic hashing
    uint64_t hash = fnv1a_hash("SPARQL_QUERY");
    
    // Look for WHERE clause patterns
    char* where = strstr(content, "WHERE");
    if (where) {
        char* brace = strchr(where, '{');
        if (brace) {
            // Hash each triple pattern
            char* line = strtok(brace + 1, "\n");
            while (line) {
                if (strstr(line, "?") && !strstr(line, "}")) {
                    // This is a triple pattern
                    hash ^= fnv1a_hash(line);
                    hash = (hash << 13) | (hash >> 51); // Rotate
                }
                line = strtok(NULL, "\n");
            }
        }
    }
    
    // Look for FILTER patterns
    char* filter = strstr(content, "FILTER");
    if (filter) {
        hash ^= fnv1a_hash(filter);
        hash = (hash << 7) | (hash >> 57); // Rotate
    }
    
    free(content);
    
    // Apply final mixing
    hash ^= (hash >> 33);
    hash *= 0xff51afd7ed558ccdULL;
    hash ^= (hash >> 33);
    hash *= 0xc4ceb9fe1a85ec53ULL;
    hash ^= (hash >> 33);
    
    return hash;
}

int main(int argc, char** argv) {
    if (argc < 2) {
        printf("Usage: %s <output.h> [query1.rq query2.rq ...]\n", argv[0]);
        return 1;
    }
    
    FILE* out = fopen(argv[1], "w");
    if (!out) {
        fprintf(stderr, "Error: Cannot create %s\n", argv[1]);
        return 1;
    }
    
    fprintf(out, "// Generated SPARQL constants - DO NOT EDIT\n");
    fprintf(out, "// Generated at compile time from .rq files\n\n");
    fprintf(out, "#ifndef SPARQL_CONSTANTS_H\n");
    fprintf(out, "#define SPARQL_CONSTANTS_H\n\n");
    fprintf(out, "#include <stdint.h>\n\n");
    
    // Process each query file
    for (int i = 2; i < argc; i++) {
        char* query_file = argv[i];
        
        // Extract query name from filename
        char name[256];
        strcpy(name, query_file);
        char* dot = strrchr(name, '.');
        if (dot) *dot = '\0';
        char* slash = strrchr(name, '/');
        char* query_name = slash ? slash + 1 : name;
        
        // Convert to uppercase for constant name
        char const_name[256];
        int j;
        for (j = 0; query_name[j]; j++) {
            if (query_name[j] >= 'a' && query_name[j] <= 'z') {
                const_name[j] = query_name[j] - 32;
            } else if (query_name[j] == '-' || query_name[j] == '_') {
                const_name[j] = '_';
            } else {
                const_name[j] = query_name[j];
            }
        }
        const_name[j] = '\0';
        
        // Generate constant
        uint64_t constant = sparql_to_constant(query_file);
        fprintf(out, "#define SPARQL_%s 0x%016lXULL\n", const_name, constant);
        
        printf("Generated: SPARQL_%s = 0x%016lX from %s\n", 
               const_name, constant, query_file);
    }
    
    fprintf(out, "\n#endif // SPARQL_CONSTANTS_H\n");
    fclose(out);
    
    printf("\nGenerated %s successfully!\n", argv[1]);
    return 0;
}
