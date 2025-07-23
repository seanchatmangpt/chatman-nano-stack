#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include "sparql_ast.h"
#include "sparql_to_bitactor.h"
#include "sparql_codegen.h"

#define ARENA_SIZE (1024 * 1024)

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <sparql_files...>\n", argv[0]);
        return 1;
    }
    
    void* arena_mem = mmap(NULL, ARENA_SIZE, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (arena_mem == MAP_FAILED) {
        perror("mmap");
        return 1;
    }
    
    arena_t arena = {
        .current = arena_mem,
        .end = (char*)arena_mem + ARENA_SIZE
    };
    
    FILE* out = fopen("src/sparql/sparql_chains.c", "w");
    if (!out) {
        perror("fopen");
        return 1;
    }
    
    generate_sparql_header(out);
    
    for (int i = 1; i < argc; i++) {
        sparql_ast_t* ast = parse_sparql_file(argv[i], &arena);
        if (!ast) {
            fprintf(stderr, "Failed to parse %s\n", argv[i]);
            continue;
        }
        
        hop_template_t hops[8];
        compile_sparql_to_hops(ast, hops);
        generate_sparql_chain(out, ast->query_name, hops);
        
        printf("Compiled %s -> %s_chain\n", argv[i], ast->query_name);
    }
    
    generate_sparql_footer(out);
    fclose(out);
    
    munmap(arena_mem, ARENA_SIZE);
    return 0;
}