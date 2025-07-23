#include "sparql_codegen.h"

void generate_sparql_header(FILE* out) {
    fprintf(out, "// Generated SPARQL chains - DO NOT EDIT\n");
    fprintf(out, "#include <stdint.h>\n");
    fprintf(out, "#include <stdbool.h>\n");
    fprintf(out, "#include \"sparql_to_bitactor.h\"\n\n");
    fprintf(out, "// Forward declarations for validators\n");
    fprintf(out, "extern bool always_true(uint64_t* data);\n");
    fprintf(out, "extern bool validate_exists(uint64_t* data);\n");
    fprintf(out, "extern bool validate_not_expired(uint64_t* data);\n");
    fprintf(out, "extern bool filter_gt(uint64_t* data);\n");
    fprintf(out, "extern bool filter_lt(uint64_t* data);\n\n");
}

void generate_sparql_chain(FILE* out, const char* query_name, hop_template_t* hops) {
    fprintf(out, "// Generated from %s.rq\n", query_name);
    fprintf(out, "static const proof_chain_t %s_chain = {\n", query_name);
    fprintf(out, "    .hops = {\n");
    
    for (int i = 0; i < 8; i++) {
        const char* validator_name = "always_true";
        if (hops[i].validation_fn == (uint64_t)validate_exists) validator_name = "validate_exists";
        else if (hops[i].validation_fn == (uint64_t)validate_not_expired) validator_name = "validate_not_expired";
        else if (hops[i].validation_fn == (uint64_t)filter_gt) validator_name = "filter_gt";
        else if (hops[i].validation_fn == (uint64_t)filter_lt) validator_name = "filter_lt";
        
        fprintf(out, "        {.capability_id = 0x%016llX, "
                     ".validation_fn = (uint64_t)&%s, "
                     ".data_offset = %llu}",
                (unsigned long long)hops[i].capability_id,
                validator_name,
                (unsigned long long)hops[i].data_offset);
        if (i < 7) fprintf(out, ",");
        fprintf(out, "\n");
    }
    
    fprintf(out, "    }\n};\n\n");
    
    fprintf(out, "bool execute_%s(void* data) {\n", query_name);
    fprintf(out, "    return bitactor_execute_8hop(&%s_chain, data);\n", query_name);
    fprintf(out, "}\n\n");
}

void generate_sparql_footer(FILE* out) {
    fprintf(out, "// 8-tick execution engine\n");
    fprintf(out, "bool bitactor_execute_8hop(const proof_chain_t* chain, void* data) {\n");
    fprintf(out, "    uint64_t result = 0xFFFFFFFFFFFFFFFF;\n");
    fprintf(out, "    \n");
    fprintf(out, "    // Unrolled loop - exactly 8 iterations, 1 tick each\n");
    fprintf(out, "    #pragma unroll 8\n");
    fprintf(out, "    for (int i = 0; i < 8; i++) {\n");
    fprintf(out, "        const hop_template_t* hop = &chain->hops[i];\n");
    fprintf(out, "        uint64_t* hop_data = (uint64_t*)((char*)data + hop->data_offset);\n");
    fprintf(out, "        bool (*validator)(uint64_t*) = (void*)hop->validation_fn;\n");
    fprintf(out, "        \n");
    fprintf(out, "        uint64_t valid = validator(hop_data);\n");
    fprintf(out, "        result &= -valid;\n");
    fprintf(out, "    }\n");
    fprintf(out, "    \n");
    fprintf(out, "    return result != 0;\n");
    fprintf(out, "}\n");
}