#ifndef SPARQL_CODEGEN_H
#define SPARQL_CODEGEN_H

#include "sparql_ast.h"
#include "sparql_to_bitactor.h"
#include <stdio.h>

void generate_sparql_chain(FILE* out, const char* query_name, hop_template_t* hops);
void generate_sparql_header(FILE* out);
void generate_sparql_footer(FILE* out);

#endif