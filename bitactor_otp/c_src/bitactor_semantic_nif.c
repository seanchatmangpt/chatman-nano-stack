#include <erl_nif.h>
static ERL_NIF_TERM init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) { return enif_make_atom(env, "ok"); }
static ErlNifFunc nif_funcs[] = { {"init", 0, init_nif, 0} };
ERL_NIF_INIT(bitactor_semantic_nif, nif_funcs, NULL, NULL, NULL, NULL)
