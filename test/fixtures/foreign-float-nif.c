#include <erl_nif.h>
#include <stdint.h>
#include <string.h>

_Static_assert(sizeof(double) == sizeof(uint64_t), "binary64 fixture requires an eight-byte double");

/* Deliberately use the documented constructor, never fabricate BEAM heap terms. */
static ERL_NIF_TERM number(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int index;
    static const uint64_t patterns[] = {
        UINT64_C(0), UINT64_C(0x8000000000000000),
        UINT64_C(0x7fefffffffffffff), UINT64_C(1),
        UINT64_C(0x7ff0000000000000), UINT64_C(0xfff0000000000000),
        UINT64_C(0x7ff8000000000001)
    };
    if (argc != 1 || !enif_get_int(env, argv[0], &index) || index < 0 || index > 6)
        return enif_make_badarg(env);
    double value;
    uint64_t bits = patterns[index];
    memcpy(&value, &bits, sizeof(value));
    return enif_make_double(env, value);
}

static ErlNifFunc functions[] = {{"number", 1, number, 0}};
ERL_NIF_INIT(foreign_float_nif, functions, NULL, NULL, NULL, NULL)
