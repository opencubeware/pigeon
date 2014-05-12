#include "erl_nif.h"

#include <wiringPi.h>

static ERL_NIF_TERM OK;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    OK = enif_make_atom(env, "ok");
    return 0;
}

static ERL_NIF_TERM msleep(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int time;

    if(!enif_get_int(env, argv[0], &time)) {
        return enif_make_badarg(env);
    }

    delay(time);
    return OK;
}

static ERL_NIF_TERM usleep(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int time;

    if(!enif_get_int(env, argv[0], &time)) {
        return enif_make_badarg(env);
    }

    delayMicroseconds(time);
    return OK;
}

static ErlNifFunc nif_funcs[] =
{
    {"msleep", 1, msleep},
    {"usleep", 1, usleep}
};

ERL_NIF_INIT(pigeon_timing, nif_funcs, load, NULL, NULL, NULL);
