#include "erl_nif.h"

#include <wiringPi.h>
#include <string.h>
#include <stdio.h>

static ERL_NIF_TERM OK;
static ERL_NIF_TERM INTERRUPT;
static ErlNifPid interrupt_pid;
static ErlNifEnv* local_env;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    wiringPiSetupGpio();

    OK = enif_make_atom(env, "ok");
    INTERRUPT = enif_make_atom(env, "interrupt");

    local_env = enif_alloc_env();
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data) {
    enif_free_env(local_env);
}

static ERL_NIF_TERM mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int pin, mode;
    char buf[256];

    if(!enif_get_int(env, argv[0], &pin)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_atom(env, argv[1], buf, 256, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    if(strcmp("output", buf) == 0) {
        mode = OUTPUT;
    }
    else if(strcmp("input", buf) == 0) {
        mode = INPUT;
    }
    else {
        return enif_make_badarg(env);
    }

    pinMode(pin, mode);

    return OK;
}

static ERL_NIF_TERM write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int pin, value;

    if(!enif_get_int(env, argv[0], &pin)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_int(env, argv[1], &value)) {
        return enif_make_badarg(env);
    }

    digitalWrite(pin, value);

    return OK;
}

void isr(void) {
    enif_send(NULL, &interrupt_pid, local_env, INTERRUPT);
}

static ERL_NIF_TERM interrupt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    enif_self(env, &interrupt_pid);
    int pin, type;
    char buf[256];

    if(!enif_get_int(env, argv[0], &pin)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_atom(env, argv[1], buf, 256, ERL_NIF_LATIN1)) {
        return enif_make_badarg(env);
    }

    if(strcmp("falling", buf) == 0) {
        type = INT_EDGE_FALLING;
    }
    else if(strcmp("rising", buf) == 0) {
        type = INT_EDGE_RISING;
    }
    else if(strcmp("both", buf) == 0) {
        type = INT_EDGE_BOTH;
    }
    else {
        return enif_make_badarg(env);
    }

    wiringPiISR(pin, type, isr);

    return OK;
}

static ErlNifFunc nif_funcs[] =
{
    {"mode", 2, mode},
    {"write_nif", 2, write},
    {"interrupt", 2, interrupt},
};

ERL_NIF_INIT(pigeon_gpio, nif_funcs, load, NULL, NULL, unload);
