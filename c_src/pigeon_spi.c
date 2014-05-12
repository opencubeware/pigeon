#include "erl_nif.h"

#include <wiringPiSPI.h>
#include <string.h>

static ERL_NIF_TERM OK;
static ERL_NIF_TERM ERROR;


static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    OK = enif_make_atom(env, "ok");
    ERROR = enif_make_atom(env, "error");

    return 0;
}

static ERL_NIF_TERM init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int channel, speed, fd;

    if(!enif_get_int(env, argv[0], &channel)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_int(env, argv[1], &speed)) {
        return enif_make_badarg(env);
    }

    fd = wiringPiSPISetup(channel, speed);
    if(fd == -1) {
        return ERROR;
    }
    else {
        return enif_make_tuple(env, 2, OK, enif_make_int(env, fd));
    }
}

static ERL_NIF_TERM rw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int channel;
    ErlNifBinary transmit, receive;

    if(!enif_get_int(env, argv[0], &channel)) {
        return enif_make_badarg(env);
    }

    if(!enif_inspect_binary(env, argv[1], &transmit)) {
        return enif_make_badarg(env);
    }

    enif_alloc_binary(transmit.size, &receive);
    memcpy(receive.data, transmit.data, transmit.size);

    if(wiringPiSPIDataRW(channel, receive.data, receive.size) == -1) {
        return ERROR;
    }

    return enif_make_tuple(env, 2, OK, enif_make_binary(env, &receive));
}

static ErlNifFunc nif_funcs[] =
{
    {"init", 2, init},
    {"rw", 2, rw}
};

ERL_NIF_INIT(pigeon_spi, nif_funcs, load, NULL, NULL, NULL);
