
// This file is auto-generated from ./test/idls/a.msgspec


#include <string.h>
#include <msgpack.h>
#include "a_types.h"


my_type_x* my_type_x_init(my_type_x* arg) {
    memset(arg, 0, sizeof(my_type_x));
}

void my_type_x_destroy(my_type_x *arg) {
}

UserInfo* UserInfo_init(UserInfo* arg) {
    memset(arg, 0, sizeof(UserInfo));
    arg->flags = 1;
}

void UserInfo_destroy(UserInfo *arg) {
    free(arg->name);
}

LogInLog* LogInLog_init(LogInLog* arg) {
    memset(arg, 0, sizeof(LogInLog));
    UserInfo_init(&arg->user);
    my_type_x_init(&arg->my1);
}

void LogInLog_destroy(LogInLog *arg) {
    UserInfo_destroy(&arg->user);
    if (arg->sites) free(arg->sites);
    if (arg->sites2_keys) {
        for (int _i = 0; _i < arg->sites2_size; ++_i) {
            free(arg->sites2_keys[_i]);
        }
        free(arg->sites2_keys);
    }
    if (arg->sites2_vals) free(arg->sites2_vals);
    free(arg->s1);
    free(arg->r1);
    my_type_x_destroy(&arg->my1);
    if (arg->user_list) {
        for (int _i = 0; _i < arg->user_list_size; ++_i) {
            UserInfo_destroy(&arg->user_list[_i]);
        }
        free(arg->user_list);
    }
    if (arg->user_map_keys) {
        for (int _i = 0; _i < arg->user_map_size; ++_i) {
            UserInfo_destroy(&arg->user_map_keys[_i]);
        }
        free(arg->user_map_keys);
    }
    if (arg->user_map_vals) {
        for (int _i = 0; _i < arg->user_map_size; ++_i) {
            UserInfo_destroy(&arg->user_map_vals[_i]);
        }
        free(arg->user_map_vals);
    }
}



int my_type_x_to_msgpack(msgpack_packer *pk, my_type_x *arg) {
    msgpack_pack_array(pk, 0);

    return 0;
}

int my_type_x_from_msgpack(msgpack_object *obj, my_type_x *arg) {
    if (obj->type != MSGPACK_OBJECT_ARRAY
        || obj->via.array.size != 0) {
        return -1;
    }

    my_type_x_init(arg);
    msgpack_object *o = obj->via.array.ptr;

    return 0;
}

int UserInfo_to_msgpack(msgpack_packer *pk, UserInfo *arg) {
    msgpack_pack_array(pk, 3);
    msgpack_pack_int(pk, arg->uid);
    if (!arg->name) {
        msgpack_pack_nil(pk);
    } else {
        do {
            size_t _l = strlen(arg->name);
            msgpack_pack_raw(pk, _l);
            msgpack_pack_raw_body(pk, arg->name, _l);
        } while(0);
    }
    msgpack_pack_int(pk, arg->flags);

    return 0;
}

int UserInfo_from_msgpack(msgpack_object *obj, UserInfo *arg) {
    if (obj->type != MSGPACK_OBJECT_ARRAY
        || obj->via.array.size != 3) {
        return -1;
    }

    UserInfo_init(arg);
    msgpack_object *o = obj->via.array.ptr;
    arg->uid = o[0].via.i64;
    if (o[1].type == MSGPACK_OBJECT_NIL) {
        arg->name = NULL;
    } else {
        arg->name = strdup(o[1].via.raw.ptr);
    }
    arg->flags = o[2].via.i64;

    return 0;
}

int LogInLog_to_msgpack(msgpack_packer *pk, LogInLog *arg) {
    msgpack_pack_array(pk, 13);
    UserInfo_to_msgpack(pk, &arg->user);
    msgpack_pack_int(pk, arg->site);
    msgpack_pack_int(pk, arg->num);
    msgpack_pack_array(pk, arg->sites_size);
    for (int _i = 0; _i < arg->sites_size; ++_i) {
        msgpack_pack_int(pk, arg->sites[_i]);
    }
    msgpack_pack_map(pk, arg->sites2_size);
    for (int _i = 0; _i < arg->sites2_size; ++_i) {
        do {
            size_t _l = strlen(arg->sites2_keys[_i]);
            msgpack_pack_raw(pk, _l);
            msgpack_pack_raw_body(pk, arg->sites2_keys[_i], _l);
        } while(0);
        msgpack_pack_int(pk, arg->sites2_vals[_i]);
    }
    do {
        size_t _l = strlen(arg->s1);
        msgpack_pack_raw(pk, _l);
        msgpack_pack_raw_body(pk, arg->s1, _l);
    } while(0);
    arg->b1 ? msgpack_pack_true(pk) : msgpack_pack_false(pk);
    msgpack_pack_raw(pk, arg->r1_size);
    msgpack_pack_raw_body(pk, arg->r1, arg->r1_size);
    msgpack_pack_double(pk, arg->d1);
    msgpack_pack_int(pk, arg->i2);
    my_type_x_to_msgpack(pk, &arg->my1);
    msgpack_pack_array(pk, arg->user_list_size);
    for (int _i = 0; _i < arg->user_list_size; ++_i) {
        UserInfo_to_msgpack(pk, &arg->user_list[_i]);
    }
    msgpack_pack_map(pk, arg->user_map_size);
    for (int _i = 0; _i < arg->user_map_size; ++_i) {
        UserInfo_to_msgpack(pk, &arg->user_map_keys[_i]);
        UserInfo_to_msgpack(pk, &arg->user_map_vals[_i]);
    }

    return 0;
}

int LogInLog_from_msgpack(msgpack_object *obj, LogInLog *arg) {
    if (obj->type != MSGPACK_OBJECT_ARRAY
        || obj->via.array.size != 13) {
        return -1;
    }

    LogInLog_init(arg);
    msgpack_object *o = obj->via.array.ptr;
    UserInfo_from_msgpack(&o[0], &arg->user);
    arg->site = o[1].via.i64;
    arg->num = o[2].via.i64;
    arg->sites = malloc(o[3].via.array.size * sizeof(Sites));
    if (arg->sites == NULL) {
        return -1;
    }
    arg->sites_size = o[3].via.array.size;
    for (int _i = 0; _i < o[3].via.array.size; ++_i) {
        arg->sites[_i] = o[3].via.array.ptr[_i].via.i64;
    }
    arg->sites2_keys = malloc(o[4].via.map.size * sizeof(void*));
    arg->sites2_vals = malloc(o[4].via.map.size * sizeof(Sites));
    if (arg->sites2_keys == NULL || arg->sites2_vals == NULL) {
        return -1;
    }
    arg->sites2_size = o[4].via.map.size;
    for (int _i = 0; _i < o[4].via.map.size; ++_i) {
        arg->sites2_keys[_i] = strdup(o[4].via.map.ptr[_i].key.via.raw.ptr);
        arg->sites2_vals[_i] = o[4].via.map.ptr[_i].val.via.i64;
    }
    arg->s1 = strdup(o[5].via.raw.ptr);
    arg->b1 = o[6].via.boolean;
    arg->r1 = malloc(o[7].via.raw.size);
    if (arg->r1 == NULL) {
        return -1;
    }
    arg->r1_size = o[7].via.raw.size;
    memcpy(arg->r1, o[7].via.raw.ptr, o[7].via.raw.size);
    arg->d1 = o[8].via.dec;
    arg->i2 = o[9].via.i64;
    my_type_x_from_msgpack(&o[10], &arg->my1);
    arg->user_list = malloc(o[11].via.array.size * sizeof(UserInfo));
    if (arg->user_list == NULL) {
        return -1;
    }
    arg->user_list_size = o[11].via.array.size;
    for (int _i = 0; _i < o[11].via.array.size; ++_i) {
        UserInfo_from_msgpack(&o[11].via.array.ptr[_i], &arg->user_list[_i]);
    }
    arg->user_map_keys = malloc(o[12].via.map.size * sizeof(UserInfo));
    arg->user_map_vals = malloc(o[12].via.map.size * sizeof(UserInfo));
    if (arg->user_map_keys == NULL || arg->user_map_vals == NULL) {
        return -1;
    }
    arg->user_map_size = o[12].via.map.size;
    for (int _i = 0; _i < o[12].via.map.size; ++_i) {
        UserInfo_from_msgpack(&o[12].via.map.ptr[_i].key, &arg->user_map_keys[_i]);
        UserInfo_from_msgpack(&o[12].via.map.ptr[_i].val, &arg->user_map_vals[_i]);
    }

    return 0;
}



