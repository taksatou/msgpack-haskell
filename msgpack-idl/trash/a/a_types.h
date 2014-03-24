
// This file is auto-generated from ./test/idls/a.msgspec


#ifndef _A_H_
#define _A_H_

#include <msgpack.h>


typedef enum {
    SiteA = 0,
    SiteB = 1,
    SiteC = 2,
} Sites;

typedef struct {
} my_type_x;

typedef struct {
    int64_t uid;
    char *name;
    int64_t flags;
} UserInfo;

typedef struct {
    UserInfo user;
    Sites site;
    int64_t num;
    size_t sites_size;
    Sites *sites;
    size_t sites2_size;
    char **sites2_keys;
    Sites *sites2_vals;
    char *s1;
    bool b1;
    size_t r1_size;
    void *r1;
    double d1;
    int64_t i2;
    my_type_x my1;
    size_t user_list_size;
    UserInfo *user_list;
    size_t user_map_size;
    UserInfo *user_map_keys;
    UserInfo *user_map_vals;
} LogInLog;



my_type_x* my_type_x_init(my_type_x* arg);
void my_type_x_destroy(my_type_x *arg);
UserInfo* UserInfo_init(UserInfo* arg);
void UserInfo_destroy(UserInfo *arg);
LogInLog* LogInLog_init(LogInLog* arg);
void LogInLog_destroy(LogInLog *arg);


int my_type_x_to_msgpack(msgpack_packer *pk, my_type_x *arg);
int my_type_x_from_msgpack(msgpack_object *obj, my_type_x *arg);
int UserInfo_to_msgpack(msgpack_packer *pk, UserInfo *arg);
int UserInfo_from_msgpack(msgpack_object *obj, UserInfo *arg);
int LogInLog_to_msgpack(msgpack_packer *pk, LogInLog *arg);
int LogInLog_from_msgpack(msgpack_object *obj, LogInLog *arg);

#endif /* _A_H_ */

