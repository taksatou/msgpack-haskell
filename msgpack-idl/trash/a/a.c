#include <stdio.h>
#include <string.h>
#include <msgpack.h>
#include "a_types.h"

int main(int argc, char *argv[]) {
    msgpack_sbuffer* buffer = msgpack_sbuffer_new();
    msgpack_packer* pk = msgpack_packer_new(buffer, msgpack_sbuffer_write);
    msgpack_unpacked msg;
    msgpack_unpacked_init(&msg);
    
    for (int i = 0; i < 10; ++i) {
        LogInLog l1, l2;
        LogInLog_init(&l1);
        l1.s1 = strdup("hoge");

        l1.user_list_size = 2;
        l1.user_list = malloc(2*sizeof(UserInfo));
        l1.user_list[0].name = strdup("name1");
        l1.user_list[1].name = strdup("name2");

        LogInLog_to_msgpack(pk, &l1);

        printf("%s\n", msgpack_unpack_next(&msg, buffer->data, buffer->size, NULL) ? "ok" : "err");
        msgpack_object_print(stdout, msg.data);
        LogInLog_from_msgpack(&msg.data, &l2);
        LogInLog_destroy(&l2);
        LogInLog_destroy(&l1);
    }
    
    
    msgpack_sbuffer_free(buffer);
    msgpack_packer_free(pk);
    
    return 0;
}

// #include <stdio.h>
// #include "a_types.h"

// int main(int argc, char *argv[]) {
//     msgpack_packer pk;
//     msgpack_sbuffer buf;
//     msgpack_unpacked msg;
    
//     msgpack_sbuffer_init(&buf);
//     msgpack_packer_init(pk, &buf, msgpack_sbuffer_write);
//     msgpack_unpacked_init(&msg);

//     LogInLog l1, l2;
//     LogInLog_init(&l1);

//     LogInLog_to_msgpack(&pk, &l1);

//     // printf("%s\n", msgpack_unpack_next(&msg, buf.data, buf.size, NULL) ? "ok" : "err");
//     // LogInLog_from_msgpack(&msg.data, &l2);
//     return 0;
// }
