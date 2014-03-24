#include <stdio.h>
#include "a_types.h"

int main(int argc, char *argv[]) {
    msgpack_packer pk;
    msgpack_sbuffer buf;
    msgpack_unpacked msg;
    
    msgpack_sbuffer_init(&buf);
    msgpack_packer_init(&pk, &buf, msgpack_sbuffer_write);
    msgpack_unpacked_init(&msg);

    LogInLog l1, l2;
    LogInLog_init(&l1);

    LogInLog_to_msgpack(&pk, &l1);

    // printf("%s\n", msgpack_unpack_next(&msg, buf.data, buf.size, NULL) ? "ok" : "err");
    // LogInLog_from_msgpack(&msg.data, &l2);
    return 0;
}
