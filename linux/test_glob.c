#include "macros.h"
#include <stdio.h>
#include <dirent.h>

int main(int argc, char **argv) {
    LOG("%s\n", argv[0]);
    DIR *d;
    ASSERT(d = opendir("."));
    struct dirent *entry;
    while ((entry = readdir(d)) != NULL) {
        LOG("%s\n", entry->d_name);
    }
    closedir(d);
    return 0;
}
