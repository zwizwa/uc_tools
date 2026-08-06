#ifndef SYS_FOR_H
#define SYS_FOR_H


#include "textfile.h"

/* Iterators for data exposed in /sys */

static inline void sys_for_hwmon_temp(
    void (*visit)(void *, const char *, int),
    void *ctx
) {
    const char *top = "/sys/class/hwmon";
    DIR *dir = opendir(top);
    ASSERT(dir);
    for(;;) {
        struct dirent *entry = readdir(dir);
        if (!entry) break;
        const char *name = entry->d_name;
        if (!strncmp("hwmon",name,5)) {
            //LOG("%s\n", entry->d_name);
            char *device_model = textfile_fmt2_n("%s/%s/device/model", top, name);
            if (device_model) {
                // LOG("device_model: %s\n", device_model);
                char *temp = textfile_fmt2_n("%s/%s/temp1_input", top, name);
                if (temp) {
                    visit(ctx, device_model, atoi(temp)/1000);
                    free(temp);
                }
                free(device_model);
            }
        }
    }
    closedir(dir);
}

#endif
