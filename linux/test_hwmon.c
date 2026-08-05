/* Example: traverse hwmon temperature sensors.
   Might need "modprobe drivetemp".

   Playing with this a bit to see if I can find some useful
   abstractions.  Should eventually run on devices that do not have
   any tooling installed, and data should probably go staight into
   database as well.
*/

#include "sys_for.h"

void printit(void *ctx, const char *model, int temp) {
    LOG("%s,%d\n", model, temp);
}

int main(int argc, char **argv) {
    sys_for_hwmon_temp(NULL, printit);
}

