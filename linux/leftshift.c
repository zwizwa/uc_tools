/* Read out the state of the shift key.
   Original purpose: read from udev script to enable test setup. */

#include <fcntl.h>
#include <linux/input.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <string.h>

int main(int argc, char **argv) {
    int fd = open(argv[1], O_RDONLY);
    if (fd < 0) { perror("open"); return 2; }

    unsigned char keys[KEY_MAX/8 + 1];
    memset(keys, 0, sizeof(keys));
    if (ioctl(fd, EVIOCGKEY(sizeof(keys)), keys) < 0) {
        perror("EVIOCGKEY"); return 2;
    }

    int code = KEY_LEFTSHIFT;  // 42
    int down = keys[code/8] & (1 << (code % 8));
    printf("%s\n", down ? "down" : "up");
    return down ? 0 : 1;
}

/*
See https://claude.ai/chat/04df0e2c-ea70-49ad-b46d-7e538b219b76
tom@luna:/i/tom/rdm-bridge/uc_tools/linux$ while sleep 1; do sudo ./leftshift.dynamic.host.elf /dev/input/by-id/usb-Lenovo_Lenovo_Traditional_USB_Keyboard-event-kbd; done
up
up
down
down
up
up
^C
*/
