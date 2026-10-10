/* Read out the state of the shift key.
   Original purpose: read from udev script to enable test setup. */

#include <fcntl.h>
#include <linux/input.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "macros.h"

void set_scroll_led(int fd, int value) {
    /* Note that I did set trigger to none in input::scrolllock in /sys
       It might not be necessary though. */

    struct input_event ev = {0};
    ev.type  = EV_LED;
    ev.code  = LED_SCROLLL;   /* 0x02 */
    ev.value = value;         /* 0 = off */
    write(fd, &ev, sizeof ev);
}

int keydown(int fd, int code) {
    unsigned char keys[KEY_MAX/8 + 1];
    memset(keys, 0, sizeof(keys));
    if (ioctl(fd, EVIOCGKEY(sizeof(keys)), keys) < 0) {
        perror("EVIOCGKEY"); return 2;
    }
    int down = keys[code/8] & (1 << (code % 8));
    return down;
}

int leftshift(int fd) {
    int code = KEY_LEFTSHIFT;  // 42
    return keydown(fd, code);
}

/* Edit these. */
#define NOTIFY_IP   "127.0.0.1"
#define NOTIFY_PORT 12345
#define NOTIFY_SEC  30
static const unsigned char notify_msg[] = {
    'l','u','n','a'
};

int notify(int fd) {
    int sock = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock < 0) { perror("socket"); return 2; }
    struct sockaddr_in addr = {
        .sin_family = AF_INET,
        .sin_port   = htons(NOTIFY_PORT),
        .sin_addr.s_addr = inet_addr(NOTIFY_IP),
    };
    long last = 0;
    struct input_event ev;
    while (read(fd, &ev, sizeof(ev)) == (ssize_t)sizeof(ev)) {
        if (ev.type != EV_KEY) continue;
        if (ev.value != 1) continue;  /* key down only */
        if (ev.time.tv_sec - last < NOTIFY_SEC) continue;
        last = ev.time.tv_sec;
        if (sendto(sock, notify_msg, sizeof(notify_msg), 0,
                   (struct sockaddr *)&addr, sizeof(addr)) < 0) {
            perror("sendto");
        }
    }
    return 0;
}

int main(int argc, char **argv) {
    if (!(argc >= 2)) goto usage;
    int fd = open(argv[1], O_RDWR);
    if (fd < 0) { perror("open"); return 2; }

    if (!(argc >= 3)) goto usage;
    const char *cmd = argv[2];

    if (!strcmp(cmd, "leftshift")) {
        int down = leftshift(fd);
        printf("%s\n", down ? "down" : "up");
        return down ? 0 : 1;
    }

    if (!strcmp(cmd, "set_scroll_led")) {
        if (!(argc == 4)) goto usage;
        const char *action = argv[3];
        if (!strcmp(action, "blink")) {
            LOG("blink\n");
            int blink_ms[2] = {200, 100};
            for (int i=1; i<=6; i++) {
                int state = i&1;
                usleep(blink_ms[state]*1000);
                set_scroll_led(fd, i&1);
            }
            set_scroll_led(fd, 0);

        }
        else {
            int state = atoi(action);
            LOG("state = %d\n", state);
            set_scroll_led(fd, state);
        }
        return 0;
    }

    if (!strcmp(cmd, "notify")) {
        return notify(fd);
    }

  usage:
    LOG("usage: %s\n", argv[0]);
    LOG("         leftshift\n");
    LOG("         set_scroll_led <state>\n");
    LOG("         notify\n");
    exit(0);
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

/*
./keyboard.sh set_scroll_led blink
*/

