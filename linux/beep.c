// beep.c — usage: beep [hz] [ms]
// https://claude.ai/chat/2226356f-950f-4d28-a165-96cb607f6571

// Note that many modern boards just don't have this any more.

#include <fcntl.h>
#include <linux/input.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#define SPKR "/dev/input/by-path/platform-pcspkr-event-spkr"

int main(int argc, char **argv) {
    int hz = argc > 1 ? atoi(argv[1]) : 880;
    int ms = argc > 2 ? atoi(argv[2]) : 200;

    int fd = open(SPKR, O_WRONLY);
    if (fd < 0) { perror("open " SPKR); return 2; }

    struct input_event e = {0};
    e.type = EV_SND; e.code = SND_TONE;

    e.value = hz;  if (write(fd, &e, sizeof e) < 0) { perror("write"); return 2; }
    usleep(ms * 1000);
    e.value = 0;   write(fd, &e, sizeof e);   // silence

    close(fd);
    return 0;
}

