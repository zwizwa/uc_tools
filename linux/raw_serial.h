#ifndef RAW_SERIAL_H
#define RAW_SERIAL_H


#include "macros.h"

#if 1

#include <termios.h>

#ifndef CRTSCTS
#  ifdef CNEW_RTSCTS
#    define CRTSCTS CNEW_RTSCTS
#  else
#    define CRTSCTS 0
#  endif /* CNEW_RTSCTS */
#endif /* !CRTSCTS */

static inline void raw_serial_config(int fd) {
    int speed = B115200;
    struct termios tty;
    ASSERT(0 == tcgetattr(fd, &tty));

    cfsetospeed(&tty, (speed_t)speed);
    cfsetispeed(&tty, (speed_t)speed);

    tty.c_cflag |= (CLOCAL | CREAD);    /* ignore modem controls */
    tty.c_cflag &= ~CSIZE;
    tty.c_cflag |= CS8;         /* 8-bit characters */
    tty.c_cflag &= ~PARENB;     /* no parity bit */
    tty.c_cflag &= ~CSTOPB;     /* only need 1 stop bit */
    tty.c_cflag &= ~CRTSCTS;    /* no hardware flowcontrol */

    /* Setup for non-canonical mode */
    tty.c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
    tty.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
    tty.c_oflag &= ~OPOST; 		/* don't modify characters when sending */

    /* Fetch bytes as they become available */
    tty.c_cc[VMIN] = 1;
    tty.c_cc[VTIME] = 0;

    ASSERT(0 == tcsetattr(fd, TCSANOW, &tty));
}

#else

#include <sys/ioctl.h>
#include <asm-generic/termbits.h>
#include <asm-generic/ioctls.h>

/* Old config.  This possibly has a problem with not always flusing
   the buffer?  I forgot.  The one above was suggested as a
   replacement and is tested in the field. */

static inline void raw_serial_config(int fd) {
    struct termios2 tio;
    ASSERT(0 == ioctl(fd, TCGETS2, &tio));

    // http://www.cs.uleth.ca/~holzmann/C/system/ttyraw.c
    tio.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    tio.c_oflag &= ~(OPOST);
    tio.c_cflag |= (CS8);
    tio.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    tio.c_cc[VMIN] = 1;
    tio.c_cc[VTIME] = 0;

    ASSERT(0 == ioctl(fd, TCSETS2, &tio));

}

#endif

#endif
