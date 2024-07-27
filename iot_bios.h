#ifndef IOT_BIOS
#define IOT_BIOS

/* Original idea: a fleet of esp32 chips has firmware code split into
   two parts: a BIOS in Flash developed using ESP IDF, and application
   code in SRAM that can change on the fly.  The BIOS provides a set
   of calls, and exposes a socket that can be used to upload code.
   This allows the application to be kept platform-independent, and
   also provides a quick mechanism to update application code.  The
   idea is that IOT code always talks to the server, so the server
   might just as well push code on each startup. */

struct iot_bios {
    void *(*malloc)(size_t size);
    int (*printf)(const char*, ...);
    void (*reboot)(void);
};

#endif
