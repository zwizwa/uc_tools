#ifndef USERFAULT_H
#define USERFAULT_H

#define _GNU_SOURCE
#include <unistd.h>
#include <sys/mman.h>
#include <linux/userfaultfd.h>
#include <sys/syscall.h>
#include <sys/ioctl.h>
#include <stdint.h>

#include "macros.h"
#include "assert_mmap.h"
#include "assert_read.h"


// Exploration notes in https://claude.ai/chat/b0f68376-2a64-419e-83e0-748adb88c3d1

// There is a way to avoid the copy as well using
// UFFDIO_REGISTER_MODE_MINOR: stop faulted thread until
// UFFDIO_CONTINUE ioctl is handled, but then pages become either file
// backed or swap backed.
//
// To get "just get rid of the page and refault when needed", the
// other side needs to call madvise() MADV_DONTNEED on a region.
//
// https://claude.ai/chat/beae3a32-731b-49b0-8805-900c9161ae8b
// Caveat: about 2^27 (128TiB) space is available, but keep in mind max_map_count limit.
//
// There is also a zeropage option


struct userfault {
    pthread_t service;
    const uint8_t *mem;
    int fd;
};

#ifndef UFFD_USER_MODE_ONLY
#define UFFD_USER_MODE_ONLY 1
#endif

/* Example service used in test_userfault.c */
static inline void *userfault_example_service(void *arg) {
    uintptr_t ps = page_size();
    uintptr_t pm = ~(ps-1);

    struct userfault *s = arg;
    for(;;) {
        struct uffd_msg msg;
        int rv = read(s->fd, &msg, sizeof(msg));
        if (rv != sizeof(msg)) {
            LOG("userfault_service: bad read rv=%d\n", rv);
            if (rv == -1) perror("userfault_service: ");
            goto done;
        }
        if (msg.event != UFFD_EVENT_PAGEFAULT) continue;
        LOG("fault address %p\n", (void*)msg.arg.pagefault.address);
        uintptr_t fault_page = msg.arg.pagefault.address & pm;
        LOG("fault page %p\n", (void*)fault_page);
        uint8_t fill[ps];
        memset(fill, 0x55, ps);
        struct uffdio_copy copy = {
            .dst = fault_page,
            .src = (uintptr_t)(&fill[0]),
            .len = ps,
            .mode = 0,
        };
        ASSERT_ERRNO(ioctl(s->fd, UFFDIO_COPY, &copy));
    }
  done:
    return NULL;
}

static inline void userfault_init(struct userfault *s,
                                  size_t size,
                                  void *(*handler)(void*),
                                  void *ctx) {

    memset(s,0,sizeof(*s));
    s->mem = assert_mmap_anonymous(size); // 1G
    LOG("mem = %p\n", s->mem);
    ASSERT_ERRNO(
        s->fd = syscall(
            SYS_userfaultfd,
            O_CLOEXEC  |
            /* Only handle faults originating in userspace. */
            UFFD_USER_MODE_ONLY
            ));
    ASSERT(s->fd >= 0);
    struct uffdio_api api = { .api = UFFD_API, .features = 0 };
    ioctl(s->fd, UFFDIO_API, &api);
    size_t size_aligned = align_page_size(size);
    struct uffdio_register reg = {
        .range = { .start = (uintptr_t)(s->mem), .len = size_aligned },
        .mode  = UFFDIO_REGISTER_MODE_MISSING,
    };
    ioctl(s->fd, UFFDIO_REGISTER, &reg);
    ASSERT(0 == pthread_create(
               &s->service,
               NULL /* attr */,
               handler,
               ctx));

}


#endif
