#ifndef OS_LINUX
#define OS_LINUX

/* All uc_tools code that runs on Linux should use this file to
   properly set all feature test macros, to avoid including the macros
   before the specific configurations are set. */

#define _GNU_SOURCE // for mremap()

//#define _XOPEN_SOURCE 500 // for ftruncate()

#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>

#endif
