#ifndef MOD_ESP_SWD
#define MOD_ESP_SWD

/* mod_swd.c */
INLINE int  swd_get_swdio(void)     { return 1; }
INLINE void swd_set_swdio(int val)  { }
INLINE void swd_swclk(int val)      { }
INLINE void swd_srst(int val)       { }
INLINE void swd_dir(int in)         { }
INLINE void swd_busywait(uint32_t ticks) { }
#include "../../cycle_counter_generic.h"
#include "xtensa/core-macros.h"
INLINE uint32_t swd_cycle_counter(void) {
    /* Note that this has one per CPU, so make sure that different
       calls are happening on the same CPU. */
    return XTHAL_GET_CCOUNT();
}
INLINE uint32_t swd_cycle_counter_future_time(uint32_t udiff) {
    return cycle_counter_future(swd_cycle_counter(), udiff);
}
INLINE int32_t swd_cycle_counter_remaining(uint32_t expiration_time) {
    return cycle_counter_diff(expiration_time, swd_cycle_counter());
}
INLINE int swd_cycle_counter_expired(uint32_t expiration_time) {
    return swd_cycle_counter_remaining(expiration_time) < 0;
}
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdangling-pointer"
#include "../../mod_swd.c"
#pragma GCC diagnostic pop

#endif
