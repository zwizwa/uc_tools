#ifndef PARAMETER_H
#define PARAMETER_H

#include <stdint.h>

union types {
    uint32_t   uint32;
};
#define DEF_PARAMETER(_num, _type, _name, _info, _init) \
    volatile union types _name = { ._type = _init };

struct parameter_info {
    const char *type;
    const char *name;
    const char *info;
    union types init;
    volatile union types *data;
    uint32_t size;
};
#define DEF_PARAMETER_INFO(_num, _type, _name,  _info, _init)           \
    [_num] = {                                                          \
        .type = #_type,                                                 \
        .name = #_name,                                                 \
        .info = _info,                                                  \
        .init._type = _init,                                            \
        .data = &_name,                                                 \
        .size = sizeof(_name)                                           \
    },


static inline int tag_u32_set_parameter(
    struct parameter_info *parameter_info, uint32_t nb_parameters,
    const uint32_t *arg,  uint32_t nb_args,
    const uint8_t *bytes, uint32_t nb_bytes) {

    /* Simple parameters can be specified in the parameter table
       and don't need much protocol handling. */
    if ((nb_args  == 2) &&
        (nb_bytes == 0) &&
        (arg[0] < nb_parameters)) {
        struct parameter_info *pi = parameter_info + arg[0];
        // FIXME: this assumes endianness allows this.
        pi->data->uint32 = arg[1];
        infof("%d: %s = %d\n", arg[0], pi->name, arg[1]);
        return 0;
    }
    infof("unknown tag_u32 command:");
    for (int i=0; i<nb_args; i++) { infof(" %d", arg[i]); }
    infof("\n");
    return -1;
}



#if 0
// Example use
#define FOR_PARAMETERS(p) \
    p(0, uint32,   osc_setpoint, "Main oscillator setpoint, in 5.27 nlog2 (period_cycles)", (15 << 27)) \
FOR_PARAMETERS(DEF_PARAMETER)
struct parameter_info parameter_info[] = {
FOR_PARAMETERS(DEF_PARAMETER_INFO)
};
#endif

#endif
