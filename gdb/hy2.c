
#define PRODUCT "hy2"

#include "mod_lab.c"

instance_status_t app_init(instance_init_t *i) {
    INSTANCE_NEED(i, &console);
    return 0;
}
DEF_INSTANCE(app);
#include "mod_map_forth.c"

void set_gpioa(int pin, int val) {
    hw_gpio_write(GPIOA, pin, val);
    hw_gpio_config(GPIOA, pin, HW_GPIO_CONFIG_OUTPUT);
}

/* 0 = normal (not actuated), nonzero = actuated. */
DEF_COMMAND(relay_a) { set_gpioa(3, !command_stack_pop() ); }
DEF_COMMAND(relay_b) { set_gpioa(4, !command_stack_pop() ); }
DEF_COMMAND(relay_c) { set_gpioa(5, !command_stack_pop() ); }
DEF_COMMAND(relay_d) { set_gpioa(6, !command_stack_pop() ); }

DEF_COMMAND(relays_off) {
    for (int i=3; i<=6; i++) set_gpioa(i, 1);
}

/* root map */
int map_root(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"forth", "map", map_forth},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}

/* Protocol handler entry point. */
int handle_tag_u32(struct tag_u32 *req) {
    return map_root(req);
}
