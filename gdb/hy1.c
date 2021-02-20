/* Configuration for hy1 board.
   Ad-hoc debugging board.

   The idea is to gather generic bits in mod_lab.c to have it work as
   an application framework, and define the app instance here.

*/

#define LEDSTRIP_NB_LEDS 1
#define FOR_LEDS(i) for(int i=0;i<LEDSTRIP_NB_LEDS;i++)
#define PRODUCT "hy1"

#include "mod_lab.c"
//#include "mod_ws2812.c"
#include "mod_ws2812_inc.c"

#include "tag_u32.h"

instance_status_t app_init(instance_init_t *i) {
    INSTANCE_NEED(i, &console, &ledstrip);
    //_service.add(ledstrip_animation_tick);
    return 0;
}
DEF_INSTANCE(app);


/* Pixel buffer + generator wrapper . */
struct grb frame[LEDSTRIP_NB_LEDS];
struct grb_buf_gen grb_buf_gen;
void ledstrip_send_frame(void) {
    grb_buf_gen_init(&grb_buf_gen, frame, ARRAY_SIZE(frame));
    /* PRECONDITION: previous transfer is finished. */
    ledstrip_send(&grb_buf_gen.gen);
}
DEF_COMMAND(leds) { // r g b --
    struct grb grb;
    grb.b = command_stack_pop();
    grb.g = command_stack_pop();
    grb.r = command_stack_pop();
    for(int i=0; i<LEDSTRIP_NB_LEDS; i++) {
        frame[i] = grb;
    }
    ledstrip_send_frame();
}


/* Doodle: parametic generator. */
/* Generator wrapper for pixel synthesizer. */

struct doodle_gen {
    struct seq_gen gen;
    uint16_t index, len;
    struct grb grb;
} doodle_gen;
const void *doodle_gen_pop(struct seq_gen *_g) {
    struct doodle_gen *g = (void*)_g;
    if (g->index >= g->len) return NULL;
    g->grb.r = 0;//g->index;
    g->grb.g = 0;//g->index;
    g->grb.b = 4 + g->index;
    g->index++;
    return &g->grb;
}
void doodle_gen_init(struct doodle_gen *g, uint32_t param) {
    g->gen.pop = doodle_gen_pop;
    g->index = 0;
    g->len = LEDSTRIP_NB_LEDS;
}

DEF_COMMAND(doodle) {
    doodle_gen_init(&doodle_gen, command_stack_pop());
    ledstrip_send(&doodle_gen.gen);
}





const char t_map[] = "map";
const char t_cmd[] = "cmd";

int reply_1(struct tag_u32 *req, uint32_t rv) {
    SEND_REPLY_TAG_U32(req, rv);
    return 0;
}
int reply_ok(struct tag_u32 *req) {
    return reply_1(req, 0);
}

static inline void set_rgb(struct grb *grb, uint8_t r, uint8_t g, uint8_t b) {
    grb->r = r; grb->g = g; grb->b = b;
}

int handle_led_set(struct tag_u32 *req) {
    TAG_U32_UNPACK(req, -2, m, led_nb, _set_cmd, r, g, b) {
        //infof("set %d (%d,%d,%d)\n", m->led_nb, m->r, m->g, m->b);
        if (m->led_nb >= LEDSTRIP_NB_LEDS) return -1;
        set_rgb(&frame[m->led_nb], m->r, m->g, m->b);
        ledstrip_send_frame();
        return reply_ok(req);
    }
    return -1;
}
int map_led_op(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"set", t_cmd, handle_led_set, 3},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}
int map_led_entry(struct tag_u32 *req, void *no_ctx,
                  struct tag_u32_entry *entry) {
    TAG_U32_UNPACK(req, 0, m, param_nb) {
        if (m->param_nb >= LEDSTRIP_NB_LEDS) return -1;
        const struct tag_u32_entry e = {
            .name = 0, // unnamed
            .type = t_map,
        };
        *entry = e;
        return 0;
    }
    return -1;
}
int map_led(struct tag_u32 *req) {
    return handle_tag_u32_map_dynamic(req, map_led_op, map_led_entry, NULL);
}

void ledstrip_progress(uint32_t scale0, uint32_t value) {
    //infof("progress %d %d\n", scale0, value);

    /* Don't allow division by zero. */
    uint32_t scale = scale0 >= 1 ? scale0 : 1;
    /* 0 maps to 0, value == scale maps to LEDSTRIP_NB_LEDS */
    uint32_t cutoff = (value * ARRAY_SIZE(frame)) / scale;

    // Ignore scale for now
    struct grb red   = { .r = 10 };
    struct grb green = { .g = 10 };
    FOR_ARRAY(frame, led) {
        if (led - frame < cutoff) {
            *led = green;
        }
        else {
            *led = red;
        }
    }
    ledstrip_send_frame();
}

int handle_led_progress(struct tag_u32 *req) {
    /* Scale comes first to make currying easier. */
    TAG_U32_UNPACK(req, 0, m, scale, value) {
        ledstrip_progress(m->scale, m->value);
        return reply_ok(req);
    }
    return -1;
}

#include "mod_map_forth.c"

/* root map */
int map_root(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"led",      t_map, map_led},
        {"progress", t_cmd, handle_led_progress, 2},
        {"forth",    t_map, map_forth},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}

/* Protocol handler entry point. */
int handle_tag_u32(struct tag_u32 *req) {
    return map_root(req);
}
