/* Simple A/V core test: oled display, pwm audio. */

#define PRODUCT "OLED+PWM test"
#include "mod_lab.c"
#include "mod_console.c"
#include "mod_map_forth.c"

#include "playback.h"

/* Use modules with defaults. */
#include "mod_oled.c"



void to_hex_u32(char *buf, uint32_t val) {
    const char hex[] = "0123456789abcdef";
    for (int i=0; i<8; i++) {
        buf[i] = hex[0xF & (val >> ((7-i)*4))];
    }
}
#define XORSHIFT_STATIC 1
#include "xorshift.h"

/* TODO: Generate a display menu on the fly from a pointer into a data
   structure, instead of drawing it first to a frame buffer.  Why does
   this matter?  It will render only when needed.
*/


void animation_poll(void) {
    char buf[10] = {};
    if (video_sync()) {
        // infof("sync\n");
        if (1) {
            if (0) {
                int last = 0;
                int cur = 0;
                for (int i=0; i<sizeof(oled_fb); i+=2) {
                    cur = !!(*(int8_t *)(0x20000000 + i) & 5);
                    uint8_t v;
                    if (last != cur) v = 0b01111110;
                    else if (cur)    v = 0b00000010;
                    else             v = 0b01000000;
                    oled_fb[i] = v;
                    if (cur)         v = 0b00000010;
                    else             v = 0b01000000;
                    oled_fb[i+1] = v;
                    last = cur;
                }
            }
            if (1) {
                static int skip = 0;
                if (!skip--) {
                    skip = 0; // random_u32() & 0x7;
                    uint32_t *fb = (void*)oled_fb;
                    for (int i=0; i<sizeof(oled_fb)/sizeof(*fb); i++) {
                        fb[i] = random_u32();
                    }
                }
            }
            oled_send_fb();
            to_hex_u32(buf, video_pulse); oled_send_text(1,10,buf);
        }
        else {
            oled_send_text(0,0,"hello friend");
            to_hex_u32(buf, video_pulse); oled_send_text(1,10,buf);
            to_hex_u32(buf, audio_pulse); oled_send_text(2,10,buf);
        }
    }
}




/* ADC */
void adc_init(void) {
}


void app_poll(void) {
    animation_poll();
}

instance_status_t app_init(instance_init_t *ctx) {
    infof("product: %s\n", PRODUCT);
    _service.add(app_poll);

    /* OLED driver */
    oled_init();
    oled_clear();
    audio_init();
    adc_init();


    scan_samples((void*)0x8008000);
    play_sample(1);

    infof("BOOT1=%d\n", hw_gpio_read(GPIOB,2));
    infof("MS_PER_TICK=%d\n", MS_PER_TICK);
    infof("TIM_AUDIO: pri=%d\n", NVIC_IPR(C_AUDIO.irq));
    return 0;
}
DEF_INSTANCE(app);

int map_root(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"forth", "map", map_forth},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}
int handle_tag_u32(struct tag_u32 *req) {
    int rv = map_root(req);
    if (rv) {
        infof("handle_tag_u32 returned %d\n", rv);
        /* Always send a reply when there is a from address. */
        send_reply_tag_u32_status_cstring(req, 1, "bad_ref");
    }
    return 0;
}
