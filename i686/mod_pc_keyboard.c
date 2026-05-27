#ifndef MOD_PC_KEYBOARD
#define MOD_PC_KEYBOARD

/* This is a mod because of the global const data involved. */

#define KBD_SHIFT  0x80
#define KBD_CTRL   0x81
#define KBD_ALT    0x82
#define KBD_CAPS   0x83

#define KBD_F1     0x90

const uint8_t pc_keyboard_normal[128] = {
    0,  27, '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', '\b',
    '\t', /* <-- Tab */
    'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']', '\r',
    KBD_CTRL, /* <-- control key */
    'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';', '\'', '`',  8,
    '\\', 'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/',  KBD_SHIFT,
    '*',
    KBD_ALT,  /* Alt */
    ' ',  /* Space bar */
    KBD_CAPS,  /* Caps lock */
    KBD_F1,  /* 59 - F1 key ... > */
    0,   0,   0,   0,   0,   0,   0,   0,
    0,  /* < ... F10 */
    0,  /* 69 - Num lock*/
    0,  /* Scroll Lock */
    0,  /* Home key */
    0,  /* Up Arrow */
    0,  /* Page Up */
    '-',
    0,  /* Left Arrow */
    0,
    0,  /* Right Arrow */
    '+',
    0,  /* 79 - End key*/
    0,  /* Down Arrow */
    0,  /* Page Down */
    0,  /* Insert Key */
    0,  /* Delete Key */
    0,   0,   0,
    0,  /* F11 Key */
    0,  /* F12 Key */
    0,  /* All other keys are undefined */
};

const uint8_t pc_keyboard_shifted[128] = {
    0,  27, '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', '\b',
    '\t', /* <-- Tab */
    'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '[', ']', '\r',
    KBD_CTRL, /* <-- control key */
    'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ';', '\'', '`',  8,
    '\\', 'Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?',  KBD_SHIFT,
    '*',
    KBD_ALT,  /* Alt */
    ' ',  /* Space bar */
    KBD_CAPS,  /* Caps lock */
    KBD_F1,  /* 59 - F1 key ... > */
    0,   0,   0,   0,   0,   0,   0,   0,
    0,  /* < ... F10 */
    0,  /* 69 - Num lock*/
    0,  /* Scroll Lock */
    0,  /* Home key */
    0,  /* Up Arrow */
    0,  /* Page Up */
    '-',
    0,  /* Left Arrow */
    0,
    0,  /* Right Arrow */
    '+',
    0,  /* 79 - End key*/
    0,  /* Down Arrow */
    0,  /* Page Down */
    0,  /* Insert Key */
    0,  /* Delete Key */
    0,   0,   0,
    0,  /* F11 Key */
    0,  /* F12 Key */
    0,  /* All other keys are undefined */
};

int pc_keyboard_shift = 0;

void reboot(void);
void f1(void);
void keyboard_input(uint8_t ascii);
static inline void pc_keyboard_scancode(uint8_t scancode) {
    int release = !!(scancode & 0x80);
    scancode &= 0x7F;

    uint8_t ascii =
        (pc_keyboard_shift ?
         pc_keyboard_shifted :
         pc_keyboard_normal)
        [scancode];

    if (release) {
        if (ascii == KBD_SHIFT) {
            pc_keyboard_shift = 0;
        }
    }
    else { // press
        // FIXME: locking / buffering?
        if (ascii == KBD_F1) {
            f1();
            // rtl8139_status();
        }
        else if (ascii == 27) {
            // Keyboard controller (8042) reset — pulse the CPU reset line
            // Some alternatives here:
            // https://claude.ai/chat/0be89304-9906-4b33-bf8c-f11e477fda0c
            reboot();
        }
        else if (ascii == KBD_SHIFT) {
            pc_keyboard_shift = 1;
        }
        else {
            // The encoding emulates a serial terminal.
            keyboard_input(ascii);
        }
    }
}

#endif
