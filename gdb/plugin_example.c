#include "base.h"

// Defined in doodle.c
void set_pin(int pin, int val);


KEEP int add1(int a) {
    return a+1;
}
KEEP void set_pin_delegate(int pin, int val) {
    set_pin(pin, val);
}
