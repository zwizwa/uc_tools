/* S-expression serializer module.
   references NS(_last_byte), NS(_byte)
*/
#ifndef NS
#error define NS
#endif

#ifndef NS_DUMP_COMMON
#define NS_DUMP_COMMON


// Erlang-style
#define DUMP_OPEN  '['
#define DUMP_CLOSE ']'
#define DUMP_SEP   ','
// Lisp-style
//#define DUMP_OPEN  '('
//#define DUMP_CLOSE ')'
//#define DUMP_SEP   ' '

#endif


static void NS(_bytes)(uint8_t *buf, uint32_t len) {
    int i = 0;
    while(i < len) {
        NS(_byte)(buf[i++]);
    }
    if (i < len) {
        LOG("dropping bytes %d %d\n", i, len);
    }
}
static void NS(_sep)(void) {
    switch(NS(_last_byte)()) {
    case 0:
    case DUMP_OPEN:
        return;
    default:
        NS(_byte)(DUMP_SEP);
    }
}
static void NS(_string)(const char *buf) {
    NS(_bytes)((void*)buf, strlen(buf));
}

static void NS(_number)(uint32_t n) {
    if (n == 0) {
        NS(_sep)();
        NS(_byte)('0');
        return;
    }
    int offset = 10;
    char buf[10];
    while(n) {
        buf[--offset] = '0' + n % 10;
        n = n / 10;
    }
    NS(_sep)();
    NS(_bytes)((void*)&buf[offset], sizeof(buf)-offset);
}
static void NS(_tag)(const char *str) {
    NS(_sep)();
    NS(_string)(str);
}
static void NS(_open)(const char *str) {
    NS(_sep)();
    NS(_byte)(DUMP_OPEN);
    if (str) NS(_tag)(str);
}
static void NS(_close)(void) {
    NS(_byte)(DUMP_CLOSE);
}


