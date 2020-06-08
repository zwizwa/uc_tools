#include "feynman.h"

#include <stdio.h>
#include <stdint.h>
#include <math.h>





#define N 32

uint32_t table[N];

double log2(double x) {
    return log(x)/log(2.0);
}
#define SCALE(shift) ((double)(0x100000000ULL >> shift))
double log2_(double x) { /* x < 0 */
    double arg = x * SCALE(0);
    uint32_t log = feynman_nlog_5_27(table, N, arg);
    return -((double)log) / SCALE(5);
}

void init_table(void) {
    printf("/* 0.32 bit fixed point [0,1[ */\n");
    printf("#define FEYNMAN_TABLE_INIT \\\n");
    for (int k=1; k<=N; k++) {
        table[k-1] = (uint32_t)(0x100000000*log2((double)(1.0+pow(2,-k))));
        printf("    /* 1+2^-%d */ [%d] = %u,\\\n", k, k-1, table[k-1]);
    }
}

void test(double x) {
    //double scaled_x = ((double)0x100000000);
    printf("%f %f %f\n", x, log2(x), log2_(x));
}

void at_limit(uint32_t arg) {
    uint32_t log = feynman_nlog_5_27(table, N, arg);
    double limit = -((double)log) / SCALE(5);
    printf("limit %d -> %ff\n", arg, limit);
}

int main(void) {
    printf("feynman\n");
    init_table();
    for (double d = 0.0000000001; d < 1.0; d *= 1.123456789) {
        test(d);
    }

    /* Also test the extreme points. */
    at_limit(0);
    at_limit(1);
    at_limit(2);
    at_limit(3);

    /* This is a ball park figure: 20Hz-20kHz measured with 72MHz
       cycle counter.  Wich is interestingly smack int the middle of
       the useful range. */
    at_limit(72000000 / 20000);
    at_limit(72000000 / 20);
}
