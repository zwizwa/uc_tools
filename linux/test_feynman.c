#include "feynman.h"

#include <stdio.h>
#include <stdint.h>
#include <math.h>



#define N 32

uint32_t table[N];

double log2(double x) {
    return log(x)/log(2.0);
}
double log2_(double x) {
    double argument = x * 0x40000000;
    uint32_t log = feynman_log(table, N, argument);
    return ((double)log) / 0x100000000;
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


int main(void) {
    printf("feynman\n");
    init_table();
    for (int i=10; i<=20; i++) {
        test(((double)i) / 10);
    }

}
