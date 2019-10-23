#ifndef CRC_H
#define CRC_H

static inline uint32_t crc(uint32_t poly_residue, uint32_t crc_order,
                           uint32_t accu, const uint8_t *start, uint32_t len) {
    const uint8_t *endx = start + len;
    uint32_t high_bit = 1 << (crc_order-1);
    for(;start<endx;start++) {
        uint8_t byte = *start;
        for(int i = 7; i>=0; i--) {
        //for(int i = 0; i<=7; i++) {
            uint32_t bit = (byte >> i) & 1;
            if (accu & high_bit) {
                accu = ((accu << 1) | bit) ^ poly_residue;
            }
            else {
                accu = ((accu << 1) | bit);
            }
        }
    }
    return accu;
}
static inline uint32_t crc_ethernet(const void *start, uint32_t len) {
    return crc(0x04C11DB7UL, 32,
               0, //
               start, len);
}


#endif
