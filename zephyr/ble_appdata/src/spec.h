#ifndef SPEC_H
#define SPEC_H

// LICENCE: Apache-2.0

#include <stdint.h>

/* Example specification file for services and characteristics. */
typedef float float32_t;

/* Combinations of capabilities are implemented explicitly.  Currently
   these 4 combinations are supported: RW,R,RN,RWN.

   R: read
   W: write
   N: notify

   Indications are currently not supported.
*/

#define UUID_MIDI \
    UUID_128(0x03, 0xB8, 0x0E, 0x5A, \
             0xED, 0xE8, \
             0x4B, 0x33, \
             0xA7, 0x51, \
             0x6C, 0xE3, 0xE, 0xC4, 0xC7, 0x00)


// C Name  | Description             | UUID              | Data Type | Capabilities | Notes
//-----------------------------------------------------------------------------------------
#define FOR_CHARACTERISTICS_SERV1(C) \
C(char11,    "Serv1, Char1",           UUID_16(0xFF11),    uint8_t,    RW,            "") \
C(char12,    "Serv1, Char2",           UUID_16(0xFF12),    uint16_t,   R,             "") \
C(char13,    "Serv1, Char3",           UUID_16(0xFF13),    float32_t,  RN,            "") \

#define FOR_CHARACTERISTICS_SERV2(C) \
C(char21,    "Serv2, Char1",           UUID_16(0xFF21),    int32_t,    RWN,           "") \

// C Name  | Description  | UUID             | Characteristics
//-------------------------------------------------------------------------
#define FOR_SERVICES(S) \
S(serv1,    "Service 1",    UUID_MIDI,         FOR_CHARACTERISTICS_SERV1) \
S(serv2,    "Service 2",    UUID_16(0xFF20),   FOR_CHARACTERISTICS_SERV2) \


#endif
