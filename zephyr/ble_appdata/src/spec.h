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



// C Name  | Description             | Characteristic | Data Type | Capabilities | Notes
//---------------------------------------------------------------------------------------------------------------------------------------------------------------
#define FOR_CHARACTERISTICS_SERV1(C) \
C(char11,    "Serv1, Char1",           FF11,            uint8_t,    RW,           "") \
C(char12,    "Serv1, Char2",           FF12,            uint16_t,   R,            "") \
C(char13,    "Serv1, Char3",           FF13,            float32_t,  RN,           "") \

#define FOR_CHARACTERISTICS_SERV2(C) \
C(char21,    "Serv2, Char1",           FF21,            int32_t,    RWN,          "") \

// C Name  | Description  | UUID    | Characteristics
//---------------------------------------------------
#define FOR_SERVICES(S) \
S(serv1,    "Service 1",    FF10,     FOR_CHARACTERISTICS_SERV1) \
S(serv2,    "Service 2",    FF20,     FOR_CHARACTERISTICS_SERV2) \


#endif
