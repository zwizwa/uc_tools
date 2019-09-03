#ifndef MDIO_H
#define MDIO_H

#include <stdint.h>

/* Defined by application. */
int mdio_get_data(void);
void mdio_set_data(int v);
void mdio_set_clock(int v);
void mdio_set_dir(int d);
void mdio_delay(void);

/* Exported */
uint16_t mdio_read(uint8_t phy, uint8_t reg);
void mdio_write(uint8_t phy, uint8_t reg, uint16_t val);


/* See section 6.6.1 in DP83848 data sheet. */




#endif
