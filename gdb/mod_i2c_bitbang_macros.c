#ifndef MOD_I2C_MACROS
#define MOD_I2C_MACROS

/* Moving some code from mod_i2c_bitbang to here. */

/* First, what does this express?

   It contains all the code needed or bit-banged I2C implementation,
   expressed as macros.

   It does *not* contain the definition of the state structs for
   these.

   What problem is this solving?

   I have come to the conclusion that in most practical cases the sm.h
   nested state machine approach is too complicated, especially in
   setups like the i2c bitbang where the nesting goes deep, but the
   blocking points are only present in the inner loop.

   I want to simplify things such that the need for nested state
   machines goes away.  One way to do this is to implement the nesting
   (e.g. nested loops) as macros, and have the containing protothread
   define the state members.  This way the logic can be separated from
   the implementation.

*/

/* These are expected to be available in the C namespace. */
struct i2c_port;
static inline void i2c_write_sda(struct i2c_port *p, int v);
static inline void i2c_write_scl(struct i2c_port *p, int v);
static inline int  i2c_read_sda(struct i2c_port *p);
static inline int  i2c_read_scl(struct i2c_port *p);



#ifndef I2C_STRETCH
#define I2C_STRETCH 1
#endif

#define I2C_DEBUG_SPACING 0

#define I2C_WAIT_STRETCH(s) {                                           \
        /* PRE: SCL released */                                         \
        if (I2C_STRETCH) I2C_WHILE(s, !i2c_read_scl(s->port));          \
    }
#define I2C_WRITE_SCL_1(s) {                     \
        i2c_write_scl(s->port,1);                \
        I2C_WAIT_STRETCH(s);                     \
    }


#define I2C_DELAY(s) {                                                  \
        s->timeout = cycle_counter_future_time(I2C_HALF_PERIOD_TICKS);  \
        I2C_WAIT(s, cycle_counter_expired(s->timeout));                  \
    }
#define I2C_NDELAY(s,n_init) \
    {int n = n_init; while(n--) I2C_DELAY(s); }

#if I2C_DEBUG_SPACING
#define I2C_SPACING_NDELAY(s, n) I2C_NDELAY(s, n);
#else
#define I2C_SPACING_NDELAY(s, n)
#endif




/* FIXME: The deblock warning events should be logged or at least
   counted.  At the moment it is not clear how to integrate warnings
   into the rest of the application framework.  Maybe treat deblog as
   an error on start that is upstream interpreted as warning: can
   continue, but log. */

#define I2C_DEBLOCK(s) {                                                \
        for(s->clock=0; s->clock<16; s->clock++) {                      \
            int sda1 = i2c_read_sda(s->port);                           \
            int scl1 = i2c_read_scl(s->port);                           \
            if (sda1 && scl1) break;                                    \
            i2c_write_scl(s->port,0); I2C_DELAY(s);                     \
            i2c_write_scl(s->port,1); I2C_DELAY(s);  /* no stretch */   \
        }                                                               \
        if (s->clock) {                                                 \
            I2C_LOG("m: WARNING: deblock clocks=%d\n", s->clock);       \
            /* LOG(" (%d)", s->clock); */                               \
        }                                                               \
    }

#define I2C_START(s) {                                                  \
        /* PRE: */                                                      \
        /* - idle:   SDA=1,SCL=1 */                                     \
        /* - repeat: SDA=?,SCL=0 */                                     \
        /* For repeated start: bring lines high without causing a stop. */ \
        i2c_write_sda(s->port,1); I2C_DELAY(s);                          \
        I2C_WRITE_SCL_1(s); I2C_DELAY(s);                               \
        /* START transition = SDA 1->0 while SCL=1 */                   \
        i2c_write_sda(s->port,0); I2C_DELAY(s);                         \
        /* Bring clock line low for first bit */                        \
        i2c_write_scl(s->port,0); I2C_DELAY(s);                         \
        /* POST: SDA=0, SCL=0 */                                        \
}


#define I2C_SEND_BIT(s, val) {  \
        /* PRE: SDA=?, SCL=0 */                                         \
        i2c_write_sda(s->port,val); I2C_DELAY(s);  /* set + propagate */ \
        I2C_WRITE_SCL_1(s);        I2C_DELAY(s);  /* set + wait read */ \
        i2c_write_scl(s->port,0);                                       \
        /* POST: SDA=x, SCL=0 */                                        \
}

#define I2C_RECV_BIT(s, lval) {                                         \
        /* PRE: SDA=?, SCL=0 */                                         \
        i2c_write_sda(s->port,1); I2C_DELAY(s);  /* release, allow slave write */ \
        I2C_WRITE_SCL_1(s); I2C_DELAY(s);  /* slave write propagation */ \
        lval = i2c_read_sda(s->port);                                   \
        i2c_write_scl(s->port,0);                                       \
        /* POST: SDA=1, SCL=0 */                                        \
    }

#define I2C_STOP(s) {                                     \
        /* PRE: SDA=?, SCL=0 */                           \
        i2c_write_sda(s->port,0); I2C_DELAY(s);           \
        I2C_WRITE_SCL_1(s); I2C_DELAY(s);                 \
        /* STOP transition = SDA 0->1 while SCL=1 */      \
        i2c_write_sda(s->port,1);  I2C_DELAY(s);          \
        /* POST: SDA=1, SCL=1  (unless held by slave) */  \
}



/* Returns 0 on correct ack, 1 on nack. */
#define I2C_SEND_BYTE(s) {                              \
        for (s->clock=7; s->clock>=0; s->clock--) {     \
            I2C_SEND_BIT(s, (s->byte >> s->clock)&1);   \
        }                                               \
        I2C_SPACING_NDELAY(s, 2);                       \
        I2C_RECV_BIT(s, s->nack);                       \
    }

#define I2C_RECV_BYTE(s) {                              \
        for (s->clock=7; s->clock>=0; s->clock--) {     \
            I2C_RECV_BIT(s, s->bitval);                 \
            s->val |= s->bitval << s->clock;            \
        }                                               \
        I2C_SPACING_NDELAY(s, 2);                       \
        I2C_SEND_BIT(s, s->nack);                       \
    }


/* Byte level code. */




#endif