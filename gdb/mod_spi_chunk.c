#ifndef MOD_SPI_CHUNK
#define MOD_SPI_CHUNK

/* Chunked SPI transfer.

   This was factored out from the WS2812 Neopixel driver.  It is
   likely useful for all kinds of "on the fly" bit pattern
   generation. */

#ifndef SPI_CHUNK_DIV
#define SPI_CHUNK_DIV SPI_CR1_BAUDRATE_FPCLK_DIV_32
#endif


/* Note that the protocol is 5V, and A7 is not 5V tolerant.  I'm using
   74HC14 inverter with Schmitt-trigger inputs (2x) to adapt. */


//          rcc_gpio   rcc_spi   spi   rst       gpio    data clk master tx  hw_dma      ie  bits mode        div
// -----------------------------------------------------------------------------------------------------------------------------------------
const struct hw_spi hw_spi_tx8_master_0rw[] = {
    [1] = { RCC_GPIOA, RCC_SPI1, SPI1, RST_SPI1, GPIOA,  7,   5,  1,     1,  HW_DMA_1_3,  1,  8,  HW_SPI_0RW, SPI_CHUNK_DIV},
};
#ifndef SPI_CHUNK_C_SPI
#define SPI_CHUNK_C_SPI hw_spi_tx8_master_0rw[1]
#endif

/* Note that currently the state of the data line is still
   unpredictable (not driven?) during the hw_spi_reset(), so be
   careful using this as a bit stream synthesizer. */
static int spi_chunk_spi_initialized;
void spi_chunk_send_dma(const uint8_t *buf, uint32_t nb_bytes) {
    if (!spi_chunk_spi_initialized) {
        spi_chunk_spi_initialized = 1;
        hw_spi_reset(SPI_CHUNK_C_SPI);
        hw_spi_start(SPI_CHUNK_C_SPI, buf, nb_bytes);
    }
    else {
        hw_spi_next(SPI_CHUNK_C_SPI, buf, nb_bytes);
    }
}


// it's not spi1_isr!
void dma1_channel3_isr(void) {
    hw_spi_ack(SPI_CHUNK_C_SPI);
#ifdef SPI_CHUNK_NEXT
    SPI_CHUNK_NEXT();
#else
#warn Need SPI_CHUNK_NEXT
#endif
}

void spi_chunk_init(void) {
    hw_spi_init(SPI_CHUNK_C_SPI);
}

#endif
