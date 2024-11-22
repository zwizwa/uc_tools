idf_component_register(SRCS "esp32s3_main.c"
                    PRIV_REQUIRES spi_flash
                    REQUIRES nvs_flash
                    REQUIRES esp_wifi
                    REQUIRES app_update
                    REQUIRES usb
                    INCLUDE_DIRS "")
