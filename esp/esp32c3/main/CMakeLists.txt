idf_component_register(SRCS "esp32c3_main.c"
                    PRIV_REQUIRES spi_flash
                    REQUIRES nvs_flash
                    REQUIRES esp_wifi
                    REQUIRES app_update
                    INCLUDE_DIRS "")
