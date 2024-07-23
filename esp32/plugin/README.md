ESP32 plugin

Idea is to use the 3if monitor to load code into free sections of IRAM
and DRAM, e.g. at the end of the memory range.

It is necessary to split it up into two binaries because IRAM only
supports 32-bit aligned data bus access.





