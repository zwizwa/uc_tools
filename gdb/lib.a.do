C="
cdcacm_desc.c
gdbstub.c
memory.c
rsp_packet.c
vector.c
bootloader.c
hw_bootloader.c
sm_etf.c
../infof.c
../tools.c
info_null.c
"
O=$(echo $C | sed s/\\.c/\\.o/g)
redo-ifchange $O
ar -r $3 $O 2>/dev/null




