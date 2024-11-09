WDC02AS -G -L prt_msg.asm
WDCLN -HB -g -t prt_msg.obj -o aaa.bin
bin2mot -L0063 -I0200 aaa.bin prt_msg.s1
mot2bin prt_msg.s1 PRT_MSG.BIN
del aaa.*
