WDC02AS -G -L prt_msg.asm
WDCLN -HB -g -t prt_msg.obj -o aaa.bin
bin2mot -L0100 -I0400 aaa.bin prt_msg.s1
mot2bin prt_msg.s1 PRT_MSG.B02
del aaa.*
