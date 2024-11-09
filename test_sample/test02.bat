WDC02AS -G -L test02.asm
WDCLN -HB -g -t test02.obj -o aaa.bin
bin2mot -L0100 -I0400 aaa.bin test02.s1
mot2bin test02.s1 TEST02.BIN
del aaa.*

