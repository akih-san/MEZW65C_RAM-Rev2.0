WDC02AS -G -L test.asm
WDCLN -HB -g -t test.obj -o aaa.bin
bin2mot -L0040 -I0200 aaa.bin test.s1
mot2bin test.s1 TEST.BIN
del aaa.*

