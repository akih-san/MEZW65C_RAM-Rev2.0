WDC02AS -G -L test16.asm
WDCLN -HB -g -t test16.obj -o aaa.bin
bin2mot -L0100 -I0400 aaa.bin test16.s1
mot2bin test16.s1 TEST16.BIN
del aaa.*

