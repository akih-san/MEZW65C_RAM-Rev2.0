WDC02AS -G -L basic.asm
WDCLN -HB -g -t basic.obj -o aaa.bin
bin2mot -L2900 -ID700 aaa.bin aaa.s
mot2bin aaa.s BASIC65.BIN
copy BASIC65.BIN ..\..\DISKS\STDALONE\.
del aaa.*
