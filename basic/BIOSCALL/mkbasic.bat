WDC02AS -G -L basic_bios.asm
WDCLN -HB -g -t basic_bios.obj -o aaa.bin
bin2mot -L2900 -IC000 aaa.bin aaa.s
mot2bin aaa.s BASIC65.B02
copy BASIC65.B02 ..\..\DISKS\.
del aaa.*
