WDC02AS -G -L mon02.asm
WDCLN -HB -g -t mon02.obj -o aaa.bin
bin2mot -L1300 -IED00 aaa.bin aaa.s
mot2bin aaa.s MON02.SYS
copy MON02.SYS ..\DISKS\.
del aaa.*

