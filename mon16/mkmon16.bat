WDC816AS -G -L mon16.asm
WDCLN -HB -g -t mon16.obj -o aaa.bin
bin2mot -L1400 -IEC00 aaa.bin aaa.s
mot2bin aaa.s MON16.SYS
copy MON16.SYS ..\DISKS\.
del aaa.*

