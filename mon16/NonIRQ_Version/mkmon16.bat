WDC02AS -G -L mon16.asm
WDCLN -HB -g -t mon16.obj -o aaa.bin
bin2mot -L1300 -IED00 aaa.bin aaa.s
mot2bin aaa.s MON16.SYS
del aaa.*

