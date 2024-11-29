WDC02AS -G -L mon02.asm
WDCLN -HB -g -t mon02.obj -o aaa.bin
rem bin2mot -L1200 -IEE00 aaa.bin aaa.s
bin2mot -L2000 -IE000 aaa.bin aaa.s
mot2bin aaa.s MON02.SYS
del aaa.*

