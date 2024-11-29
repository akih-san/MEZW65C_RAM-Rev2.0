WDC02AS -G -L test.asm
WDCLN -HB -g -t test.obj -o aaa.bin
rem bin2mot -L1200 -IEE00 aaa.bin aaa.s
bin2mot -L0080 -I4000 aaa.bin aaa.s
mot2bin aaa.s test.bin
del aaa.*

