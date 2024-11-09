WDC02AS -G -L unimon_816.asm
WDCLN -HB -g -t unimon_816.obj -o aaa.bin
bin2mot -L1300 -IED00 aaa.bin aaa.s
mot2bin aaa.s UMON_816.BIN
copy UMON_816.BIN ..\DISKS\STDALONE\.
del aaa.*

