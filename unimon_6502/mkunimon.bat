WDC02AS -G -L unimon_6502.asm
WDCLN -HB -g -t unimon_6502.obj
bin2mot -L1100 -IEF00 unimon_6502.bin aaa.s
mot2bin aaa.s UMON_W65.BIN
copy UMON_W65.BIN ..\DISKS\STDALONE\.
del aaa.*

