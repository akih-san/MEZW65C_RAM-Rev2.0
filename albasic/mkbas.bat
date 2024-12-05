WDC816AS -G -L .\albasic.asm
WDCLN -HB -g -t albasic.obj -o aaa.bin
rem bin2mot -L2000 -IE000 aaa.bin aaa.s
bin2mot -L2000 -IA000 aaa.bin aaa.s
mot2bin aaa.s albasic.b02
del aaa.*

