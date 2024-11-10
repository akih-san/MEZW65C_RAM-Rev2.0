# MEZW65C_RAM file header

| offset |  サイズ	| 設定内容					| ファイルタイプ			| 補足
|--------|----------|---------------------------|-----------------------|-----------------
|     0  |  バイト	| $4C						| standalone（独立型）	| jump オペコード
|        |			| program bank				| BIOS CALL (W65C816)	|
|        |			| 0							| BIOS CALL (W65C02)	|
|--------|----------|---------------------------|-----------------------|-----------------
|     1  |  ワード	|コールドスタートアドレス	| standalone/BIOS CALL	|
|     3  |  バイト	| $4C						| standalone（独立型）	| jump オペコード
|        |			| program bank				| BIOS CALL (W65C816)	|
|        |			| 0							| BIOS CALL (W65C02)	|

    db  USER_DB     ; data bank:W65C816, 0:W65C02
    dw  WSTART

    dw  USER_DP     ; DP

mezID:  db  "MEZW65C",0 ; Unique ID

start_p:    ;file load address
    dw  PRG_B       ; load address (Low)
    db  USER_PB     ; PBR : program bank(W65C816)
    db  0       ; reserve

    ; define Common memory address
PIC_IF: dw  0   ; reserve
    dw  0   ; reserve

SW_816: db  1   ; 0 : W65C02
            ; 1 : W65C816 native mode 
            ; 2 : works in both modes
irq_sw  db  0   ; 0 : no use IRQ console I/O
            ; 1 : use IRQ timer interrupt driven console I/O
reg_tp  dw  0   ; monitor reserve (register save pointer)
reg_ts  dw  0   ; monitor reserve (register table size)
nmi_sw  db  0   ; 0 : No NMI support, 1: NMI support
bios_sw db  1   ; 0 : standalone program
            ; 1 : program call bios command
            ; 2 : monitor program (.SYS)
COLD_START:
;--------- MEZW65C_RAM file header --------------------------