;;;
;;; Universal Monitor 6502 Copyright (C) 2019 Haruo Asano
;;; https://electrelic.com/electrelic/node/1317
;;;
;;; -- original disassemble sorce code --
;;;-------------------------------------------------------------------------------
;:; https://github.com/andrew-jacobs/w65c816sxb-hacker
;;;
;;;   A program for Hacking your W65C265SXB or W65C816SXB
;;;   Copyright (C),2015-2018 Andrew Jacobs
;;;
;;;   All rights reserved.
;;;
;;;   This work is made available under the terms of the Creative Commons
;;;   Attribution-NonCommercial-ShareAlike 4.0 International license. Open the
;;;   following URL to see the details.
;;;
;;; http://creativecommons.org/licenses/by-nc-sa/4.0/
;;;-------------------------------------------------------------------------------
;;;
;;; This program is based on Universal Monitor 6502
;;; Programed by Akihito Honda. 2024.10
;;;
;;; Thanks all.
;;;

	pl	0
	pw      132
	chip    65816
;                inclist on
;;;
;;; MEZW65C_RAM Monitor for W65C186
;;;
	include "w65c816.inc"

;;;
;;; Memory
;;;

PRG_B	EQU	$ED00

UNIMON_DB	equ	0
UNIMON_DP	equ	PRG_B-256	; $EC00
STACK	EQU	UNIMON_DP-1		; $EBFF

USER_M	equ	$200
USER_S	equ	USER_M-1		; $01FF

COUT_SIZE	equ $80		; 128byte console output buffer
CIN_SIZE	equ COUT_SIZE

ZERO_B	EQU	$0018		; Must fit in ZERO page


BUFLEN	EQU	16		; Buffer length ( 16 or above )
cpu_id	equ	$00		; memory address $0000 is written CPU ID by PIC

; PIC function code

CONIN_REQ	EQU	$01
CONOUT_REQ	EQU	$02
CONST_REQ	EQU	$03
STROUT_REQ	equ	$04
STRIN_REQ	equ	$07
WUP_REQ		equ	$ff

;;; Constants
NULL	EQU	$00
SOH	equ	$01
EOT	equ	$04
ACK	equ	$06
BEL	equ	$07
BS	equ	$08
TAB	equ	$09
LF	equ	$0a
CR	equ	$0d
NAK	equ	$15
CAN	equ	$18
ESC	equ	$1b
DEL	equ	$7f

;--------------------------------------
;ZERO page
;--------------------------------------
	;;
	;; Work Area
	;;

	.page0
	ORG	ZERO_B

; PIC18F47QXX I/F
UREQ_COM	ds	1	; unimon CONIN/CONOUT request command
UNI_CHR		ds	1	; charcter (CONIN/CONOUT) or number of strings
CREQ_COM	ds	1	; unimon CONIN/CONOUT request command
CBI_CHR		ds	1	; charcter (CONIN/CONOUT) or number of strings
disk_drive	ds	1	;
disk_track	ds	2	;
disk_sector	ds	2	;
data_adr	ds	2	;
bank		ds	1	;
reserve		ds	1	;

INBUF	ds	BUFLEN		; Line input buffer
arg1st		ds	1
arg2nd		ds	2
arg3rd		ds	2
argtype		ds	1

; argtype pattern	123
;0  no parameter	000
;4 xx:			100
;6 xx:xxxx		110
;7 xx:xxxx,xxxx		111
;5 xx:,xxxx		101
;2 xxxx			010
;3 xxxx,xxxx		011
;1 ,xxxx		001

DSADDR	ds	2		; Dump start address
DEADDR	ds	2		; Dump end address
dumpdb	ds	1		; Dump data bank
DSTATE	ds	1		; Dump state
GADDR	ds	2		; Go address
SADDR	ds	2		; Set address
HEXMOD	ds	1		; HEX file mode
RECTYP	ds	1		; Record type

reg_tbls
REGA	ds	2		; Accumulator A
REGX	ds	2		; Index register X
REGY	ds	2		; Index register Y
REGSP	ds	2		; Stack pointer SP
REGPC	ds	2		; Program counter PC
REGPSR	ds	1		; Processor status register PSR
REGPB	ds	1		; Program Bank register
REGDB	ds	1		; Data Bank register
REGDP	ds	2		; Direct Page register
reg_tble
reg_size	equ reg_tble-reg_tbls

ILL_PC	ds	2

REGSIZ	ds	1		; Register size
	
DMPPT	ds	2
CKSUM	ds	1		; Checksum
HITMP	ds	1		; Temporary (used in HEXIN)

PT0	ds	2		; Generic Pointer 0
PT0_DB	ds	1
PT1	ds	2		; Generic Pointer 1
PT1_DB	ds	1
CNT	ds	1		; Generic Counter
bop_adr	ds	2
bop_bnk	ds	1

bcode	ds	1

; disassemble  value

lines	ds	1
FLAGS	ds	1	; Emulated processor flags
ADDR_S	ds	3	; Start address
ADDR_E	ds	3	; End address

;Go command  value
stp_flg		ds	1
sav_dat		ds	2
sv_adr		ds	2
sv_bnk		ds	1

; console interrupt handler string input count value
STRIN_CNT	ds	1


;;;
;;; Program area
;;;	
	.code
	ORG	PRG_B

CSTART:
;--------- MEZW65C_RAM file header --------------------------
	jmp	COLD_START
	jmp	WSTART

	; uinimon config data
	;
	dw	UNIMON_DP	; DP
	; Unique ID
mezID:	db	"MEZW65C",0

start_p:	; File load address(program start address)
	dw	PRG_B		; start address(16 bit)
pbr_p	db	0		; program bank
	db	0		; reserve
	; define Common memory address
PIC_IF:	dw	UNIMON_DP+UREQ_COM	;  Common memory address for PIC (Low)
	dw	0		; (high)

SW_816:	db	1	; 0 : W65C02
			; 1 : W65C816 native mode 
irq_sw	db	0	; 0 : no use IRQ console I/O
			; 1 : use IRQ timer interrupt driven console I/O
reg_tp	dw	UNIMON_DP+reg_tbls	; register save pointer
reg_ts	dw	reg_size		; register table size
nmi_sw	db	1	; 0 : No NMI support, 1: NMI support
bios_sw	db	2	; 0 : standalone program
			; 1 : program call bios command
			; 2 : monitor program (.SYS)
COLD_START:
;--------- MEZW65C_RAM file header --------------------------

; user program infomation pointer
u_sw	equ	mezID+0
u_addr	equ	mezID+1
u_pbp	equ	mezID+3
u_dbp	equ	mezID+4
u_dpp	equ	mezID+5


	sei			; disable interrupt
	native
	short_ai
	long_a
	LDA	#STACK
	TAS			; set sp

	lda	#UNIMON_DP
	pha
	pld			; set Direct Page

	short_a
	lda	#UNIMON_DB
	pha
	plb			; set DATA BANK

	JSR	INIT
	long_a
	LDA	#0
	STA	DSADDR
	STA	SADDR
	STA	GADDR

	short_a
	LDA	#'S'
	STA	HEXMOD
	stz	REGPB		; clear Program Bank
	stz	REGDB		; clear data Bank
	stz	stp_flg
	stz	dumpdb
	lda	#%00110100
	STA	REGPSR
	STA	FLAGS		; save disassemble EmPCR

	long_a
	stz	REGA
	stz	REGX
	stz	REGY
	stz	REGDP		; clear Direct Page

	LDA	#USER_S
	STA	REGSP
	lda	#USER_M
	STA	REGPC
	sta	ADDR_S
	stz	ADDR_E
	
	short_a

	lda	u_sw
	beq	wup_umon
	cmp	#1
	beq	go_uapl
;
; sleep moniotr
;
wup
	lda	#1
	sta	UNI_CHR		; sleep signal
	jsr	NMI_SIG
	bra	wup

;
; goto user program
;
go_uapl
	long_a
	LDA	#USER_S
	TAS			; set user stack (standerd 6502)
	ldx	u_pbp		; X : 8bit
	phx			; push PBR : 8bit
	lda	u_addr		; A : 16bit
	dec	a
	pha			; push PC : 16bit
	lda	u_dpp
	pha			; push DP
	short_a
	lda	u_dbp
	pha			; push DBR
	plb			; get user data bank
	pld			; get user direct page

; jump user program

	rtl			;return long

	;; Opening message
wup_umon
	long_a
	LDA	#OPNMSG
	STA	PT0
	JSR	STROUT

WSTART
	sei			; disable interrupt
	native
	short_ai
	long_a
	LDA	#STACK
	TAS			; set sp

	lda	#UNIMON_DP
	pha
	pld			; set Direct Page

	short_a
	lda	#UNIMON_DB
	pha
	plb			; set DATA BANK

	long_a
	LDA	#PROMPT
	STA	PT0
	JSR	STROUT
	JSR	GETLIN

	LDX	#0
	longa off
	lda	INBUF,x
	CMP	#0
	BEQ	WSTART

	CMP	#'D'
	BNE	M00
	JMP	DUMP
M00
	CMP	#'G'
	BNE	M01
	JMP	GO
M01
	CMP	#'S'
	BNE	M02
	JMP	SETM
M02
	CMP	#'L'
	BNE	M03
	JMP	LOADH
M03
	
	CMP	#'R'
	BNE	M05
	JMP	REG
M05	
	cmp	#'B'
	bne	M06
	inx
	LDA	INBUF,X
	JSR	UPPER
	CMP	#'Y'
	bne	ERR
	inx
	LDA	INBUF,X
	JSR	UPPER
	CMP	#'E'
	bne	ERR
	jsr	CRLF
	jmp	wup

M06
	CMP	#'?'
	BNE	ERR
	jmp	prt_help

ERR
	long_a
	LDA	#ERRMSG
	STA	PT0
	JSR	STROUT
	JMP	WSTART

; check string hex?
;
; X : pointer (check from X+1)
; return Z=1 : no hex
;        Z=0 : hex in CNT
chk_strhex
	longi off
	longa off
	INX
	JSR	RDHEX
	LDA	CNT
	rts

;
; command line parsing
;
arg_parsing
	longi off
	longa off
	stz	argtype		; set type = 0
	jsr	chk_strhex
	BNE	PP0		; jmp, if 1st arg. exist

	LDA	INBUF,X
	BNE	PP01		; jmp, if remain strings exist
PP00
	; no arg.
	rts			; type = 0
	
	
PP0	;; 1st arg. found

	LDA	INBUF,X
	cmp	#':'		; check bank
	bne	f_1st_adr
	
	lda	PT1
	sta	arg1st		; save 1st arg. (8bit)
	lda	#4
	sta	argtype		; type = 4
	jsr	chk_strhex
	beq	PP01

f_1st_adr
	long_a
	LDA	PT1
	sta	arg2nd		; save 2nd arg.
	short_a
	lda	argtype
	ora	#2		; type = 6 or 2
	sta	argtype

	; check 3rd arg. exist

PP01
	LDA	INBUF,X		; get next string
	CMP	#','
	BEQ	PP1		; jmp if 2nd parameter exist
	cmp	#0
	beq	PP00		; jmp if no 2nd parameter

D_ERR
	JMP	ERR

PP1	;; check 2nd arg.

	jsr	chk_strhex
	BEQ	D_ERR
	LDA	INBUF,X
	BNE	D_ERR

	;; set 2nd arg.

	long_a
	LDA	PT1
	STA	arg3rd
	short_a
	lda	argtype
	ora	#1		; type = 7 or 3 or 5 or 1
	sta	argtype
	rts

;;;
;;; Dump memory
;;;
ptt_db
	longi off
	longa off
	lda	dumpdb
	JSR	HEXOUT2
	lda	#':'
	jmp	CONOUT

put_dpl
	long_a
	lda	#dmplmsg
	sta	PT0
	jmp	STROUT

;;; Dump memory entry
DUMP
	longa off
	INX
	LDA	INBUF,X
	cmp	#'I'
	bne	dmp1
	jmp	disassemble
dmp1
	dex
	jsr	arg_parsing	; get line input & parsing data

	ldx	argtype
	txa
	and	#4
	beq	dmp3

	;; 1st arg. found
	LDA	arg1st
	STA	dumpdb		; set data bank

dmp3
	txa
	and	#2
	beq	dmp4
	
	;; set 2nd arg.
	long_a
	LDA	arg2nd
	STA	DSADDR		; set start address
	short_a

dmp4
	txa
	and	#1
	beq	dmp5

	;; set 3rd arg.
	long_a
	LDA	arg3rd
;	SEC
;	ADC	#0
	STA	DEADDR
	short_a

dmp5
	txa
	beq	endcalc
	cmp	#2
	beq	endcalc
	cmp	#4
	beq	endcalc
	cmp	#6
	bne	DPM

endcalc
	long_a
	LDA	DSADDR
	CLC
	ADC	#128
	STA	DEADDR

	;; DUMP main
DPM	
	short_a
	LDA	DSADDR
	AND	#$F0
	STA	PT1
	LDA	DSADDR+1
	STA	PT1+1
	LDA	#0
	STA	DSTATE
	jsr	put_dpl		;print dump index
DPM0
	JSR	DPL

	long_a
	LDA	PT1
	CLC
	ADC	#16
	STA	PT1
	short_a

	JSR	CONST
	BNE	DPM1
	LDA	DSTATE
	CMP	#2
	BCC	DPM0

	jsr	put_dpl		;print dump index
	long_a
	LDA	DEADDR
	STA	DSADDR
	short_a
	JMP	WSTART

DPM1
	JSR	CONIN
	jsr	put_dpl		;print dump index
	long_a
	LDA	PT1
	STA	DSADDR
	short_a
	JMP	WSTART

	;; Dump line
DPL
	;print bank
	jsr	ptt_db

	LDA	PT1+1
	JSR	HEXOUT2
	LDA	PT1
	JSR	HEXOUT2

	long_a
	LDA	#DSEP0
	STA	PT0
	JSR	STROUT
	LDX	#0
	LDY	#0
DPL0
	JSR	DPB
	CPX	#16
	BNE	DPL0

	long_a
	LDA	#DSEP1
	STA	PT0
	JSR	STROUT

	;; Print ASCII area
	LDX	#0
DPL1
	longa off
	LDA	INBUF,X
	CMP	#' '
	BCC	DPL2
	CMP	#$7F
	BCS	DPL2
	JSR	CONOUT
	JMP	DPL3
DPL2
	LDA	#'.'
	JSR	CONOUT
DPL3
	INX
	CPX	#16
	BNE	DPL1
	JMP	CRLF

	;; Dump byte
DPB
	LDA	#' '
	JSR	CONOUT
	LDA	DSTATE
	BNE	DPB2
	;; Dump state 0
	TYA
	SEC
	SBC	DSADDR
	AND	#$0F
	BEQ	DPB1
	;; Still 0 or 2
DPB0
	LDA	#' '
	STA	INBUF,X
	JSR	CONOUT
	LDA	#' '
	JSR	CONOUT
	INX
	INY
	RTS
	;; Found start address
DPB1
	LDA	#1
	STA	DSTATE
DPB2
	LDA	DSTATE
	CMP	#1
	BNE	DPB0

	;; Dump state 1

	; get a dump data-----------
	phb			; push DBR
	lda	dumpdb
	pha
	plb			; set Dump Data Bank
	LDA	(PT1),Y		; get Dump data
	plb			; pop DBR
	; ---------------------------

	STA	INBUF,X
	JSR	HEXOUT2
	INX
	INY
	TYA
	CLC
	ADC	PT1
	STA	PT0
	LDA	PT1+1
	ADC	#0
	STA	PT0+1
	LDA	PT0
	CMP	DEADDR
	BNE	DPBE
	LDA	PT0+1
	CMP	DEADDR+1
	BNE	DPBE
	;; Found end address
	LDA	#2
	STA	DSTATE
DPBE
	RTS

;++++++++++++++++++++++++++++++++++++++++++++++++++++++
;
; disassemble 
;
;++++++++++++++++++++++++++++++++++++++++++++++++++++++

OP_ADC	equ	0<<1
OP_AND	equ	1<<1
OP_ASL	equ	2<<1
OP_BCC	equ	3<<1
OP_BCS	equ	4<<1
OP_BEQ	equ	5<<1
OP_BIT	equ	6<<1
OP_BMI	equ	7<<1
OP_BNE	equ	8<<1
OP_BPL	equ	9<<1
OP_BRA	equ	10<<1
OP_BRK	equ	11<<1
OP_BRL	equ	12<<1
OP_BVC	equ	13<<1
OP_BVS	equ	14<<1
OP_CLC	equ	15<<1
OP_CLD	equ	16<<1
OP_CLI	equ	17<<1
OP_CLV	equ	18<<1
OP_CMP	equ	19<<1
OP_COP	equ	20<<1
OP_CPX	equ	21<<1
OP_CPY	equ	22<<1
OP_DEC	equ	23<<1
OP_DEX	equ	24<<1
OP_DEY	equ	25<<1
OP_EOR	equ	26<<1
OP_INC	equ	27<<1
OP_INX	equ	28<<1
OP_INY	equ	29<<1
OP_JML	equ	30<<1
OP_JMP	equ	31<<1
OP_JSL	equ	32<<1
OP_JSR	equ	33<<1
OP_LDA	equ	34<<1
OP_LDX	equ	35<<1
OP_LDY	equ	36<<1
OP_LSR	equ	37<<1
OP_MVN	equ	38<<1
OP_MVP	equ	39<<1
OP_NOP	equ	40<<1
OP_ORA	equ	41<<1
OP_PEA	equ	42<<1
OP_PEI	equ	43<<1
OP_PER	equ	44<<1
OP_PHA	equ	45<<1
OP_PHB	equ	46<<1
OP_PHD	equ	47<<1
OP_PHK	equ	48<<1
OP_PHP	equ	49<<1
OP_PHX	equ	50<<1
OP_PHY	equ	51<<1
OP_PLA	equ	52<<1
OP_PLB	equ	53<<1
OP_PLD	equ	54<<1
OP_PLP	equ	55<<1
OP_PLX	equ	56<<1
OP_PLY	equ	57<<1
OP_REP	equ	58<<1
OP_ROL	equ	59<<1
OP_ROR	equ	60<<1
OP_RTI	equ	61<<1
OP_RTL	equ	62<<1
OP_RTS	equ	63<<1
OP_SBC	equ	64<<1
OP_SEC	equ	65<<1
OP_SED	equ	66<<1
OP_SEI	equ	67<<1
OP_SEP	equ	68<<1
OP_STA	equ	69<<1
OP_STP	equ	70<<1
OP_STX	equ	71<<1
OP_STY	equ	72<<1
OP_STZ	equ	73<<1
OP_TAX	equ	74<<1
OP_TAY	equ	75<<1
OP_TCD	equ	76<<1
OP_TCS	equ	77<<1
OP_TDC	equ	78<<1
OP_TRB	equ	79<<1
OP_TSB	equ	80<<1
OP_TSC	equ	81<<1
OP_TSX	equ	82<<1
OP_TXA	equ	83<<1
OP_TXS	equ	84<<1
OP_TXY	equ	85<<1
OP_TYA	equ	86<<1
OP_TYX	equ	87<<1
OP_WAI	equ	88<<1
OP_WDM	equ	89<<1
OP_XBA	equ	90<<1
OP_XCE	equ	91<<1

MD_ABS	equ	0<<1	; a
MD_ACC	equ	1<<1	; A
MD_ABX	equ	2<<1	; a,x
MD_ABY	equ	3<<1	; a,y
MD_ALG	equ	4<<1	; al
MD_ALX	equ	5<<1	; al,x
MD_AIN	equ	6<<1	; (a)
MD_AIX	equ	7<<1	; (a,x)
MD_DPG	equ	8<<1	; d
MD_STK	equ	9<<1	; d,s
MD_DPX	equ	10<<1	; d,x
MD_DPY	equ	11<<1	; d,x
MD_DIN	equ	12<<1	; (d)
MD_DLI	equ	13<<1	; [d]
MD_SKY	equ	14<<1	; (d,s),y
MD_DIX	equ	15<<1	; (d,x)
MD_DIY	equ	16<<1	; (d),y
MD_DLY	equ	17<<1	; [d],y
MD_IMP	equ	18<<1	;
MD_REL	equ	19<<1	; r
MD_RLG	equ	20<<1	; rl
MD_MOV	equ	21<<1	; xyc
MD_IMM	equ	22<<1	; # (A or M)
MD_INT	equ	23<<1	; # (BRK/COP/WDM)
MD_IMX	equ	24<<1	; # (X or Y)

MD_PEA	equ	25<<1	; Push Effective Absolute Address
MD_PEI	equ	26<<1	; Push Effective Indirect Address
MD_PER	equ	27<<1	; Push effective PC Relative Indirect Address

; disassemble code

disassemble:
	longi off
	longa off
	jsr	arg_parsing
	ldx	argtype
	txa
	and	#4
	beq	dis3

	;; 1st arg. found
	LDA	arg1st
	STA	dumpdb		; set data bank

dis3
	txa
	and	#2
	beq	dis4
	
	;; set 2nd arg.
	long_a
	LDA	arg2nd
	sta	ADDR_S		; save start address
	short_a

dis4
	txa
	and	#1
	beq	dis5

	;; set 3rd arg.
	long_a
	LDA	arg3rd
	STA	ADDR_E
	short_a
	stz	lines

dis5
	txa
	beq	dis6
	cmp	#2	; ex) dixxxx
	beq	dis6
	cmp	#4	; ex) dixx:
	beq	dis6
	cmp	#6	; ex) dixx:xxxx
	bne	dis7	; jmp if argtype = 1(di,xxxx),3(dixxxx,xxxx)
			;                  5(dixx:,xxxx),7(dixx:xxxx,xxxx)
	; no arg.
dis6	; set lines = 16

	lda	#16
	sta	lines

dis7
	lda	dumpdb
	sta	ADDR_S+2		;set bank
	sta	ADDR_E+2		;set bank

;	php
;	pla
;	sta	FLAGS

dis_main:
	jsr	CRLF
	JSR	CONST
	BNE	dis_end

	jsr	TxSpace
	lda	ADDR_S+2		; Show memory address
	jsr	HEXOUT2
	lda	#':'
	jsr	CONOUT
	lda	ADDR_S+1
	jsr	HEXOUT2
	lda	ADDR_S+0
	jsr	HEXOUT2
	jsr	TxSpace

	jsr	TxCodeBytes		; Show code bytes
	jsr	TxSymbolic		; And instruction

	lda	[ADDR_S]		; Fetch opcode again

	pha
	ldy	#1

	cmp	#$18			; CLC?
	bne	NotCLC
	lda	#C_FLAG
	bra	DoREP
NotCLC:
	cmp	#$38			; SEC?
	bne	NotSEC
	lda	#C_FLAG
	bra	DoSEP
NotSEC:
	cmp	#$c2			; REP?
	bne	NotREP
	lda	[ADDR_S],Y
DoREP:
	trb	FLAGS
	bra	NextOpcode
NotREP:
	cmp	#$e2			; SEP?
	bne	NextOpcode
	lda	[ADDR_S],Y
DoSEP:	tsb	FLAGS

NextOpcode:
	pla
	jsr	OpcodeSize

	clc
	adc	ADDR_S+0		; And move start address on
	sta	ADDR_S+0
	bcc	$+4
	inc	ADDR_S+1

	; Exceeded the end address?
	lda	lines
	beq	chk_addr_e
	dec	lines
	bne	dis_main
dis_end
	jsr	CRLF
	JMP	WSTART

chk_addr_e
	lda	ADDR_S+0
	sec
	sbc	ADDR_E+0
	lda	ADDR_S+1
	sbc	ADDR_E+1
	bmi	dis_main		; No, show more
	bra	dis_end
;
TxSpace:
	lda	#' '		; Transmit a space
	jmp	CONOUT

	longa	off
	longi	off
TxCodeBytes:
	lda	[ADDR_S]	; Fetch the opcode
	jsr	OpcodeSize	; and work out its size
	tax
	ldy	#0		; Clear byte count
CodeLoop:
	lda	[ADDR_S],Y	; Fetch a byte of code
	jsr	HEXOUT2
	jsr	TxSpace
	iny
	dex
	bne	CodeLoop
PadLoop:
	cpy	#4		; Need to pad out?
	bne	$+3
	rts
	jsr	TxSpace
	jsr	TxSpace
	jsr	TxSpace
	iny
	bra	PadLoop

;

	longa	off
	longi	off
TxSymbolic:
	lda	[ADDR_S]	; Fetch opcode
	pha
	jsr	TxOpcode
	pla
	jsr	TxOperand
	rts

;

	longa	off
	longi	off
TxOpcode:
	tax		; Work out the mnemonic
	lda	OPCODES,x
	tax

	long_a
	lda	MNEMONICS,x
	pha		; Save last character
	lsr	a	; Shift second down
	lsr	a
	lsr	a
	lsr	a
	lsr	a
	pha		; Save it
	lsr	a	; Shift first down
	lsr	a
	lsr	a
	lsr	a
	lsr	a
	jsr	ExpandMnem	; Print first
	pla
	jsr	ExpandMnem	; .. second
	pla
	jsr	ExpandMnem	; .. and third
	short_a

	jsr	TxSpace
	rts

ExpandMnem:
	short_a
	clc
	and	#$1f		; Expand letter code
	adc	#'@'
	jsr	CONOUT
	long_a
	rts
;

	longa	off
	longi	off
TxOperand:
	tax			; Work out addressing mode
	lda	MODES,x
	tax
	jmp	(MODE_SHOW,x)

MODE_SHOW:
	dw	TxAbsolute		; a
	dw	TxAccumulator		; A
	dw	TxAbsoluteX		; a,x
	dw	TxAbsoluteY		; a,y
	dw	TxLong			; al
	dw	TxLongX			; al,x
	dw	TxAbsoluteIndirect	; (a)
	dw	TxAbsoluteXIndirect	; (a,x)
	dw	TxDirect		; d
	dw	TxStack			; d,s
	dw	TxDirectX		; d,x
	dw	TxDirectY		; d,y
	dw	TxDirectIndirect	; (d)
	dw	TxDirectIndirectLong	; [d]
	dw	TxStackIndirectY	; (d,s),y
	dw	TxDirectXIndirect	; (d,x)
	dw	TxDirectIndirectY	; (d),y
	dw	TxDirectIndirectLongY	; [d],y
	dw	TxImplied		;
	dw	TxRelative		; r
	dw	TxRelativeLong		; rl
	dw	TxMove			; xyc
	dw	TxImmediateM		; # (A & M)
	dw	TxImmediateByte		; # (BRK/COP/WDM)
	dw	TxImmediateX		; # (X or Y)

	dw	TxAbsolute		; Push Absolute Address
	dw	TxDirectIndirect	; Push Direct Page Indirect
	dw	TxRelativeLong		; Push PC Relative Long


TxAccumulator:
	lda	#'A'
	jmp	CONOUT

TxImmediateM:
	lda	#M_FLAG
	bit	FLAGS
	beq	TxImmediateWord
	bra	TxImmediateByte

TxImmediateX:
	lda	#X_FLAG
	bit	FLAGS
	beq	TxImmediateWord
	bra	TxImmediateByte

TxImplied:
	rts

TxMove:
	lda	#'$'
	jsr	CONOUT
	ldy	#1
	lda	[ADDR_S],Y
	jsr	HEXOUT2
	lda	#','
	jsr	CONOUT
	lda	#'$'
	jsr	CONOUT
	iny
	lda	[ADDR_S],Y
	jmp	HEXOUT2

TxImmediateByte:
	lda	#'#'
	jsr	CONOUT
	bra	TxDirect

TxImmediateWord:
	lda	#'#'
	jsr	CONOUT
	bra	TxAbsolute

TxStack:
	jsr	TxDirect
	lda	#','
	jsr	CONOUT
	lda	#'S'
	jmp	CONOUT

TxDirect:
	lda	#'$'
	jsr	CONOUT
	ldy	#1
	lda	[ADDR_S],Y
	jmp	HEXOUT2

TxDirectX:
	jsr	TxDirect
TxX:	lda	#','
	jsr	CONOUT
	lda	#'X'
	jmp	CONOUT

TxDirectY:
	jsr	TxDirect
TxY:	lda	#','
	jsr	CONOUT
	lda	#'Y'
	jmp	CONOUT

TxAbsolute:
	lda	#'$'
	jsr	CONOUT
	ldy	#2
	lda	[ADDR_S],Y
	jsr	HEXOUT2
	dey
	lda	[ADDR_S],Y
	jmp	HEXOUT2

TxAbsoluteX:
	jsr	TxAbsolute
	bra	TxX

TxAbsoluteY:
	jsr	TxAbsolute
	bra	TxY

TxLong:
	lda	#'$'
	jsr	CONOUT
	ldy	#3
	lda	[ADDR_S],Y
	jsr	HEXOUT2
;	lda	#':'
;	jsr	CONOUT
	dey
	lda	[ADDR_S],Y
	jsr	HEXOUT2
	dey
	lda	[ADDR_S],Y
	jmp	HEXOUT2

TxLongX:
	jsr	TxLong
	bra	TxX

TxAbsoluteIndirect:
	lda	#'('
	jsr	CONOUT
	jsr	TxAbsolute
	lda	#')'
	jmp	CONOUT

TxAbsoluteXIndirect:
	lda	#'('
	jsr	CONOUT
	jsr	TxAbsoluteX
	lda	#')'
	jmp	CONOUT

TxDirectIndirect:
	lda	#'('
	jsr	CONOUT
	jsr	TxDirect
	lda	#')'
	jmp	CONOUT

TxDirectXIndirect:
	lda	#'('
	jsr	CONOUT
	jsr	TxDirectX
	lda	#')'
	jmp	CONOUT

TxDirectIndirectY:
	lda	#'('
	jsr	CONOUT
	jsr	TxDirect
	lda	#')'
	jsr	CONOUT
	jmp	TxY

TxDirectIndirectLong:
	lda	#'['
	jsr	CONOUT
	jsr	TxDirect
	lda	#']'
	jmp	CONOUT

TxDirectIndirectLongY:
	jsr	TxDirectIndirectLong
	jmp	TxY

TxStackIndirectY:
	lda	#'('
	jsr	CONOUT
	jsr	TxStack
	lda	#')'
	jsr	CONOUT
	jmp	TxY

TxRelative:
	ldx	ADDR_S+1	; Work out next PC
	lda	ADDR_S+0
	clc
	adc	#2
	bcc	$+3
	inx

	pha		; Add relative offset
	ldy	#1
	lda	[ADDR_S],y
	bpl	$+3
	dex
	clc
	adc	1,s
	sta	1,s
	bcc	$+3
	inx
	bra	TxAddr

TxRelativeLong:
	ldx	ADDR_S+1	; Work out next PC
	lda	ADDR_S+0
	clc
	adc	#3
	bcc	$+3
	inx

	clc		; Add relative offset
	ldy	#1
	adc	[ADDR_S],y
	pha
	iny
	txa
	adc	[ADDR_S],Y
	tax

TxAddr:
	lda	#'$'		; Print address
	jsr	CONOUT
	txa
	jsr	HEXOUT2
	pla
	jmp	HEXOUT2

;  Returns the size of the opcode in A given the current flag settings.

	longa	off
	longi	off
OpcodeSize:
	tax		; Work out addressing mode
	lda	MODES,x
	tax
	jmp	(MODE_SIZE,x)


MODE_SIZE:
	dw	Size3		; a
	dw	Size1		; A
	dw	Size3		; a,x
	dw	Size3		; a,y
	dw	Size4		; al
	dw	Size4		; al,x
	dw	Size3		; (a)
	dw	Size3		; (a,x)
	dw	Size2		; d
	dw	Size2		; d,s
	dw	Size2		; d,x
	dw	Size2		; d,y
	dw	Size2		; (d)
	dw	Size2		; [d]
	dw	Size2		; (d,s),y
	dw	Size2		; (d,x)
	dw	Size2		; (d),y
	dw	Size2		; [d],y
	dw	Size1		;
	dw	Size2		; r
	dw	Size3		; rl
	dw	Size3		; xyc
	dw	TestM		; # (A & M)
	dw	Size2		; # (BRK/COP/WDM)
	dw	TestX		; # (X or Y)

	dw	Size3		; PEA
	dw	Size2		; PEI
	dw	Size3		; PER

TestM
	lda	#M_FLAG		; Is M bit set?
	and	FLAGS
	beq	Size3		; No, word
	bra	Size2		; else byte

TestX
	lda	#X_FLAG		; Is X bit set?
	and	FLAGS
	beq	Size3		; No, word
	bra	Size2		; else byte

Size1:	lda	#1
	rts
Size2:	lda	#2
	rts
Size3	lda	#3
	rts
Size4:	lda	#4
	rts

OPCODES:
	db	OP_BRK,OP_ORA,OP_COP,OP_ORA	; 00
	db	OP_TSB,OP_ORA,OP_ASL,OP_ORA
	db	OP_PHP,OP_ORA,OP_ASL,OP_PHD
	db	OP_TSB,OP_ORA,OP_ASL,OP_ORA
	db	OP_BPL,OP_ORA,OP_ORA,OP_ORA	; 10
	db	OP_TRB,OP_ORA,OP_ASL,OP_ORA
	db	OP_CLC,OP_ORA,OP_INC,OP_TCS
	db	OP_TRB,OP_ORA,OP_ASL,OP_ORA
	db	OP_JSR,OP_AND,OP_JSL,OP_AND	; 20
	db	OP_BIT,OP_AND,OP_ROL,OP_AND
	db	OP_PLP,OP_AND,OP_ROL,OP_PLD
	db	OP_BIT,OP_AND,OP_ROL,OP_AND
	db	OP_BMI,OP_AND,OP_AND,OP_AND	; 30
	db	OP_BIT,OP_AND,OP_ROL,OP_AND
	db	OP_SEC,OP_AND,OP_DEC,OP_TSC
	db	OP_BIT,OP_AND,OP_ROL,OP_AND
	db	OP_RTI,OP_EOR,OP_WDM,OP_EOR	; 40
	db	OP_MVP,OP_EOR,OP_LSR,OP_EOR
	db	OP_PHA,OP_EOR,OP_LSR,OP_PHK
	db	OP_JMP,OP_EOR,OP_LSR,OP_EOR
	db	OP_BVC,OP_EOR,OP_EOR,OP_EOR	; 50
	db	OP_MVN,OP_EOR,OP_LSR,OP_EOR
	db	OP_CLI,OP_EOR,OP_PHY,OP_TCD
	db	OP_JMP,OP_EOR,OP_LSR,OP_EOR
	db	OP_RTS,OP_ADC,OP_PER,OP_ADC	; 60
	db	OP_STZ,OP_ADC,OP_ROR,OP_ADC
	db	OP_PLA,OP_ADC,OP_ROR,OP_RTL
	db	OP_JMP,OP_ADC,OP_ROR,OP_ADC
	db	OP_BVS,OP_ADC,OP_ADC,OP_ADC	; 70
	db	OP_STZ,OP_ADC,OP_ROR,OP_ADC
	db	OP_SEI,OP_ADC,OP_PLY,OP_TDC
	db	OP_JMP,OP_ADC,OP_ROR,OP_ADC
	db	OP_BRA,OP_STA,OP_BRL,OP_STA	; 80
	db	OP_STY,OP_STA,OP_STX,OP_STA
	db	OP_DEY,OP_BIT,OP_TXA,OP_PHB
	db	OP_STY,OP_STA,OP_STX,OP_STA
	db	OP_BCC,OP_STA,OP_STA,OP_STA	; 90
	db	OP_STY,OP_STA,OP_STX,OP_STA
	db	OP_TYA,OP_STA,OP_TXS,OP_TXY
	db	OP_STZ,OP_STA,OP_STZ,OP_STA
	db	OP_LDY,OP_LDA,OP_LDX,OP_LDA	; A0
	db	OP_LDY,OP_LDA,OP_LDX,OP_LDA
	db	OP_TAY,OP_LDA,OP_TAX,OP_PLB
	db	OP_LDY,OP_LDA,OP_LDX,OP_LDA
	db	OP_BCS,OP_LDA,OP_LDA,OP_LDA	; B0
	db	OP_LDA,OP_LDY,OP_LDX,OP_LDA
	db	OP_CLV,OP_LDA,OP_TSX,OP_TYX
	db	OP_LDY,OP_LDA,OP_LDX,OP_LDA
	db	OP_CPY,OP_CMP,OP_REP,OP_CMP	; C0
	db	OP_CPY,OP_CMP,OP_DEC,OP_CMP
	db	OP_INY,OP_CMP,OP_DEX,OP_WAI
	db	OP_CPY,OP_CMP,OP_DEC,OP_CMP
	db	OP_BNE,OP_CMP,OP_CMP,OP_CMP	; D0
	db	OP_PEI,OP_CMP,OP_DEC,OP_CMP
	db	OP_CLD,OP_CMP,OP_PHX,OP_STP
	db	OP_JML,OP_CMP,OP_DEC,OP_CMP
	db	OP_CPX,OP_SBC,OP_SEP,OP_SBC	; E0
	db	OP_CPX,OP_SBC,OP_INC,OP_SBC
	db	OP_INX,OP_SBC,OP_NOP,OP_XBA
	db	OP_CPX,OP_SBC,OP_INC,OP_SBC
	db	OP_BEQ,OP_SBC,OP_SBC,OP_SBC	; F0
	db	OP_PEA,OP_SBC,OP_INC,OP_SBC
	db	OP_SED,OP_SBC,OP_PLX,OP_XCE
	db	OP_JSR,OP_SBC,OP_INC,OP_SBC

MODES:
	db	MD_INT,MD_DIX,MD_INT,MD_STK	; 00
	db	MD_DPG,MD_DPG,MD_DPG,MD_DLI
	db	MD_IMP,MD_IMM,MD_ACC,MD_IMP
	db	MD_ABS,MD_ABS,MD_ABS,MD_ALG
	db	MD_REL,MD_DIY,MD_DIN,MD_SKY	; 10
	db	MD_DPG,MD_DPX,MD_DPX,MD_DLY
	db	MD_IMP,MD_ABY,MD_ACC,MD_IMP
	db	MD_ABS,MD_ABX,MD_ABX,MD_ALX
	db	MD_ABS,MD_DIX,MD_ALG,MD_STK	; 20
	db	MD_DPG,MD_DPG,MD_DPG,MD_DLI
	db	MD_IMP,MD_IMM,MD_ACC,MD_IMP
	db	MD_ABS,MD_ABS,MD_ABS,MD_ALG
	db	MD_REL,MD_DIY,MD_DIN,MD_SKY	; 30
	db	MD_DPX,MD_DPX,MD_DPX,MD_DLY
	db	MD_IMP,MD_ABY,MD_ACC,MD_IMP
	db	MD_ABX,MD_ABX,MD_ABX,MD_ALX
	db	MD_IMP,MD_DIX,MD_INT,MD_STK	; 40
	db	MD_MOV,MD_DPG,MD_DPG,MD_DLI
	db	MD_IMP,MD_IMM,MD_ACC,MD_IMP
	db	MD_ABS,MD_ABS,MD_ABS,MD_ALG
	db	MD_REL,MD_DIY,MD_DIN,MD_SKY	; 50
	db	MD_MOV,MD_DPX,MD_DPX,MD_DLY
	db	MD_IMP,MD_ABY,MD_IMP,MD_IMP
	db	MD_ALG,MD_ABX,MD_ABX,MD_ALX
	db	MD_IMP,MD_DIX,MD_PER,MD_STK	; 60
	db	MD_DPG,MD_DPG,MD_DPG,MD_DLI
	db	MD_IMP,MD_IMM,MD_ACC,MD_IMP
	db	MD_AIN,MD_ABS,MD_ABS,MD_ALG
	db	MD_REL,MD_DIY,MD_DIN,MD_SKY	; 70
	db	MD_DPX,MD_DPX,MD_DPX,MD_DLY
	db	MD_IMP,MD_ABY,MD_IMP,MD_IMP
	db	MD_AIX,MD_ABX,MD_ABX,MD_ALX
	db	MD_REL,MD_DIX,MD_RLG,MD_STK	; 80
	db	MD_DPG,MD_DPG,MD_DPG,MD_DLI
	db	MD_IMP,MD_IMM,MD_IMP,MD_IMP
	db	MD_ABS,MD_ABS,MD_ABS,MD_ALG
	db	MD_REL,MD_DIY,MD_DIN,MD_SKY	; 90
	db	MD_DPX,MD_DPX,MD_DPY,MD_DLY
	db	MD_IMP,MD_ABY,MD_IMP,MD_IMP
	db	MD_ABS,MD_ABX,MD_ABX,MD_ALX
	db	MD_IMX,MD_DIX,MD_IMX,MD_STK	; A0
	db	MD_DPG,MD_DPG,MD_DPG,MD_DLI
	db	MD_IMP,MD_IMM,MD_IMP,MD_IMP
	db	MD_ABS,MD_ABS,MD_ABS,MD_ALG
	db	MD_REL,MD_DIY,MD_DIN,MD_SKY	; B0
	db	MD_DPX,MD_DPX,MD_DPY,MD_DLY
	db	MD_IMP,MD_ABY,MD_IMP,MD_IMP
	db	MD_ABX,MD_ABX,MD_ABY,MD_ALX
	db	MD_IMX,MD_DIX,MD_INT,MD_STK	; C0
	db	MD_DPG,MD_DPG,MD_DPG,MD_DLI
	db	MD_IMP,MD_IMM,MD_IMP,MD_IMP
	db	MD_ABS,MD_ABS,MD_ABS,MD_ALG
	db	MD_REL,MD_DIY,MD_DIN,MD_SKY	; D0
	db	MD_PEI,MD_DPX,MD_DPX,MD_DLY
	db	MD_IMP,MD_ABY,MD_IMP,MD_IMP
	db	MD_AIN,MD_ABX,MD_ABX,MD_ALX
	db	MD_IMX,MD_DIX,MD_INT,MD_STK	; E0
	db	MD_DPG,MD_DPG,MD_DPG,MD_DLI
	db	MD_IMP,MD_IMM,MD_IMP,MD_IMP
	db	MD_ABS,MD_ABS,MD_ABS,MD_ALG
	db	MD_REL,MD_DIY,MD_DIN,MD_SKY	; F0
	db	MD_PEA,MD_DPX,MD_DPX,MD_DLY
	db	MD_IMP,MD_ABY,MD_IMP,MD_IMP
	db	MD_AIX,MD_ABX,MD_ABX,MD_ALX

MNEMONICS:
	MNEM	'A','D','C'
	MNEM	'A','N','D'
	MNEM	'A','S','L'
	MNEM	'B','C','C'
	MNEM	'B','C','S'
	MNEM	'B','E','Q'
	MNEM	'B','I','T'
	MNEM	'B','M','I'
	MNEM	'B','N','E'
	MNEM	'B','P','L'
	MNEM	'B','R','A'
	MNEM	'B','R','K'
	MNEM	'B','R','L'
	MNEM	'B','V','C'
	MNEM	'B','V','S'
	MNEM	'C','L','C'
	MNEM	'C','L','D'
	MNEM	'C','L','I'
	MNEM	'C','L','V'
	MNEM	'C','M','P'
	MNEM	'C','O','P'
	MNEM	'C','P','X'
	MNEM	'C','P','Y'
	MNEM	'D','E','C'
	MNEM	'D','E','X'
	MNEM	'D','E','Y'
	MNEM	'E','O','R'
	MNEM	'I','N','C'
	MNEM	'I','N','X'
	MNEM	'I','N','Y'
	MNEM	'J','M','L'
	MNEM	'J','M','P'
	MNEM	'J','S','L'
	MNEM	'J','S','R'
	MNEM	'L','D','A'
	MNEM	'L','D','X'
	MNEM	'L','D','Y'
	MNEM	'L','S','R'
	MNEM	'M','V','N'
	MNEM	'M','V','P'
	MNEM	'N','O','P'
	MNEM	'O','R','A'
	MNEM	'P','E','A'
	MNEM	'P','E','I'
	MNEM	'P','E','R'
	MNEM	'P','H','A'
	MNEM	'P','H','B'
	MNEM	'P','H','D'
	MNEM	'P','H','K'
	MNEM	'P','H','P'
	MNEM	'P','H','X'
	MNEM	'P','H','Y'
	MNEM	'P','L','A'
	MNEM	'P','L','B'
	MNEM	'P','L','D'
	MNEM	'P','L','P'
	MNEM	'P','L','X'
	MNEM	'P','L','Y'
	MNEM	'R','E','P'
	MNEM	'R','O','L'
	MNEM	'R','O','R'
	MNEM	'R','T','I'
	MNEM	'R','T','L'
	MNEM	'R','T','S'
	MNEM	'S','B','C'
	MNEM	'S','E','C'
	MNEM	'S','E','D'
	MNEM	'S','E','I'
	MNEM	'S','E','P'
	MNEM	'S','T','A'
	MNEM	'S','T','P'
	MNEM	'S','T','X'
	MNEM	'S','T','Y'
	MNEM	'S','T','Z'
	MNEM	'T','A','X'
	MNEM	'T','A','Y'
	MNEM	'T','C','D'
	MNEM	'T','C','S'
	MNEM	'T','D','C'
	MNEM	'T','R','B'
	MNEM	'T','S','B'
	MNEM	'T','S','C'
	MNEM	'T','S','X'
	MNEM	'T','X','A'
	MNEM	'T','X','S'
	MNEM	'T','X','Y'
	MNEM	'T','Y','A'
	MNEM	'T','Y','X'
	MNEM	'W','A','I'
	MNEM	'W','D','M'
	MNEM	'X','B','A'
	MNEM	'X','C','E'

;;;
;;;  Go address
;;;
GO
	longi off
	longa off
	stz	stp_flg		; clear stop flag
	jsr	arg_parsing
	lda	argtype
	beq	G0		;; No arg.

	tax
	and	#4
	beq	go_3

	;; 1st arg. found
	LDA	arg1st
	sta	REGPB		; save program bank reg
go_3
	txa
	and	#2
	beq	go_4
	
	;; set 2nd arg.
	long_a
	LDA	arg2nd
	STA	REGPC		; set start address
	short_a

go_4
	txa
	and	#1
	beq	G0

	;; set 3rd arg.

	sta	stp_flg		; set stop flag

	; save original binary at break point

	lda	REGPB
	sta	sv_bnk		; save program bank reg

	long_a
	lda	arg3rd
	sta	sv_adr

	lda	[sv_adr]	; get save code
	sta	sav_dat		; save original binary
	lda	#0		; BRK 0 (A = 0000)
	sta	[sv_adr]	; set BRK 0 code

G0
	long_a
	lda	REGSP
	tas			; SP
	short_a
	lda	REGPB
	pha			; Program Bank register
	LDA	REGPC+1
	PHA			; PC(H)
	LDA	REGPC
	PHA			; PC(L)
	LDA	REGPSR
	PHA			; PSR
	long_ai
	LDX	REGX
	LDY	REGY
	LDA	REGA

	pha
	pei	(REGDP)		; push Direct Page register (16 bit)
	short_a
	lda	REGDB		; push Data Bank register (8 bit)
	pha

	plb			; restore Data Bank register
	pld			; restore Direct Page register
	long_a
	pla			; restore A register

	RTI

;;;
;;; Set memory
;;;
SETM
	longi off
	longa off
	jsr	arg_parsing
	lda	argtype
	beq	SM1 ;; No arg.

	tax
	and	#4
	beq	set3

	;; 1st arg. found
	LDA	arg1st
	STA	dumpdb		; set data bank

set3
	txa
	and	#2
	beq	set4
	
	;; set 2nd arg.
	long_a
	LDA	arg2nd
	STA	SADDR		; set start address
	short_a

set4
	txa
	and	#1
	beq	SM1

	;; 3rd arg.
	jmp	ERR

SM1:
	jsr	ptt_db		; print bank reg
	LDA	SADDR+1
	JSR	HEXOUT2
	LDA	SADDR
	JSR	HEXOUT2

	LDA	#$FF&DSEP1
	STA	PT0
	LDA	#DSEP1>>8
	STA	PT0+1
	JSR	STROUT		; " : "
	LDY	#0
	; get a data----------------
	phb			; push DBR
	lda	dumpdb
	pha
	plb			; set Data Bank
	LDA	(SADDR),Y	; get a data
	plb			; pop DBR
	; ---------------------------
	JSR	HEXOUT2

	LDA	#' '
	JSR	CONOUT
	JSR	GETLIN
	LDX	#0
	LDA	INBUF,X
	BNE	SM2
SM10	
	;; Empty (Increment address)
	LDA	SADDR
	CLC
	ADC	#1
	STA	SADDR
	LDA	SADDR+1
	ADC	#0
	STA	SADDR+1
	JMP	SM1
SM2
	CMP	#'-'
	BNE	SM3
	;; '-' (Decrement address)
	LDA	SADDR
	SEC
	SBC	#1
	STA	SADDR
	LDA	SADDR+1
	SBC	#0
	STA	SADDR+1
	JMP	SM1
SM3
	CMP	#'.'
	BNE	SM4
	;; '.' (Quit)
	JMP	WSTART
SM4
	JSR	RDHEX
	LDA	CNT
	BNE	SM40
SMER
	JMP	ERR
SM40
	; repar original bug -------
	LDA	INBUF,X
	bne	SMER
	; repar original bug -------

	LDA	PT1

	; get a data(65C816)----------------
	phb			; push DBR
	ldy	dumpdb
	phy
	plb			; set Data Bank
	LDY	#0
	STA	(SADDR),Y	; set data
	plb			; pop DBR
	; get a data(65C816)----------------

	JMP	SM10

;;;
;;; LOAD HEX file
;;;
LOADH
	longi off
	longa off
	INX
	JSR	RDHEX
	LDA	INBUF,X
	BNE	SMER
LH0
	JSR	CONIN
	JSR	UPPER
	CMP	#'S'
	BEQ	LHS0
LH1a
	CMP	#':'
	BEQ	LHI0
LH2
	;; Skip to EOL
	CMP	#CR
	BEQ	LH0
	CMP	#LF
	BEQ	LH0
LH3
	JSR	CONIN
	JMP	LH2

LHI0
	JSR	HEXIN
	STA	CKSUM
	STA	CNT		; Length

	JSR	HEXIN
	STA	DMPPT+1		; Address H
	CLC
	ADC	CKSUM
	STA	CKSUM

	JSR	HEXIN
	STA	DMPPT		; Address L
	CLC
	ADC	CKSUM
	STA	CKSUM

	;; Add offset
	LDA	DMPPT
	CLC
	ADC	PT1
	STA	DMPPT
	LDA	DMPPT+1
	ADC	PT1+1
	STA	DMPPT+1
	LDY	#0
	
	JSR	HEXIN
	STA	RECTYP		; Record Type
	CLC
	ADC	CKSUM
	STA	CKSUM

	LDA	CNT
	BEQ	LHI3
LHI1
	JSR	HEXIN
	PHA
	CLC
	ADC	CKSUM
	STA	CKSUM

	LDA	RECTYP
	BNE	LHI2

	PLA
	STA	(DMPPT),Y
	INY
	PHA			; Dummy, better than JMP to skip next PLA
LHI2
	PLA
	DEC	CNT
	BNE	LHI1
LHI3
	JSR	HEXIN
	CLC
	ADC	CKSUM
	BNE	LHIE		; Checksum error
	LDA	RECTYP
	BEQ	LH3
	JMP	WSTART
LHIE
	LDA	#$FF&IHEMSG
	STA	PT0
	LDA	#IHEMSG>>8
	STA	PT0+1
	JSR	STROUT
	JMP	WSTART

LHS0	
	lda	#','
	jsr	CONOUT
	JSR	CONIN
	STA	RECTYP		; Record Type

	JSR	HEXIN
	STA	CNT		; (CNT) = Length+3
	STA	CKSUM

	JSR	HEXIN
	STA	DMPPT+1		; Address H
	CLC
	ADC	CKSUM
	STA	CKSUM
	
	JSR	HEXIN
	STA	DMPPT		; Address L
	CLC
	ADC	CKSUM
	STA	CKSUM

	;; Add offset
	LDA	DMPPT
	CLC
	ADC	PT1
	STA	DMPPT
	LDA	DMPPT+1
	ADC	PT1+1
	STA	DMPPT+1
	LDY	#0

	DEC	CNT
	DEC	CNT
	DEC	CNT
	BEQ	LHS3
LHS1
	JSR	HEXIN
	PHA
	CLC
	ADC	CKSUM
	STA	CKSUM		; Checksum

	LDA	RECTYP
	CMP	#'1'
	BNE	LHS2

	PLA
	STA	(DMPPT),Y
	INY
	PHA			; Dummy, better than JMP to skip next PLA
LHS2
	PLA
	DEC	CNT
	BNE	LHS1
LHS3
	JSR	HEXIN
	CLC
	ADC	CKSUM
	CMP	#$FF
	BNE	LHSE		; Checksum error

	LDA	RECTYP
	CMP	#'9'
	BEQ	LHSR
	JMP	LH3
LHSE
	LDA	#$FF&SHEMSG
	STA	PT0
	LDA	#SHEMSG>>8
	STA	PT0+1
	JSR	STROUT
LHSR	
	JMP	WSTART

;;;
;;; Register
;;;
REG
	longi off
	longa off
	
	INX
	LDA	INBUF,X
	CMP	#0
	BNE	RG0
	JSR	RDUMP
	JMP	WSTART
RG0
	long_i
	LDY	#RNTAB
	STY	PT1
	short_i
	LDY	#0
RG1
	CMP	(PT1),Y
	BEQ	RG2	; match a register
	INY
	PHA
	LDA	(PT1),Y
	BEQ	RGE
	PLA
	INY
	INY
	INY
	INY
	INY
	JMP	RG1
RGE
	PLA
	JMP	ERR
RG2
	INY
	LDA	(PT1),Y		; $80 or 2 or 1
	CMP	#$80
	BNE	RG3		; 2 or 1
	;; Next table
	INY
	LDA	(PT1),Y
	STA	CNT		; Temporary
	INY
	LDA	(PT1),Y
	STA	PT1+1
	LDA	CNT
	STA	PT1
	LDY	#0
	INX
	LDA	INBUF,X
	JSR	UPPER
	JMP	RG1
RG3
	CMP	#0
	BEQ	RGE0

	INY			; +2
	LDA	(PT1),Y		; get address of register value
	TAX			;  ex: X = &REGA(in Direct Ppage)
	INY

	INY			; +4
	LDA	(PT1),Y		;
	STA	PT0		;
	INY			;  PT0 = char register point
	LDA	(PT1),Y		;  (ex: PT0 = &RNA;   *RNA = 'A')
	STA	PT0+1		;

	STY	CNT		; Save Y (STROUT destroys Y)
	JSR	STROUT
	LDA	#'='
	JSR	CONOUT
	LDY	CNT		; Restore Y
	DEY
	DEY
	DEY
	DEY
	LDA	(PT1),Y		; get register size. 1 or 2
	STA	REGSIZ
	CMP	#1
	BNE	RG4
	;; 8 bit register
	LDA	UNIMON_DP+0,X
	JSR	HEXOUT2
	JMP	RG5
RG4
	;; 16 bit register
	LDA	UNIMON_DP+1,X
	JSR	HEXOUT2
	LDA	UNIMON_DP+0,X
	JSR	HEXOUT2
RG5
	LDA	#' '
	JSR	CONOUT
	STX	CKSUM		; Save X (GETLIN destroys X)
	JSR	GETLIN
	LDX	#0
	JSR	RDHEX
	LDA	CNT
	BEQ	RGR
	LDX	CKSUM		; Restore X
	LDA	REGSIZ
	CMP	#1
	BNE	RG6
	;; 8 bit register
	LDA	PT1
	STA	UNIMON_DP+0,X
	JMP	RG7
RG6
	;; 16 bit address
	LDA	PT1
	STA	UNIMON_DP+0,X		; (L)
	LDA	PT1+1
	STA	UNIMON_DP+1,X		; (H)
RG7	
RGR	
	LDA	REGPSR
	STA	FLAGS		; save disassemble EmPCR
	JMP	WSTART
	
RGE0	
	JMP	ERR
	
;
; print all registers
;
RDUMP
	longi off
	longa off

	ldy	#40
	lda	#' '
spc_out
	jsr	CONOUT
	dey
	bne	spc_out

	long_a
	LDA	#psn_bm
	STA	PT0
	jsr	STROUT

	long_a
	LDA	#RDSA	; A
	STA	PT0
	JSR	STROUT
	longa off
	LDA	REGA+1
	JSR	HEXOUT2
	LDA	REGA
	JSR	HEXOUT2

	long_a
	LDA	#RDSX	; X
	STA	PT0
	JSR	STROUT
	longa off
	LDA	REGX+1
	JSR	HEXOUT2
	LDA	REGX
	JSR	HEXOUT2

	long_a
	LDA	#RDSY	; Y
	STA	PT0
	JSR	STROUT
	longa off
	LDA	REGY+1
	JSR	HEXOUT2
	LDA	REGY
	JSR	HEXOUT2

	long_a
	LDA	#RDSSP	; SP
	STA	PT0
	JSR	STROUT
	longa off
	LDA	REGSP+1
	JSR	HEXOUT2
	LDA	REGSP
	JSR	HEXOUT2

	long_a
	LDA	#RDSPC	; PC
	STA	PT0
	JSR	STROUT
	longa off
	LDA	REGPC+1		; PC(H)
	JSR	HEXOUT2
	LDA	REGPC		; PC(L)
	JSR	HEXOUT2

	long_a
	LDA	#RDSPSR		; PSR
	STA	PT0
	JSR	STROUT

	longa off
	LDY	#8
	LDA	REGPSR
	
psr_bloop
	asl	a
	bcc	set_31
	tax			; save
	lda	#'1'
	jsr	CONOUT
set_30
	txa
	dey
	bne	psr_bloop
	bra	prt_dpdbpb

set_31
	tax			; save
	lda	#'0'
	jsr	CONOUT
	bra	set_30

prt_dpdbpb
	long_a
	LDA	#RDSPB		; PBR
	STA	PT0
	JSR	STROUT
	longa off
	LDA	REGPB
	JSR	HEXOUT2

	long_a
	LDA	#RDSDB		; DBR
	STA	PT0
	JSR	STROUT
	longa off
	LDA	REGDB
	JSR	HEXOUT2

	long_a
	LDA	#RDSDP		; DPR
	STA	PT0
	JSR	STROUT
	longa off
	LDA	REGDP+1		; DPR(H)
	JSR	HEXOUT2
	LDA	REGDP		; DPR(L)
	JSR	HEXOUT2
	JMP	CRLF

;
; command help
;
prt_help:
	longi off
	longa off
	INX
	LDA	INBUF,X
	BEQ	ph_1	; jmp if string exist
	JMP	ERR
ph_1
	; must strings <= 255 : Y = 8 bit

	LDA	#$FF&hlp_meg1
	STA	PT0
	LDA	#hlp_meg1>>8
	STA	PT0+1
	JSR	STROUT

	LDA	#$FF&hlp_meg2
	STA	PT0
	LDA	#hlp_meg2>>8
	STA	PT0+1
	JSR	STROUT
	JMP	WSTART

hlp_meg1
	db	"<<< Universal Monitor Command List >>>",CR,LF
	db	"?  : Command Summary", CR, LF
	db	"D  [bank:][start addr][, end addr] : Dump Memory", CR, LF
	db	"DI [bank:][start addr][, end addr] : Disassembler", CR, LF,0
hlp_meg2
	db	"G  [bank:][start addr][, stop addr] : Go and Stop", CR, LF
	db	"L  [offset] : Load HexFile", CR, LF
	db	"R  [register] : Show or Set register", CR, LF
	db	"S  [bank:][addr] : Set Memory", CR, LF
	db	"BYE : Terminate",CR,LF,0

;;;
;;; Other support routines
;;;

STROUT
	short_ai
	LDY	#0
STRO0
	LDA	(PT0),Y
	BEQ	STROE
	JSR	CONOUT
	INY
	JMP	STRO0
STROE
	RTS

HEXOUT2
	longa off
	PHA
	LSR	A
	LSR	A
	LSR	A
	LSR	A
	JSR	HEXOUT1
	PLA
HEXOUT1
	longa off
	AND	#$0F
	CLC
	ADC	#'0'
	CMP	#'9'+1
	BCC	HEXOUTE
	CLC
	ADC	#'A'-'9'-1
HEXOUTE
	JMP	CONOUT

HEXIN
	longa off
	LDA	#0
	JSR	HI0
	ASL
	ASL
	ASL
	ASL
HI0
	STA	HITMP
	JSR	CONIN
	JSR	UPPER
	CMP	#'0'
	BCC	HIR
	CMP	#'9'+1
	BCC	HI1
	CMP	#'A'
	BCC	HIR
	CMP	#'F'+1
	BCS	HIR
	SEC
	SBC	#'A'-'9'-1
HI1
	SEC
	SBC	#'0'
	CLC
	ADC	HITMP
HIR
	RTS
	
CRLF
	longa off
	LDA	#CR
	JSR	CONOUT
	LDA	#LF
	JMP	CONOUT

GETLIN
	longi off
	longa off
	LDX	#0
GL0
	JSR	CONIN
	CMP	#CR
	BEQ	GLE
	CMP	#LF
	BEQ	GLE
	CMP	#BS
	BEQ	GLB
	CMP	#DEL
	BEQ	GLB
	CMP	#' '
	BCC	GL0
	CMP	#$80
	BCS	GL0
	CPX	#BUFLEN-1
	BCS	GL0		; Too long
	STA	INBUF,X
	INX
	JSR	CONOUT
	bra	GL0
GLB
	CPX	#0
	BEQ	GL0
	DEX
	LDA	#BS
	JSR	CONOUT
	LDA	#' '
	JSR	CONOUT
	LDA	#BS
	JSR	CONOUT
	bra	GL0
GLE
	JSR	CRLF
	LDA	#0
	STA	INBUF,X

; skip space
	tax			; X=0
	tay			; Y=0
skp_splp
	lda	INBUF,X
	cmp	#0
	beq	glee
	cmp	#' '		; check space
	beq	skip_space

	jsr	UPPER
	sta	UNIMON_DP+INBUF,y	; Absolute Indexed!! INBUF is at DPR
	iny
skip_space
	inx
	bra	skp_splp
glee
	sta	UNIMON_DP+INBUF,y	; Absolute Indexed! set null
	RTS

UPPER
	longa off
	CMP	#'a'
	BCC	UPE
	CMP	#'z'+1
	BCS	UPE
	ADC	#'A'-'a'
UPE
	RTS

RDHEX
	longi off
	longa off
	LDA	#0
	STA	PT1
	STA	PT1+1
	STA	CNT
RH0
	LDA	INBUF,X
	CMP	#'0'
	BCC	RHE
	CMP	#'9'+1
	BCC	RH1
	CMP	#'A'
	BCC	RHE
	CMP	#'F'+1
	BCS	RHE
	SEC
	SBC	#'A'-'9'-1
RH1
	SEC
	SBC	#'0'
	ASL	PT1
	ROL	PT1+1
	ASL	PT1
	ROL	PT1+1
	ASL	PT1
	ROL	PT1+1
	ASL	PT1
	ROL	PT1+1
	CLC
	ADC	PT1
	STA	PT1
	INC	CNT
	INX
	JMP	RH0
RHE
	RTS

;---------- unimon message data ---------------
OPNMSG
	db	CR,LF,"MEZW65C_RAM Monitor W65C816",CR,LF,$00
PROMPT
	db	"] ",$00
IHEMSG
	db	"Error ihex",CR,LF,$00

SHEMSG
	db	"Error srec",CR,LF,$00

ERRMSG
	db	"Error",CR,LF,$00

DSEP0
	db	" :",$00
DSEP1
	db	" : ",$00
dmplmsg
	db	"          +0 +1 +2 +3 +4 +5 +6 +7 +8 +9 +A +B +C +D +E +F"
	db	"   0123456789ABCDEF",CR,LF,0


;IHEXER
;        db	":00000001FF",CR,LF,$00
;SRECER
;        db	"S9030000FC",CR,LF,$00

RDSA	db	"A=",$00
RDSX	db	" X=",$00
RDSY	db	" Y=",$00
RDSSP	db	" SP=",$00
RDSPC	db	" PC=",$00
RDSPSR	db	" PSR=",$00
psn_bm	db	"(NVMXDIZC)",CR,LF,0

RDSPB	db	CR,LF,"PBR=",0
RDSDB	db	" DBR=",0
RDSDP	db	" DPR=",0

RNTAB
	db	'A',2
	dw	REGA,RNA
	db	'X',2
	dw	REGX,RNX
	db	'Y',2
	dw	REGY,RNY
	db	'S',$80		; SP
	dw	RNTABS,0
	db	'P',$80		; PC, PSR, PBR
	dw	RNTABP,0
	db	'D',$80		; DBR, DPR
	dw	RNTABD,0
	
	db	$00,0		; End mark
	dw	0,0

RNTABS
	db	'P',2
	dw	REGSP,RNSP
	
	db	$00,0		; End mark
	dw	0,0

RNTABP
	db	'C',2
	dw	REGPC,RNPC
	db	'B',$80
	dw	RNTPBS,0
	db	'S',$80
	dw	RNTABPS,0

	db	$00,0		; End mark
	dw	0,0

RNTABPS
	db	'R',1
	dw	REGPSR,RNPSR

	db	$00,0		; End mark
	dw	0,0
	
RNTPBS
	db	'R',1
	dw	REGPB,RNPB

	db	$00,0		; End mark
	dw	0,0

RNTABD
	db	'B',$80
	dw	RNTDBR,0
	db	'P',$80
	dw	RNTDPR,0

	db	$00,0		; End mark
	dw	0,0
	
RNTDBR
	db	'R',1
	dw	REGDB,RNDB

	db	$00,0		; End mark
	dw	0,0

RNTDPR
	db	'R',2
	dw	REGDP,RNDP

	db	$00,0		; End mark
	dw	0,0

RNA	db	"A",$00
RNX	db	"X",$00
RNY	db	"Y",$00
RNSP	db	"SP",$00
RNPC	db	"PC",$00
RNPSR	db	"PSR",$00

RNPB	db	"PBR",0
RNDB	db	"DBR",0
RNDP	db	"DPR",0

emu_msg		db	CR,LF,"(Emulation mode)",0
ntv_msg		db	CR,LF,"(Native mode)",0
NMI_MSG		db	"NMI!",CR,LF,$00
IRQ_MSG		db	"IRQ!",CR,LF,$00
BRK_MSG		db	"BRK!",CR,LF,$00
IBRK_MSG	db	"IRQ/BRK!",CR,LF,0
dct_msg		db	" Detect ",0
stpmsg		db	"STOP!(User break)",CR,LF,$00

;;;
;;;	Console Driver
;;;

;CONIN_REQ	EQU	0x01
;CONOUT_REQ	EQU	0x02
;CONST_REQ	EQU	0x03
;STROUT_REQ	equ	$04
;WUP_REQ	equ	$ff
;  ---- request command to PIC
; UREQ_COM = 1 ; CONIN  : return char in UNI_CHR
;          = 2 ; CONOUT : UNI_CHR = output char
;          = 3 ; CONST  : return status in UNI_CHR
;                       : ( 0: no key, 1 : key exist )
;          = 4 ; STROUT : string address = (PTRSAV, PTRSAV_SEG)
;
;UREQ_COM	ds	1	; unimon CONIN/CONOUT request command
;UNI_CHR	ds	1	; charcter (CONIN/CONOUT) or number of strings

INIT
	; clear Reqest Parameter Block
	short_a
	lda	#0
	sta	UREQ_COM
	sta	CREQ_COM
	sta	bank
	sta	reserve
	RTS

;
; request CONIN, CONST CONOUT to PIC18F47QXX
;

CONIN
	short_a			; Make A 8-bits
	lda	#CONIN_REQ

wup_pic
	sta	UREQ_COM
;wait_again
	wai			; RDY = 0, wait /IRQ detect

	lda	UNI_CHR
	RTS

CONST
	short_a			; Make A 8-bits
	lda	#CONST_REQ
	jsr	wup_pic
	AND	#$01
	RTS

CONOUT
	short_a			; Make A 8-bits
	pha
	sta	UNI_CHR		; set char
	lda	#CONOUT_REQ
	jsr	wup_pic
	pla
	rts

NMI_SIG
	short_a			; Make A 8-bits
	pha
	lda	#WUP_REQ
	jsr	wup_pic
	pla
	rts

;===============================================================================
; Interrupt Handlers message out
;-------------------------------------------------------------------------------
prt_nmode
	long_a
	LDA	#ntv_msg
	STA	PT0
	bra	prt_str

prt_emode
	long_a
	lda	#emu_msg
	STA	PT0
	bra	prt_str

prt_dtct
	long_a
	lda	#dct_msg
	STA	PT0
prt_str
	jmp	STROUT

prt_ibkm
	long_a
	lda	#IBRK_MSG
	STA	PT0
	bra	prt_str

prt_nmim
	long_a
	lda	#NMI_MSG
	STA	PT0
	bra	prt_str

prt_irqm
	long_a
	lda	#IRQ_MSG
	STA	PT0
	bra	prt_str
;
; Handle IRQ interrupts in native mode.
;
IRQ_N:
	long_a
	pha

	lda	#UNIMON_DP
	pha
	pld			; set Direct Page

	short_a
	lda	#UNIMON_DB
	pha
	plb			; set DATA BANK

	long_ai
	pla			; A
	STA	REGA
	TXA			; X
	STA	REGX
	TYA			; Y
	STA	REGY

	short_a
	PLA			; PSR (Pushed by IRQ)
	STA	REGPSR		; save status register
	STA	FLAGS		; save disassemble EmPCR

	long_a
	PLA			; PC (Pushed by IRQ)
	STA	REGPC
	stz	REGDP		; clear DP

	short_a
	stz	REGPB		; clear PB
	stz	REGDB		; clear DB

	TSX			; get SP
	STX	REGSP
	short_i

	jsr	prt_nmode	; (Native mode)
	jsr	prt_dtct	; Detect 
	jsr	prt_irqm	; IRQ

	JSR	RDUMP		; dump registers
	JMP	WSTART

;-----------------------------------------
; Handle BRK interrupts in native mode.
;-----------------------------------------
;		| SP
;---------------+----
; Y	16bit	| +1
; X	16bit	| +3
; DBR	8bit	| +5
; DPR	16bit	| +6
; A	16bit	| +8
; PSR	8bit	| +10
; PC	16bit	| +11(L), +12(H)
; PBR	8bit	| +13

BRK_N:
	long_ai
	pha
	phd			; push DPR
	phb			; push DBR

	lda	#UNIMON_DP
	pha
	pld			; set Direct Page

	short_a
	lda	#UNIMON_DB
	pha
	plb			; set DATA BANK

	phx
	phy

	short_i
	
; check bios call
;
	long_a
	lda	11,s		; get PC
	dec	a		; get address of #n : (BRK '#n')
	sta	bop_adr
	short_a
	lda	13,s		; get PBR
	sta	bop_bnk
	lda	[bop_adr]	; get #n

	sta	bcode

	cmp	#$ff
	bne	bk_n
	jmp	wup
bk_n
	cmp	#0
	beq	go_brk
	cmp	#5
	bpl	go_brk

	jsr	bios_call
	sta	8,s		; save return code to A reg

	long_ai
	ply
	plx
	plb
	pld
	pla
	rti

	longa off
	longi off

creq_p
	dw	CONIN
	dw	CONOUT
	dw	CONST
	dw	str_prt

str_prt
	STA	PT0
	STY	PT0+1
	lda	7,s		; 5 + 2(jsr bios_call)
	STA	PT0_DB

	LDY	#0
dtr_prt1
	LDA	[PT0],Y
	BEQ	dtr_prt2
	JSR	CONOUT
	INY
	bra	dtr_prt1
dtr_prt2
	RTS

bios_call
	dec	A
	asl	A		; A = A * 2
	tax
	lda	10,s		; get A [stack = 8 + 2(jsr bios_call)]
	jmp	(creq_p,x)

	; BRK instruction
go_brk
	long_a

	pla		; get Y
	STA	REGY
	pla		; get X
	STA	REGX

	short_a
	pla			; get user DBR
	sta	REGDB		; save Data Bank register

	long_a
	pla			; get user DPR
	sta	REGDP		; save Direct Page register
	pla			; A
	STA	REGA

	short_a
	PLA			; PSR (Pushed by BRK)
	STA	REGPSR		; save status register
	STA	FLAGS		; save disassemble EmPCR

	long_ai
	PLA			; PC (Pushed by BRK)
	sta	ILL_PC
	dec	a
	dec	a
	STA	REGPC

	short_a
	pla			; PB register
	sta	REGPB
	
	TSX			; get SP
	STX	REGSP
	short_i

	; check break point
	lda	stp_flg
	beq	ill_stop

	; restore original code
	stz	stp_flg
	long_a
	lda	sav_dat		; get save data
	sta	[sv_adr]	; restore original code

	lda	sv_adr
	cmp	REGPC
	bne	ill_stop

	short_a
	lda	sv_bnk
	cmp	REGPB
	bne	ill_stop
	
	jsr	prt_nmode	; (Native mode)
	long_a
	lda	#stpmsg
	STA	PT0
	bra	b_outmsg

ill_stop
	; re-adjust PC
	long_a
	lda	ILL_PC
	sta	REGPC
	
	jsr	prt_nmode	; (Native mode)
	jsr	prt_dtct	; Detect 
	long_a
	lda	#BRK_MSG
	STA	PT0
b_outmsg
	JSR	STROUT
	JSR	RDUMP
	JMP	WSTART

;------------------------------------------
; Handle NMI interrupts in emulation mode.
;------------------------------------------
NMI_E:
	native		; set native mode
	long_a
	pha

	lda	#UNIMON_DP
	pha
	pld			; set Direct Page

	short_a
	lda	#UNIMON_DB
	pha
	plb			; set DATA BANK

	long_ai
	pla			; A
	STA	REGA
	TXA			; X
	STA	REGX
	TYA			; Y
	STA	REGY

	short_a
	PLA			; PSR (Pushed by NMI)
	STA	REGPSR		; save status register
	STA	FLAGS		; save disassemble EmPCR

	long_a
	PLA			; PC (Pushed by NMI)
	STA	REGPC
	stz	REGDP		; clear DP

	short_a
	stz	REGPB		; clear PB
	stz	REGDB		; clear DB

	TSX			; get SP
	STX	REGSP
	short_i

	jsr	prt_emode	; (Emulation mode)
	jsr	prt_dtct	; Detect 
	jsr	prt_nmim	; NMI

	JSR	RDUMP		; dump registers
	JMP	WSTART

;------------------------------------------
; Handle NMI interrupts in native mode.
;------------------------------------------
NMI_N:
	long_a
	pha
	phd			; push DPR
	phb			; push DBR

	lda	#UNIMON_DP
	pha
	pld			; set Direct Page

	short_a
	lda	#UNIMON_DB
	pha
	plb			; set DATA BANK

	pla			; get user DBR
	sta	REGDB		; save Data Bank register

	long_ai
	pla			; get user DPR
	sta	REGDP		; save Direct Page register(L)

	pla			; A
	STA	REGA
	TXA			; X
	STA	REGX
	TYA			; Y
	STA	REGY

	short_a
	PLA			; PSR (Pushed by NMI)
	STA	REGPSR		; save status register
	STA	FLAGS		; save disassemble EmPCR

	long_a
	PLA			; PC (Pushed by NMI)
	STA	REGPC

	short_a
	pla			; PB register
	sta	REGPB
	
	TSX			; get SP
	STX	REGSP
	short_i

	lda	#$ff		; NMI signal
	sta	UNI_CHR
	jsr	NMI_SIG

	jmp	G0


IRQBRK
	native		; set native mode
	long_a
	pha

	lda	#UNIMON_DP
	pha
	pld			; set Direct Page

	short_a
	lda	#UNIMON_DB
	pha
	plb			; set DATA BANK

	long_ai
	pla			; A
	STA	REGA
	TXA			; X
	STA	REGX
	TYA			; Y
	STA	REGY

	short_a
	PLA			; PSR (Pushed by IRQ/BRK)
	STA	REGPSR		; save status register
	STA	FLAGS		; save disassemble EmPCR

	long_a
	PLA			; PC (Pushed by IRQ/BRK)
	STA	REGPC
	stz	REGDP		; clear DP

	short_a
	stz	REGPB		; clear PB
	stz	REGDB		; clear DB

	TSX			; get SP
	STX	REGSP
	short_i

	jsr	prt_emode	; (Emulation mode)
	jsr	prt_dtct	; Detect 
	jsr	prt_ibkm	; IRQ/BRK
	JSR	RDUMP		; dump registers
	JMP	WSTART

; COP and ABORT interrupts are not handled.

COP_E:
COP_N:
ABORT_E:
ABORT_N:
	stp

	;;
	;; Vector area
	;; 

;Vectors         section offset $ffe0

	org	$ffe0

	ds	4		; Reserved
	dw	COP_N		; $FFE4 - COP(816)
	dw	BRK_N		; $FFE6 - BRK(816)
	dw	ABORT_N		; $FFE8 - ABORT(816)
	dw	NMI_N		; $FFEA - NMI(816)
	ds	2		; Reserved
	dw	IRQ_N		; $FFEE - IRQ(816)

	ds	4
	dw	COP_E		; $FFF4 - COP(C02)
	ds	2		; $Reserved
	dw	ABORT_E		; $FFF8 - ABORT(C02)

	dw	NMI_E		; NMI
	dw	CSTART		; RESET
	dw	IRQBRK		; IRQ/BRK

	END
