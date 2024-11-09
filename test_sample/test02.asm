	pl	0
	pw      132
	chip    65C02
;                inclist on

;;;
;;; File header template (BIOS CALL version)
;;;	

PRG_B	EQU	$400
WORK_B	EQU	PRG_B-$200	; $200

ZERO_B	EQU	$00

;-----------------------------------------------------
;ZERO page
; NOTE
;  Since ZERO page is shared with the monitor program,
;  you must use a free area.
;  See the monitor program.
;-----------------------------------------------------

	.page0
	ORG	ZERO_B


;--------------------------------------
; Data area
;--------------------------------------
	.data
	org	WORK_B




	.code
	ORG	PRG_B

;--------- MEZW65C_RAM file header --------------------------
	db	0		; program bank:W65C816, 0:W65C02
	dw	COLD_START
	db	0		; data bank:W65C816, 0:W65C02
	dw	WSTART

	dw	0		; DP

mezID:	db	"MEZW65C",0	; Unique ID

start_p:	;file load address
	dw	PRG_B		; load address (Low)
	db	0		; PBR : program bank(W65C816)
	db	0		; reserve

	; define Common memory address
PIC_IF:	dw	0	; reserve
	dw	0	; reserve

SW_816:	db	0	; 0 : W65C02
			; 1 : W65C816 native mode 
			; 2 : works in both modes
irq_sw	db	0	; 0 : no use IRQ console I/O
			; 1 : use IRQ timer interrupt driven console I/O
reg_tp	dw	0	; monitor reserve (register save pointer)
reg_ts	dw	0	; monitor reserve (register table size)
nmi_sw	db	0	; 0 : No NMI support, 1: NMI support
bios_sw	db	1	; 0 : standalone program
			; 1 : program call bios command
			; 2 : monitor program (.SYS)
;--------- MEZW65C_RAM file header --------------------------

COLD_START:
WSTART:

;-----------
; conput
;-----------
CONIN	equ	1
CONOUT	equ	2
CONST	equ	3
STROUT	equ	4
PEND	equ	$FF

	ldx	#0
	
msg_loop
	lda	msg,x
	beq	prt_end
	BRK	CONOUT		; 2 : conout A = char
	inx
	bra	msg_loop
	
prt_end
	lda	#$FF&endm
	ldy	#endm>>8
	BRK	STROUT
	BRK	CONIN

;-----------
;program end
;-----------

	BRK	PEND

msg	db	"Good morning! master",13,10,00
endm	db	"Press Key to end. ",13,10,0

	end
