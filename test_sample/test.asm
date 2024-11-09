	pl	0
	pw      132
	chip    65C02
;                inclist on

;	include "w65c816.inc"

CONIN	equ	1
CONOUT	equ	2
CONST	equ	3
STROUT	equ	4

	.code
	ORG	$200
	
	; echo back.  (terminate : assert NMI)

lp00
	ldx	#'A'
lp0
	ldy	#0
lpback
	brk	CONST
	and	#1
	beq	no_key
	brk	CONIN
no_key
	txa
	BRK	CONOUT		; ECHO BACK
	iny
	tya
	cmp	#80
	bne	lpback
	lda	#13
	BRK	CONOUT		; ECHO BACK
	lda	#10
	BRK	CONOUT		; ECHO BACK
	inx
	txa
	cmp	#'Z'+1
	beq	lp00
	bra	lp0

	end
