	pl	0
	pw      132
	chip    65C02
;                inclist on

CONIN	equ	1
CONOUT	equ	2
CONST	equ	3
STROUT	equ	4

	.code
	ORG	$200
	
;-----------
; conput
;-----------

	lda	#0
	tax
	
msg_loop
	lda	msg,x
	beq	prt_end
	BRK	CONOUT		; 2 : conout A = char
	inx
	bra	msg_loop
	
prt_end

;-----------
;strout
;-----------

	lda	#$FF&msg1
	ldy	#msg1>>8
	BRK	STROUT		; 4 : strings out A = strings addr(Low)
				; Y = strings addr(High)

;-----------
; conin
;-----------
;
	; console out "echo back"
	lda	#$FF&msg2
	ldy	#msg2>>8
	BRK	STROUT
	; echo back.  (terminate : assert NMI)
lpback

	BRK	CONST
	and	#1
	beq	beee
	
	BRK	CONIN		; 1 : conin  A : get char

	BRK	CONOUT		; ECHO BACK
	bra	lpback

beee
	lda	#' '
	BRK	CONOUT
	bra	lpback
	

msg	db	"Good morning! master",13,10,00
msg1	db	"Hello World!",13,10,0
msg2	db	"Echo back test.",13,10,0

	end
