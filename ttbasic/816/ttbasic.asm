;:ts=8
R0	equ	1
R1	equ	5
R2	equ	9
R3	equ	13
	code
	xdef	__main
	func
__main:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L2
	tcs
	phd
	tcd
	pea	#<$1966
	jsr	__srand
	jsr	__basic
	lda	#$0
L4:
	tay
	pld
	tsc
	clc
	adc	#L2
	tcs
	tya
	rts
L2	equ	0
L3	equ	1
	ends
	efunc
	code
	xdef	__newline
	func
__newline:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L5
	tcs
	phd
	tcd
	pea	#<$d
	jsr	__c_putch
	pea	#<$a
	jsr	__c_putch
L7:
	pld
	tsc
	clc
	adc	#L5
	tcs
	rts
L5	equ	0
L6	equ	1
	ends
	efunc
	code
	xdef	__getrnd
	func
__getrnd:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L8
	tcs
	phd
	tcd
value_0	set	3
	jsr	__rand
	lda	<R0
	ldx	<L8+value_0
	xref	__~mod
	jsr	__~mod
	sta	<R0
	lda	<R0
	ina
L10:
	tay
	lda	<L8+1
	sta	<L8+1+2
	pld
	tsc
	clc
	adc	#L8+2
	tcs
	tya
	rts
L8	equ	4
L9	equ	5
	ends
	efunc
	data
	xdef	__kwtbl
__kwtbl:
	dw	L1+0
	dw	L1+5
	dw	L1+11
	dw	L1+18
	dw	L1+22
	dw	L1+25
	dw	L1+30
	dw	L1+35
	dw	L1+38
	dw	L1+42
	dw	L1+47
	dw	L1+53
	dw	L1+59
	dw	L1+63
	dw	L1+65
	dw	L1+67
	dw	L1+69
	dw	L1+71
	dw	L1+73
	dw	L1+75
	dw	L1+77
	dw	L1+79
	dw	L1+82
	dw	L1+84
	dw	L1+86
	dw	L1+88
	dw	L1+91
	dw	L1+93
	dw	L1+95
	dw	L1+99
	dw	L1+103
	dw	L1+108
	dw	L1+113
	dw	L1+117
	dw	L1+121
	ends
	data
L1:
	db	$47,$4F,$54,$4F,$00,$47,$4F,$53,$55,$42,$00,$52,$45,$54,$55
	db	$52,$4E,$00,$46,$4F,$52,$00,$54,$4F,$00,$53,$54,$45,$50,$00
	db	$4E,$45,$58,$54,$00,$49,$46,$00,$52,$45,$4D,$00,$53,$54,$4F
	db	$50,$00,$49,$4E,$50,$55,$54,$00,$50,$52,$49,$4E,$54,$00,$4C
	db	$45,$54,$00,$2C,$00,$3B,$00,$2D,$00,$2B,$00,$2A,$00,$2F,$00
	db	$28,$00,$29,$00,$3E,$3D,$00,$23,$00,$3E,$00,$3D,$00,$3C,$3D
	db	$00,$3C,$00,$40,$00,$52,$4E,$44,$00,$41,$42,$53,$00,$53,$49
	db	$5A,$45,$00,$4C,$49,$53,$54,$00,$52,$55,$4E,$00,$4E,$45,$57
	db	$00,$53,$59,$53,$54,$45,$4D,$00
	ends
	data
	xdef	__i_nsa
__i_nsa:
	db	$2,$9,$D,$F,$10,$11,$12,$13,$14,$15
	db	$16,$17,$18,$19,$1A,$1B,$1C,$1D,$1E
	ends
	data
	xdef	__i_nsb
__i_nsb:
	db	$F,$10,$11,$12,$13,$14,$15,$16,$17,$18
	db	$19,$1A,$D,$E,$26
	ends
	code
	xdef	__sstyle
	func
__sstyle:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L12
	tcs
	phd
	tcd
code_0	set	3
table_0	set	5
count_0	set	7
L10001:
	sep	#$20
	longa	off
	lda	<L12+count_0
	sta	<R0
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	dec	<L12+count_0
	rep	#$20
	longa	on
	lda	<R0
	and	#$ff
	bne	L14
	brl	L10002
L14:
	lda	<L12+count_0
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	lda	<L12+code_0
	ldy	<R0
	cmp	(<L12+table_0),Y
	rep	#$20
	longa	on
	beq	L15
	brl	L10003
L15:
	lda	#$1
L16:
	tay
	lda	<L12+1
	sta	<L12+1+6
	pld
	tsc
	clc
	adc	#L12+6
	tcs
	tya
	rts
L10003:
	brl	L10001
L10002:
	lda	#$0
	brl	L16
L12	equ	4
L13	equ	5
	ends
	efunc
	data
	xdef	__errmsg
__errmsg:
	dw	L11+0
	dw	L11+3
	dw	L11+20
	dw	L11+29
	dw	L11+52
	dw	L11+70
	dw	L11+80
	dw	L11+102
	dw	L11+125
	dw	L11+145
	dw	L11+162
	dw	L11+183
	dw	L11+201
	dw	L11+222
	dw	L11+237
	dw	L11+258
	dw	L11+279
	dw	L11+301
	dw	L11+321
	dw	L11+334
	dw	L11+350
	dw	L11+363
	dw	L11+378
	ends
	data
L11:
	db	$4F,$4B,$00,$44,$65,$76,$69,$73,$69,$6F,$6E,$20,$62,$79,$20
	db	$7A,$65,$72,$6F,$00,$4F,$76,$65,$72,$66,$6C,$6F,$77,$00,$53
	db	$75,$62,$73,$63,$72,$69,$70,$74,$20,$6F,$75,$74,$20,$6F,$66
	db	$20,$72,$61,$6E,$67,$65,$00,$49,$63,$6F,$64,$65,$20,$62,$75
	db	$66,$66,$65,$72,$20,$66,$75,$6C,$6C,$00,$4C,$69,$73,$74,$20
	db	$66,$75,$6C,$6C,$00,$47,$4F,$53,$55,$42,$20,$74,$6F,$6F,$20
	db	$6D,$61,$6E,$79,$20,$6E,$65,$73,$74,$65,$64,$00,$52,$45,$54
	db	$55,$52,$4E,$20,$73,$74,$61,$63,$6B,$20,$75,$6E,$64,$65,$72
	db	$66,$6C,$6F,$77,$00,$46,$4F,$52,$20,$74,$6F,$6F,$20,$6D,$61
	db	$6E,$79,$20,$6E,$65,$73,$74,$65,$64,$00,$4E,$45,$58,$54,$20
	db	$77,$69,$74,$68,$6F,$75,$74,$20,$46,$4F,$52,$00,$4E,$45,$58
	db	$54,$20,$77,$69,$74,$68,$6F,$75,$74,$20,$63,$6F,$75,$6E,$74
	db	$65,$72,$00,$4E,$45,$58,$54,$20,$6D,$69,$73,$6D,$61,$74,$63
	db	$68,$20,$46,$4F,$52,$00,$46,$4F,$52,$20,$77,$69,$74,$68,$6F
	db	$75,$74,$20,$76,$61,$72,$69,$61,$62,$6C,$65,$00,$46,$4F,$52
	db	$20,$77,$69,$74,$68,$6F,$75,$74,$20,$54,$4F,$00,$4C,$45,$54
	db	$20,$77,$69,$74,$68,$6F,$75,$74,$20,$76,$61,$72,$69,$61,$62
	db	$6C,$65,$00,$49,$46,$20,$77,$69,$74,$68,$6F,$75,$74,$20,$63
	db	$6F,$6E,$64,$69,$74,$69,$6F,$6E,$00,$55,$6E,$64,$65,$66,$69
	db	$6E,$65,$64,$20,$6C,$69,$6E,$65,$20,$6E,$75,$6D,$62,$65,$72
	db	$00,$27,$28,$27,$20,$6F,$72,$20,$27,$29,$27,$20,$65,$78,$70
	db	$65,$63,$74,$65,$64,$00,$27,$3D,$27,$20,$65,$78,$70,$65,$63
	db	$74,$65,$64,$00,$49,$6C,$6C,$65,$67,$61,$6C,$20,$63,$6F,$6D
	db	$6D,$61,$6E,$64,$00,$53,$79,$6E,$74,$61,$78,$20,$65,$72,$72
	db	$6F,$72,$00,$49,$6E,$74,$65,$72,$6E,$61,$6C,$20,$65,$72,$72
	db	$6F,$72,$00,$41,$62,$6F,$72,$74,$20,$62,$79,$20,$5B,$45,$53
	db	$43,$5D,$00
	ends
	code
	xdef	__c_toupper
	func
__c_toupper:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L18
	tcs
	phd
	tcd
c_0	set	3
	sep	#$20
	longa	off
	lda	#$7a
	cmp	<L18+c_0
	rep	#$20
	longa	on
	bcs	L21
	brl	L20
L21:
	sep	#$20
	longa	off
	lda	<L18+c_0
	cmp	#<$61
	rep	#$20
	longa	on
	bcs	L22
	brl	L20
L22:
	lda	<L18+c_0
	and	#$ff
	sta	<R0
	clc
	lda	#$ffe0
	adc	<R0
	sta	<R1
	lda	<R1
	bra	L23
L20:
	lda	<L18+c_0
	and	#$ff
	sta	<R0
	lda	<R0
L23:
	sta	<R0
	lda	<R0
	and	#$ff
L24:
	tay
	lda	<L18+1
	sta	<L18+1+2
	pld
	tsc
	clc
	adc	#L18+2
	tcs
	tya
	rts
L18	equ	8
L19	equ	9
	ends
	efunc
	code
	xdef	__c_isprint
	func
__c_isprint:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L25
	tcs
	phd
	tcd
c_0	set	3
	stz	<R0
	sep	#$20
	longa	off
	lda	<L25+c_0
	cmp	#<$20
	rep	#$20
	longa	on
	bcs	L28
	brl	L27
L28:
	sep	#$20
	longa	off
	lda	#$7e
	cmp	<L25+c_0
	rep	#$20
	longa	on
	bcs	L29
	brl	L27
L29:
	inc	<R0
L27:
	lda	<R0
	and	#$ff
L30:
	tay
	lda	<L25+1
	sta	<L25+1+2
	pld
	tsc
	clc
	adc	#L25+2
	tcs
	tya
	rts
L25	equ	4
L26	equ	5
	ends
	efunc
	code
	xdef	__c_isspace
	func
__c_isspace:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L31
	tcs
	phd
	tcd
c_0	set	3
	stz	<R0
	sep	#$20
	longa	off
	lda	<L31+c_0
	cmp	#<$20
	rep	#$20
	longa	on
	bne	L35
	brl	L34
L35:
	sep	#$20
	longa	off
	lda	#$d
	cmp	<L31+c_0
	rep	#$20
	longa	on
	bcs	L36
	brl	L33
L36:
	sep	#$20
	longa	off
	lda	<L31+c_0
	cmp	#<$9
	rep	#$20
	longa	on
	bcs	L37
	brl	L33
L37:
L34:
	inc	<R0
L33:
	lda	<R0
	and	#$ff
L38:
	tay
	lda	<L31+1
	sta	<L31+1+2
	pld
	tsc
	clc
	adc	#L31+2
	tcs
	tya
	rts
L31	equ	4
L32	equ	5
	ends
	efunc
	code
	xdef	__c_isdigit
	func
__c_isdigit:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L39
	tcs
	phd
	tcd
c_0	set	3
	stz	<R0
	sep	#$20
	longa	off
	lda	#$39
	cmp	<L39+c_0
	rep	#$20
	longa	on
	bcs	L42
	brl	L41
L42:
	sep	#$20
	longa	off
	lda	<L39+c_0
	cmp	#<$30
	rep	#$20
	longa	on
	bcs	L43
	brl	L41
L43:
	inc	<R0
L41:
	lda	<R0
	and	#$ff
L44:
	tay
	lda	<L39+1
	sta	<L39+1+2
	pld
	tsc
	clc
	adc	#L39+2
	tcs
	tya
	rts
L39	equ	4
L40	equ	5
	ends
	efunc
	code
	xdef	__c_isalpha
	func
__c_isalpha:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L45
	tcs
	phd
	tcd
c_0	set	3
	stz	<R0
	sep	#$20
	longa	off
	lda	#$7a
	cmp	<L45+c_0
	rep	#$20
	longa	on
	bcs	L50
	brl	L49
L50:
	sep	#$20
	longa	off
	lda	<L45+c_0
	cmp	#<$61
	rep	#$20
	longa	on
	bcc	L51
	brl	L48
L51:
L49:
	sep	#$20
	longa	off
	lda	#$5a
	cmp	<L45+c_0
	rep	#$20
	longa	on
	bcs	L52
	brl	L47
L52:
	sep	#$20
	longa	off
	lda	<L45+c_0
	cmp	#<$41
	rep	#$20
	longa	on
	bcs	L53
	brl	L47
L53:
L48:
	inc	<R0
L47:
	lda	<R0
	and	#$ff
L54:
	tay
	lda	<L45+1
	sta	<L45+1+2
	pld
	tsc
	clc
	adc	#L45+2
	tcs
	tya
	rts
L45	equ	4
L46	equ	5
	ends
	efunc
	code
	xdef	__c_puts
	func
__c_puts:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L55
	tcs
	phd
	tcd
s_0	set	3
L10004:
	lda	(<L55+s_0)
	and	#$ff
	bne	L57
	brl	L10005
L57:
	lda	<L55+s_0
	sta	<R0
	inc	<L55+s_0
	lda	(<R0)
	pha
	jsr	__c_putch
	brl	L10004
L10005:
L58:
	lda	<L55+1
	sta	<L55+1+2
	pld
	tsc
	clc
	adc	#L55+2
	tcs
	rts
L55	equ	4
L56	equ	5
	ends
	efunc
	code
	xdef	__c_gets
	func
__c_gets:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L59
	tcs
	phd
	tcd
c_1	set	0
len_1	set	1
	sep	#$20
	longa	off
	stz	<L60+len_1
	rep	#$20
	longa	on
L10006:
	jsr	__c_getch
	sep	#$20
	longa	off
	sta	<L60+c_1
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	lda	<L60+c_1
	cmp	#<$d
	rep	#$20
	longa	on
	bne	L61
	brl	L10007
L61:
	sep	#$20
	longa	off
	lda	<L60+c_1
	cmp	#<$9
	rep	#$20
	longa	on
	beq	L62
	brl	L10008
L62:
	sep	#$20
	longa	off
	lda	#$20
	sta	<L60+c_1
	rep	#$20
	longa	on
L10008:
	sep	#$20
	longa	off
	lda	<L60+c_1
	cmp	#<$8
	rep	#$20
	longa	on
	bne	L64
	brl	L63
L64:
	sep	#$20
	longa	off
	lda	<L60+c_1
	cmp	#<$7f
	rep	#$20
	longa	on
	beq	L65
	brl	L10009
L65:
L63:
	sep	#$20
	longa	off
	lda	#$0
	cmp	<L60+len_1
	rep	#$20
	longa	on
	bcc	L66
	brl	L10009
L66:
	sep	#$20
	longa	off
	dec	<L60+len_1
	rep	#$20
	longa	on
	pea	#<$8
	jsr	__c_putch
	pea	#<$20
	jsr	__c_putch
	pea	#<$8
	jsr	__c_putch
	brl	L10010
L10009:
	pei	<L60+c_1
	jsr	__c_isprint
	and	#$ff
	bne	L67
	brl	L10011
L67:
	sep	#$20
	longa	off
	lda	<L60+len_1
	cmp	#<$9f
	rep	#$20
	longa	on
	bcc	L68
	brl	L10011
L68:
	lda	<L60+len_1
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	lda	<L60+c_1
	ldx	<R0
	sta	|__lbuf,X
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	inc	<L60+len_1
	rep	#$20
	longa	on
	pei	<L60+c_1
	jsr	__c_putch
L10011:
L10010:
	brl	L10006
L10007:
	jsr	__newline
	lda	<L60+len_1
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	lda	#$0
	ldx	<R0
	sta	|__lbuf,X
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	lda	#$0
	cmp	<L60+len_1
	rep	#$20
	longa	on
	bcc	L69
	brl	L10012
L69:
L10013:
	sep	#$20
	longa	off
	dec	<L60+len_1
	rep	#$20
	longa	on
	lda	<L60+len_1
	and	#$ff
	sta	<R0
	ldx	<R0
	lda	|__lbuf,X
	pha
	jsr	__c_isspace
	and	#$ff
	bne	L70
	brl	L10014
L70:
	brl	L10013
L10014:
	sep	#$20
	longa	off
	inc	<L60+len_1
	rep	#$20
	longa	on
	lda	<L60+len_1
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	lda	#$0
	ldx	<R0
	sta	|__lbuf,X
	rep	#$20
	longa	on
L10012:
L71:
	pld
	tsc
	clc
	adc	#L59
	tcs
	rts
L59	equ	6
L60	equ	5
	ends
	efunc
	code
	xdef	__putnum
	func
__putnum:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L72
	tcs
	phd
	tcd
value_0	set	3
d_0	set	5
i_1	set	0
sign_1	set	1
	lda	<L72+value_0
	bmi	L74
	brl	L10015
L74:
	sep	#$20
	longa	off
	lda	#$1
	sta	<L73+sign_1
	rep	#$20
	longa	on
	sec
	lda	#$0
	sbc	<L72+value_0
	sta	<L72+value_0
	brl	L10016
L10015:
	sep	#$20
	longa	off
	stz	<L73+sign_1
	rep	#$20
	longa	on
L10016:
	sep	#$20
	longa	off
	stz	|__lbuf+6
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	lda	#$6
	sta	<L73+i_1
	rep	#$20
	longa	on
L10019:
	sep	#$20
	longa	off
	dec	<L73+i_1
	rep	#$20
	longa	on
	lda	<L73+i_1
	and	#$ff
	sta	<R0
	lda	<L72+value_0
	ldx	#<$a
	xref	__~mod
	jsr	__~mod
	sta	<R1
	clc
	lda	#$30
	adc	<R1
	sep	#$20
	longa	off
	ldx	<R0
	sta	|__lbuf,X
	rep	#$20
	longa	on
	lda	<L72+value_0
	ldx	#<$a
	xref	__~div
	jsr	__~div
	sta	<L72+value_0
L10017:
	sec
	lda	#$0
	sbc	<L72+value_0
	bvs	L75
	eor	#$8000
L75:
	bmi	L76
	brl	L10019
L76:
L10018:
	lda	<L73+sign_1
	and	#$ff
	bne	L77
	brl	L10020
L77:
	sep	#$20
	longa	off
	dec	<L73+i_1
	rep	#$20
	longa	on
	lda	<L73+i_1
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	lda	#$2d
	ldx	<R0
	sta	|__lbuf,X
	rep	#$20
	longa	on
L10020:
L10021:
	lda	<L73+i_1
	and	#$ff
	sta	<R0
	sec
	lda	#$6
	sbc	<R0
	sta	<R1
	sec
	lda	<R1
	sbc	<L72+d_0
	bvs	L78
	eor	#$8000
L78:
	bpl	L79
	brl	L10022
L79:
	pea	#<$20
	jsr	__c_putch
	dec	<L72+d_0
	brl	L10021
L10022:
	lda	<L73+i_1
	and	#$ff
	sta	<R0
	clc
	lda	#<__lbuf
	adc	<R0
	sta	<R1
	pei	<R1
	jsr	__c_puts
L80:
	lda	<L72+1
	sta	<L72+1+4
	pld
	tsc
	clc
	adc	#L72+4
	tcs
	rts
L72	equ	10
L73	equ	9
	ends
	efunc
	code
	xdef	__getnum
	func
__getnum:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L81
	tcs
	phd
	tcd
value_1	set	0
tmp_1	set	2
c_1	set	4
len_1	set	5
sign_1	set	6
	sep	#$20
	longa	off
	stz	<L82+len_1
	rep	#$20
	longa	on
L10023:
	jsr	__c_getch
	sep	#$20
	longa	off
	sta	<L82+c_1
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	lda	<L82+c_1
	cmp	#<$d
	rep	#$20
	longa	on
	bne	L83
	brl	L10024
L83:
	sep	#$20
	longa	off
	lda	<L82+c_1
	cmp	#<$8
	rep	#$20
	longa	on
	bne	L85
	brl	L84
L85:
	sep	#$20
	longa	off
	lda	<L82+c_1
	cmp	#<$7f
	rep	#$20
	longa	on
	beq	L86
	brl	L10025
L86:
L84:
	sep	#$20
	longa	off
	lda	#$0
	cmp	<L82+len_1
	rep	#$20
	longa	on
	bcc	L87
	brl	L10025
L87:
	sep	#$20
	longa	off
	dec	<L82+len_1
	rep	#$20
	longa	on
	pea	#<$8
	jsr	__c_putch
	pea	#<$20
	jsr	__c_putch
	pea	#<$8
	jsr	__c_putch
	brl	L10026
L10025:
	lda	<L82+len_1
	and	#$ff
	beq	L90
	brl	L89
L90:
	sep	#$20
	longa	off
	lda	<L82+c_1
	cmp	#<$2b
	rep	#$20
	longa	on
	bne	L91
	brl	L88
L91:
	sep	#$20
	longa	off
	lda	<L82+c_1
	cmp	#<$2d
	rep	#$20
	longa	on
	bne	L92
	brl	L88
L92:
L89:
	sep	#$20
	longa	off
	lda	<L82+len_1
	cmp	#<$6
	rep	#$20
	longa	on
	bcc	L93
	brl	L10027
L93:
	pei	<L82+c_1
	jsr	__c_isdigit
	and	#$ff
	bne	L94
	brl	L10027
L94:
L88:
	lda	<L82+len_1
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	lda	<L82+c_1
	ldx	<R0
	sta	|__lbuf,X
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	inc	<L82+len_1
	rep	#$20
	longa	on
	pei	<L82+c_1
	jsr	__c_putch
L10027:
L10026:
	brl	L10023
L10024:
	jsr	__newline
	lda	<L82+len_1
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	lda	#$0
	ldx	<R0
	sta	|__lbuf,X
	rep	#$20
	longa	on
	lda	|__lbuf
	and	#$ff
	brl	L10028
L10030:
	sep	#$20
	longa	off
	lda	#$1
	sta	<L82+sign_1
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	lda	#$1
	sta	<L82+len_1
	rep	#$20
	longa	on
	brl	L10029
L10031:
	sep	#$20
	longa	off
	stz	<L82+sign_1
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	lda	#$1
	sta	<L82+len_1
	rep	#$20
	longa	on
	brl	L10029
L10032:
	sep	#$20
	longa	off
	stz	<L82+sign_1
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	stz	<L82+len_1
	rep	#$20
	longa	on
	brl	L10029
L10028:
	xref	__~swt
	jsr	__~swt
	dw	2
	dw	43
	dw	L10031-1
	dw	45
	dw	L10030-1
	dw	L10032-1
L10029:
	stz	<L82+value_1
	stz	<L82+tmp_1
L10033:
	lda	<L82+len_1
	and	#$ff
	sta	<R0
	ldx	<R0
	lda	|__lbuf,X
	and	#$ff
	bne	L95
	brl	L10034
L95:
	lda	<L82+len_1
	and	#$ff
	sta	<R0
	ldx	<R0
	lda	|__lbuf,X
	and	#$ff
	sta	<R1
	lda	<L82+value_1
	asl	A
	asl	A
	adc	<L82+value_1
	asl	A
	sta	<R2
	clc
	lda	<R2
	adc	<R1
	sta	<R3
	clc
	lda	#$ffd0
	adc	<R3
	sta	<L82+tmp_1
	sep	#$20
	longa	off
	inc	<L82+len_1
	rep	#$20
	longa	on
	sec
	lda	<L82+tmp_1
	sbc	<L82+value_1
	bvs	L96
	eor	#$8000
L96:
	bpl	L97
	brl	L10035
L97:
	sep	#$20
	longa	off
	lda	#$2
	sta	|__err
	rep	#$20
	longa	on
L10035:
	lda	<L82+tmp_1
	sta	<L82+value_1
	brl	L10033
L10034:
	lda	<L82+sign_1
	and	#$ff
	bne	L98
	brl	L10036
L98:
	sec
	lda	#$0
	sbc	<L82+value_1
L99:
	tay
	pld
	tsc
	clc
	adc	#L81
	tcs
	tya
	rts
L10036:
	lda	<L82+value_1
	brl	L99
L81	equ	23
L82	equ	17
	ends
	efunc
	code
	xdef	__toktoi
	func
__toktoi:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L100
	tcs
	phd
	tcd
i_1	set	0
len_1	set	1
pkw_1	set	2
ptok_1	set	4
s_1	set	6
c_1	set	8
value_1	set	9
tmp_1	set	11
	sep	#$20
	longa	off
	stz	<L101+len_1
	rep	#$20
	longa	on
	stz	<L101+pkw_1
	lda	#<__lbuf
	sta	<L101+s_1
L10037:
	lda	(<L101+s_1)
	and	#$ff
	bne	L102
	brl	L10038
L102:
L10039:
	lda	(<L101+s_1)
	pha
	jsr	__c_isspace
	and	#$ff
	bne	L103
	brl	L10040
L103:
	inc	<L101+s_1
	brl	L10039
L10040:
	sep	#$20
	longa	off
	stz	<L101+i_1
	rep	#$20
	longa	on
	brl	L10042
L10041:
	sep	#$20
	longa	off
	inc	<L101+i_1
	rep	#$20
	longa	on
L10042:
	sep	#$20
	longa	off
	lda	<L101+i_1
	cmp	#<$23
	rep	#$20
	longa	on
	bcc	L104
	brl	L10043
L104:
	lda	<L101+i_1
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__kwtbl
	adc	<R0
	sta	<R1
	lda	(<R1)
	sta	<L101+pkw_1
	lda	<L101+s_1
	sta	<L101+ptok_1
L10044:
	lda	(<L101+pkw_1)
	and	#$ff
	bne	L105
	brl	L10045
L105:
	lda	(<L101+ptok_1)
	pha
	jsr	__c_toupper
	sep	#$20
	longa	off
	sta	<R0
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	lda	<R0
	cmp	(<L101+pkw_1)
	rep	#$20
	longa	on
	beq	L106
	brl	L10045
L106:
	inc	<L101+pkw_1
	inc	<L101+ptok_1
	brl	L10044
L10045:
	lda	(<L101+pkw_1)
	and	#$ff
	beq	L107
	brl	L10046
L107:
	sep	#$20
	longa	off
	lda	<L101+len_1
	cmp	#<$9f
	rep	#$20
	longa	on
	bcs	L108
	brl	L10047
L108:
	sep	#$20
	longa	off
	lda	#$4
	sta	|__err
	rep	#$20
	longa	on
	lda	#$0
L109:
	tay
	pld
	tsc
	clc
	adc	#L100
	tcs
	tya
	rts
L10047:
	lda	<L101+len_1
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	lda	<L101+i_1
	ldx	<R0
	sta	|__ibuf,X
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	inc	<L101+len_1
	rep	#$20
	longa	on
	lda	<L101+ptok_1
	sta	<L101+s_1
	brl	L10043
L10046:
	brl	L10041
L10043:
	sep	#$20
	longa	off
	lda	<L101+i_1
	cmp	#<$8
	rep	#$20
	longa	on
	beq	L110
	brl	L10048
L110:
L10049:
	lda	(<L101+s_1)
	pha
	jsr	__c_isspace
	and	#$ff
	bne	L111
	brl	L10050
L111:
	inc	<L101+s_1
	brl	L10049
L10050:
	lda	<L101+s_1
	sta	<L101+ptok_1
	sep	#$20
	longa	off
	stz	<L101+i_1
	rep	#$20
	longa	on
	brl	L10052
L10051:
	sep	#$20
	longa	off
	inc	<L101+i_1
	rep	#$20
	longa	on
L10052:
	lda	<L101+ptok_1
	sta	<R0
	inc	<L101+ptok_1
	lda	(<R0)
	and	#$ff
	bne	L112
	brl	L10053
L112:
	brl	L10051
L10053:
	sep	#$20
	longa	off
	sec
	lda	#$9e
	sbc	<L101+i_1
	sta	<R0
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	lda	<L101+len_1
	cmp	<R0
	rep	#$20
	longa	on
	bcs	L113
	brl	L10054
L113:
	sep	#$20
	longa	off
	lda	#$4
	sta	|__err
	rep	#$20
	longa	on
	lda	#$0
	brl	L109
L10054:
	lda	<L101+len_1
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	lda	<L101+i_1
	ldx	<R0
	sta	|__ibuf,X
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	inc	<L101+len_1
	rep	#$20
	longa	on
L10055:
	sep	#$20
	longa	off
	lda	<L101+i_1
	sta	<R0
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	dec	<L101+i_1
	rep	#$20
	longa	on
	lda	<R0
	and	#$ff
	bne	L114
	brl	L10056
L114:
	lda	<L101+len_1
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	lda	(<L101+s_1)
	ldx	<R0
	sta	|__ibuf,X
	rep	#$20
	longa	on
	inc	<L101+s_1
	sep	#$20
	longa	off
	inc	<L101+len_1
	rep	#$20
	longa	on
	brl	L10055
L10056:
	brl	L10038
L10048:
	lda	(<L101+pkw_1)
	and	#$ff
	bne	L115
	brl	L10037
L115:
	lda	<L101+s_1
	sta	<L101+ptok_1
	lda	(<L101+ptok_1)
	pha
	jsr	__c_isdigit
	and	#$ff
	bne	L116
	brl	L10057
L116:
	stz	<L101+value_1
	stz	<L101+tmp_1
L10060:
	lda	(<L101+ptok_1)
	and	#$ff
	sta	<R0
	lda	<L101+value_1
	asl	A
	asl	A
	adc	<L101+value_1
	asl	A
	sta	<R1
	clc
	lda	<R1
	adc	<R0
	sta	<R2
	clc
	lda	#$ffd0
	adc	<R2
	sta	<L101+tmp_1
	inc	<L101+ptok_1
	sec
	lda	<L101+tmp_1
	sbc	<L101+value_1
	bvs	L117
	eor	#$8000
L117:
	bpl	L118
	brl	L10061
L118:
	sep	#$20
	longa	off
	lda	#$2
	sta	|__err
	rep	#$20
	longa	on
	lda	#$0
	brl	L109
L10061:
	lda	<L101+tmp_1
	sta	<L101+value_1
L10058:
	lda	(<L101+ptok_1)
	pha
	jsr	__c_isdigit
	and	#$ff
	beq	L119
	brl	L10060
L119:
L10059:
	sep	#$20
	longa	off
	lda	<L101+len_1
	cmp	#<$9d
	rep	#$20
	longa	on
	bcs	L120
	brl	L10062
L120:
	sep	#$20
	longa	off
	lda	#$4
	sta	|__err
	rep	#$20
	longa	on
	lda	#$0
	brl	L109
L10062:
	lda	<L101+len_1
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	lda	#$23
	ldx	<R0
	sta	|__ibuf,X
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	inc	<L101+len_1
	rep	#$20
	longa	on
	lda	<L101+len_1
	and	#$ff
	sta	<R0
	lda	<L101+value_1
	and	#<$ff
	sep	#$20
	longa	off
	ldx	<R0
	sta	|__ibuf,X
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	inc	<L101+len_1
	rep	#$20
	longa	on
	lda	<L101+len_1
	and	#$ff
	sta	<R0
	lda	<L101+value_1
	ldx	#<$8
	xref	__~asr
	jsr	__~asr
	sep	#$20
	longa	off
	ldx	<R0
	sta	|__ibuf,X
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	inc	<L101+len_1
	rep	#$20
	longa	on
	lda	<L101+ptok_1
	sta	<L101+s_1
	brl	L10063
L10057:
	sep	#$20
	longa	off
	lda	(<L101+s_1)
	cmp	#<$22
	rep	#$20
	longa	on
	bne	L122
	brl	L121
L122:
	sep	#$20
	longa	off
	lda	(<L101+s_1)
	cmp	#<$27
	rep	#$20
	longa	on
	beq	L123
	brl	L10064
L123:
L121:
	sep	#$20
	longa	off
	lda	(<L101+s_1)
	sta	<L101+c_1
	rep	#$20
	longa	on
	inc	<L101+s_1
	lda	<L101+s_1
	sta	<L101+ptok_1
	sep	#$20
	longa	off
	stz	<L101+i_1
	rep	#$20
	longa	on
	brl	L10066
L10065:
	sep	#$20
	longa	off
	inc	<L101+i_1
	rep	#$20
	longa	on
L10066:
	sep	#$20
	longa	off
	lda	(<L101+ptok_1)
	cmp	<L101+c_1
	rep	#$20
	longa	on
	bne	L124
	brl	L10067
L124:
	lda	(<L101+ptok_1)
	pha
	jsr	__c_isprint
	and	#$ff
	bne	L125
	brl	L10067
L125:
	inc	<L101+ptok_1
	brl	L10065
L10067:
	sep	#$20
	longa	off
	sec
	lda	#$9f
	sbc	<L101+i_1
	sta	<R0
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	lda	<L101+len_1
	cmp	<R0
	rep	#$20
	longa	on
	bcs	L126
	brl	L10068
L126:
	sep	#$20
	longa	off
	lda	#$4
	sta	|__err
	rep	#$20
	longa	on
	lda	#$0
	brl	L109
L10068:
	lda	<L101+len_1
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	lda	#$25
	ldx	<R0
	sta	|__ibuf,X
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	inc	<L101+len_1
	rep	#$20
	longa	on
	lda	<L101+len_1
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	lda	<L101+i_1
	ldx	<R0
	sta	|__ibuf,X
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	inc	<L101+len_1
	rep	#$20
	longa	on
L10069:
	sep	#$20
	longa	off
	lda	<L101+i_1
	sta	<R0
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	dec	<L101+i_1
	rep	#$20
	longa	on
	lda	<R0
	and	#$ff
	bne	L127
	brl	L10070
L127:
	lda	<L101+len_1
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	lda	(<L101+s_1)
	ldx	<R0
	sta	|__ibuf,X
	rep	#$20
	longa	on
	inc	<L101+s_1
	sep	#$20
	longa	off
	inc	<L101+len_1
	rep	#$20
	longa	on
	brl	L10069
L10070:
	sep	#$20
	longa	off
	lda	(<L101+s_1)
	cmp	<L101+c_1
	rep	#$20
	longa	on
	beq	L128
	brl	L10071
L128:
	inc	<L101+s_1
L10071:
	brl	L10072
L10064:
	lda	(<L101+ptok_1)
	pha
	jsr	__c_isalpha
	and	#$ff
	bne	L129
	brl	L10073
L129:
	sep	#$20
	longa	off
	lda	<L101+len_1
	cmp	#<$9e
	rep	#$20
	longa	on
	bcs	L130
	brl	L10074
L130:
	sep	#$20
	longa	off
	lda	#$4
	sta	|__err
	rep	#$20
	longa	on
	lda	#$0
	brl	L109
L10074:
	sep	#$20
	longa	off
	lda	<L101+len_1
	cmp	#<$4
	rep	#$20
	longa	on
	bcs	L131
	brl	L10075
L131:
	lda	<L101+len_1
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	ldx	<R0
	lda	|__ibuf-2,X
	cmp	#<$24
	rep	#$20
	longa	on
	beq	L132
	brl	L10075
L132:
	lda	<L101+len_1
	and	#$ff
	sta	<R1
	sep	#$20
	longa	off
	ldx	<R1
	lda	|__ibuf-4,X
	cmp	#<$24
	rep	#$20
	longa	on
	beq	L133
	brl	L10075
L133:
	sep	#$20
	longa	off
	lda	#$14
	sta	|__err
	rep	#$20
	longa	on
	lda	#$0
	brl	L109
L10075:
	lda	<L101+len_1
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	lda	#$24
	ldx	<R0
	sta	|__ibuf,X
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	inc	<L101+len_1
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	lda	<L101+len_1
	sta	<R0
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	inc	<L101+len_1
	rep	#$20
	longa	on
	lda	<R0
	and	#$ff
	sta	<R0
	lda	(<L101+ptok_1)
	pha
	jsr	__c_toupper
	sep	#$20
	longa	off
	sta	<R1
	rep	#$20
	longa	on
	lda	<R1
	and	#$ff
	sta	<R1
	clc
	lda	#$ffbf
	adc	<R1
	sep	#$20
	longa	off
	ldx	<R0
	sta	|__ibuf,X
	rep	#$20
	longa	on
	inc	<L101+s_1
	brl	L10076
L10073:
	sep	#$20
	longa	off
	lda	#$14
	sta	|__err
	rep	#$20
	longa	on
	lda	#$0
	brl	L109
L10076:
L10072:
L10063:
	brl	L10037
L10038:
	lda	<L101+len_1
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	lda	#$26
	ldx	<R0
	sta	|__ibuf,X
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	inc	<L101+len_1
	rep	#$20
	longa	on
	lda	<L101+len_1
	and	#$ff
	brl	L109
L100	equ	25
L101	equ	13
	ends
	efunc
	code
	xdef	__getlineno
	func
__getlineno:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L134
	tcs
	phd
	tcd
lp_0	set	3
	lda	(<L134+lp_0)
	and	#$ff
	beq	L136
	brl	L10077
L136:
	lda	#$7fff
L137:
	tay
	lda	<L134+1
	sta	<L134+1+2
	pld
	tsc
	clc
	adc	#L134+2
	tcs
	tya
	rts
L10077:
	ldy	#$1
	lda	(<L134+lp_0),Y
	and	#$ff
	sta	<R0
	ldy	#$2
	lda	(<L134+lp_0),Y
	and	#$ff
	sta	<R2
	lda	<R2
	xba
	and	#$ff00
	sta	<R1
	lda	<R1
	ora	<R0
	brl	L137
L134	equ	12
L135	equ	13
	ends
	efunc
	code
	xdef	__getlp
	func
__getlp:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L138
	tcs
	phd
	tcd
lineno_0	set	3
lp_1	set	0
	lda	#<__listbuf
	sta	<L139+lp_1
	brl	L10079
L10078:
	lda	(<L139+lp_1)
	and	#$ff
	sta	<R0
	clc
	lda	<L139+lp_1
	adc	<R0
	sta	<L139+lp_1
L10079:
	lda	(<L139+lp_1)
	and	#$ff
	bne	L140
	brl	L10080
L140:
	pei	<L139+lp_1
	jsr	__getlineno
	sta	<R0
	sec
	lda	<R0
	sbc	<L138+lineno_0
	bvs	L141
	eor	#$8000
L141:
	bpl	L142
	brl	L10080
L142:
	brl	L10078
L10080:
	lda	<L139+lp_1
L143:
	tay
	lda	<L138+1
	sta	<L138+1+2
	pld
	tsc
	clc
	adc	#L138+2
	tcs
	tya
	rts
L138	equ	6
L139	equ	5
	ends
	efunc
	code
	xdef	__getsize
	func
__getsize:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L144
	tcs
	phd
	tcd
lp_1	set	0
	lda	#<__listbuf
	sta	<L145+lp_1
	brl	L10082
L10081:
	lda	(<L145+lp_1)
	and	#$ff
	sta	<R0
	clc
	lda	<L145+lp_1
	adc	<R0
	sta	<L145+lp_1
L10082:
	lda	(<L145+lp_1)
	and	#$ff
	bne	L146
	brl	L10083
L146:
	brl	L10081
L10083:
	sec
	lda	#<__listbuf+30592
	sbc	<L145+lp_1
	sta	<R0
	clc
	lda	#$ffff
	adc	<R0
L147:
	tay
	pld
	tsc
	clc
	adc	#L144
	tcs
	tya
	rts
L144	equ	6
L145	equ	5
	ends
	efunc
	code
	xdef	__inslist
	func
__inslist:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L148
	tcs
	phd
	tcd
insp_1	set	0
p1_1	set	2
p2_1	set	4
len_1	set	6
	lda	|__ibuf
	and	#$ff
	sta	<R0
	jsr	__getsize
	sta	<R1
	sec
	lda	<R1
	sbc	<R0
	bvs	L150
	eor	#$8000
L150:
	bpl	L151
	brl	L10084
L151:
	sep	#$20
	longa	off
	lda	#$5
	sta	|__err
	rep	#$20
	longa	on
L152:
	pld
	tsc
	clc
	adc	#L148
	tcs
	rts
L10084:
	pea	#<__ibuf
	jsr	__getlineno
	pha
	jsr	__getlp
	sta	<L149+insp_1
	pea	#<__ibuf
	jsr	__getlineno
	sta	<R0
	pei	<L149+insp_1
	jsr	__getlineno
	sta	<R1
	lda	<R1
	cmp	<R0
	beq	L153
	brl	L10085
L153:
	lda	<L149+insp_1
	sta	<L149+p1_1
	lda	(<L149+p1_1)
	and	#$ff
	sta	<R0
	clc
	lda	<L149+p1_1
	adc	<R0
	sta	<L149+p2_1
L10086:
	lda	(<L149+p2_1)
	and	#$ff
	sta	<L149+len_1
	lda	<L149+len_1
	bne	L154
	brl	L10087
L154:
L10088:
	lda	<L149+len_1
	sta	<R0
	dec	<L149+len_1
	lda	<R0
	bne	L155
	brl	L10089
L155:
	sep	#$20
	longa	off
	lda	(<L149+p2_1)
	sta	(<L149+p1_1)
	rep	#$20
	longa	on
	inc	<L149+p2_1
	inc	<L149+p1_1
	brl	L10088
L10089:
	brl	L10086
L10087:
	sep	#$20
	longa	off
	lda	#$0
	sta	(<L149+p1_1)
	rep	#$20
	longa	on
L10085:
	sep	#$20
	longa	off
	lda	|__ibuf
	cmp	#<$4
	rep	#$20
	longa	on
	beq	L156
	brl	L10090
L156:
	brl	L152
L10090:
	lda	<L149+insp_1
	sta	<L149+p1_1
	brl	L10092
L10091:
	lda	(<L149+p1_1)
	and	#$ff
	sta	<R0
	clc
	lda	<L149+p1_1
	adc	<R0
	sta	<L149+p1_1
L10092:
	lda	(<L149+p1_1)
	and	#$ff
	bne	L157
	brl	L10093
L157:
	brl	L10091
L10093:
	sec
	lda	<L149+p1_1
	sbc	<L149+insp_1
	sta	<R0
	lda	<R0
	ina
	sta	<L149+len_1
	lda	|__ibuf
	and	#$ff
	sta	<R0
	clc
	lda	<L149+p1_1
	adc	<R0
	sta	<L149+p2_1
L10094:
	lda	<L149+len_1
	sta	<R0
	dec	<L149+len_1
	lda	<R0
	bne	L158
	brl	L10095
L158:
	sep	#$20
	longa	off
	lda	(<L149+p1_1)
	sta	(<L149+p2_1)
	rep	#$20
	longa	on
	dec	<L149+p1_1
	dec	<L149+p2_1
	brl	L10094
L10095:
	lda	|__ibuf
	and	#$ff
	sta	<L149+len_1
	lda	<L149+insp_1
	sta	<L149+p1_1
	lda	#<__ibuf
	sta	<L149+p2_1
L10096:
	lda	<L149+len_1
	sta	<R0
	dec	<L149+len_1
	lda	<R0
	bne	L159
	brl	L10097
L159:
	sep	#$20
	longa	off
	lda	(<L149+p2_1)
	sta	(<L149+p1_1)
	rep	#$20
	longa	on
	inc	<L149+p2_1
	inc	<L149+p1_1
	brl	L10096
L10097:
	brl	L152
L148	equ	16
L149	equ	9
	ends
	efunc
	code
	xdef	__putlist
	func
__putlist:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L160
	tcs
	phd
	tcd
ip_0	set	3
i_1	set	0
L10098:
	sep	#$20
	longa	off
	lda	(<L160+ip_0)
	cmp	#<$26
	rep	#$20
	longa	on
	bne	L162
	brl	L10099
L162:
	sep	#$20
	longa	off
	lda	(<L160+ip_0)
	cmp	#<$23
	rep	#$20
	longa	on
	bcc	L163
	brl	L10100
L163:
	lda	(<L160+ip_0)
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__kwtbl
	adc	<R0
	sta	<R1
	lda	(<R1)
	pha
	jsr	__c_puts
	pea	#<$13
	pea	#<__i_nsa
	lda	(<L160+ip_0)
	pha
	jsr	__sstyle
	and	#$ff
	beq	L164
	brl	L10101
L164:
	pea	#<$20
	jsr	__c_putch
L10101:
	sep	#$20
	longa	off
	lda	(<L160+ip_0)
	cmp	#<$8
	rep	#$20
	longa	on
	beq	L165
	brl	L10102
L165:
	inc	<L160+ip_0
	sep	#$20
	longa	off
	lda	(<L160+ip_0)
	sta	<L161+i_1
	rep	#$20
	longa	on
	inc	<L160+ip_0
L10103:
	sep	#$20
	longa	off
	lda	<L161+i_1
	sta	<R0
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	dec	<L161+i_1
	rep	#$20
	longa	on
	lda	<R0
	and	#$ff
	bne	L166
	brl	L10104
L166:
	lda	<L160+ip_0
	sta	<R0
	inc	<L160+ip_0
	lda	(<R0)
	pha
	jsr	__c_putch
	brl	L10103
L10104:
L167:
	lda	<L160+1
	sta	<L160+1+2
	pld
	tsc
	clc
	adc	#L160+2
	tcs
	rts
L10102:
	inc	<L160+ip_0
	brl	L10105
L10100:
	sep	#$20
	longa	off
	lda	(<L160+ip_0)
	cmp	#<$23
	rep	#$20
	longa	on
	beq	L168
	brl	L10106
L168:
	inc	<L160+ip_0
	pea	#<$0
	lda	(<L160+ip_0)
	and	#$ff
	sta	<R0
	ldy	#$1
	lda	(<L160+ip_0),Y
	and	#$ff
	sta	<R2
	lda	<R2
	xba
	and	#$ff00
	sta	<R1
	lda	<R1
	ora	<R0
	pha
	jsr	__putnum
	inc	<L160+ip_0
	inc	<L160+ip_0
	pea	#<$f
	pea	#<__i_nsb
	lda	(<L160+ip_0)
	pha
	jsr	__sstyle
	and	#$ff
	beq	L169
	brl	L10107
L169:
	pea	#<$20
	jsr	__c_putch
L10107:
	brl	L10108
L10106:
	sep	#$20
	longa	off
	lda	(<L160+ip_0)
	cmp	#<$24
	rep	#$20
	longa	on
	beq	L170
	brl	L10109
L170:
	inc	<L160+ip_0
	lda	<L160+ip_0
	sta	<R0
	inc	<L160+ip_0
	lda	(<R0)
	and	#$ff
	sta	<R0
	clc
	lda	#$41
	adc	<R0
	pha
	jsr	__c_putch
	pea	#<$f
	pea	#<__i_nsb
	lda	(<L160+ip_0)
	pha
	jsr	__sstyle
	and	#$ff
	beq	L171
	brl	L10110
L171:
	pea	#<$20
	jsr	__c_putch
L10110:
	brl	L10111
L10109:
	sep	#$20
	longa	off
	lda	(<L160+ip_0)
	cmp	#<$25
	rep	#$20
	longa	on
	beq	L172
	brl	L10112
L172:
c_2	set	1
	sep	#$20
	longa	off
	lda	#$22
	sta	<L161+c_2
	rep	#$20
	longa	on
	inc	<L160+ip_0
	sep	#$20
	longa	off
	lda	(<L160+ip_0)
	sta	<L161+i_1
	rep	#$20
	longa	on
	brl	L10114
L10113:
	sep	#$20
	longa	off
	dec	<L161+i_1
	rep	#$20
	longa	on
L10114:
	lda	<L161+i_1
	and	#$ff
	bne	L173
	brl	L10115
L173:
	lda	<L161+i_1
	and	#$ff
	sta	<R0
	sep	#$20
	longa	off
	ldy	<R0
	lda	(<L160+ip_0),Y
	cmp	#<$22
	rep	#$20
	longa	on
	beq	L174
	brl	L10116
L174:
	sep	#$20
	longa	off
	lda	#$27
	sta	<L161+c_2
	rep	#$20
	longa	on
	brl	L10115
L10116:
	brl	L10113
L10115:
	pei	<L161+c_2
	jsr	__c_putch
	sep	#$20
	longa	off
	lda	(<L160+ip_0)
	sta	<L161+i_1
	rep	#$20
	longa	on
	inc	<L160+ip_0
L10117:
	sep	#$20
	longa	off
	lda	<L161+i_1
	sta	<R0
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	dec	<L161+i_1
	rep	#$20
	longa	on
	lda	<R0
	and	#$ff
	bne	L175
	brl	L10118
L175:
	lda	<L160+ip_0
	sta	<R0
	inc	<L160+ip_0
	lda	(<R0)
	pha
	jsr	__c_putch
	brl	L10117
L10118:
	pei	<L161+c_2
	jsr	__c_putch
	sep	#$20
	longa	off
	lda	(<L160+ip_0)
	cmp	#<$24
	rep	#$20
	longa	on
	beq	L176
	brl	L10119
L176:
	pea	#<$20
	jsr	__c_putch
L10119:
	brl	L10120
L10112:
	sep	#$20
	longa	off
	lda	#$15
	sta	|__err
	rep	#$20
	longa	on
	brl	L167
L10120:
L10111:
L10108:
L10105:
	brl	L10098
L10099:
	brl	L167
L160	equ	14
L161	equ	13
	ends
	efunc
	code
	xdef	__getparam
	func
__getparam:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L177
	tcs
	phd
	tcd
value_1	set	0
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$13
	rep	#$20
	longa	on
	bne	L179
	brl	L10121
L179:
	sep	#$20
	longa	off
	lda	#$11
	sta	|__err
	rep	#$20
	longa	on
	lda	#$0
L180:
	tay
	pld
	tsc
	clc
	adc	#L177
	tcs
	tya
	rts
L10121:
	inc	|__cip
	jsr	__iexp
	sta	<L178+value_1
	lda	|__err
	and	#$ff
	bne	L181
	brl	L10122
L181:
	lda	#$0
	brl	L180
L10122:
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$14
	rep	#$20
	longa	on
	bne	L182
	brl	L10123
L182:
	sep	#$20
	longa	off
	lda	#$11
	sta	|__err
	rep	#$20
	longa	on
	lda	#$0
	brl	L180
L10123:
	inc	|__cip
	lda	<L178+value_1
	brl	L180
L177	equ	6
L178	equ	5
	ends
	efunc
	code
	xdef	__ivalue
	func
__ivalue:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L183
	tcs
	phd
	tcd
value_1	set	0
	lda	|__cip
	sta	<R0
	lda	(<R0)
	and	#$ff
	brl	L10124
L10126:
	inc	|__cip
	lda	|__cip
	sta	<R0
	lda	(<R0)
	and	#$ff
	sta	<R0
	lda	|__cip
	sta	<R2
	ldy	#$1
	lda	(<R2),Y
	and	#$ff
	sta	<R2
	lda	<R2
	xba
	and	#$ff00
	sta	<R1
	lda	<R1
	ora	<R0
	sta	<L184+value_1
	inc	|__cip
	inc	|__cip
	brl	L10125
L10127:
	inc	|__cip
	jsr	__ivalue
	sta	<L184+value_1
	brl	L10125
L10128:
	inc	|__cip
	jsr	__ivalue
	sta	<R0
	sec
	lda	#$0
	sbc	<R0
	sta	<L184+value_1
	brl	L10125
L10129:
	inc	|__cip
	lda	|__cip
	sta	<R1
	lda	(<R1)
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__var
	adc	<R0
	sta	<R1
	lda	(<R1)
	sta	<L184+value_1
	inc	|__cip
	brl	L10125
L10130:
	jsr	__getparam
	sta	<L184+value_1
	brl	L10125
L10131:
	inc	|__cip
	jsr	__getparam
	sta	<L184+value_1
	lda	|__err
	and	#$ff
	beq	L185
	brl	L10125
L185:
	sec
	lda	<L184+value_1
	sbc	#<$40
	bvs	L186
	eor	#$8000
L186:
	bmi	L187
	brl	L10132
L187:
	sep	#$20
	longa	off
	lda	#$3
	sta	|__err
	rep	#$20
	longa	on
	brl	L10125
L10132:
	lda	<L184+value_1
	asl	A
	sta	<R0
	clc
	lda	#<__arr
	adc	<R0
	sta	<R1
	lda	(<R1)
	sta	<L184+value_1
	brl	L10125
L10133:
	inc	|__cip
	jsr	__getparam
	sta	<L184+value_1
	lda	|__err
	and	#$ff
	beq	L188
	brl	L10125
L188:
	pei	<L184+value_1
	jsr	__getrnd
	sta	<L184+value_1
	brl	L10125
L10134:
	inc	|__cip
	jsr	__getparam
	sta	<L184+value_1
	lda	|__err
	and	#$ff
	beq	L189
	brl	L10125
L189:
	lda	<L184+value_1
	bmi	L190
	brl	L10135
L190:
	sec
	lda	#$0
	sbc	<L184+value_1
	sta	<L184+value_1
L10135:
	brl	L10125
L10136:
	inc	|__cip
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$13
	rep	#$20
	longa	on
	beq	L192
	brl	L191
L192:
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	ldy	#$1
	lda	(<R0),Y
	cmp	#<$14
	rep	#$20
	longa	on
	bne	L193
	brl	L10137
L193:
L191:
	sep	#$20
	longa	off
	lda	#$11
	sta	|__err
	rep	#$20
	longa	on
	brl	L10125
L10137:
	inc	|__cip
	inc	|__cip
	jsr	__getsize
	sta	<L184+value_1
	brl	L10125
L10138:
	sep	#$20
	longa	off
	lda	#$14
	sta	|__err
	rep	#$20
	longa	on
	brl	L10125
L10124:
	xref	__~fsw
	jsr	__~fsw
	dw	15
	dw	22
	dw	L10138-1
	dw	L10128-1
	dw	L10127-1
	dw	L10138-1
	dw	L10138-1
	dw	L10130-1
	dw	L10138-1
	dw	L10138-1
	dw	L10138-1
	dw	L10138-1
	dw	L10138-1
	dw	L10138-1
	dw	L10138-1
	dw	L10131-1
	dw	L10133-1
	dw	L10134-1
	dw	L10136-1
	dw	L10138-1
	dw	L10138-1
	dw	L10138-1
	dw	L10138-1
	dw	L10126-1
	dw	L10129-1
L10125:
	lda	<L184+value_1
L194:
	tay
	pld
	tsc
	clc
	adc	#L183
	tcs
	tya
	rts
L183	equ	14
L184	equ	13
	ends
	efunc
	code
	xdef	__imul
	func
__imul:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L195
	tcs
	phd
	tcd
value_1	set	0
tmp_1	set	2
	jsr	__ivalue
	sta	<L196+value_1
	lda	|__err
	and	#$ff
	bne	L197
	brl	L10139
L197:
	lda	#$ffff
L198:
	tay
	pld
	tsc
	clc
	adc	#L195
	tcs
	tya
	rts
L10139:
L10140:
	lda	|__cip
	sta	<R0
	lda	(<R0)
	and	#$ff
	brl	L10142
L10144:
	inc	|__cip
	jsr	__ivalue
	sta	<L196+tmp_1
	lda	<L196+value_1
	ldx	<L196+tmp_1
	xref	__~mul
	jsr	__~mul
	sta	<L196+value_1
	brl	L10143
L10145:
	inc	|__cip
	jsr	__ivalue
	sta	<L196+tmp_1
	lda	<L196+tmp_1
	beq	L199
	brl	L10146
L199:
	sep	#$20
	longa	off
	lda	#$1
	sta	|__err
	rep	#$20
	longa	on
	lda	#$ffff
	brl	L198
L10146:
	lda	<L196+value_1
	ldx	<L196+tmp_1
	xref	__~div
	jsr	__~div
	sta	<L196+value_1
	brl	L10143
L10147:
	lda	<L196+value_1
	brl	L198
L10142:
	xref	__~swt
	jsr	__~swt
	dw	2
	dw	17
	dw	L10144-1
	dw	18
	dw	L10145-1
	dw	L10147-1
L10143:
	brl	L10140
L195	equ	8
L196	equ	5
	ends
	efunc
	code
	xdef	__iplus
	func
__iplus:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L200
	tcs
	phd
	tcd
value_1	set	0
tmp_1	set	2
	jsr	__imul
	sta	<L201+value_1
	lda	|__err
	and	#$ff
	bne	L202
	brl	L10148
L202:
	lda	#$ffff
L203:
	tay
	pld
	tsc
	clc
	adc	#L200
	tcs
	tya
	rts
L10148:
L10149:
	lda	|__cip
	sta	<R0
	lda	(<R0)
	and	#$ff
	brl	L10151
L10153:
	inc	|__cip
	jsr	__imul
	sta	<L201+tmp_1
	clc
	lda	<L201+value_1
	adc	<L201+tmp_1
	sta	<L201+value_1
	brl	L10152
L10154:
	inc	|__cip
	jsr	__imul
	sta	<L201+tmp_1
	sec
	lda	<L201+value_1
	sbc	<L201+tmp_1
	sta	<L201+value_1
	brl	L10152
L10155:
	lda	<L201+value_1
	brl	L203
L10151:
	xref	__~swt
	jsr	__~swt
	dw	2
	dw	15
	dw	L10154-1
	dw	16
	dw	L10153-1
	dw	L10155-1
L10152:
	brl	L10149
L200	equ	8
L201	equ	5
	ends
	efunc
	code
	xdef	__iexp
	func
__iexp:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L204
	tcs
	phd
	tcd
value_1	set	0
tmp_1	set	2
	jsr	__iplus
	sta	<L205+value_1
	lda	|__err
	and	#$ff
	bne	L206
	brl	L10156
L206:
	lda	#$ffff
L207:
	tay
	pld
	tsc
	clc
	adc	#L204
	tcs
	tya
	rts
L10156:
L10157:
	lda	|__cip
	sta	<R0
	lda	(<R0)
	and	#$ff
	brl	L10159
L10161:
	inc	|__cip
	jsr	__iplus
	sta	<L205+tmp_1
	stz	<R0
	lda	<L205+value_1
	cmp	<L205+tmp_1
	beq	L209
	brl	L208
L209:
	inc	<R0
L208:
	lda	<R0
	sta	<L205+value_1
	brl	L10160
L10162:
	inc	|__cip
	jsr	__iplus
	sta	<L205+tmp_1
	stz	<R0
	lda	<L205+value_1
	cmp	<L205+tmp_1
	bne	L211
	brl	L210
L211:
	inc	<R0
L210:
	lda	<R0
	sta	<L205+value_1
	brl	L10160
L10163:
	inc	|__cip
	jsr	__iplus
	sta	<L205+tmp_1
	stz	<R0
	sec
	lda	<L205+value_1
	sbc	<L205+tmp_1
	bvs	L213
	eor	#$8000
L213:
	bpl	L214
	brl	L212
L214:
	inc	<R0
L212:
	lda	<R0
	sta	<L205+value_1
	brl	L10160
L10164:
	inc	|__cip
	jsr	__iplus
	sta	<L205+tmp_1
	stz	<R0
	sec
	lda	<L205+tmp_1
	sbc	<L205+value_1
	bvs	L216
	eor	#$8000
L216:
	bmi	L217
	brl	L215
L217:
	inc	<R0
L215:
	lda	<R0
	sta	<L205+value_1
	brl	L10160
L10165:
	inc	|__cip
	jsr	__iplus
	sta	<L205+tmp_1
	stz	<R0
	sec
	lda	<L205+tmp_1
	sbc	<L205+value_1
	bvs	L219
	eor	#$8000
L219:
	bpl	L220
	brl	L218
L220:
	inc	<R0
L218:
	lda	<R0
	sta	<L205+value_1
	brl	L10160
L10166:
	inc	|__cip
	jsr	__iplus
	sta	<L205+tmp_1
	stz	<R0
	sec
	lda	<L205+value_1
	sbc	<L205+tmp_1
	bvs	L222
	eor	#$8000
L222:
	bmi	L223
	brl	L221
L223:
	inc	<R0
L221:
	lda	<R0
	sta	<L205+value_1
	brl	L10160
L10167:
	lda	<L205+value_1
	brl	L207
L10159:
	xref	__~fsw
	jsr	__~fsw
	dw	21
	dw	6
	dw	L10167-1
	dw	L10166-1
	dw	L10162-1
	dw	L10165-1
	dw	L10161-1
	dw	L10164-1
	dw	L10163-1
L10160:
	brl	L10157
L204	equ	8
L205	equ	5
	ends
	efunc
	code
	xdef	__iprint
	func
__iprint:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L224
	tcs
	phd
	tcd
value_1	set	0
len_1	set	2
i_1	set	4
	stz	<L225+len_1
L10168:
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$e
	rep	#$20
	longa	on
	bne	L226
	brl	L10169
L226:
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$26
	rep	#$20
	longa	on
	bne	L227
	brl	L10169
L227:
	lda	|__cip
	sta	<R0
	lda	(<R0)
	and	#$ff
	brl	L10170
L10172:
	inc	|__cip
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	sta	<L225+i_1
	rep	#$20
	longa	on
	inc	|__cip
L10173:
	sep	#$20
	longa	off
	lda	<L225+i_1
	sta	<R0
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	dec	<L225+i_1
	rep	#$20
	longa	on
	lda	<R0
	and	#$ff
	bne	L228
	brl	L10174
L228:
	lda	|__cip
	sta	<R0
	inc	|__cip
	lda	(<R0)
	pha
	jsr	__c_putch
	brl	L10173
L10174:
	brl	L10171
L10175:
	inc	|__cip
	jsr	__iexp
	sta	<L225+len_1
	lda	|__err
	and	#$ff
	bne	L229
	brl	L10176
L229:
L230:
	pld
	tsc
	clc
	adc	#L224
	tcs
	rts
L10176:
	brl	L10171
L10177:
	jsr	__iexp
	sta	<L225+value_1
	lda	|__err
	and	#$ff
	bne	L231
	brl	L10178
L231:
	brl	L230
L10178:
	pei	<L225+len_1
	pei	<L225+value_1
	jsr	__putnum
	brl	L10171
L10170:
	xref	__~swt
	jsr	__~swt
	dw	2
	dw	22
	dw	L10175-1
	dw	37
	dw	L10172-1
	dw	L10177-1
L10171:
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$d
	rep	#$20
	longa	on
	beq	L232
	brl	L10179
L232:
	inc	|__cip
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$e
	rep	#$20
	longa	on
	bne	L234
	brl	L233
L234:
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$26
	rep	#$20
	longa	on
	beq	L235
	brl	L10180
L235:
L233:
	brl	L230
L10180:
	brl	L10181
L10179:
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$e
	rep	#$20
	longa	on
	bne	L236
	brl	L10182
L236:
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$26
	rep	#$20
	longa	on
	bne	L237
	brl	L10182
L237:
	sep	#$20
	longa	off
	lda	#$14
	sta	|__err
	rep	#$20
	longa	on
	brl	L230
L10182:
L10181:
	brl	L10168
L10169:
	jsr	__newline
	brl	L230
L224	equ	9
L225	equ	5
	ends
	efunc
	code
	xdef	__iinput
	func
__iinput:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L238
	tcs
	phd
	tcd
value_1	set	0
index_1	set	2
i_1	set	4
prompt_1	set	5
L10183:
	sep	#$20
	longa	off
	lda	#$1
	sta	<L239+prompt_1
	rep	#$20
	longa	on
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$25
	rep	#$20
	longa	on
	beq	L240
	brl	L10185
L240:
	inc	|__cip
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	sta	<L239+i_1
	rep	#$20
	longa	on
	inc	|__cip
L10186:
	sep	#$20
	longa	off
	lda	<L239+i_1
	sta	<R0
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	dec	<L239+i_1
	rep	#$20
	longa	on
	lda	<R0
	and	#$ff
	bne	L241
	brl	L10187
L241:
	lda	|__cip
	sta	<R0
	inc	|__cip
	lda	(<R0)
	pha
	jsr	__c_putch
	brl	L10186
L10187:
	sep	#$20
	longa	off
	stz	<L239+prompt_1
	rep	#$20
	longa	on
L10185:
	lda	|__cip
	sta	<R0
	lda	(<R0)
	and	#$ff
	brl	L10188
L10190:
	inc	|__cip
	lda	<L239+prompt_1
	and	#$ff
	bne	L242
	brl	L10191
L242:
	lda	|__cip
	sta	<R0
	lda	(<R0)
	and	#$ff
	sta	<R0
	clc
	lda	#$41
	adc	<R0
	pha
	jsr	__c_putch
	pea	#<$3a
	jsr	__c_putch
L10191:
	jsr	__getnum
	sta	<L239+value_1
	lda	|__err
	and	#$ff
	bne	L243
	brl	L10192
L243:
L244:
	pld
	tsc
	clc
	adc	#L238
	tcs
	rts
L10192:
	lda	|__cip
	sta	<R1
	lda	(<R1)
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__var
	adc	<R0
	sta	<R1
	lda	<L239+value_1
	sta	(<R1)
	inc	|__cip
	brl	L10189
L10193:
	inc	|__cip
	jsr	__getparam
	sta	<L239+index_1
	lda	|__err
	and	#$ff
	bne	L245
	brl	L10194
L245:
	brl	L244
L10194:
	sec
	lda	<L239+index_1
	sbc	#<$40
	bvs	L246
	eor	#$8000
L246:
	bmi	L247
	brl	L10195
L247:
	sep	#$20
	longa	off
	lda	#$3
	sta	|__err
	rep	#$20
	longa	on
	brl	L244
L10195:
	lda	<L239+prompt_1
	and	#$ff
	bne	L248
	brl	L10196
L248:
	pea	#<L17
	jsr	__c_puts
	pea	#<$0
	pei	<L239+index_1
	jsr	__putnum
	pea	#<L17+3
	jsr	__c_puts
L10196:
	jsr	__getnum
	sta	<L239+value_1
	lda	|__err
	and	#$ff
	bne	L249
	brl	L10197
L249:
	brl	L244
L10197:
	lda	<L239+index_1
	asl	A
	sta	<R0
	clc
	lda	#<__arr
	adc	<R0
	sta	<R1
	lda	<L239+value_1
	sta	(<R1)
	brl	L10189
L10198:
	sep	#$20
	longa	off
	lda	#$14
	sta	|__err
	rep	#$20
	longa	on
	brl	L244
L10188:
	xref	__~swt
	jsr	__~swt
	dw	2
	dw	27
	dw	L10193-1
	dw	36
	dw	L10190-1
	dw	L10198-1
L10189:
	lda	|__cip
	sta	<R0
	lda	(<R0)
	and	#$ff
	brl	L10199
L10201:
	inc	|__cip
	brl	L10200
L10202:
L10203:
	brl	L244
L10204:
	sep	#$20
	longa	off
	lda	#$14
	sta	|__err
	rep	#$20
	longa	on
	brl	L244
L10199:
	xref	__~swt
	jsr	__~swt
	dw	3
	dw	13
	dw	L10201-1
	dw	14
	dw	L10202-1
	dw	38
	dw	L10203-1
	dw	L10204-1
L10200:
	brl	L10183
L238	equ	14
L239	equ	9
	ends
	efunc
	data
L17:
	db	$40,$28,$00,$29,$3A,$00
	ends
	code
	xdef	__ivar
	func
__ivar:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L251
	tcs
	phd
	tcd
value_1	set	0
index_1	set	2
	lda	|__cip
	sta	<R0
	lda	(<R0)
	and	#$ff
	sta	<L252+index_1
	inc	|__cip
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$18
	rep	#$20
	longa	on
	bne	L253
	brl	L10205
L253:
	sep	#$20
	longa	off
	lda	#$12
	sta	|__err
	rep	#$20
	longa	on
L254:
	pld
	tsc
	clc
	adc	#L251
	tcs
	rts
L10205:
	inc	|__cip
	jsr	__iexp
	sta	<L252+value_1
	lda	|__err
	and	#$ff
	bne	L255
	brl	L10206
L255:
	brl	L254
L10206:
	lda	<L252+index_1
	asl	A
	sta	<R0
	clc
	lda	#<__var
	adc	<R0
	sta	<R1
	lda	<L252+value_1
	sta	(<R1)
	brl	L254
L251	equ	12
L252	equ	9
	ends
	efunc
	code
	xdef	__iarray
	func
__iarray:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L256
	tcs
	phd
	tcd
value_1	set	0
index_1	set	2
	jsr	__getparam
	sta	<L257+index_1
	lda	|__err
	and	#$ff
	bne	L258
	brl	L10207
L258:
L259:
	pld
	tsc
	clc
	adc	#L256
	tcs
	rts
L10207:
	sec
	lda	<L257+index_1
	sbc	#<$40
	bvs	L260
	eor	#$8000
L260:
	bmi	L261
	brl	L10208
L261:
	sep	#$20
	longa	off
	lda	#$3
	sta	|__err
	rep	#$20
	longa	on
	brl	L259
L10208:
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$18
	rep	#$20
	longa	on
	bne	L262
	brl	L10209
L262:
	sep	#$20
	longa	off
	lda	#$12
	sta	|__err
	rep	#$20
	longa	on
	brl	L259
L10209:
	inc	|__cip
	jsr	__iexp
	sta	<L257+value_1
	lda	|__err
	and	#$ff
	bne	L263
	brl	L10210
L263:
	brl	L259
L10210:
	lda	<L257+index_1
	asl	A
	sta	<R0
	clc
	lda	#<__arr
	adc	<R0
	sta	<R1
	lda	<L257+value_1
	sta	(<R1)
	brl	L259
L256	equ	12
L257	equ	9
	ends
	efunc
	code
	xdef	__ilet
	func
__ilet:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L264
	tcs
	phd
	tcd
	lda	|__cip
	sta	<R0
	lda	(<R0)
	and	#$ff
	brl	L10211
L10213:
	inc	|__cip
	jsr	__ivar
	brl	L10212
L10214:
	inc	|__cip
	jsr	__iarray
	brl	L10212
L10215:
	sep	#$20
	longa	off
	lda	#$e
	sta	|__err
	rep	#$20
	longa	on
	brl	L10212
L10211:
	xref	__~swt
	jsr	__~swt
	dw	2
	dw	27
	dw	L10214-1
	dw	36
	dw	L10213-1
	dw	L10215-1
L10212:
L266:
	pld
	tsc
	clc
	adc	#L264
	tcs
	rts
L264	equ	4
L265	equ	5
	ends
	efunc
	code
	xdef	__iexe
	func
__iexe:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L267
	tcs
	phd
	tcd
lineno_1	set	0
lp_1	set	2
index_1	set	4
vto_1	set	6
vstep_1	set	8
condition_1	set	10
L10216:
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$26
	rep	#$20
	longa	on
	bne	L269
	brl	L10217
L269:
	jsr	__c_kbhit
	and	#$ff
	bne	L270
	brl	L10218
L270:
	jsr	__c_getch
	sep	#$20
	longa	off
	sta	<R0
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	lda	<R0
	cmp	#<$1b
	rep	#$20
	longa	on
	beq	L271
	brl	L10219
L271:
	sep	#$20
	longa	off
	lda	#$16
	sta	|__err
	rep	#$20
	longa	on
	lda	#$0
L272:
	tay
	pld
	tsc
	clc
	adc	#L267
	tcs
	tya
	rts
L10219:
L10218:
	lda	|__cip
	sta	<R0
	lda	(<R0)
	and	#$ff
	brl	L10220
L10222:
	inc	|__cip
	jsr	__iexp
	sta	<L268+lineno_1
	lda	|__err
	and	#$ff
	beq	L273
	brl	L10221
L273:
	pei	<L268+lineno_1
	jsr	__getlp
	sta	<L268+lp_1
	pei	<L268+lp_1
	jsr	__getlineno
	sta	<R0
	lda	<R0
	cmp	<L268+lineno_1
	bne	L274
	brl	L10223
L274:
	sep	#$20
	longa	off
	lda	#$10
	sta	|__err
	rep	#$20
	longa	on
	brl	L10221
L10223:
	lda	<L268+lp_1
	sta	|__clp
	clc
	lda	#$3
	adc	|__clp
	sta	|__cip
	brl	L10221
L10224:
	inc	|__cip
	jsr	__iexp
	sta	<L268+lineno_1
	lda	|__err
	and	#$ff
	beq	L275
	brl	L10221
L275:
	pei	<L268+lineno_1
	jsr	__getlp
	sta	<L268+lp_1
	pei	<L268+lp_1
	jsr	__getlineno
	sta	<R0
	lda	<R0
	cmp	<L268+lineno_1
	bne	L276
	brl	L10225
L276:
	sep	#$20
	longa	off
	lda	#$10
	sta	|__err
	rep	#$20
	longa	on
	brl	L10221
L10225:
	sep	#$20
	longa	off
	lda	|__gstki
	cmp	#<$6
	rep	#$20
	longa	on
	bcs	L277
	brl	L10226
L277:
	sep	#$20
	longa	off
	lda	#$6
	sta	|__err
	rep	#$20
	longa	on
	brl	L10221
L10226:
	lda	|__gstki
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__gstk
	adc	<R0
	sta	<R1
	lda	|__clp
	sta	(<R1)
	sep	#$20
	longa	off
	inc	|__gstki
	rep	#$20
	longa	on
	lda	|__gstki
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__gstk
	adc	<R0
	sta	<R1
	lda	|__cip
	sta	(<R1)
	sep	#$20
	longa	off
	inc	|__gstki
	rep	#$20
	longa	on
	lda	|__gstki
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__gstk
	adc	<R0
	sta	<R1
	lda	|__lstki
	and	#$ff
	sta	(<R1)
	sep	#$20
	longa	off
	inc	|__gstki
	rep	#$20
	longa	on
	lda	<L268+lp_1
	sta	|__clp
	clc
	lda	#$3
	adc	|__clp
	sta	|__cip
	brl	L10221
L10227:
	sep	#$20
	longa	off
	lda	|__gstki
	cmp	#<$3
	rep	#$20
	longa	on
	bcc	L278
	brl	L10228
L278:
	sep	#$20
	longa	off
	lda	#$7
	sta	|__err
	rep	#$20
	longa	on
	brl	L10221
L10228:
	sep	#$20
	longa	off
	dec	|__gstki
	rep	#$20
	longa	on
	lda	|__gstki
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__gstk
	adc	<R0
	sta	<R1
	sep	#$20
	longa	off
	lda	(<R1)
	sta	|__lstki
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	dec	|__gstki
	rep	#$20
	longa	on
	lda	|__gstki
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__gstk
	adc	<R0
	sta	<R1
	lda	(<R1)
	sta	|__cip
	sep	#$20
	longa	off
	dec	|__gstki
	rep	#$20
	longa	on
	lda	|__gstki
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__gstk
	adc	<R0
	sta	<R1
	lda	(<R1)
	sta	|__clp
	brl	L10221
L10229:
	inc	|__cip
	lda	|__cip
	sta	<R0
	inc	|__cip
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$24
	rep	#$20
	longa	on
	bne	L279
	brl	L10230
L279:
	sep	#$20
	longa	off
	lda	#$c
	sta	|__err
	rep	#$20
	longa	on
	brl	L10221
L10230:
	lda	|__cip
	sta	<R0
	lda	(<R0)
	and	#$ff
	sta	<L268+index_1
	jsr	__ivar
	lda	|__err
	and	#$ff
	beq	L280
	brl	L10221
L280:
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$4
	rep	#$20
	longa	on
	beq	L281
	brl	L10231
L281:
	inc	|__cip
	jsr	__iexp
	sta	<L268+vto_1
	brl	L10232
L10231:
	sep	#$20
	longa	off
	lda	#$d
	sta	|__err
	rep	#$20
	longa	on
	brl	L10221
L10232:
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$5
	rep	#$20
	longa	on
	beq	L282
	brl	L10233
L282:
	inc	|__cip
	jsr	__iexp
	sta	<L268+vstep_1
	brl	L10234
L10233:
	lda	#$1
	sta	<L268+vstep_1
L10234:
	lda	<L268+vstep_1
	bmi	L285
	brl	L284
L285:
	sec
	lda	#$8001
	sbc	<L268+vstep_1
	sta	<R0
	sec
	lda	<L268+vto_1
	sbc	<R0
	bvs	L286
	eor	#$8000
L286:
	bmi	L287
	brl	L283
L287:
L284:
	sec
	lda	#$0
	sbc	<L268+vstep_1
	bvs	L288
	eor	#$8000
L288:
	bpl	L289
	brl	L10235
L289:
	sec
	lda	#$7fff
	sbc	<L268+vstep_1
	sta	<R0
	sec
	lda	<R0
	sbc	<L268+vto_1
	bvs	L290
	eor	#$8000
L290:
	bpl	L291
	brl	L10235
L291:
L283:
	sep	#$20
	longa	off
	lda	#$2
	sta	|__err
	rep	#$20
	longa	on
	brl	L10221
L10235:
	sep	#$20
	longa	off
	lda	|__lstki
	cmp	#<$a
	rep	#$20
	longa	on
	bcs	L292
	brl	L10236
L292:
	sep	#$20
	longa	off
	lda	#$8
	sta	|__err
	rep	#$20
	longa	on
	brl	L10221
L10236:
	lda	|__lstki
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__lstk
	adc	<R0
	sta	<R1
	lda	|__clp
	sta	(<R1)
	sep	#$20
	longa	off
	inc	|__lstki
	rep	#$20
	longa	on
	lda	|__lstki
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__lstk
	adc	<R0
	sta	<R1
	lda	|__cip
	sta	(<R1)
	sep	#$20
	longa	off
	inc	|__lstki
	rep	#$20
	longa	on
	lda	|__lstki
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__lstk
	adc	<R0
	sta	<R1
	lda	<L268+vto_1
	sta	(<R1)
	sep	#$20
	longa	off
	inc	|__lstki
	rep	#$20
	longa	on
	lda	|__lstki
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__lstk
	adc	<R0
	sta	<R1
	lda	<L268+vstep_1
	sta	(<R1)
	sep	#$20
	longa	off
	inc	|__lstki
	rep	#$20
	longa	on
	lda	|__lstki
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__lstk
	adc	<R0
	sta	<R1
	lda	<L268+index_1
	sta	(<R1)
	sep	#$20
	longa	off
	inc	|__lstki
	rep	#$20
	longa	on
	brl	L10221
L10237:
	inc	|__cip
	sep	#$20
	longa	off
	lda	|__lstki
	cmp	#<$5
	rep	#$20
	longa	on
	bcc	L293
	brl	L10238
L293:
	sep	#$20
	longa	off
	lda	#$9
	sta	|__err
	rep	#$20
	longa	on
	brl	L10221
L10238:
	lda	|__lstki
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#$fffe
	adc	#<__lstk
	sta	<R1
	clc
	lda	<R1
	adc	<R0
	sta	<R2
	lda	(<R2)
	sta	<L268+index_1
	lda	|__cip
	sta	<R0
	inc	|__cip
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$24
	rep	#$20
	longa	on
	bne	L294
	brl	L10239
L294:
	sep	#$20
	longa	off
	lda	#$a
	sta	|__err
	rep	#$20
	longa	on
	brl	L10221
L10239:
	lda	|__cip
	sta	<R0
	inc	|__cip
	lda	(<R0)
	and	#$ff
	sta	<R0
	lda	<R0
	cmp	<L268+index_1
	bne	L295
	brl	L10240
L295:
	sep	#$20
	longa	off
	lda	#$b
	sta	|__err
	rep	#$20
	longa	on
	brl	L10221
L10240:
	lda	|__lstki
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#$fffc
	adc	#<__lstk
	sta	<R1
	clc
	lda	<R1
	adc	<R0
	sta	<R2
	lda	(<R2)
	sta	<L268+vstep_1
	lda	<L268+index_1
	asl	A
	sta	<R0
	clc
	lda	#<__var
	adc	<R0
	sta	<R1
	clc
	lda	(<R1)
	adc	<L268+vstep_1
	sta	(<R1)
	lda	|__lstki
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#$fffa
	adc	#<__lstk
	sta	<R1
	clc
	lda	<R1
	adc	<R0
	sta	<R2
	lda	(<R2)
	sta	<L268+vto_1
	lda	<L268+vstep_1
	bmi	L298
	brl	L297
L298:
	lda	<L268+index_1
	asl	A
	sta	<R0
	clc
	lda	#<__var
	adc	<R0
	sta	<R1
	sec
	lda	(<R1)
	sbc	<L268+vto_1
	bvs	L299
	eor	#$8000
L299:
	bmi	L300
	brl	L296
L300:
L297:
	sec
	lda	#$0
	sbc	<L268+vstep_1
	bvs	L301
	eor	#$8000
L301:
	bpl	L302
	brl	L10241
L302:
	lda	<L268+index_1
	asl	A
	sta	<R0
	clc
	lda	#<__var
	adc	<R0
	sta	<R1
	sec
	lda	<L268+vto_1
	sbc	(<R1)
	bvs	L303
	eor	#$8000
L303:
	bpl	L304
	brl	L10241
L304:
L296:
	lda	|__lstki
	and	#$ff
	sta	<R0
	clc
	lda	#$fffb
	adc	<R0
	sta	<R1
	sep	#$20
	longa	off
	lda	<R1
	sta	|__lstki
	rep	#$20
	longa	on
	brl	L10221
L10241:
	lda	|__lstki
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#$fff8
	adc	#<__lstk
	sta	<R1
	clc
	lda	<R1
	adc	<R0
	sta	<R2
	lda	(<R2)
	sta	|__cip
	lda	|__lstki
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#$fff6
	adc	#<__lstk
	sta	<R1
	clc
	lda	<R1
	adc	<R0
	sta	<R2
	lda	(<R2)
	sta	|__clp
	brl	L10221
L10242:
	inc	|__cip
	jsr	__iexp
	sta	<L268+condition_1
	lda	|__err
	and	#$ff
	bne	L305
	brl	L10243
L305:
	sep	#$20
	longa	off
	lda	#$f
	sta	|__err
	rep	#$20
	longa	on
	brl	L10221
L10243:
	lda	<L268+condition_1
	beq	L306
	brl	L10221
L306:
L10244:
L10245:
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$26
	rep	#$20
	longa	on
	bne	L307
	brl	L10246
L307:
	inc	|__cip
	brl	L10245
L10246:
	brl	L10221
L10247:
L10248:
	lda	|__clp
	sta	<R0
	lda	(<R0)
	and	#$ff
	bne	L308
	brl	L10249
L308:
	lda	|__clp
	sta	<R0
	lda	(<R0)
	and	#$ff
	sta	<R0
	clc
	lda	|__clp
	adc	<R0
	sta	|__clp
	brl	L10248
L10249:
	lda	|__clp
	brl	L272
L10250:
	inc	|__cip
	jsr	__ivar
	brl	L10221
L10251:
	inc	|__cip
	jsr	__iarray
	brl	L10221
L10252:
	inc	|__cip
	jsr	__ilet
	brl	L10221
L10253:
	inc	|__cip
	jsr	__iprint
	brl	L10221
L10254:
	inc	|__cip
	jsr	__iinput
	brl	L10221
L10255:
	inc	|__cip
	brl	L10221
L10256:
L10257:
L10258:
	sep	#$20
	longa	off
	lda	#$13
	sta	|__err
	rep	#$20
	longa	on
	brl	L10221
L10259:
	sep	#$20
	longa	off
	lda	#$14
	sta	|__err
	rep	#$20
	longa	on
	brl	L10221
L10220:
	xref	__~fsw
	jsr	__~fsw
	dw	0
	dw	37
	dw	L10259-1
	dw	L10222-1
	dw	L10224-1
	dw	L10227-1
	dw	L10229-1
	dw	L10259-1
	dw	L10259-1
	dw	L10237-1
	dw	L10242-1
	dw	L10244-1
	dw	L10247-1
	dw	L10254-1
	dw	L10253-1
	dw	L10252-1
	dw	L10259-1
	dw	L10255-1
	dw	L10259-1
	dw	L10259-1
	dw	L10259-1
	dw	L10259-1
	dw	L10259-1
	dw	L10259-1
	dw	L10259-1
	dw	L10259-1
	dw	L10259-1
	dw	L10259-1
	dw	L10259-1
	dw	L10259-1
	dw	L10251-1
	dw	L10259-1
	dw	L10259-1
	dw	L10259-1
	dw	L10256-1
	dw	L10258-1
	dw	L10257-1
	dw	L10259-1
	dw	L10259-1
	dw	L10250-1
L10221:
	lda	|__err
	and	#$ff
	bne	L309
	brl	L10260
L309:
	lda	#$0
	brl	L272
L10260:
	brl	L10216
L10217:
	lda	|__clp
	sta	<R0
	lda	(<R0)
	and	#$ff
	sta	<R0
	clc
	lda	|__clp
	adc	<R0
	sta	<R1
	lda	<R1
	brl	L272
L267	equ	24
L268	equ	13
	ends
	efunc
	code
	xdef	__irun
	func
__irun:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L310
	tcs
	phd
	tcd
lp_1	set	0
	sep	#$20
	longa	off
	stz	|__gstki
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	stz	|__lstki
	rep	#$20
	longa	on
	lda	#<__listbuf
	sta	|__clp
L10261:
	lda	|__clp
	sta	<R0
	lda	(<R0)
	and	#$ff
	bne	L312
	brl	L10262
L312:
	clc
	lda	#$3
	adc	|__clp
	sta	|__cip
	jsr	__iexe
	sta	<L311+lp_1
	lda	|__err
	and	#$ff
	bne	L313
	brl	L10263
L313:
L314:
	pld
	tsc
	clc
	adc	#L310
	tcs
	rts
L10263:
	lda	<L311+lp_1
	sta	|__clp
	brl	L10261
L10262:
	brl	L314
L310	equ	6
L311	equ	5
	ends
	efunc
	code
	xdef	__ilist
	func
__ilist:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L315
	tcs
	phd
	tcd
lineno_1	set	0
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$23
	rep	#$20
	longa	on
	beq	L318
	brl	L317
L318:
	lda	|__cip
	pha
	jsr	__getlineno
	bra	L319
L317:
	lda	#$0
L319:
	sta	<L316+lineno_1
	lda	#<__listbuf
	sta	|__clp
	brl	L10265
L10264:
	lda	|__clp
	sta	<R0
	lda	(<R0)
	and	#$ff
	sta	<R0
	clc
	lda	|__clp
	adc	<R0
	sta	|__clp
L10265:
	lda	|__clp
	sta	<R0
	lda	(<R0)
	and	#$ff
	bne	L320
	brl	L10266
L320:
	lda	|__clp
	pha
	jsr	__getlineno
	sta	<R0
	sec
	lda	<R0
	sbc	<L316+lineno_1
	bvs	L321
	eor	#$8000
L321:
	bpl	L322
	brl	L10266
L322:
	brl	L10264
L10266:
L10267:
	lda	|__clp
	sta	<R0
	lda	(<R0)
	and	#$ff
	bne	L323
	brl	L10268
L323:
	pea	#<$0
	lda	|__clp
	pha
	jsr	__getlineno
	pha
	jsr	__putnum
	pea	#<$20
	jsr	__c_putch
	clc
	lda	#$3
	adc	|__clp
	sta	<R0
	pei	<R0
	jsr	__putlist
	lda	|__err
	and	#$ff
	beq	L324
	brl	L10268
L324:
	jsr	__newline
	lda	|__clp
	sta	<R0
	lda	(<R0)
	and	#$ff
	sta	<R0
	clc
	lda	|__clp
	adc	<R0
	sta	|__clp
	brl	L10267
L10268:
L325:
	pld
	tsc
	clc
	adc	#L315
	tcs
	rts
L315	equ	6
L316	equ	5
	ends
	efunc
	code
	xdef	__inew
	func
__inew:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L326
	tcs
	phd
	tcd
i_1	set	0
	sep	#$20
	longa	off
	stz	<L327+i_1
	rep	#$20
	longa	on
	brl	L10270
L10269:
	sep	#$20
	longa	off
	inc	<L327+i_1
	rep	#$20
	longa	on
L10270:
	sep	#$20
	longa	off
	lda	<L327+i_1
	cmp	#<$1a
	rep	#$20
	longa	on
	bcc	L328
	brl	L10271
L328:
	lda	<L327+i_1
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__var
	adc	<R0
	sta	<R1
	lda	#$0
	sta	(<R1)
	brl	L10269
L10271:
	sep	#$20
	longa	off
	stz	<L327+i_1
	rep	#$20
	longa	on
	brl	L10273
L10272:
	sep	#$20
	longa	off
	inc	<L327+i_1
	rep	#$20
	longa	on
L10273:
	sep	#$20
	longa	off
	lda	<L327+i_1
	cmp	#<$40
	rep	#$20
	longa	on
	bcc	L329
	brl	L10274
L329:
	lda	<L327+i_1
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__arr
	adc	<R0
	sta	<R1
	lda	#$0
	sta	(<R1)
	brl	L10272
L10274:
	sep	#$20
	longa	off
	stz	|__gstki
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	stz	|__lstki
	rep	#$20
	longa	on
	sep	#$20
	longa	off
	stz	|__listbuf
	rep	#$20
	longa	on
	lda	#<__listbuf
	sta	|__clp
L330:
	pld
	tsc
	clc
	adc	#L326
	tcs
	rts
L326	equ	9
L327	equ	9
	ends
	efunc
	code
	xdef	__icom
	func
__icom:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L331
	tcs
	phd
	tcd
	lda	#<__ibuf
	sta	|__cip
	lda	|__cip
	sta	<R0
	lda	(<R0)
	and	#$ff
	brl	L10275
L10277:
	inc	|__cip
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$26
	rep	#$20
	longa	on
	beq	L333
	brl	L10278
L333:
	jsr	__inew
	brl	L10279
L10278:
	sep	#$20
	longa	off
	lda	#$14
	sta	|__err
	rep	#$20
	longa	on
L10279:
	brl	L10276
L10280:
	inc	|__cip
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	lda	(<R0)
	cmp	#<$26
	rep	#$20
	longa	on
	bne	L335
	brl	L334
L335:
	lda	|__cip
	sta	<R0
	sep	#$20
	longa	off
	ldy	#$3
	lda	(<R0),Y
	cmp	#<$26
	rep	#$20
	longa	on
	beq	L336
	brl	L10281
L336:
L334:
	jsr	__ilist
	brl	L10282
L10281:
	sep	#$20
	longa	off
	lda	#$14
	sta	|__err
	rep	#$20
	longa	on
L10282:
	brl	L10276
L10283:
	inc	|__cip
	jsr	__irun
	brl	L10276
L10284:
	jsr	__iexe
	brl	L10276
L10275:
	xref	__~swt
	jsr	__~swt
	dw	3
	dw	31
	dw	L10280-1
	dw	32
	dw	L10283-1
	dw	33
	dw	L10277-1
	dw	L10284-1
L10276:
L337:
	pld
	tsc
	clc
	adc	#L331
	tcs
	rts
L331	equ	4
L332	equ	5
	ends
	efunc
	code
	xdef	__error
	func
__error:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L338
	tcs
	phd
	tcd
	lda	|__err
	and	#$ff
	bne	L340
	brl	L10285
L340:
	lda	|__cip
	cmp	#<__listbuf
	bcs	L341
	brl	L10286
L341:
	lda	|__cip
	cmp	#<__listbuf+30592
	bcc	L342
	brl	L10286
L342:
	lda	|__clp
	sta	<R0
	lda	(<R0)
	and	#$ff
	bne	L343
	brl	L10286
L343:
	jsr	__newline
	pea	#<L250
	jsr	__c_puts
	pea	#<$0
	lda	|__clp
	pha
	jsr	__getlineno
	pha
	jsr	__putnum
	pea	#<$20
	jsr	__c_putch
	clc
	lda	#$3
	adc	|__clp
	sta	<R0
	pei	<R0
	jsr	__putlist
	brl	L10287
L10286:
	jsr	__newline
	pea	#<L250+6
	jsr	__c_puts
	pea	#<__lbuf
	jsr	__c_puts
L10287:
L10285:
	jsr	__newline
	lda	|__err
	and	#$ff
	sta	<R1
	lda	<R1
	asl	A
	sta	<R0
	clc
	lda	#<__errmsg
	adc	<R0
	sta	<R1
	lda	(<R1)
	pha
	jsr	__c_puts
	jsr	__newline
	sep	#$20
	longa	off
	stz	|__err
	rep	#$20
	longa	on
L344:
	pld
	tsc
	clc
	adc	#L338
	tcs
	rts
L338	equ	8
L339	equ	9
	ends
	efunc
	data
L250:
	db	$4C,$49,$4E,$45,$3A,$00,$59,$4F,$55,$20,$54,$59,$50,$45,$3A
	db	$20,$00
	ends
	code
	xdef	__basic
	func
__basic:
	longa	on
	longi	on
	tsc
	sec
	sbc	#L346
	tcs
	phd
	tcd
len_1	set	0
	jsr	__inew
	pea	#<L345
	jsr	__c_puts
	jsr	__newline
	pea	#<L345+21
	jsr	__c_puts
	pea	#<L345+33
	jsr	__c_puts
	jsr	__newline
	jsr	__error
L10288:
	pea	#<$3e
	jsr	__c_putch
	jsr	__c_gets
	jsr	__toktoi
	sep	#$20
	longa	off
	sta	<L347+len_1
	rep	#$20
	longa	on
	lda	|__err
	and	#$ff
	bne	L348
	brl	L10290
L348:
	jsr	__error
	brl	L10288
L10290:
	sep	#$20
	longa	off
	lda	|__ibuf
	cmp	#<$22
	rep	#$20
	longa	on
	beq	L349
	brl	L10291
L349:
L350:
	pld
	tsc
	clc
	adc	#L346
	tcs
	rts
L10291:
	sep	#$20
	longa	off
	lda	|__ibuf
	cmp	#<$23
	rep	#$20
	longa	on
	beq	L351
	brl	L10292
L351:
	sep	#$20
	longa	off
	lda	<L347+len_1
	sta	|__ibuf
	rep	#$20
	longa	on
	jsr	__inslist
	lda	|__err
	and	#$ff
	bne	L352
	brl	L10293
L352:
	jsr	__error
L10293:
	brl	L10288
L10292:
	jsr	__icom
	jsr	__error
	brl	L10288
L346	equ	1
L347	equ	1
	ends
	efunc
	data
L345:
	db	$54,$4F,$59,$4F,$53,$48,$49,$4B,$49,$20,$54,$49,$4E,$59,$20
	db	$42,$41,$53,$49,$43,$00,$4D,$45,$5A,$57,$36,$35,$43,$5F,$52
	db	$41,$4D,$00,$20,$45,$44,$49,$54,$49,$4F,$4E,$00
	ends
	xref	__rand
	xref	__srand
	xref	__c_putch
	xref	__c_getch
	xref	__c_kbhit
	udata
	xdef	__lstki
__lstki
	ds	1
	ends
	udata
	xdef	__lstk
__lstk
	ds	30
	ends
	udata
	xdef	__gstki
__gstki
	ds	1
	ends
	udata
	xdef	__gstk
__gstk
	ds	18
	ends
	udata
	xdef	__cip
__cip
	ds	2
	ends
	udata
	xdef	__clp
__clp
	ds	2
	ends
	udata
	xdef	__listbuf
__listbuf
	ds	30592
	ends
	udata
	xdef	__arr
__arr
	ds	128
	ends
	udata
	xdef	__var
__var
	ds	52
	ends
	udata
	xdef	__ibuf
__ibuf
	ds	160
	ends
	udata
	xdef	__lbuf
__lbuf
	ds	160
	ends
	udata
	xdef	__err
__err
	ds	1
	ends
	end