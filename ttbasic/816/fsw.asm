	lda	cip
	sta	<R0
	lda	(<R0)
	and	#$ff
	brl	L10124

;enum{
;	 0:I_GOTO	1:I_GOSUB	2:I_RETURN	3:I_FOR		4:I_TO
;	 5:I_STEP	6:I_NEXT	7:I_IF		8:I_REM		9:I_STOP
;	10:I_INPUT	11:I_PRINT	12:I_LET	13:I_COMMA	14:I_SEMI
;	15:I_MINUS	16:I_PLUS	17:I_MUL	18:I_DIV	19:I_OPEN
;	20:I_CLOSE	21:I_GTE	22:I_SHARP	23:I_GT		24:I_EQ
;	25:I_LTE	26:I_LT		27:I_ARRAY	28:I_RND	29:I_ABS
;	30:I_SIZE	31:I_LIST	32:I_RUN	33:I_NEW	34:I_SYSTEM
;	35:I_NUM	36:I_VAR	37:I_STR	38:I_EOL
;};

; （ジャンプテーブルの構成例）
	if 0
L10124:
	jsr	fsw
	dw	15					; 比較値の最低値
	dw	22					; ジャンプテーブルオフセット（＊２）の最大値
	dw	L10138-1	; default		; 0/ 0
(t_base)
	dw	L10128-1	; I_MINUS (15)		; 2/ 1
	dw	L10127-1	; I_PLUS (16)		; 4/ 2
	dw	L10138-1	; default		; 6/ 3
	dw	L10138-1	; default		; 8/ 4
	dw	L10130-1	; I_OPEN (19)		;10/ 5
	dw	L10138-1	; default		;12/ 6
	dw	L10138-1	; default		;14/ 7
	dw	L10138-1	; default		;16/ 8
	dw	L10138-1	; default		;18/ 9
	dw	L10138-1	; default		;20/10
	dw	L10138-1	; default		;22/11
	dw	L10138-1	; default		;24/12
	dw	L10131-1	; I_ARRAY (27)		;26/13
	dw	L10133-1	; I_RND (28)		;28/14
	dw	L10134-1	; I_ABS (29)		;30/15
	dw	L10136-1	; I_SIZE (30)		;32/16
	dw	L10138-1	; default		;34/17
	dw	L10138-1	; default		;36/18
	dw	L10138-1	; default		;38/19
	dw	L10138-1	; default		;40/20
	dw	L10126-1	; I_NUM (35)		;42/21
	dw	L10129-1	; I_VAR (36)		;44/22

	比較値（A)が15未満、または、（15+22）以上の場合は、０番目のテーブルにジャンプ（ポップ）
	それ以外の番号は、
	 tbase + ((A - 15)<<2)のテーブルにジャンプ（ポップ）
	endif
;
; input A : 比較値
fsw:
	longa on
	longi on
	
	plx
;SP->
;  |	PC(L)-1 : L10124(L) -1
;  v
;SP->	PC(H)   : L10124(H)

	tay
	inx		; get table address -> dw 15
	lda	0,x	; get Comparison minimum : 15
	sta	min_cmp	; (15)
	inx
	inx		; table address -> dw 22
	lda	0,x	; get Maximum jump table offset
	clc
	adc	min_cmp
	sta	max_off	; (15+22)
	inx
	inx		; get default jump address
	stx	deff_p
	inx
	inx		; get t_base address
	stx	t_base

	tya
	cmp	min_cmp
	bmi	def_jmp	; less
	cmp	max_off	; 
	bpl	def_jmp
	
	sec
	sbc	min_cmp
	asl
	adc	t_base	; get jump address

fsw_jmp:
	pha
	rts		; jump desired address

def_jmp:
	lda	deff_p	; get default jump address
	bra	fsw_jmp
	