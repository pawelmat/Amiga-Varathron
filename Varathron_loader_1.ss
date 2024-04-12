
;		*****************************************	
;		*	VARATHRON MAIN CONTROLER	*
;		*  ----------------------------------	*
;		*	   Coded 08-25.04.1993		*
;		*	   by  KANE  of SUSPECT		*
;		*****************************************


org	$28800
load	$28800

exe=1
mus=1

waitblt:macro
	btst.b	#14,$2(a0)
	bne.s	*-8
	endm

raster: macro
	cmp.b	#$ff,6(a0)
	bne.s	*-8
	cmp.b	#$ff,6(a0)
	beq.s	*-8
	endm

;---------------------------------------------------------------------

s:
if exe=1
		move.l	#ss,$80.w
		trap	#0
		rts
ss:
endif
		lea	$dff000,a0
		move	#$83c0,$96(a0)
		move.l	#p_blackcop,$80(a0)
		move	#0,$88(a0)

		lea	pentagram,a0
		lea	pent,a1
		move.l	#6276,d0
		bsr.L	decrunch
		lea	$dff000,a0

		move.l	#p_copper,$80(a0)
		bsr	setcol
		lea	$dff000,a0
		move	#$4000,$9a(a0)
		raster
		raster
		move	#$7fff,$9c(a0)
		move	#$20,$96(a0)

		lea	music,a0		;depack mus
		lea	mt_data,a1
		move.l	#105364,d0
		bsr.L	decrunch
		lea	$dff000,a0

if mus=1
		bsr	mt_init
		lea	$dff000,a0
		move.l	$6c,oldlev
		move.l	#newlev,$6c.w
		move	#$7fff,$9a(a0)
		move	#$c020,$9a(a0)
endif

		bsr	fadcol
		lea	$dff000,a0

;----------------------------------------------------------------------
;depacking and running the parts
;d0-ile, a0-skad, a1-dokad

;bra	q
		lea	tun_1,a0
		lea	$50000,a1
		move.l	#17752,d0
		bsr.L	decrunch
		jsr	$50000

;		lea	$dff000,a0
;		move	#$0008,$9a(a0)

		lea	rgb_2,a0
		lea	$50000,a1
		move.l	#8768,d0
		bsr.L	decrunch
		jsr	$50000

		lea	fla_3,a0
		lea	$4c000,a1
		move.l	#3228,d0
		bsr.L	decrunch
		jsr	$4c000

		move.l	#blue_copper,$dff080
		lea	run_4,a0
		lea	$48000,a1
		move.l	#11700,d0
		bsr.L	decrunch
		jsr	$48000

		lea	p192_5,a0
		lea	$48000,a1
		move.l	#7852,d0
		bsr.L	decrunch
		jsr	$48000

		move.l	#purple_copper,$dff080
		lea	GER_6,a0
		lea	$48000,a1
		move.l	#33332,d0
		bsr.L	decrunch
		jsr	$48000

q:
		lea	$dff000,a0
		bsr	l_odlot
		move.l	#blue_copper2,$dff080


;----------------------------------------------------------------------
if mus=1
		move.l	oldlev,$6c
		bsr	mt_end
endif

		lea	$dff000,a0
if exe=0
		move	#$e02c,$9a(a0)
endif
		move	#$8020,$96(a0)

		lea	endpart,a0
		lea	$50000,a1
		move.l	#53956,d0
		bsr.L	decrunch
		lea	$dff000,a0
if exe=1
		jmp	$50000
endif


if exe=1
		rte
else
		rts
endif

newlev:		movem.l d0-d7/a0-a6,-(sp)
		bsr	mt_music
	tst	doit
	bne.s	new2
		bsr.s	cc_keys
new2:		movem.l	(sp)+,d0-d7/a0-a6
	tst	doit
	bne.s	out

	tst	pointer
	bpl.s	out
	move.l	#load_hidden,2(a7)	;start hidden if password ok
	move	#1,doit

out:		move	#$40,$dff09a
		move	#$20,$dff09c
		rte

oldlev:		dc.l	0
;----------------------------------------------------------------------
load_hidden:
		move.l	#purple_copper,$dff080
		lea	hidden,a0
		lea	$50000,a1
		move.l	#5508,d0
		bsr.L	decrunch

		jmp	$50000

doit:	dc.w	0
;----------------------------------------------------------------------
;		*****************************************
;		*	NON SYSTEM PASSWORD CHECK	*
;		*****************************************

cc_keys:	lea	$bfe001,a1		;test keys
		move.b	$c00(a1),d0
		clr.b	$e00(a1)
		ori.b	#$40,$e00(a1)
		clr.b	$c00(a1)
		andi.b	#$bf,$e00(a1)

		tst.b	d0			;co drugie wcisniecie
		beq.s	cc_quit
		tst	key_wait
		beq.s	cc_ok
		move	#0,key_wait
		bra.L	cc_quit
cc_ok:
		move	#1,key_wait
		ori	#1,d0

		lea	password,a1
		move	pointer(pc),d1

		cmp.b	(a1,d1.w),d0
		beq.s	cc_letter_ok

		move	#0,pointer
		bra.s	cc_quit
cc_letter_ok:	addi	#1,pointer
		tst.b	1(a1,d1.w)
		bne.s	cc_quit

		move	#-1,pointer		;if all password ok
cc_quit:	rts

;---------------------------------------------------------------------
key_wait:	dc.w	1

pointer:	dc.w	0

password:	dc.b	$d7,$cf,$cd, $7f, $bd,$db,$9b,$d9,$db,$d7,0
		;password in crudekey codes !!!
even

;---------------------------------------------------------------------
;----------------------------------------------------------------------
blue_copper:
dc.l	$1000300,$1800003
dc.l	$e001ff00,$1800005,-2

purple_copper:
dc.l	$1000300,$1800101,-2

blue_copper2:
dc.l	$1000300,$1800002,-2

;----------------------------------------------------------------------
****************************************************
*** PowerPacker 2.0 FAST decrunch routine (v1.5) ***
*** Resourced by BC/LUZERS			 ***

;a0.l - crunched data
;a1.l - buffer
;d0.l - length of data

Decrunch:
	movem.l	d1-d7/a2-a6,-(sp)
	lea	4(a0),a2
	add.l	d0,a0
	lea	l0494(pc),a5
	moveq	#$18,d6
	moveq	#0,d4
	move.w	#$00FF,d7
	moveq	#1,d5
	move.l	a1,a4
	move.l	-(a0),d1
	tst.b	d1
	beq.s	l0266
	lsr.l	#1,d5
	beq.s	l02A2
l0262:	subq.b	#1,d1
	lsr.l	d1,d5
l0266:	lsr.l	#8,d1
	add.l	d1,a1
l026A:	lsr.l	#1,d5
	beq.s	l02A8
l026E:	bcs	l0310
	moveq	#0,d2
l0274:	moveq	#0,d1
	lsr.l	#1,d5
	beq.s	l02AE
l027A:	roxl.w	#1,d1
	lsr.l	#1,d5
	beq.s	l02B4
l0280:	roxl.w	#1,d1
	add.w	d1,d2
	subq.w	#3,d1
	beq.s	l0274
	moveq	#0,d0
l028A:	move.b	d5,d4
	lsr.l	#8,d5
	beq.s	l02C6
l0290:	move.b	-$0080(a5,d4.w),d0
	move.b	d0,-(a1)
	dbra	d2,l028A

	cmp.l	a1,a4
	bcs.s	l0310
	bra	l03F0

l02A2:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l0262

l02A8:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l026E

l02AE:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l027A

l02B4:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l0280

l02BA:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l0316

l02C0:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l031C

l02C6:	move.b	$007F(a5,d4.w),d0
	move.l	-(a0),d5
	move.w	d5,d3
	lsl.w	d0,d3
	bchg	d0,d3
	eor.w	d3,d4
	and.w	d7,d4
	moveq	#8,d1
	sub.w	d0,d1
	lsr.l	d1,d5
	add.w	d6,d0
	bset	d0,d5
	bra.s	l0290

l02E2:	move.b	$007F(a5,d4.w),d0
	move.l	-(a0),d5
	move.w	d5,d3
	lsl.w	d0,d3
	bchg	d0,d3
	eor.w	d3,d4
	and.w	d7,d4
	moveq	#8,d1
	sub.w	d0,d1
	lsr.l	d1,d5
	add.w	d6,d0
	bset	d0,d5
	bra.s	l0324

l02FE:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l035E

l0304:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l0364

l030A:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l036A

l0310:	moveq	#0,d2
	lsr.l	#1,d5
	beq.s	l02BA
l0316:	roxl.w	#1,d2
	lsr.l	#1,d5
	beq.s	l02C0
l031C:	roxl.w	#1,d2
	move.b	d5,d4
	lsr.l	#8,d5
	beq.s	l02E2
l0324:	moveq	#0,d3
	move.b	-$0080(a5,d4.w),d3
	cmp.w	#3,d2
	bne.s	l03AC
	bclr	#7,d3
	beq.s	l037E
	moveq	#13,d0
	sub.b	0(a2,d2.w),d0
	move.w	d0,d1
	add.w	d0,d0
	add.w	d1,d0
	add.w	d0,d0
	jmp	l035A(pc,d0.w)

l0348:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l0370

l034E:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l0376

l0354:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l037C

l035A:	lsr.l	#1,d5
	beq.s	l02FE
l035E:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l0304
l0364:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l030A
l036A:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l0348
l0370:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l034E
l0376:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l0354
l037C:	roxl.w	#1,d3
l037E:	moveq	#0,d1
	lsr.l	#1,d5
	beq.s	l039A
l0384:	roxl.w	#1,d1
	lsr.l	#1,d5
	beq.s	l03A0
l038A:	roxl.w	#1,d1
	lsr.l	#1,d5
	beq.s	l03A6
l0390:	roxl.w	#1,d1
	add.w	d1,d2
	subq.w	#7,d1
	beq.s	l037E
	bra.s	l03DC

l039A:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l0384

l03A0:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l038A

l03A6:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l0390

l03AC:	moveq	#13,d0
	sub.b	0(a2,d2.w),d0
	move.w	d0,d1
	add.w	d0,d0
	add.w	d1,d0
	add.w	d0,d0
	jmp	l03BE(pc,d0.w)

l03BE:	lsr.l	#1,d5
	beq.s	l03F6
l03C2:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l03FC
l03C8:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l0402
l03CE:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l0408
l03D4:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l040E
l03DA:	roxl.w	#1,d3
l03DC:	move.b	0(a1,d3.w),-(a1)
l03E0:	move.b	0(a1,d3.w),-(a1)
	dbra	d2,l03E0

	cmp.l	a1,a4
	bcs	l026A
l03F0:	movem.l	(sp)+,d1-d7/a2-a6
	rts

l03F6:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l03C2

l03FC:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l03C8

l0402:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l03CE

l0408:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l03D4

l040E:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l03DA

	or.l	#$40C020A0,d0
	bra.s	l03FC

	dc.l	$109050D0,$30B070F0,$088848C8,$28A868E8,$189858D8
	dc.l	$38B878F8,$048444C4,$24A464E4,$149454D4,$34B474F4
	dc.l	$0C8C4CCC,$2CAC6CEC,$1C9C5CDC,$3CBC7CFC,$028242C2
	dc.l	$22A262E2,$129252D2,$32B272F2,$0A8A4ACA,$2AAA6AEA
	dc.l	$1A9A5ADA,$3ABA7AFA,$068646C6,$26A666E6,$169656D6
	dc.l	$36B676F6,$0E8E4ECE,$2EAE6EEE,$1E9E5EDE,$3EBE7EFE
l0494:	dc.l	$018141C1,$21A161E1,$119151D1,$31B171F1,$098949C9
	dc.l	$29A969E9,$199959D9,$39B979F9,$058545C5,$25A565E5
	dc.l	$159555D5,$35B575F5,$0D8D4DCD,$2DAD6DED,$1D9D5DDD
	dc.l	$3DBD7DFD,$038343C3,$23A363E3,$139353D3,$33B373F3
	dc.l	$0B8B4BCB,$2BAB6BEB,$1B9B5BDB,$3BBB7BFB,$078747C7
	dc.l	$27A767E7,$179757D7,$37B777F7,$0F8F4FCF,$2FAF6FEF
	dc.l	$1F9F5FDF,$3FBF7FFF,$00010102,$02020203,$03030303
	dc.l	$03030304,$04040404,$04040404,$04040404,$04040405
	dc.l	$05050505,$05050505,$05050505,$05050505,$05050505
	dc.l	$05050505,$05050505,$05050506,$06060606,$06060606
	dc.l	$06060606,$06060606,$06060606,$06060606,$06060606
	dc.l	$06060606,$06060606,$06060606,$06060606,$06060606
	dc.l	$06060606,$06060606,$06060606,$06060607,$07070707
	dc.l	$07070707,$07070707,$07070707,$07070707,$07070707
	dc.l	$07070707,$07070707,$07070707,$07070707,$07070707
	dc.l	$07070707,$07070707,$07070707,$07070707,$07070707
	dc.l	$07070707,$07070707,$07070707,$07070707,$07070707
	dc.l	$07070707,$07070707,$07070707,$07070707,$07070707
	dc.l	$07070707,$07070707,$07070707,$07070707,$07070707
	dc.l	$07070700

;----------------------------------------------------------------------
;				REPLAYER
;----------------------------------------------------------------------
mt_lev6use=		1		; 0=NO, 1=YES
mt_finetuneused=	1		; 0=NO, 1=YES

mt_init	LEA	mt_data,A0
	MOVE.L	A0,mt_SongDataPtr
	LEA	250(A0),A1
	MOVE.W	#511,D0
	MOVEQ	#0,D1
mtloop	MOVE.L	D1,D2
	SUBQ.W	#1,D0
mtloop2	MOVE.B	(A1)+,D1
	CMP.W	D2,D1
	BGT.S	mtloop
	DBRA	D0,mtloop2
	ADDQ	#1,D2

	MOVE.W	D2,D3
	MULU	#128,D3
	ADD.L	#766,D3
	ADD.L	mt_SongDataPtr(PC),D3
	MOVE.L	D3,mt_LWTPtr

	LEA	mt_SampleStarts(PC),A1
	MULU	#128,D2
	ADD.L	#762,D2
	ADD.L	(A0,D2.L),D2
	ADD.L	mt_SongDataPtr(PC),D2
	ADDQ.L	#4,D2
	MOVE.L	D2,A2
	MOVEQ	#30,D0
mtloop3	MOVE.L	A2,(A1)+
	MOVEQ	#0,D1
	MOVE.W	(A0),D1
	ADD.L	D1,D1
	ADD.L	D1,A2
	LEA	8(A0),A0
	DBRA	D0,mtloop3

	OR.B	#2,$BFE001
	lea	mt_speed(PC),A4
	MOVE.B	#6,(A4)
	CLR.B	mt_counter-mt_speed(A4)
	CLR.B	mt_SongPos-mt_speed(A4)
	CLR.W	mt_PatternPos-mt_speed(A4)
mt_end	LEA	$DFF096,A0
	CLR.W	$12(A0)
	CLR.W	$22(A0)
	CLR.W	$32(A0)
	CLR.W	$42(A0)
	MOVE.W	#$F,(A0)
	RTS

mt_music
	MOVEM.L	D0-D4/D7/A0-A6,-(SP)
	ADDQ.B	#1,mt_counter
	MOVE.B	mt_counter(PC),D0
	CMP.B	mt_speed(PC),D0
	BLO.S	mt_NoNewNote
	CLR.B	mt_counter
	TST.B	mt_PattDelTime2
	BEQ.S	mt_GetNewNote
	BSR.S	mt_NoNewAllChannels
	BRA.W	mt_dskip

mt_NoNewNote
	BSR.S	mt_NoNewAllChannels
	BRA.W	mt_NoNewPosYet

mt_NoNewAllChannels
	LEA	$dff090,A5
	LEA	mt_chan1temp-44(PC),A6
	BSR.W	mt_CheckEfx
	BSR.W	mt_CheckEfx
	BSR.W	mt_CheckEfx
	BRA.W	mt_CheckEfx

mt_GetNewNote
	MOVE.L	mt_SongDataPtr(PC),A0
	LEA	(A0),A3
	LEA	122(A0),A2	;pattpo
	LEA	762(A0),A0	;patterndata
	CLR.W	mt_DMACONtemp

	LEA	$DFF090,A5
	LEA	mt_chan1temp-44(PC),A6
	BSR.S	mt_DoVoice
	BSR.S	mt_DoVoice
	BSR.s	mt_DoVoice
	BSR.s	mt_DoVoice
	BRA.W	mt_SetDMA

mt_DoVoice
	MOVEQ	#0,D0
	MOVEQ	#0,D1
	MOVE.B	mt_SongPos(PC),D0
	LEA	128(A2),A2
	MOVE.B	(A2,D0.W),D1
	MOVE.W	mt_PatternPos(PC),D2
	LSL	#7,D1
	LSR.W	#1,D2
	ADD.W	D2,D1
	LEA	44(A6),A6
	lea	$10(a5),a5

	TST.L	(A6)
	BNE.S	mt_plvskip
	BSR.W	mt_PerNop
mt_plvskip
	MOVE.W	(A0,D1.W),D1
	LSL.W	#2,D1
	MOVE.L	A0,-(sp)
	MOVE.L	mt_LWTPtr(PC),A0
	MOVE.L	(A0,D1.W),(A6)
	MOVE.L	(sp)+,A0
	MOVE.B	2(A6),D2
	AND.L	#$F0,D2
	LSR.B	#4,D2
	MOVE.B	(A6),D0
	AND.B	#$F0,D0
	OR.B	D0,D2
	BEQ.s	mt_SetRegs
	MOVEQ	#0,D3
	LEA	mt_SampleStarts(PC),A1
	SUBQ	#1,D2
	MOVE	D2,D4
	ADD	D2,D2
	ADD	D2,D2
	LSL	#3,D4
	MOVE.L	(A1,D2.L),4(A6)
	MOVE.W	(A3,D4.W),8(A6)
	MOVE.W	(A3,D4.W),40(A6)
	MOVE.W	2(A3,D4.W),18(A6)
	MOVE.L	4(A6),D2	; Get start
	MOVE.W	4(A3,D4.W),D3	; Get repeat
	BEQ.S	mt_NoLoop
	MOVE.W	D3,D0		; Get repeat
	ADD.W	D3,D3
	ADD.L	D3,D2		; Add repeat
	ADD.W	6(A3,D4.W),D0	; Add replen
	MOVE.W	D0,8(A6)

mt_NoLoop
	MOVE.L	D2,10(A6)
	MOVE.L	D2,36(A6)
	MOVE.W	6(A3,D4.W),14(A6)	; Save replen
		movem.l	a5/d1,-(sp)
		move	a5,d1
		andi	#$f0,d1
		lsr	#4,d1
		subi	#10,d1
		lsl	#1,d1
		lea	mt_volumes,a5
		move.b	19(a6),1(A5,d1.w)
		movem.l	(sp)+,a5/d1
mt_SetRegs
	MOVE.W	(A6),D0
	AND.W	#$0FFF,D0
	BEQ.W	mt_CheckMoreEfx	; If no note

	IF mt_finetuneused=1
	MOVE.W	2(A6),D0
	AND.W	#$0FF0,D0
	CMP.W	#$0E50,D0
	BEQ.S	mt_DoSetFineTune
	ENDif

	MOVE.B	2(A6),D0
	AND.B	#$0F,D0
	CMP.B	#3,D0	; TonePortamento
	BEQ.S	mt_ChkTonePorta
	CMP.B	#5,D0
	BEQ.S	mt_ChkTonePorta
	CMP.B	#9,D0	; Sample Offset
	BNE.S	mt_SetPeriod
	BSR.W	mt_CheckMoreEfx
	BRA.S	mt_SetPeriod

mt_ChkTonePorta
	BSR.W	mt_SetTonePorta
	BRA.W	mt_CheckMoreEfx

mt_DoSetFineTune
	BSR.W	mt_SetFineTune

mt_SetPeriod
	MOVEM.L	D1/A1,-(SP)
	MOVE.W	(A6),D1
	AND.W	#$0FFF,D1

	IF mt_finetuneused=0
	MOVE.W	D1,16(A6)

	ELSE
mt_SetPeriod2
	LEA	mt_PeriodTable(PC),A1
	MOVEQ	#36,D7
mt_ftuloop
	CMP.W	(A1)+,D1
	BHS.S	mt_ftufound
	DBRA	D7,mt_ftuloop
mt_ftufound
	MOVEQ	#0,D1
	MOVE.B	18(A6),D1
	LSL	#3,D1
	MOVE	D1,D0
	LSL	#3,D1
	ADD	D0,D1
	MOVE.W	-2(A1,D1.W),16(A6)
	ENDif

	MOVEM.L	(SP)+,D1/A1

	MOVE.W	2(A6),D0
	AND.W	#$0FF0,D0
	CMP.W	#$0ED0,D0 ; Notedelay
	BEQ.W	mt_CheckMoreEfx

	MOVE.W	20(A6),$DFF096
	BTST	#2,30(A6)
	BNE.S	mt_vibnoc
	CLR.B	27(A6)
mt_vibnoc
	BTST	#6,30(A6)
	BNE.S	mt_trenoc
	CLR.B	29(A6)
mt_trenoc
	MOVE.L	4(A6),(A5)	; Set start
	MOVE.W	8(A6),4(A5)	; Set length
	MOVE.W	16(A6),6(A5)	; Set period
	MOVE.W	20(A6),D0
	OR.W	D0,mt_DMACONtemp
	BRA.W	mt_CheckMoreEfx
 
mt_SetDMA
	IF mt_lev6use=1
	lea	$bfd000,a3
	move.b	#$7f,$d00(a3)
	move.w	#$2000,$dff09c
	move.w	#$a000,$dff09a
	move.l	$78.w,mt_oldirq
	move.l	#mt_irq1,$78.w
	moveq	#0,d0
	move.b	d0,$e00(a3)
	move.b	#$a8,$400(a3)
	move.b	d0,$500(a3)
	move.b	#$11,$e00(a3)
	move.b	#$81,$d00(a3)
	OR.W	#$8000,mt_DMACONtemp
	BRA.w	mt_dskip

	ELSE
	OR.W	#$8000,mt_DMACONtemp
	bsr.w	mt_WaitDMA
	ENDif

	IF mt_lev6use=1
mt_irq1:tst.b	$bfdd00
	MOVE.W	mt_dmacontemp(pc),$DFF096
	move.w	#$2000,$dff09c
	move.l	#mt_irq2,$78.w
	rte

	ELSE
	MOVE.W	mt_dmacontemp(pc),$DFF096
	bsr.w	mt_WaitDMA
	ENDif

	IF mt_lev6use=1
mt_irq2:tst.b	$bfdd00
	movem.l	a5-a6,-(a7)
	ENDif

	LEA	$DFF0A0,A5
	LEA	mt_chan1temp(PC),A6
	MOVE.L	10(A6),(A5)
	MOVE.W	14(A6),4(A5)
	MOVE.L	54(A6),$10(A5)
	MOVE.W	58(A6),$14(A5)
	MOVE.L	98(A6),$20(A5)
	MOVE.W	102(A6),$24(A5)
	MOVE.L	142(A6),$30(A5)
	MOVE.W	146(A6),$34(A5)

	IF mt_lev6use=1
	move.b	#0,$bfde00
	move.b	#$7f,$bfdd00
	move.l	mt_oldirq(pc),$78.w
	move.w	#$2000,$dff09c
	movem.l	(a7)+,a5-a6
	rte
	ENDif

mt_dskip
	lea	mt_speed(PC),A4
	ADDQ.W	#4,mt_PatternPos-mt_speed(A4)
	MOVE.B	mt_PattDelTime-mt_speed(A4),D0
	BEQ.S	mt_dskc
	MOVE.B	D0,mt_PattDelTime2-mt_speed(A4)
	CLR.B	mt_PattDelTime-mt_speed(A4)
mt_dskc	TST.B	mt_PattDelTime2-mt_speed(A4)
	BEQ.S	mt_dska
	SUBQ.B	#1,mt_PattDelTime2-mt_speed(A4)
	BEQ.S	mt_dska
	SUBQ.W	#4,mt_PatternPos-mt_speed(A4)
mt_dska	TST.B	mt_PBreakFlag-mt_speed(A4)
	BEQ.S	mt_nnpysk
	SF	mt_PBreakFlag-mt_speed(A4)
	MOVEQ	#0,D0
	MOVE.B	mt_PBreakPos(PC),D0
	CLR.B	mt_PBreakPos-mt_speed(A4)
	LSL	#2,D0
	MOVE.W	D0,mt_PatternPos-mt_speed(A4)
mt_nnpysk
	CMP.W	#256,mt_PatternPos-mt_speed(A4)
	BLO.S	mt_NoNewPosYet
mt_NextPosition	
	MOVEQ	#0,D0
	MOVE.B	mt_PBreakPos(PC),D0
	LSL	#2,D0
	MOVE.W	D0,mt_PatternPos-mt_speed(A4)
	CLR.B	mt_PBreakPos-mt_speed(A4)
	CLR.B	mt_PosJumpFlag-mt_speed(A4)
	ADDQ.B	#1,mt_SongPos-mt_speed(A4)
	AND.B	#$7F,mt_SongPos-mt_speed(A4)
	MOVE.B	mt_SongPos(PC),D1
	MOVE.L	mt_SongDataPtr(PC),A0
	CMP.B	248(A0),D1
	BLO.S	mt_NoNewPosYet
	CLR.B	mt_SongPos-mt_speed(A4)
mt_NoNewPosYet
	lea	mt_speed(PC),A4
	TST.B	mt_PosJumpFlag-mt_speed(A4)
	BNE.S	mt_NextPosition

	lea	mt_volumes,a1			;control volume
	lea	$dff0a0,a2
	move	mt_percent,d0
	move	#3,d7
mt_putvol:
	move	(a1)+,d1
	mulu	d0,d1
	lsr	#7,d1
	move	d1,8(a2)
	lea	$10(a2),a2
	dbf	d7,mt_putvol
	MOVEM.L	(SP)+,D0-D4/D7/A0-A6
	RTS

mt_CheckEfx
	lea	$10(a5),a5
	lea	44(a6),a6
	BSR.W	mt_UpdateFunk
	MOVE.W	2(A6),D0
	AND.W	#$0FFF,D0
	BEQ.S	mt_PerNop
	MOVE.B	2(A6),D0
	MOVEQ	#$0F,D1
	AND.L	D1,D0
	BEQ.S	mt_Arpeggio
	SUBQ	#1,D0
	BEQ.W	mt_PortaUp
	SUBQ	#1,D0
	BEQ.W	mt_PortaDown
	SUBQ	#1,D0
	BEQ.W	mt_TonePortamento
	SUBQ	#1,D0
	BEQ.W	mt_Vibrato
	SUBQ	#1,D0
	BEQ.W	mt_TonePlusVolSlide
	SUBQ	#1,D0
	BEQ.W	mt_VibratoPlusVolSlide
	SUBQ	#8,D0
	BEQ.W	mt_E_Commands
SetBack	MOVE.W	16(A6),6(A5)
	ADDQ	#7,D0
	BEQ.W	mt_Tremolo
	SUBQ	#3,D0
	BEQ.W	mt_VolumeSlide
mt_Return2
	RTS

mt_PerNop
	MOVE.W	16(A6),6(A5)
	RTS

mt_Arpeggio
	MOVEQ	#0,D0
	MOVE.B	mt_counter(PC),D0
	DIVS	#3,D0
	SWAP	D0
	TST.W	D0
	BEQ.S	mt_Arpeggio2
	SUBQ	#2,D0
	BEQ.S	mt_Arpeggio1
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	LSR.B	#4,D0
	BRA.S	mt_Arpeggio3

mt_Arpeggio2
	MOVE.W	16(A6),6(A5)
	RTS

mt_Arpeggio1
	MOVE.B	3(A6),D0
	AND.W	#15,D0
mt_Arpeggio3
	ADD.W	D0,D0
	LEA	mt_PeriodTable(PC),A0

	IF mt_finetuneused=1
	MOVEQ	#0,D1
	MOVE.B	18(A6),D1
	LSL	#3,D1
	MOVE	D1,D2
	LSL	#3,D1
	ADD	D2,D1
	ADD.L	D1,A0
	ENDif

	MOVE.W	16(A6),D1
	MOVEQ	#36,D7
mt_arploop
	CMP.W	(A0)+,D1
	BHS.S	mt_Arpeggio4
	DBRA	D7,mt_arploop
	RTS

mt_Arpeggio4
	MOVE.W	-2(A0,D0.W),6(A5)
	RTS

mt_FinePortaUp
	TST.B	mt_counter
	BNE.S	mt_Return2
	MOVE.B	#$0F,mt_LowMask
mt_PortaUp
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	AND.B	mt_LowMask(PC),D0
	MOVE.B	#$FF,mt_LowMask
	SUB.W	D0,16(A6)
	MOVE.W	16(A6),D0
	AND.W	#$0FFF,D0
	CMP.W	#113,D0
	BPL.S	mt_PortaUskip
	AND.W	#$F000,16(A6)
	OR.W	#113,16(A6)
mt_PortaUskip
	MOVE.W	16(A6),D0
	AND.W	#$0FFF,D0
	MOVE.W	D0,6(A5)
	RTS	
 
mt_FinePortaDown
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	#$0F,mt_LowMask
mt_PortaDown
	CLR.W	D0
	MOVE.B	3(A6),D0
	AND.B	mt_LowMask(PC),D0
	MOVE.B	#$FF,mt_LowMask
	ADD.W	D0,16(A6)
	MOVE.W	16(A6),D0
	AND.W	#$0FFF,D0
	CMP.W	#856,D0
	BMI.S	mt_PortaDskip
	AND.W	#$F000,16(A6)
	OR.W	#856,16(A6)
mt_PortaDskip
	MOVE.W	16(A6),D0
	AND.W	#$0FFF,D0
	MOVE.W	D0,6(A5)
	RTS

mt_SetTonePorta
	MOVE.L	A0,-(SP)
	MOVE.W	(A6),D2
	AND.W	#$0FFF,D2
	LEA	mt_PeriodTable(PC),A0

	IF	mt_finetuneused=1
	MOVEQ	#0,D0
	MOVE.B	18(A6),D0
	ADD	D0,D0
	MOVE	D0,D7
	ADD	D0,D0
	ADD	D0,D0
	ADD	D0,D7
	LSL	#3,D0
	ADD	D7,D0
	ADD.L	D0,A0
	ENDif

	MOVEQ	#0,D0
mt_StpLoop
	CMP.W	(A0,D0.W),D2
	BHS.S	mt_StpFound
	ADDQ	#2,D0
	CMP.W	#37*2,D0
	BLO.S	mt_StpLoop
	MOVEQ	#35*2,D0
mt_StpFound
	BTST	#3,18(A6)
	BEQ.S	mt_StpGoss
	TST.W	D0
	BEQ.S	mt_StpGoss
	SUBQ	#2,D0
mt_StpGoss
	MOVE.W	(A0,D0.W),D2
	MOVE.L	(SP)+,A0
	MOVE.W	D2,24(A6)
	MOVE.W	16(A6),D0
	CLR.B	22(A6)
	CMP.W	D0,D2
	BEQ.S	mt_ClearTonePorta
	BGE.W	mt_Return2
	MOVE.B	#1,22(A6)
	RTS

mt_ClearTonePorta
	CLR.W	24(A6)
	RTS

mt_TonePortamento
	MOVE.B	3(A6),D0
	BEQ.S	mt_TonePortNoChange
	MOVE.B	D0,23(A6)
	CLR.B	3(A6)
mt_TonePortNoChange
	TST.W	24(A6)
	BEQ.W	mt_Return2
	MOVEQ	#0,D0
	MOVE.B	23(A6),D0
	TST.B	22(A6)
	BNE.S	mt_TonePortaUp
mt_TonePortaDown
	ADD.W	D0,16(A6)
	MOVE.W	24(A6),D0
	CMP.W	16(A6),D0
	BGT.S	mt_TonePortaSetPer
	MOVE.W	24(A6),16(A6)
	CLR.W	24(A6)
	BRA.S	mt_TonePortaSetPer

mt_TonePortaUp
	SUB.W	D0,16(A6)
	MOVE.W	24(A6),D0
	CMP.W	16(A6),D0
	BLT.S	mt_TonePortaSetPer
	MOVE.W	24(A6),16(A6)
	CLR.W	24(A6)

mt_TonePortaSetPer
	MOVE.W	16(A6),D2
	MOVE.B	31(A6),D0
	AND.B	#$0F,D0
	BEQ.S	mt_GlissSkip
	LEA	mt_PeriodTable(PC),A0

	IF mt_finetuneused=1
	MOVEQ	#0,D0
	MOVE.B	18(A6),D0
	LSL	#3,D0
	MOVE	D0,D1
	LSL	#3,D0
	ADD	D1,D0
	ADD.L	D0,A0
	ENDif

	MOVEQ	#0,D0
mt_GlissLoop
	CMP.W	(A0,D0.W),D2
	BHS.S	mt_GlissFound
	ADDQ	#2,D0
	CMP.W	#36*2,D0
	BLO.S	mt_GlissLoop
	MOVEQ	#35*2,D0
mt_GlissFound
	MOVE.W	(A0,D0.W),D2
mt_GlissSkip
	MOVE.W	D2,6(A5) ; Set period
	RTS

mt_Vibrato
	MOVE.B	3(A6),D0
	BEQ.S	mt_Vibrato2
	MOVE.B	26(A6),D2
	AND.B	#$0F,D0
	BEQ.S	mt_vibskip
	AND.B	#$F0,D2
	OR.B	D0,D2
mt_vibskip
	MOVE.B	3(A6),D0
	AND.B	#$F0,D0
	BEQ.S	mt_vibskip2
	AND.B	#$0F,D2
	OR.B	D0,D2
mt_vibskip2
	MOVE.B	D2,26(A6)
mt_Vibrato2
	MOVE.B	27(A6),D0
	LEA	mt_VibratoTable(PC),A4
	LSR.W	#2,D0
	AND.W	#$001F,D0
	MOVE.B	30(A6),D2
	AND.W	#$03,D2
	BEQ.S	mt_vib_sine
	LSL.B	#3,D0
	CMP.B	#1,D2
	BEQ.S	mt_vib_rampdown
	MOVE.B	#255,D2
	BRA.S	mt_vib_set
mt_vib_rampdown
	TST.B	27(A6)
	BPL.S	mt_vib_rampdown2
	MOVE.B	#255,D2
	SUB.B	D0,D2
	BRA.S	mt_vib_set
mt_vib_rampdown2
	MOVE.B	D0,D2
	BRA.S	mt_vib_set
mt_vib_sine
	MOVE.B	0(A4,D0.W),D2
mt_vib_set
	MOVE.B	26(A6),D0
	AND.W	#15,D0
	MULU	D0,D2
	LSR.W	#7,D2
	MOVE.W	16(A6),D0
	TST.B	27(A6)
	BMI.S	mt_VibratoNeg
	ADD.W	D2,D0
	BRA.S	mt_Vibrato3
mt_VibratoNeg
	SUB.W	D2,D0
mt_Vibrato3
	MOVE.W	D0,6(A5)
	MOVE.B	26(A6),D0
	LSR.W	#2,D0
	AND.W	#$003C,D0
	ADD.B	D0,27(A6)
	RTS

mt_TonePlusVolSlide
	BSR.W	mt_TonePortNoChange
	BRA.W	mt_VolumeSlide

mt_VibratoPlusVolSlide
	BSR.S	mt_Vibrato2
	BRA.W	mt_VolumeSlide

mt_Tremolo
	MOVE.B	3(A6),D0
	BEQ.S	mt_Tremolo2
	MOVE.B	28(A6),D2
	AND.B	#$0F,D0
	BEQ.S	mt_treskip
	AND.B	#$F0,D2
	OR.B	D0,D2
mt_treskip
	MOVE.B	3(A6),D0
	AND.B	#$F0,D0
	BEQ.S	mt_treskip2
	AND.B	#$0F,D2
	OR.B	D0,D2
mt_treskip2
	MOVE.B	D2,28(A6)
mt_Tremolo2
	MOVE.B	29(A6),D0
	LEA	mt_VibratoTable(PC),A4
	LSR.W	#2,D0
	AND.W	#$001F,D0
	MOVEQ	#0,D2
	MOVE.B	30(A6),D2
	LSR.B	#4,D2
	AND.B	#$03,D2
	BEQ.S	mt_tre_sine
	LSL.B	#3,D0
	CMP.B	#1,D2
	BEQ.S	mt_tre_rampdown
	MOVE.B	#255,D2
	BRA.S	mt_tre_set
mt_tre_rampdown
	TST.B	27(A6)
	BPL.S	mt_tre_rampdown2
	MOVE.B	#255,D2
	SUB.B	D0,D2
	BRA.S	mt_tre_set
mt_tre_rampdown2
	MOVE.B	D0,D2
	BRA.S	mt_tre_set
mt_tre_sine
	MOVE.B	0(A4,D0.W),D2
mt_tre_set
	MOVE.B	28(A6),D0
	AND.W	#15,D0
	MULU	D0,D2
	LSR.W	#6,D2
	MOVEQ	#0,D0
	MOVE.B	19(A6),D0
	TST.B	29(A6)
	BMI.S	mt_TremoloNeg
	ADD.W	D2,D0
	BRA.S	mt_Tremolo3
mt_TremoloNeg
	SUB.W	D2,D0
mt_Tremolo3
	BPL.S	mt_TremoloSkip
	CLR.W	D0
mt_TremoloSkip
	CMP.W	#$40,D0
	BLS.S	mt_TremoloOk
	MOVE.W	#$40,D0
mt_TremoloOk
		movem.l	a5/d1,-(sp)
		move	a5,d1
		andi	#$f0,d1
		lsr	#4,d1
		subi	#10,d1
		lsl	#1,d1
		lea	mt_volumes,a5
		MOVE	D0,(A5,d1.w)
		movem.l	(sp)+,a5/d1
	MOVE.B	28(A6),D0
	LSR.W	#2,D0
	AND.W	#$003C,D0
	ADD.B	D0,29(A6)
	RTS

mt_SampleOffset
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	BEQ.S	mt_sononew
	MOVE.B	D0,32(A6)
mt_sononew
	MOVE.B	32(A6),D0
	LSL.W	#7,D0
	CMP.W	8(A6),D0
	BGE.S	mt_sofskip
	SUB.W	D0,8(A6)
	ADD.W	D0,D0
	ADD.L	D0,4(A6)
	RTS
mt_sofskip
	MOVE.W	#$0001,8(A6)
	RTS

mt_VolumeSlide
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	LSR.B	#4,D0
	TST.B	D0
	BEQ.S	mt_VolSlideDown
mt_VolSlideUp
	ADD.B	D0,19(A6)
	CMP.B	#$40,19(A6)
	BMI.S	mt_vsuskip
	MOVE.B	#$40,19(A6)
mt_vsuskip
		movem.l	a5/d1,-(sp)
		move	a5,d1
		andi	#$f0,d1
		lsr	#4,d1
		subi	#10,d1
		lsl	#1,d1
		lea	mt_volumes,a5
		move.b	19(a6),1(A5,d1.w)
		movem.l	(sp)+,a5/d1
	RTS

mt_VolSlideDown
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
mt_VolSlideDown2
	SUB.B	D0,19(A6)
	BPL.S	mt_vsdskip
	CLR.B	19(A6)
mt_vsdskip
		movem.l	a5/d1,-(sp)
		move	a5,d1
		andi	#$f0,d1
		lsr	#4,d1
		subi	#10,d1
		lsl	#1,d1
		lea	mt_volumes,a5
		move.b	19(a6),1(A5,d1.w)
		movem.l	(sp)+,a5/d1
	RTS

mt_PositionJump
	MOVE.B	3(A6),D0
	SUBQ	#1,D0
	MOVE.B	D0,mt_SongPos
mt_pj2	CLR.B	mt_PBreakPos
	ST 	mt_PosJumpFlag
	RTS

mt_VolumeChange
	MOVE.B	3(A6),D0
	CMP.B	#$40,D0
	BLS.S	mt_VolumeOk
	MOVEQ	#$40,D0
mt_VolumeOk
	MOVE.B	D0,19(A6)
		movem.l	a5/d1,-(sp)
		move	a5,d1
		andi	#$f0,d1
		lsr	#4,d1
		subi	#10,d1
		lsl	#1,d1
		lea	mt_volumes,a5
		MOVE.b	D0,1(A5,d1.w)
		movem.l	(sp)+,a5/d1
	RTS

mt_PatternBreak
	MOVEQ	#0,D0
	MOVE.B	3(A6),D0
	MOVE.W	D0,D2
	LSR.B	#4,D0
	ADD	D0,D0
	MOVE	D0,D1
	ADD	D0,D0
	ADD	D0,D0
	ADD	D1,D0
	AND.B	#$0F,D2
	ADD.B	D2,D0
	CMP.B	#63,D0
	BHI.S	mt_pj2
	MOVE.B	D0,mt_PBreakPos
	ST	mt_PosJumpFlag
	RTS

mt_SetSpeed
	MOVE.B	3(A6),D0
	BEQ.W	mt_Return2
	CLR.B	mt_counter
	MOVE.B	D0,mt_speed
	RTS

mt_CheckMoreEfx
	BSR.W	mt_UpdateFunk
	MOVE.B	2(A6),D0
	AND.B	#$0F,D0
	SUB.B	#9,D0
	BEQ.W	mt_SampleOffset
	SUBQ	#2,D0
	BEQ.W	mt_PositionJump
	SUBQ	#1,D0
	BEQ.L	mt_VolumeChange
	SUBQ	#1,D0
	BEQ.S	mt_PatternBreak
	SUBQ	#1,D0
	BEQ.S	mt_E_Commands
	SUBQ	#1,D0
	BEQ.S	mt_SetSpeed
	BRA.W	mt_PerNop

mt_E_Commands
	MOVE.B	3(A6),D0
	AND.W	#$F0,D0
	LSR.B	#4,D0
	BEQ.S	mt_FilterOnOff
	SUBQ	#1,D0
	BEQ.W	mt_FinePortaUp
	SUBQ	#1,D0
	BEQ.W	mt_FinePortaDown
	SUBQ	#1,D0
	BEQ.S	mt_SetGlissControl
	SUBQ	#1,D0
	BEQ.s	mt_SetVibratoControl

	IF mt_finetuneused=1
	SUBQ	#1,D0
	BEQ.s	mt_SetFineTune
	SUBQ	#1,D0

	ELSE
	SUBQ	#2,D0
	ENDif

	BEQ.s	mt_JumpLoop
	SUBQ	#1,D0
	BEQ.W	mt_SetTremoloControl
	SUBQ	#2,D0
	BEQ.W	mt_RetrigNote
	SUBQ	#1,D0
	BEQ.W	mt_VolumeFineUp
	SUBQ	#1,D0
	BEQ.W	mt_VolumeFineDown
	SUBQ	#1,D0
	BEQ.W	mt_NoteCut
	SUBQ	#1,D0
	BEQ.W	mt_NoteDelay
	SUBQ	#1,D0
	BEQ.W	mt_PatternDelay
	BRA.W	mt_FunkIt

mt_FilterOnOff
	MOVE.B	3(A6),D0
	AND.B	#1,D0
	ADD.B	D0,D0
	AND.B	#$FD,$BFE001
	OR.B	D0,$BFE001
	RTS	

mt_SetGlissControl
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	AND.B	#$F0,31(A6)
	OR.B	D0,31(A6)
	RTS

mt_SetVibratoControl
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	AND.B	#$F0,30(A6)
	OR.B	D0,30(A6)
	RTS

mt_SetFineTune
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	MOVE.B	D0,18(A6)
	RTS

mt_JumpLoop
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	BEQ.S	mt_SetLoop
	TST.B	34(A6)
	BEQ.S	mt_jumpcnt
	SUBQ.B	#1,34(A6)
	BEQ.W	mt_Return2
mt_jmploop 	MOVE.B	33(A6),mt_PBreakPos
	ST	mt_PBreakFlag
	RTS

mt_jumpcnt
	MOVE.B	D0,34(A6)
	BRA.S	mt_jmploop

mt_SetLoop
	MOVE.W	mt_PatternPos(PC),D0
	LSR	#2,D0
	MOVE.B	D0,33(A6)
	RTS

mt_SetTremoloControl
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	LSL.B	#4,D0
	AND.B	#$0F,30(A6)
	OR.B	D0,30(A6)
	RTS

mt_RetrigNote
	MOVE.L	D1,-(SP)
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	BEQ.S	mt_rtnend
	MOVEQ	#0,d1
	MOVE.B	mt_counter(PC),D1
	BNE.S	mt_rtnskp
	MOVE.W	(A6),D1
	AND.W	#$0FFF,D1
	BNE.S	mt_rtnend
	MOVEQ	#0,D1
	MOVE.B	mt_counter(PC),D1
mt_rtnskp
	DIVU	D0,D1
	SWAP	D1
	TST.W	D1
	BNE.S	mt_rtnend
mt_DoRetrig
	MOVE.W	20(A6),$DFF096	; Channel DMA off
	MOVE.L	4(A6),(A5)	; Set sampledata pointer
	MOVE.W	8(A6),4(A5)	; Set length
	BSR.W	mt_WaitDMA
	MOVE.W	20(A6),D0
	BSET	#15,D0
	MOVE.W	D0,$DFF096
	BSR.W	mt_WaitDMA
	MOVE.L	10(A6),(A5)
	MOVE.L	14(A6),4(A5)
mt_rtnend
	MOVE.L	(SP)+,D1
	RTS

mt_VolumeFineUp
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.W	#$F,D0
	BRA.W	mt_VolSlideUp

mt_VolumeFineDown
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	BRA.W	mt_VolSlideDown2

mt_NoteCut
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	CMP.B	mt_counter(PC),D0
	BNE.W	mt_Return2
	CLR.B	19(A6)
		movem.l	a5/d1,-(sp)
		move	a5,d1
		andi	#$f0,d1
		lsr	#4,d1
		subi	#10,d1
		lsl	#1,d1
		lea	mt_volumes,a5
		clr	(A5,d1.w)
		movem.l	(sp)+,a5/d1
	RTS

mt_NoteDelay
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	CMP.B	mt_Counter(PC),D0
	BNE.W	mt_Return2
	MOVE.W	(A6),D0
	BEQ.W	mt_Return2
	MOVE.L	D1,-(SP)
	BRA.W	mt_DoRetrig

mt_PatternDelay
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.W	#$0F,D0
	TST.B	mt_PattDelTime2
	BNE.W	mt_Return2
	ADDQ.B	#1,D0
	MOVE.B	D0,mt_PattDelTime
	RTS

mt_FunkIt
	TST.B	mt_counter
	BNE.W	mt_Return2
	MOVE.B	3(A6),D0
	AND.B	#$0F,D0
	LSL.B	#4,D0
	AND.B	#$0F,31(A6)
	OR.B	D0,31(A6)
	TST.B	D0
	BEQ.W	mt_Return2
mt_UpdateFunk
	MOVEM.L	D1/A0,-(SP)
	MOVEQ	#0,D0
	MOVE.B	31(A6),D0
	LSR.B	#4,D0
	BEQ.S	mt_funkend
	LEA	mt_FunkTable(PC),A0
	MOVE.B	(A0,D0.W),D0
	ADD.B	D0,35(A6)
	BTST	#7,35(A6)
	BEQ.S	mt_funkend
	CLR.B	35(A6)

	MOVE.L	10(A6),D0
	MOVEQ	#0,D1
	MOVE.W	14(A6),D1
	ADD.L	D1,D0
	ADD.L	D1,D0
	MOVE.L	36(A6),A0
	ADDQ.L	#1,A0
	CMP.L	D0,A0
	BLO.S	mt_funkok
	MOVE.L	10(A6),A0
mt_funkok
	MOVE.L	A0,36(A6)
	NEG.B	(A0)
	SUBQ.B	#1,(A0)
mt_funkend
	MOVEM.L	(SP)+,D1/A0
	RTS

mt_WaitDMA
	MOVEQ	#3,D0
mt_WaitDMA2
	MOVE.B	$DFF006,D1
mt_WaitDMA3
	CMP.B	$DFF006,D1
	BEQ.S	mt_WaitDMA3
	DBF	D0,mt_WaitDMA2
	RTS

mt_FunkTable dc.b 0,5,6,7,8,10,11,13,16,19,22,26,32,43,64,128

mt_VibratoTable	
	dc.b   0, 24, 49, 74, 97,120,141,161
	dc.b 180,197,212,224,235,244,250,253
	dc.b 255,253,250,244,235,224,212,197
	dc.b 180,161,141,120, 97, 74, 49, 24

mt_PeriodTable
; Tuning 0, Normal
	dc.w	856,808,762,720,678,640,604,570,538,508,480,453
	dc.w	428,404,381,360,339,320,302,285,269,254,240,226
	dc.w	214,202,190,180,170,160,151,143,135,127,120,113
; Tuning 1
	dc.w	850,802,757,715,674,637,601,567,535,505,477,450
	dc.w	425,401,379,357,337,318,300,284,268,253,239,225
	dc.w	213,201,189,179,169,159,150,142,134,126,119,113
; Tuning 2
	dc.w	844,796,752,709,670,632,597,563,532,502,474,447
	dc.w	422,398,376,355,335,316,298,282,266,251,237,224
	dc.w	211,199,188,177,167,158,149,141,133,125,118,112
; Tuning 3
	dc.w	838,791,746,704,665,628,592,559,528,498,470,444
	dc.w	419,395,373,352,332,314,296,280,264,249,235,222
	dc.w	209,198,187,176,166,157,148,140,132,125,118,111
; Tuning 4
	dc.w	832,785,741,699,660,623,588,555,524,495,467,441
	dc.w	416,392,370,350,330,312,294,278,262,247,233,220
	dc.w	208,196,185,175,165,156,147,139,131,124,117,110
; Tuning 5
	dc.w	826,779,736,694,655,619,584,551,520,491,463,437
	dc.w	413,390,368,347,328,309,292,276,260,245,232,219
	dc.w	206,195,184,174,164,155,146,138,130,123,116,109
; Tuning 6
	dc.w	820,774,730,689,651,614,580,547,516,487,460,434
	dc.w	410,387,365,345,325,307,290,274,258,244,230,217
	dc.w	205,193,183,172,163,154,145,137,129,122,115,109
; Tuning 7
	dc.w	814,768,725,684,646,610,575,543,513,484,457,431
	dc.w	407,384,363,342,323,305,288,272,256,242,228,216
	dc.w	204,192,181,171,161,152,144,136,128,121,114,108
; Tuning -8
	dc.w	907,856,808,762,720,678,640,604,570,538,508,480
	dc.w	453,428,404,381,360,339,320,302,285,269,254,240
	dc.w	226,214,202,190,180,170,160,151,143,135,127,120
; Tuning -7
	dc.w	900,850,802,757,715,675,636,601,567,535,505,477
	dc.w	450,425,401,379,357,337,318,300,284,268,253,238
	dc.w	225,212,200,189,179,169,159,150,142,134,126,119
; Tuning -6
	dc.w	894,844,796,752,709,670,632,597,563,532,502,474
	dc.w	447,422,398,376,355,335,316,298,282,266,251,237
	dc.w	223,211,199,188,177,167,158,149,141,133,125,118
; Tuning -5
	dc.w	887,838,791,746,704,665,628,592,559,528,498,470
	dc.w	444,419,395,373,352,332,314,296,280,264,249,235
	dc.w	222,209,198,187,176,166,157,148,140,132,125,118
; Tuning -4
	dc.w	881,832,785,741,699,660,623,588,555,524,494,467
	dc.w	441,416,392,370,350,330,312,294,278,262,247,233
	dc.w	220,208,196,185,175,165,156,147,139,131,123,117
; Tuning -3
	dc.w	875,826,779,736,694,655,619,584,551,520,491,463
	dc.w	437,413,390,368,347,328,309,292,276,260,245,232
	dc.w	219,206,195,184,174,164,155,146,138,130,123,116
; Tuning -2
	dc.w	868,820,774,730,689,651,614,580,547,516,487,460
	dc.w	434,410,387,365,345,325,307,290,274,258,244,230
	dc.w	217,205,193,183,172,163,154,145,137,129,122,115
; Tuning -1
	dc.w	862,814,768,725,684,646,610,575,543,513,484,457
	dc.w	431,407,384,363,342,323,305,288,272,256,242,228
	dc.w	216,203,192,181,171,161,152,144,136,128,121,114

mt_chan1temp	blk.l	5
		dc.w	1
		blk.w	21
		dc.w	2
		blk.w	21
		dc.w	4
		blk.w	21
		dc.w	8
		blk.w	11

mt_SampleStarts	blk.l	31,0

mt_SongDataPtr	dc.l 0
mt_LWTPtr	dc.l 0
mt_oldirq	dc.l 0

mt_speed	dc.b 6
mt_counter	dc.b 0
mt_SongPos	dc.b 0
mt_PBreakPos	dc.b 0
mt_PosJumpFlag	dc.b 0
mt_PBreakFlag	dc.b 0
mt_LowMask	dc.b 0
mt_PattDelTime	dc.b 0
mt_PattDelTime2	dc.b 0,0
mt_PatternPos	dc.w 0
mt_DMACONtemp	dc.w 0
mt_volumes:	dc.w 0,0,0,0
mt_percent:	dc.w 128

;----------------------------------------------------------------------
;		*****************************************
;		*     QUICK 2D TRACE VECTORS (4 btpl)	*
;		*	-------------------------	*
;		*Coding on 28.03.1993 by KANE of SUSPECT*
;		*****************************************


l_odlot:	lea	$dff000,a0
		bsr.L	l_maketab
		waitblt
		move.l	#-1,$44(a0)
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#l_screen,$54(a0)
		move	#[635*64],$58(a0)
		lea	l_szescian(pc),a6
		bsr.L	l_vector
		bsr.L	l_vector
		bsr.L	l_vector
		bsr.L	l_vector
		bsr.L	l_vector
		bsr.L	l_vector
		bsr.L	l_vector
		bsr.L	l_vector
		waitblt
		move.l	#l_copper,$80(a0)

		move	#6,6(a6)
		move	#-2,8(a6)
l_control20:	raster
		bsr.s	l_vector
		addi	#2,4(a6)
		cmpi	#1024,4(a6)
		bpl.s	l_control21
		raster
		bsr.s	l_vector
		addi	#2,4(a6)
		cmpi	#1024,4(a6)
		bpl.s	l_control21
		raster
		bsr.s	l_vector
		tst	mt_percent
		beq.s	l_ccc2
		subi	#1,mt_percent
l_ccc2:		addi	#2,4(a6)
		cmpi	#1024,4(a6)
		bmi.s	l_control20

l_control21:
		move	#1024,4(a6)
		bsr.s	l_vector
		bsr.s	l_vector
		bsr.s	l_vector
		bsr.s	l_vector
		bsr.s	l_vector
		bsr.s	l_vector
		rts

;-------------------------------------------------------------------

l_vector:
		lea	l_scrtab(pc),a1
		lea	l_scr+24(pc),a2
		subq	#4,(a1)
		bpl.s	l_addok1
		move	#5*4,(a1)
l_addok1:	move	(a1),d0
		moveq	#3,d7
l_scrloop:	move.l	2(a1,d0.w),d1
		move	d1,6(a2)
		swap	d1
		move	d1,2(a2)
		lea	-8(a2),a2
		subq	#4,d0
		bpl.s	l_addok2
		moveq	#5*4,d0
l_addok2:	dbf	d7,l_scrloop
		move.l	2(a1,d0.w),l_scroff
		subq	#4,d0
		bpl.s	l_addok3
		moveq	#5*4,d0
l_addok3:	waitblt
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	2(a1,d0.w),$54(a0)
		move	#[l_heith*64]+[l_row/2],$58(a0)


		movem	6(a6),d0-d2
		add	d0,12(a6)
		andi	#$1fe,12(a6)
		add	d1,14(a6)
		andi	#$1fe,14(a6)
		add	d2,16(a6)
		andi	#$1fe,16(a6)

		lea	l_sinus,a1
		lea	l_sinus+$80,a2		;cosinus
		lea	l_matrix,a3
		move.l	20(a6),a4		;point tab
		move	18(a6),d7		;ilosc punktow-1
l_twodim:	bsr.L	l_rotate

		move	#1024,d3	;zooming - obiekt
		sub	d2,d3
		muls	d3,d0
		asr.l	#8,d0
		asr.l	#2,d0
		muls	d3,d1
		asr.l	#8,d1
		asr.l	#2,d1

		move	4(a6),d2	;dod.srodek z
		move	#1024,d3	;zooming - calosc
		sub	d2,d3
		muls	d3,d0
		asr.l	#8,d0
		asr.l	#2,d0
		muls	d3,d1
		asr.l	#8,d1
		asr.l	#2,d1

		add	(a6),d0		;dodaj srodek x,y
		add	2(a6),d1
		move	d0,(a3)+
		move	d1,(a3)+
		lea	4(a4),a4
		dbf	d7,l_twodim


		lea	l_matrix,a4
		move.l	24(a6),a3		;line tab
		move.l	l_scroff(pc),a5
		lea	l_ytable(pc),a2
l_rys1:		move	(a3)+,d4
		bmi.s	l_rys2
		move	(a3)+,d5
		movem	(a4,d4.w),d0/d1
		movem	(a4,d5.w),d2/d3
		bsr.L	l_drawline
		bra.s	l_rys1

l_rys2:

		waitblt					;fill right
		move	#l_row-4,$60(a0)
		move	#l_row-4,$62(a0)
		move	#l_row-4,$66(a0)
		move.l	#1,$44(a0)
		move.l	#$076a0000,$40(a0)
		move.l	a5,d4
		addi.l	#l_row-4,d4
		move.l	d4,$4c(a0)		;B
		addi.l	#l_row,d4
		move.l	d4,$48(a0)		;C
		move.l	d4,$54(a0)		;D
		move	#1,$74(a0)		;Adat
		move	#[[l_heith-1]*64]+2,$58(a0)
		waitblt					;fill
		move.l	#-1,$44(a0)
		move.l	#0,$64(a0)
		move.l	#$09f00012,$40(a0)
		move.l	a5,d4
		addi.l	#[l_heith*l_row]-2,d4
		move.l	d4,$54(a0)
		move.l	d4,$50(a0)
		move	#[l_heith*64]+[l_row/2],$58(a0)
		rts

;-------------------------------------------------------------------
;d3-kat, d0-x , d1-y

l_rotate:	move	16(a6),d3	;wez kat (*2)
		moveq	#0,d0
		move	(a4),d1		;zxy
		move	d0,d4
		move	d1,d5
		muls	(a1,d3.w),d0
		muls	(a2,d3.w),d5
		add.l	d5,d0		;x'=x*sin+y*cos
		add.l	d0,d0
		swap	d0
		muls	(a2,d3.w),d4	; x*cos
		muls	(a1,d3.w),d1	; y*sin
		sub.l	d4,d1		;y'=y*sin-x*cos
		add.l	d1,d1
		swap	d1

		move	14(a6),d3
		move	d1,d2
		move	2(a4),d1	;zyx
		move	d0,d4
		move	d1,d5
		muls	(a1,d3.w),d0
		muls	(a2,d3.w),d5
		add.l	d5,d0		;x'=x*sin+y*cos
		add.l	d0,d0
		swap	d0
		muls	(a2,d3.w),d4	; x*cos
		muls	(a1,d3.w),d1	; y*sin
		sub.l	d4,d1		;y'=y*sin-x*cos
		add.l	d1,d1
		swap	d1

		move	12(a6),d3
		exg	d0,d2		;xyz
		move	d0,d4
		move	d1,d5
		muls	(a1,d3.w),d0
		muls	(a2,d3.w),d5
		add.l	d5,d0		;x'=x*sin+y*cos
		add.l	d0,d0
		swap	d0
		muls	(a2,d3.w),d4	; x*cos
		muls	(a1,d3.w),d1	; y*sin
		sub.l	d4,d1		;y'=y*sin-x*cos
		add.l	d1,d1
		swap	d1
		rts

;-------------------------------------------------------------------
l_drawline:	cmp	d0,d2
		bpl.s	l_okx
		exg	d0,d2
		exg	d1,d3
l_okx:		cmpi	#[l_row*8]-2,d2
		bmi.s	l_left
		cmpi	#[l_row*8]-2,d0
		bpl.L	l_noline2
		move	#[l_row*8]-2,d4
		sub	d0,d4
		sub	d0,d2
		sub	d1,d3
		muls	d4,d3
		divs	d2,d3
		add	d1,d3
		move	#[l_row*8]-2,d2

		move	d3,d4
		addq	#1,d4
		bgt.s	l_right2
		moveq	#1,d4
l_right2:	cmpi	#l_heith,d4
		bpl.s	l_left
		add	d4,d4
		move	(a2,d4.w),d4		;mulu #l_row,d4
		waitblt
		eori	#1,l_row-2(a5,d4.w)

l_left:		tst	d0
		bpl.s	l_up
		tst	d2
		bmi.L	l_noline2
		sub	d2,d0
		sub	d3,d1
		muls	d2,d1
		divs	d0,d1
		neg	d1
		add	d3,d1
		moveq	#0,d0

l_up:		cmp	d1,d3
		bpl.s	l_oky
		exg	d0,d2
		exg	d1,d3
l_oky:		tst	d1
		bpl.s	l_down
		tst	d3
		bmi.L	l_noline2
		sub	d2,d0
		sub	d3,d1
		muls	d3,d0
		divs	d1,d0
		neg	d0
		add	d2,d0
		moveq	#0,d1

l_down:		cmpi	#l_heith-1,d3
		bmi.s	l_line
		cmpi	#l_heith-1,d1
		bpl.L	l_noline2
		move	#l_heith-1,d4
		sub	d1,d4
		sub	d0,d2
		sub	d1,d3
		muls	d4,d2
		divs	d3,d2
		add	d0,d2
		move	#l_heith-1,d3

l_line:		movem	d4-d7,-(sp)
		cmpi	d1,d3
		beq.L	l_noline
		bpl.s	l_lineok
		exg	d0,d2
		exg	d1,d3
l_lineok:	moveq	#3,d4
		addq	#1,d1
		move	d0,d5
		move	d1,d6
		subi	d3,d1
		bpl.s	l_dr1
		neg	d1
l_dr1:		subi	d2,d0
		bpl.s	l_dr2
		eori	#%01,d4
		neg	d0
l_dr2:		cmpi	d0,d1
		bmi.s	l_dr3
		exg	d0,d1
		eori	#%10,d4
l_dr3:		move	d5,d7
		and.l	#$f,d7
		ror	#4,d7
		ori	#$0b4a,d7
		swap	d7
		move.b	l_octant(pc,d4.w),d7
		lsl	#1,d1
		add	d6,d6
		move	l_ytable(pc,d6.w),d6
		and.l	#$fff0,d5
		lsr	#3,d5
		addi	d6,d5
		add.l	a5,d5
		waitblt
		move.l	#$ffff8000,$72(a0)
		move	#l_row,$60(a0)
		move	d1,$62(a0)
		move.l	d5,$48(a0)
		move.l	d5,$54(a0)
		subi	d0,d1
		bpl.s	l_dr4
		ori	#$40,d7
l_dr4:		move	d1,$52(a0)
		move.l	d7,$40(a0)
		subi	d0,d1
		move	d1,$64(a0)
		addq	#1,d0
		lsl	#6,d0
		addq	#2,d0
		move	d0,$58(a0)
l_noline:	movem	(sp)+,d4-d7
l_noline2:	rts

l_octant:	dc.b	3,8+3,16+3,20+3

;---------------------------------------------------------------------
l_heith=268			;screen dimensions
l_row=44

;---------------------------------------------------------------------
l_ytable:	ds.w	l_heith,0
;---------------------------------------------------------------------
l_maketab:	lea	l_ytable(pc),a1
		moveq	#0,d0
		move	#l_heith-1,d7
l_tabloop:	move	d0,(a1)+
		addi	#l_row,d0
		dbf	d7,l_tabloop
		rts

;---------------------------------------------------------------------
setcol:
		move	#16,d0
se_setcol:	lea	p_copper(pc),a1
		lea	p_coltab(pc),a2
		moveq	#15,d3				;color nr. - 1
se_scol1:	move	(a2),d1
		andi	#$f,d1
		move	2(a1),d2
		andi	#$f,d2
		cmpi	d1,d2
		beq.s	se_scol2
		addi	#1,2(a1)
se_scol2:	move	(a2),d1
		andi	#$f0,d1
		move	2(a1),d2
		andi	#$f0,d2
		cmpi	d1,d2
		beq.s	se_scol3
		addi	#$10,2(a1)
se_scol3:	move	(a2)+,d1
		andi	#$f00,d1
		move	2(a1),d2
		andi	#$f00,d2
		cmpi	d1,d2
		beq.s	se_scol4
		addi	#$100,2(a1)
se_scol4:	addi.l	#4,a1
		dbf	d3,se_scol1
		raster
		raster
		raster
		dbf	d0,se_setcol
		rts

fadcol:		move	#18,d0
fa_fadcol:	lea	p_copper(pc),a1
		moveq	#15,d3			;no. of colors - 1
fa_fad1:	move	2(a1),d1
		andi	#$f,d1
		beq.s	fa_fad2
		subi	#1,2(a1)
fa_fad2:	move	2(a1),d1
		andi	#$f0,d1
		beq.s	fa_fad3
		subi	#$10,2(a1)
fa_fad3:	move	2(a1),d1
		andi	#$f00,d1
		beq.s	fa_fad4
		subi	#$100,2(a1)
fa_fad4:	addi.l	#4,a1
		dbf	d3,fa_fad1
		raster
		raster
		raster
		dbf	d0,fa_fadcol
		rts

;---------------------------------------------------------------------
l_copper:
dc.l	$1820fc0,$1840f40,$1860f80,$1880f90,$18a0f40,$18c0f20,$18e0f50
dc.l	$1900fa0,$1920fb0,$1940f90,$1960f80,$1980f70,$19a0f40,$19c0f40,$19e0f00

dc.l	$1800002

dc.l	$960020
dc.l	$920030,$9400d8,$8e2071,$9032d1
dc.l	$1020000,$1040000
dc.w	$108,0,$10a,0
l_scr:
dc.w	$e0,l_screen/$10000,$e2,l_screen&$ffff
dc.w	$e4,l_screen+[l_heith*l_row]/$10000,$e6,l_screen+[l_heith*l_row]&$ffff
dc.w	$e8,l_screen+[2*l_heith*l_row]/$10000,$ea,l_screen+[2*l_heith*l_row]&$ffff
dc.w	$ec,l_screen+[3*l_heith*l_row]/$10000,$ee,l_screen+[3*l_heith*l_row]&$ffff

dc.l	$2601ff00,$01004300
dc.l	$ffdffffe
dc.l	$3201ff00,$01000300
dc.l	-2

;-------------------------------------------------------------------
l_screen=$6c000
l_scroff:	dc.l	l_screen+[l_heith*l_row]

l_scrtab:	dc.w	0	;pointer,:   clr, draw, scr 1-4
dc.l	l_screen,l_screen+[l_heith*l_row],l_screen+[2*l_heith*l_row]
dc.l	l_screen+[3*l_heith*l_row],l_screen+[4*l_heith*l_row]
dc.l	l_screen+[5*l_heith*l_row]

l_matrix=$6a000

;-------------------------------------------------------------------
l_szescian:
dc.w	176,128,200			;x,y,z mid
;dc.w	6,-2,2
dc.w	0,0,0
dc.w	128,128,128
dc.w	3				;ilosc punktow-1
dc.l	l_szdots,l_szline		;20, 24

l_szdots:
dc.w	-500,-500
dc.w	500,-500
dc.w	500,500
dc.w	-500,500

l_szline:
dc.w	0*4,1*4,1*4,2*4,2*4,3*4,3*4,0*4
dc.w	-1

;-------------------------------------------------------------------
p_blackcop:	dc.l	$1000300,$1800000,-2
p_coltab:
dc.w	$0000,$0fea,$0ed9,$0dc8,$0ca7,$0b86,$0a65,$0954
dc.w	$0943,$0833,$0722,$0612,$0511,$0401,$0300,$0200

p_copper:
dc.l	$1800000,$1820000,$1840000,$1860000,$1880000,$18a0000,$18c0000,$18e0000
dc.l	$1900000,$1920000,$1940000,$1960000,$1980000,$19a0000,$19c0000,$19e0000

dc.l	$920050,$9400b8,$8e2071,$9020d1
dc.l	$1020000,$1040000,$1080000,$10a0000

dc.w	$e0,pent/$10000,$e2,pent&$ffff
dc.w	$e4,[pent+[5264]]/$10000,$e6,[pent+[5264]]&$ffff
dc.w	$e8,[pent+[2*5264]]/$10000,$ea,[pent+[2*5264]]&$ffff
dc.w	$ec,[pent+[3*5264]]/$10000,$ee,[pent+[3*5264]]&$ffff

dc.l	$4e01ff00,$01004300
dc.l	$ffdffffe
dc.l	$0a01ff00,$01000300
dc.l	-2

pent=$7a000
;-------------------------------------------------------------------
endall:

pentagram:
l_sinus=pentagram+6276
RUN_4=l_sinus+640
p192_5=run_4+11700
GER_6=p192_5+7852
endpart=ger_6+33332
HIDDEN=endpart+53956

FLA_3=HIDDEN+5508		;not over $48000 !!!!!!
TUN_1=fla_3+3228
RGB_2=tun_1+17752



>extern "df0:.packed/logo_start.raw.pp",pentagram,-1
>extern "df0:.store/vec_sine(7fff).dat",l_sinus,-1

>extern "df0:.packed/run_4.dat.pp",run_4,-1
>extern "df0:.packed/192_5.dat.pp",p192_5,-1
>extern "df0:.packed/ger_6.dat.pp",ger_6,-1
>extern "df0:.packed/endpart.pp",endpart,-1
>extern "df0:.packed/hidden.pp",hidden,-1

>extern "df0:.packed/fla_3.dat.pp",fla_3,-1
>extern "df0:.packed/tun_1.dat.pp",tun_1,-1
>extern "df0:.packed/rgb_2.dat.pp",rgb_2,-1

if exe=0

music=$88000
mt_data=$b0000

else

music=rgb_2+8768
mt_data=$8000

endif

>extern	"df0:.store/mod.intoxicate.pro.pp",music,-1

end=music+105364

