;		*****************************************
;		*	 BOBS + RUNNING + PLASM		*
;		*	-------------------------	*
;		*Coding on 05.04.1993 by KANE of SUSPECT*
;		*****************************************

exe=0

org $48000
load $48000

waitblt: macro
	btst.b	#14,2(a0)
	bne.s	*-8
	endm
raster: macro
	cmp.b	#$ff,6(a0)
	bne.s	*-8
	cmp.b	#$ff,6(a0)
	beq.s	*-8
	endm

s:		lea	$dff000,a0
if exe=0
		move	#$4000,$9a(a0)
		move	#$83e0,$96(a0)
endif
		move	#$20,$96(a0)

		bsr.L	l_maketab
		waitblt
		move.l	#-1,$44(a0)
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#l_screen,$54(a0)
		move	#[250*64],$58(a0)
		waitblt
		move.l	#l_copper1,$80(a0)

		lea	l_kwadr(pc),a6
l_control1:	raster
		bsr	l_chgscr1
		bsr.L	l_vector
		lea	l_kwc(pc),a1
		moveq	#3,d7
		cmpi	#-10,(a1)
		bmi.s	l_con1_1
		move	#0,l_up1+2
		move	#0,l_up2+2
l_con1_1:	addi	#6,(a1)
		subi	#6,16(a1)
		lea	4(a1),a1
		dbf	d7,l_con1_1
		lea	l_kwc(pc),a1
		cmpi	#470,(a1)
		bmi.s	l_control1

;-------------------------------------------------------------------
		bsr	sb_shadebobs
		move.l	#l_copper3,$80(a0)
		bsr	l_setpic
		move	#10,d7
l_wto0:		raster
		dbf	d7,l_wto0
		waitblt
		move.l	#-1,$44(a0)
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#l_screen,$54(a0)
		move	#[l_heith*64]+[l_row],$58(a0)
		waitblt
		move.l	#l_copper2,$80(a0)

		lea	l_copper2(pc),a3
		lea	l_coltab2(pc),a4
		moveq	#15,d7			;color nr. - 1
		bsr.L	se_set

		move	#70,d7
l_wto:		raster
		dbf	d7,l_wto


		lea	l_plasm,a6
		move	#580,(a6)
		move	#120,2(a6)
		move	#-300,4(a6)
l_control22:	raster
		bsr	l_chgscr2
		bsr.L	l_vector

		addi	#7,4(a6)
		subi	#3,(a6)
		cmpi	#1024,4(a6)
		bmi.s	l_control22
		move	#1024,4(a6)
		raster
		bsr	l_chgscr2
		bsr.L	l_vector
		raster
		bsr	l_chgscr2
		bsr.L	l_vector

		move	#$3300,l_bul2+6
		waitblt
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#c_plane,$54(a0)
		move	#[c_heith*2*64]+[c_row],$58(a0)
		waitblt
		bsr.L	c_maketab
		bsr.L	o_maketab
		bsr.L	c_makecircle

		lea	l_copper2(pc),a2
		moveq	#15,d7			;no. of colors - 1
		bsr.L	fa_fade

		bsr	c_plasm

if exe=0
		move	#$c000,$9a(a0)
		move	#$8020,$96(a0)
endif
		rts

;-------------------------------------------------------------------
se_set:		move	#15,d0
se_setcol:	move	d7,d3
		lea	(a3),a1
		lea	(a4),a2
		raster
		raster
		raster
		raster
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
		dbf	d0,se_setcol
		rts

fa_fade:	move	#18,d0
fa_fadcol:	move	d7,d3
		lea	(a2),a1
		raster
		raster
		raster
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
		dbf	d0,fa_fadcol
		rts

;-------------------------------------------------------------------
l_chgscr1:
		move.l	scron(pc),d0
		move.l	scroff(pc),d1
		move.l	d0,scroff
		move.l	d1,scron
		lea	l_scr(pc),a1
		move	d1,6(a1)
		swap	d1
		move	d1,2(a1)
		waitblt
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	d0,$54(a0)
		move	#[l_heith*64]+[l_row/2],$58(a0)
		rts
;-------------------------------------------------------------------
l_chgscr2:
		move.l	scron(pc),d0
		move.l	scroff(pc),d1
		move.l	d0,scroff
		move.l	d1,scron
		lea	l_scr2(pc),a1
		move	d1,6(a1)
		swap	d1
		move	d1,2(a1)
		waitblt
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	d0,$54(a0)
		move	#[l_heith*64]+[l_row/2],$58(a0)
		rts

;-------------------------------------------------------------------
l_vector:
		move.l	22(a6),d0		;coord tab
		lea	(a6,d0.w),a1
		lea	l_matrix,a2
		move	18(a6),d7		;ilosc punktow-1
l_zoom:		move	(a1)+,d0
		move	(a1)+,d1
		move	4(a6),d2
		move	#1024,d3		;zooming
		sub	d2,d3
		muls	d3,d0
		asr.l	#8,d0
		asr.l	#2,d0
		muls	d3,d1
		asr.l	#8,d1
		asr.l	#2,d1

		add	(a6),d0			;dodaj srodek x,y
		add	2(a6),d1
		move	d0,(a2)+
		move	d1,(a2)+
		dbf	d7,l_zoom

		move.l	26(a6),d0		;line tab
		lea	(a6,d0.w),a1
		lea	l_ytable(pc),a2
		lea	l_matrix,a3
		move.l	scroff(pc),a5
		move	20(a6),d7
l_draw1:	move	(a1)+,d6
l_draw2:	move	(a1)+,d4
		move	(a1)+,d5
		movem	(a3,d4.w),d0/d1
		movem	(a3,d5.w),d2/d3
		bsr.L	l_drawline
		dbf	d6,l_draw2
		dbf	d7,l_draw1

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
l_heith=276			;screen dimensions
l_row=46

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
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
;            SHADE BOBS -CODE BY PILLAR OF SUSPECT '93
;          SPECIAL VERSION PREPARED TO NEW SUSPECT DEMO.
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
VBLANK:	MACRO
	CMPI.B	#$FF,$06(A6)
	BNE.S	*-8
	CMPI.B	#$FF,$06(A6)
	BEQ.S	*-8
	ENDM

sb_shadebobs:
	MOVEM.L	D0-D7/A0-A6,-(A7)
	LEA	$DFF000,A6
	BSR	SB_INIT
	MOVE.L	#SB_COPPER,$80(A6)

	LEA	SB_SHOWST,A0
	MOVE.L	(A0)+,SB_SHIFTS
	MOVE.L	(A0)+,SB_TIMER
	MOVE.L	(A0)+,SB_NUMBER
	MOVE.L	(A0),A2
	BSR	SB_SETCOL
SB_LOOP:VBLANK
	BSR	SB_MAIN
	SUBQ.L	#1,SB_TIMER
	BNE.S	SB_LOOP
	BSR	SB_FADE

	BSR	SB_INIT
	LEA	SB_SHOWND,A0
	MOVE.L	(A0)+,SB_SHIFTS
	MOVE.L	(A0)+,SB_TIMER
	MOVE.L	(A0)+,SB_NUMBER
	MOVE.L	(A0),A2
	BSR	SB_SETCOL
SB_LOOP2:
	VBLANK
	BSR	SB_MAIN
	SUBQ.L	#1,SB_TIMER
	BNE.S	SB_LOOP2
	BSR	SB_FADE

	BSR	SB_INIT
	LEA	SB_SHOWRD,A0
	MOVE.L	(A0)+,SB_SHIFTS
	MOVE.L	(A0)+,SB_TIMER
	MOVE.L	(A0)+,SB_NUMBER
	MOVE.L	(A0),A2
	BSR.S	SB_SETCOL
SB_LOOP3:
	VBLANK
	BSR	SB_MAIN
	SUBQ.L	#1,SB_TIMER
	BNE.S	SB_LOOP3
	BSR	SB_FADE

	MOVEM.L	(A7)+,D0-D7/A0-A6
	RTS
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
SB_INIT:LEA	$70000,A0
	LEA	$7F000,A1
	MOVEQ	#0,D0
SB_IN0:	MOVE.L	D0,(A0)+
	CMPA.L	A1,A0
	BNE.S	SB_IN0
	LEA	SB_CRX1,A0
	MOVEQ	#0,D0
	MOVE.L	D0,(A0)+
	MOVE.L	D0,(A0)+
	RTS
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
SB_SETCOL:
	MOVE.L	A2,A0
	MOVEQ	#14,D0
SB_SCOL0:
	LEA	SB_COPPER,A1
	MOVEQ	#31,D3
SB_SCOL1:
	MOVE	(A2),D1
	AND	#$F,D1
	MOVE	2(A1),D2
	AND	#$F,D2
	CMP	D1,D2
	BEQ.S	SB_SCOL2
	ADDQ	#1,2(A1)
SB_SCOL2:
	MOVE	(A2),D1
	AND	#$F0,D1
	MOVE	2(A1),D2
	AND	#$F0,D2
	CMP	D1,D2
	BEQ.S	SB_SCOL3
	ADDI	#$10,2(A1)
SB_SCOL3:
	MOVE	(A2)+,D1
	AND	#$F00,D1
	MOVE	2(A1),D2
	AND	#$F00,D2
	CMP	D1,D2
	BEQ.S	SB_SCOL4
	ADDI	#$100,2(A1)
SB_SCOL4:
	ADDQ	#4,A1
	DBF	D3,SB_SCOL1
	MOVEQ	#1,D3
SB_WAIT4:
	VBLANK
	MOVEM.L	D0-D7/A0-A6,-(A7)
	BSR.S	SB_MAIN
	MOVEM.L	(A7)+,D0-D7/A0-A6
	MOVE.L	A0,A2
	DBF	D3,SB_WAIT4
	DBF	D0,SB_SCOL0
	RTS

;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
SB_FADE:MOVEQ	#15,D0
SB_FADCOL:
	LEA	SB_COPPER,A1
	MOVEQ	#31,D3
SB_FD1:	MOVE	2(A1),D1
	AND	#$F,D1
	BEQ.S	SB_FD2
	SUBQ	#1,2(A1)
SB_FD2:	MOVE	2(A1),D1
	AND	#$F0,D1
	BEQ.S	SB_FD3
	SUB	#$10,2(A1)
SB_FD3:	MOVE	2(A1),D1
	AND	#$F00,D1
	BEQ.S	SB_FD4
	SUB	#$100,2(A1)
SB_FD4:	ADDQ	#4,A1
	DBF	D3,SB_FD1
	MOVEQ	#1,D3
SB_WAIT5:
	VBLANK
	MOVEM.L	D0-D7/A0-A6,-(A7)
	BSR.S	SB_MAIN
	MOVEM.L	(A7)+,D0-D7/A0-A6
	DBF	D3,SB_WAIT5
	DBF	D0,SB_FADCOL
	RTS
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
SB_MAIN:LEA	SB_SINX,A0
	LEA	SB_SINY,A1
	MOVE.L	SB_SHIFTS,A2
	MOVE.L	SB_NUMBER,D7
	MOVE	SB_CRX1,D3
	MOVE	SB_CRY1,D4
	MOVE	SB_CRX2,D5
	MOVE	SB_CRY2,D6
	MOVE	#$1FF,D2
	
SB_MN1:	MOVE	0(A0,D3.W),D0
	MOVE	0(A1,D4.W),D1
	ADD	0(A0,D5.W),D0
	ADD	0(A1,D6.W),D1
	LSR	#1,D0
	LSR	#1,D1	

	MOVEM.L	D2-D7/A0-A2,-(A7)
	LEA	SB_BOB,A0
	MOVEQ	#$28,D6
	MULU	D6,D1
	MOVE	D0,D2
	LSR	#4,D2
	ADD	D2,D2
	ADD	D2,D1
	MOVE	D0,D2
	AND	#%1111,D2
	LEA	$70000,A1
	LEA	$72800,A2
	LEA	$75000,A3
	LEA	$77800,A4
	LEA	$7A000,A5
	ADDA	D1,A1
	ADDA	D1,A2
	ADDA	D1,A3
	ADDA	D1,A4
	ADDA	D1,A5

	MOVEQ	#15,D7
SB_MN0:	MOVE.L	(A0)+,D0
	LSR.L	D2,D0
	MOVE.L	(A1),D1
	EOR.L	D0,(A1)	
	AND.L	D1,D0
	MOVE.L	(A2),D1
	EOR.L	D0,(A2)
	AND.L	D1,D0
	MOVE.L	(A3),D1
	EOR.L	D0,(A3)
	AND.L	D1,D0
	MOVE.L	(A4),D1
	EOR.L	D0,(A4)
	AND.L	D1,D0
	MOVE.L	(A5),D1
	EOR.L	D0,(A5)
	AND.L	D1,D0
	ADDA	D6,A1
	ADDA	D6,A2
	ADDA	D6,A3
	ADDA	D6,A4
	ADDA	D6,A5
	DBF	D7,SB_MN0
	MOVEM.L	(A7)+,D2-D7/A0-A2

	ADD	00(A2),D3
	ADD	02(A2),D4
	ADD	04(A2),D5
	ADD	06(A2),D6
	AND	D2,D3
	AND	D2,D4
	AND	D2,D5
	AND	D2,D6
	DBF	D7,SB_MN1

	MOVE	08(A2),D0
	ADD	D0,SB_CRX1
	MOVE	10(A2),D0
	ADD	D0,SB_CRY1
	MOVE	12(A2),D0
	ADD	D0,SB_CRX2
	MOVE	14(A2),D0
	ADD	D0,SB_CRY2
	AND	D2,SB_CRX1
	AND	D2,SB_CRY1
	AND	D2,SB_CRX2
	AND	D2,SB_CRY2
	RTS
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
SB_CRX1:	DC.W	0
SB_CRY1:	DC.W	0
SB_CRX2:	DC.W	0
SB_CRY2:	DC.W	0
SB_SHIFTS:	DC.L	0
SB_NUMBER:	DC.L	0
SB_TIMER:	DC.L	0
SB_SHOWST:	DC.L	SB_SHOW1,300,7,SB_COLORS1
SB_SHOWND:	DC.L	SB_SHOW2,300,9,SB_COLORS2
SB_SHOWRD:	DC.L	SB_SHOW3,360,19,SB_COLORS3
SB_SHOW1:	DC.W 20,-12,34,-22,2,-8,-6,10
SB_SHOW2:	DC.W 12,-10,8,-22,4,16,-12,-20
SB_SHOW3:	DC.W 30,-52,74,-62,40,28,-26,30
SB_BOB:	DC.L	$0FE00000,$1FF80000,$3FFC0000,$7FFE0000
	DC.L	$7FFE0000,$FFFF0000,$FFFF0000,$FFFF0000
	DC.L	$FFFF0000,$FFFF0000,$FFFF0000,$7FFE0000
	DC.L	$7FFE0000,$3FFC0000,$1FF80000,$07E00000
SB_COPPER:
	DC.W $0180,$0000,$0182,$0000,$0184,$0000,$0186,$0000,$0188,$0000
	DC.W $018A,$0000,$018C,$0000,$018E,$0000,$0190,$0000,$0192,$0000
	DC.W $0194,$0000,$0196,$0000,$0198,$0000,$019A,$0000,$019C,$0000
	DC.W $019E,$0000,$01A0,$0000,$01A2,$0000,$01A4,$0000,$01A6,$0000
	DC.W $01A8,$0000,$01AA,$0000,$01AC,$0000,$01AE,$0000,$01B0,$0000
	DC.W $01B2,$0000,$01B4,$0000,$01B6,$0000,$01B8,$0000,$01BA,$0000
	DC.W $01BC,$0000,$01BE,$0000
	DC.W $0102,$0000,$0104,$0000,$0108,$0000,$010A,$0000
	DC.W $008E,$29C1,$0090,$29D1,$0092,$0038,$0094,$00D0
	DC.W $00E0,$0007,$00E2,$0000,$00E4,$0007,$00E6,$2800
	DC.W $00E8,$0007,$00EA,$5000,$00EC,$0007,$00EE,$7800
	DC.W $00F0,$0007,$00F2,$A000,$0100,$5300,$FFFF,$FFFE

SB_SINX:DC.W	$00A0,$00A3,$00A7,$00AA,$00AE,$00B1,$00B5,$00B8
	DC.W	$00BC,$00BF,$00C3,$00C6,$00C9,$00CD,$00D0,$00D4
	DC.W	$00D7,$00DA,$00DD,$00E0,$00E4,$00E7,$00EA,$00ED
	DC.W	$00F0,$00F3,$00F6,$00F8,$00FB,$00FE,$0101,$0103
	DC.W	$0106,$0108,$010B,$010D,$010F,$0111,$0113,$0116
	DC.W	$0118,$0119,$011B,$011D,$011F,$0120,$0122,$0123
	DC.W	$0125,$0126,$0127,$0128,$012A,$012A,$012B,$012C
	DC.W	$012D,$012E,$012E,$012F,$012F,$012F,$012F,$012F
	DC.W	$012F,$012F,$012F,$012F,$012F,$012E,$012E,$012D
	DC.W	$012D,$012C,$012B,$012A,$0129,$0128,$0127,$0125
	DC.W	$0124,$0123,$0121,$0120,$011E,$011C,$011A,$0119
	DC.W	$0117,$0115,$0112,$0110,$010E,$010C,$0109,$0107
	DC.W	$0104,$0102,$00FF,$00FD,$00FA,$00F7,$00F4,$00F1
	DC.W	$00EE,$00EB,$00E8,$00E5,$00E2,$00DF,$00DC,$00D8
	DC.W	$00D5,$00D2,$00CF,$00CB,$00C8,$00C4,$00C1,$00BD
	DC.W	$00BA,$00B6,$00B3,$00AF,$00AC,$00A8,$00A5,$00A1
	DC.W	$009F,$009B,$0098,$0094,$0091,$008D,$008A,$0086
	DC.W	$0083,$007F,$007C,$0078,$0075,$0071,$006E,$006B
	DC.W	$0068,$0064,$0061,$005E,$005B,$0058,$0055,$0052
	DC.W	$004F,$004C,$0049,$0046,$0043,$0041,$003E,$003C
	DC.W	$0039,$0037,$0034,$0032,$0030,$002E,$002B,$0029
	DC.W	$0027,$0026,$0024,$0022,$0020,$001F,$001D,$001C
	DC.W	$001B,$0019,$0018,$0017,$0016,$0015,$0014,$0013
	DC.W	$0013,$0012,$0012,$0011,$0011,$0011,$0011,$0011
	DC.W	$0011,$0011,$0011,$0011,$0011,$0012,$0012,$0013
	DC.W	$0014,$0015,$0016,$0016,$0018,$0019,$001A,$001B
	DC.W	$001D,$001E,$0020,$0021,$0023,$0025,$0027,$0028
	DC.W	$002A,$002D,$002F,$0031,$0033,$0035,$0038,$003A
	DC.W	$003D,$003F,$0042,$0045,$0048,$004A,$004D,$0050
	DC.W	$0053,$0056,$0059,$005C,$0060,$0063,$0066,$0069
	DC.W	$006C,$0070,$0073,$0077,$007A,$007D,$0081,$0084
	DC.W	$0088,$008B,$008F,$0092,$0096,$0099,$009D,$00A0

SB_SINY:DC.W	$0080,$0082,$0085,$0088,$008B,$008D,$0090,$0093
	DC.W	$0095,$0098,$009B,$009D,$00A0,$00A3,$00A5,$00A8
	DC.W	$00AB,$00AD,$00B0,$00B2,$00B4,$00B7,$00B9,$00BC
	DC.W	$00BE,$00C0,$00C2,$00C5,$00C7,$00C9,$00CB,$00CD
	DC.W	$00CF,$00D1,$00D3,$00D5,$00D6,$00D8,$00DA,$00DB
	DC.W	$00DD,$00DE,$00E0,$00E1,$00E2,$00E4,$00E5,$00E6
	DC.W	$00E7,$00E8,$00E9,$00EA,$00EB,$00EC,$00EC,$00ED
	DC.W	$00ED,$00EE,$00EE,$00EF,$00EF,$00EF,$00EF,$00EF
	DC.W	$00EF,$00EF,$00EF,$00EF,$00EF,$00EF,$00EE,$00EE
	DC.W	$00ED,$00ED,$00EC,$00EB,$00EA,$00EA,$00E9,$00E8
	DC.W	$00E7,$00E6,$00E4,$00E3,$00E2,$00E0,$00DF,$00DE
	DC.W	$00DC,$00DB,$00D9,$00D7,$00D5,$00D4,$00D2,$00D0
	DC.W	$00CE,$00CC,$00CA,$00C8,$00C6,$00C4,$00C1,$00BF
	DC.W	$00BD,$00BA,$00B8,$00B6,$00B3,$00B1,$00AE,$00AC
	DC.W	$00A9,$00A7,$00A4,$00A1,$009F,$009C,$0099,$0097
	DC.W	$0094,$0091,$008F,$008C,$0089,$0086,$0084,$0081
	DC.W	$007F,$007C,$007A,$0077,$0074,$0071,$006F,$006C
	DC.W	$0069,$0067,$0064,$0061,$005F,$005C,$0059,$0057
	DC.W	$0054,$0052,$004F,$004D,$004A,$0048,$0046,$0043
	DC.W	$0041,$003F,$003C,$003A,$0038,$0036,$0034,$0032
	DC.W	$0030,$002E,$002C,$002B,$0029,$0027,$0025,$0024
	DC.W	$0022,$0021,$0020,$001E,$001D,$001C,$001A,$0019
	DC.W	$0018,$0017,$0016,$0016,$0015,$0014,$0013,$0013
	DC.W	$0012,$0012,$0011,$0011,$0011,$0011,$0011,$0011
	DC.W	$0011,$0011,$0011,$0011,$0011,$0012,$0012,$0013
	DC.W	$0013,$0014,$0014,$0015,$0016,$0017,$0018,$0019
	DC.W	$001A,$001B,$001C,$001E,$001F,$0020,$0022,$0023
	DC.W	$0025,$0026,$0028,$002A,$002B,$002D,$002F,$0031
	DC.W	$0033,$0035,$0037,$0039,$003B,$003E,$0040,$0042
	DC.W	$0044,$0047,$0049,$004C,$004E,$0050,$0053,$0055
	DC.W	$0058,$005B,$005D,$0060,$0063,$0065,$0068,$006B
	DC.W	$006D,$0070,$0073,$0075,$0078,$007B,$007E,$0080

SB_COLORS1:
	DC.W	$0303,$0404,$0505,$0605,$0706
	DC.W	$0806,$0907,$0A07,$0B07,$0C07
	DC.W	$0D07,$0E07,$0F06,$0F04,$0F02
	DC.W	$0F10,$0F30,$0F50,$0F80,$0FA0
	DC.W	$0FC0,$0FF0,$0EA0,$0D70,$0C40
	DC.W	$0B10,$0901,$0803,$0704,$0605
	DC.W	$0505,$0406
SB_COLORS2:
	DC.W	$0330,$0440,$0540,$0550,$0650
	DC.W	$0760,$0860,$0970,$0970,$0A80
	DC.W	$0B80,$0C80,$0D80,$0D80,$0E90
	DC.W	$0F90,$0FA0,$0FA1,$0FB1,$0EB1
	DC.W	$0EC2,$0EC2,$0ED2,$0ED3,$0EE3
	DC.W	$0CB2,$0B92,$0971,$0751,$0630
	DC.W	$0520,$0410
SB_COLORS3:
	DC.W	$0020,$0030,$0040,$0050,$0161
	DC.W	$0272,$0373,$0484,$0594,$06A4
	DC.W	$07B4,$08C4,$09D4,$0AD3,$0BD2
	DC.W	$0CD1,$0DD0,$0DC1,$0DB2,$0DA3
	DC.W	$0D94,$0C85,$0B76,$0A67,$0958
	DC.W	$0849,$073A,$0629,$0518,$0407
	DC.W	$0306,$0205
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
l_setpic:	lea	l_picl,a1
		lea	l_pic,a2
		bsr.s	l_copy
		lea	l_picl+$2800,a1
		lea	l_pic+[l_heith*l_row],a2
		bsr.s	l_copy
		lea	l_picl+$5000,a1
		lea	l_pic+[2*l_heith*l_row],a2

l_copy:
		lea	2(a1),a1
		moveq	#0,d0
		move	#[l_heith]-1,d7
l_c2:		moveq	#9,d6
l_c3:		move.l	(a1)+,(a2)+
		dbf	d6,l_c3
		move.l	d0,(a2)+
		move	d0,(a2)+
		dbf	d7,l_c2
		rts

;---------------------------------------------------------------------
;---------------------------------------------------------------------

l_copper1:
dc.w	$182,$830
l_up1:
dc.l	$1800003

dc.l	$920028,$9400d8,$8e2071,$9035e1
dc.l	$1020044,$1040000
dc.w	$108,0,$10a,0
l_scr:
dc.l	$e00007,$e28000

dc.l	$2001ff00,$01001300
dc.l	$e001ff00
l_up2:
dc.l	$1800005
dc.l	$e001ff00,$1820940
dc.l	$ffdffffe
dc.l	$3401ff00,$01000300
dc.l	$1820000,$1800000
dc.l	-2
;---------------------------------------------------------------------
;---------------------------------------------------------------------
l_coltab2:
dc.w	$210,$bbb,$222,$444
dc.w	$666,$111,$ddd,$fff
dc.w	$66f,$99b,$112,$224
dc.w	$446,$112,$bbd,$ddf

l_copper2:
dc.l	$1800000,$1820000,$1840000,$1860000
dc.l	$1880000,$18a0000,$18c0000,$18e0000

dc.l	$1900000,$1920000,$1940000,$1960000
dc.l	$1980000,$19a0000,$19c0000,$19e0000

dc.l	$920038,$9400d0,$8e2071,$9035e1
dc.l	$1020000,$1040000
dc.w	$108,6,$10a,6
l_scr2:
dc.w	$ec,l_screen/$10000,$ee,l_screen&$ffff
dc.w	$e0,l_pic/$10000,$e2,l_pic&$ffff
dc.w	$e4,l_pic+[l_heith*l_row]/$10000,$e6,l_pic+[l_heith*l_row]&$ffff
dc.w	$e8,l_pic+[2*l_heith*l_row]/$10000,$ea,l_pic+[2*l_heith*l_row]&$ffff

l_bul2:
dc.l	$2801ff00,$01004300
dc.l	$ffdffffe
dc.l	$2801ff00,$01000300
dc.l	-2

l_copper3:
dc.l	$1000300,$1800000,-2

;-------------------------------------------------------------------
l_screen=$78000
scron:		dc.l	l_screen
scroff:		dc.l	l_screen+[l_heith*l_row]

l_matrix=$72000
l_pic=$60000
;-------------------------------------------------------------------
l_kwadr:
dc.w	0,0,0
dc.w	0,0,0
dc.w	128,128,128
dc.w	7,1
dc.l	l_kwc-l_kwadr,l_kwp-l_kwadr
l_kwc:
dc.w	-510,192,-10,192,-10,-20,-510,-10
dc.w	362,394,862,394,862,192,362,192
l_kwp:
dc.w	3,0*4,1*4,1*4,2*4,2*4,3*4,3*4,0*4
dc.w	3,4*4,5*4,5*4,6*4,6*4,7*4,7*4,4*4

;-------------------------------------------------------------------
;		*****************************************
;		*	TECHNO EFFECT + PLASM	 	*
;		*	--------------------------	*
;		* Coding 02.03.1992  by KANE of SUSPECT	*
;		*****************************************

c_plasm:
		lea	o_szescian(pc),a6
		bsr.L	o_vector
		bsr.L	o_vector
		move.l	#c_copper,$80(a0)


		moveq	#16,d7
c_control0:	raster
		bsr.L	c_ruchaj
		raster
		bsr.L	c_ruchaj
		raster
		bsr.L	c_ruchaj
		raster
		bsr.L	c_ruchaj
		bsr.s	so_setcol
		dbf	d7,c_control0

c_control1:	raster
		bsr.L	c_ruchaj
		subi	#1,c_licz
		bne.s	c_control1

		lea	o_szescian(pc),a6
o_control2:	raster
		bsr.L	o_vector
		bsr.L	c_ruchaj
		addi	#4,4(a6)
		cmpi	#1024,4(a6)
		bmi.s	o_control2
		move	#1024,4(a6)
		bsr.L	o_vector
		bsr.L	o_vector

		move.l	#c_copper0,$80(a0)
		rts

c_licz:		dc.w	600
;---------------------------------------------------------------------

so_setcol:	lea	c_copper,a1
		lea	c_coltab,a2
		moveq	#7,d3				;color nr. - 1
so_scol1:	move	(a2),d1
		andi	#$f,d1
		move	2(a1),d2
		andi	#$f,d2
		cmpi	d1,d2
		beq.s	so_scol2
		addi	#1,2(a1)
so_scol2:	move	(a2),d1
		andi	#$f0,d1
		move	2(a1),d2
		andi	#$f0,d2
		cmpi	d1,d2
		beq.s	so_scol3
		addi	#$10,2(a1)
so_scol3:	move	(a2)+,d1
		andi	#$f00,d1
		move	2(a1),d2
		andi	#$f00,d2
		cmpi	d1,d2
		beq.s	so_scol4
		addi	#$100,2(a1)
so_scol4:	addi.l	#4,a1
		dbf	d3,so_scol1
		rts

;---------------------------------------------------------------------
c_ruchaj:
		lea	c_sine(pc),a1
		lea	6(a1),a2
		lea	c_ytable(pc),a3
		lea	c_scr(pc),a4
		addi	#2,(a1)
		andi	#$fe,(a1)
		addi	#4,2(a1)
		cmpi	#255,2(a1)
		bmi.s	c_ruchu0
		addi	#6,2(a1)
c_ruchu0:	andi	#$fe,2(a1)
		addi	#-2,4(a1)
		bpl.s	c_ruchu2
		addi	#-10,4(a1)
c_ruchu2:	andi	#$fe,4(a1)
		moveq	#0,d0			;steady picture y
		move	(a1),d0
		move	(a2,d0.w),d0
		add	d0,d0
		move	(a3,d0.w),d0
		add.l	#c_plane+c_row-14,d0
		move	d0,6(a4)
		swap	d0
		move	d0,2(a4)

		moveq	#0,d0			;movable picture y
		move	4(a1),d0
		move	(a2,d0.w),d0
		add	d0,d0
		move	(a3,d0.w),d0
		add.l	#c_plane+c_row-2,d0

		move	2(a1),d1		;movable x
		move	(a2,d1.w),d1
		muls	#350,d1
		divs	#272,d1
		move	d1,d2
		lsr	#4,d1
		add	d1,d1
		sub	d1,d0

		andi	#$f,d2
		lsl	#4,d2
		move	d2,18(a4)
		move	d0,14(a4)
		swap	d0
		move	d0,10(a4)
		rts

;---------------------------------------------------------------------
c_makecircle:	lea	c_plane,a5
		move	#c_row*8,d5
		move	#c_heith-2,d6		;center x,y
		moveq	#4,d3			;r
c_makecloop:	move	d3,-(sp)
		bsr.s	c_drawit
		move	(sp)+,d3
		addq	#3,d3			;try 3 here !!!
		cmpi	#440,d3
		bmi.s	c_makecloop

		waitblt				;fill it
 		move.l	#0,$64(a0)
		move.l	#$09f00012,$40(a0)
		move.l	#c_plane+[c_heith*c_row*2]-2,d0
		move.l	d0,$54(a0)
		move.l	d0,$50(a0)
		move	#[c_heith*64]+[c_row],$58(a0)
		waitblt
		move	#-4*c_row,$64(a0)	;copy lower half
		move.l	#$09f00000,$40(a0)
		move.l	#c_plane+[c_heith*c_row*2]-[4*c_row],$50(a0)
		move.l	#c_plane+[c_heith*c_row*2]-[2*c_row],$54(a0)
		move	#[c_heith*64]+[c_row],$58(a0)
		waitblt
		rts

;---------------------------------------------------------------------
c_drawit:	lea	c_ytable(pc),a3
		moveq	#0,d1			;x
		move	d3,d0			;y

		moveq	#3,d2
		add	d3,d3
		sub	d3,d2			;d
		bsr.L	c_plot0
		exg	d0,d1

circle_loop:	cmp	d1,d0
		beq.L	c_nomore
		bpl	c_nomore0
		tst	d2
		bpl.s	circle_ru
		move	d0,d3
		add	d3,d3
		add	d3,d3
		addq	#6,d3
		add	d3,d2
		addq	#1,d0			;x=x+1
		bra.s	c_draw1
circle_ru:	move	d0,d3
		sub	d1,d3
		add	d3,d3
		add	d3,d3
		addq	#8,d3
		add	d3,d2
		addq	#1,d0			;x=x+1
		subq	#1,d1			;y=y-1

c_draw:		bsr.s	c_plot
		exg	d0,d1
		bsr.s	c_plot
		exg	d0,d1
		bra.s	circle_loop

c_draw1:	exg	d0,d1
		bsr.s	c_plot
		exg	d0,d1
		bra.s	circle_loop

c_plot:		move	d0,d3			;fix middles
		move	d1,d4
		add	d6,d4			;y
		add	d5,d3			;x

		bpl.s	c_chkx1		;check X left
		moveq	#0,d3
c_chkx1:	cmpi	#c_borX,d3	;X right
		bmi.s	c_chky11
		move	#c_borX-1,d3
c_chky11:	tst	d4		;Y up
		bpl.s	c_chky21
		move	#c_borY,d4
c_chky21:	cmpi	#c_borY,d4	;Y down
		bmi.s	c_nochk1
		move	#c_borY,d4
c_nochk1:
		move	d3,d7
		lsr	#3,d7
		not	d3			;x
		add	d4,d4
		move	(a3,d4.w),d4		;y
		add	d7,d4
		bchg	d3,(a5,d4.w)		;down-right
		move	d6,d4
		sub	d1,d4

		bpl.s	c_chky22
		move	#c_borY,d4
c_chky22:	cmpi	#c_borY,d4
		bmi.s	c_nochk2
		move	#c_borY,d4
c_nochk2:
		add	d4,d4
		move	(a3,d4.w),d4
		add	d4,d7
		bchg	d3,(a5,d7.w)		;up-right
		move	d5,d3
		sub	d0,d3

		bpl.s	c_chkx3
		moveq	#0,d3
c_chkx3:	cmpi	#c_borX,d3
		bmi.s	c_chky13
		move	#c_borX-1,d3
c_chky13:
		move	d3,d7
		lsr	#3,d7
		not	d3
		add	d7,d4
		bchg	d3,(a5,d4.w)		;up-left
		move	d6,d4
		add	d1,d4

		bpl.s	c_chky24
		move	#c_borY,d4
c_chky24:	cmpi	#c_borY,d4
		bmi.s	c_nochk4
		move	#c_borY,d4
c_nochk4:
		add	d4,d4
		move	(a3,d4.w),d4
		add	d4,d7
		bchg	d3,(a5,d7.w)		;up-right
		rts

;----------------------------------------------------

c_nomore0:	bsr	c_nomore
		subq	#1,d0
		addq	#1,d1
c_nomore:	move	d0,d3
		move	d1,d4
		add	d6,d4			;center
		add	d5,d3

		bpl.s	c_chkx5
		moveq	#0,d3
c_chkx5:	cmpi	#c_borX,d3
		bmi.s	c_chky15
		move	#c_borX-1,d3
c_chky15:	tst	d4
		bpl.s	c_chky25
		move	#c_borY,d4
c_chky25:	cmpi	#c_borY,d4
		bmi.s	c_nochk5
		move	#c_borY,d4
c_nochk5:
		move	d3,d7
		lsr	#3,d7
		not	d3			;x
		add	d4,d4
		move	(a3,d4.w),d4		;y
		add	d7,d4
		bchg	d3,(a5,d4.w)		;down-right
		move	d6,d4
		sub	d1,d4

		bpl.s	c_chky26
		move	#c_borY,d4
c_chky26:	cmpi	#c_borY,d4
		bmi.s	c_nochk6
		move	#c_borY,d4
c_nochk6:
		add	d4,d4
		move	(a3,d4.w),d4
		add	d4,d7
		bchg	d3,(a5,d7.w)		;up-right
		move	d5,d3
		sub	d0,d3

		bpl.s	c_chkx7
		moveq	#0,d3
c_chkx7:	cmpi	#c_borX,d3
		bmi.s	c_nochk7
		move	#c_borX-1,d3
c_nochk7:
		move	d3,d7
		lsr	#3,d7
		not	d3
		add	d7,d4
		bchg	d3,(a5,d4.w)		;up-left
		move	d6,d4
		add	d1,d4

		bpl.s	c_chky28
		move	#c_borY,d4
c_chky28:	cmpi	#c_borY,d4
		bmi.s	c_nochk8
		move	#c_borY,d4
c_nochk8:
		add	d4,d4
		move	(a3,d4.w),d4
		add	d4,d7
		bchg	d3,(a5,d7.w)		;up-right
		rts

;----------------------------------------------------

c_plot0:	move	d0,d3			;fix x left/right
		moveq	#0,d4
		add	d6,d4
		add	d5,d3

		bpl.s	c_chkx9
		moveq	#0,d3
c_chkx9:	cmpi	#c_borX,d3
		bmi.s	c_chky19
		move	#c_borX-1,d3
c_chky19:	tst	d4
		bpl.s	c_chky29
		move	#c_borY,d4
c_chky29:	cmpi	#c_borY,d4
		bmi.s	c_nochk9
		move	#c_borY,d4
c_nochk9:
		move	d3,d7
		lsr	#3,d7
		not	d3			;x
		add	d4,d4
		move	(a3,d4.w),d4		;y
		add	d4,d7
		bchg	d3,(a5,d7.w)
		move	d5,d3
		sub	d0,d3

		bpl.s	c_chkx10
		moveq	#0,d3
c_chkx10:	cmpi	#c_borX,d3
		bmi.s	c_nochk10
		move	#c_borX-1,d3
c_nochk10:
		move	d3,d7
		lsr	#3,d7
		not	d3
		add	d4,d7
		bchg	d3,(a5,d7.w)		;down-right
		rts

;---------------------------------------------------------------------
c_maketab:	lea	c_ytable(pc),a1
		moveq	#0,d0
		move	#c_heith,d7
c_tabloop:	move	d0,(a1)+
		addi	#c_row*2,d0
		dbf	d7,c_tabloop
		rts
;---------------------------------------------------------------------
c_heith=272
c_row=44
;---------------------------------------------------------------------
c_ytable:	ds.w	[c_heith]+1,0
;---------------------------------------------------------------------
c_plane=$70000

;---------------------------------------------------------------------
c_borX=c_row*8*2
c_borY=[c_heith]-1

c_sine:	dc.w	64,30,198		;y1, x2,y2
	dc.w	$0088,$008E,$0095,$009C,$00A2,$00A9,$00AF,$00B6
	dc.w	$00BC,$00C2,$00C8,$00CE,$00D4,$00D9,$00DE,$00E3
	dc.w	$00E8,$00ED,$00F1,$00F5,$00F9,$00FD,$0100,$0103
	dc.w	$0106,$0108,$010A,$010C,$010D,$010E,$010F,$010F
	dc.w	$010F,$010F,$010F,$010E,$010D,$010B,$0109,$0107
	dc.w	$0104,$0102,$00FE,$00FB,$00F7,$00F3,$00EF,$00EB
	dc.w	$00E6,$00E1,$00DC,$00D6,$00D1,$00CB,$00C5,$00BF
	dc.w	$00B9,$00B2,$00AC,$00A6,$009F,$0098,$0092,$008B
	dc.w	$0085,$007E,$0078,$0071,$006A,$0064,$005E,$0057
	dc.w	$0051,$004B,$0045,$003F,$003A,$0034,$002F,$002A
	dc.w	$0025,$0021,$001D,$0019,$0015,$0012,$000E,$000C
	dc.w	$0009,$0007,$0005,$0003,$0002,$0001,$0001,$0001,1
	dc.w	$0001,$0001,$0002,$0003,$0004,$0006,$0008,$000A
	dc.w	$000D,$0010,$0013,$0017,$001B,$001F,$0023,$0028
	dc.w	$002D,$0032,$0037,$003C,$0042,$0048,$004E,$0054
	dc.w	$005A,$0061,$0067,$006E,$0074,$007B,$0082

;---------------------------------------------------------------------
;		*****************************************
;		*	QUICK 2D VECTORS (1 btpl)	*
;		*	-------------------------	*
;		*Coding on 28.03.1993 by KANE of SUSPECT*
;		*****************************************

o_vectors:
		lea	o_szescian(pc),a6
o_control:	raster
		bsr.s	o_vector
		addi	#10,4(a6)
		cmpi	#1024,4(a6)
		bmi.s	o_control
		move	#1024,4(a6)
		bsr.s	o_vector

		rts

;-------------------------------------------------------------------

o_vector:	move.l	c_scron(pc),d0
		move.l	c_scroff(pc),d1
		move.l	d0,c_scroff
		move.l	d1,c_scron
		lea	o_scr(pc),a1
		move	d1,6(a1)
		swap	d1
		move	d1,2(a1)
		waitblt
		move	#o_row-2,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	d0,$54(a0)
		move	#[o_heith*64]+[[o_row+2]/2],$58(a0)


		movem	6(a6),d0-d2
		add	d0,12(a6)
		andi	#$1fe,12(a6)
		add	d1,14(a6)
		andi	#$1fe,14(a6)
		add	d2,16(a6)
		andi	#$1fe,16(a6)

		lea	o_sinus,a1
		lea	o_sinus+$80,a2		;cosinus
		lea	o_matrix,a3
		move.l	20(a6),a4		;point tab
		move	18(a6),d7		;ilosc punktow-1
o_twodim:	bsr.L	o_rotate

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
		dbf	d7,o_twodim


		lea	o_matrix,a4
		move.l	24(a6),a3		;line tab
		move.l	c_scroff(pc),a5
		lea	o_ytable(pc),a2
o_rys1:		move	(a3)+,d4
		bmi.s	o_rys2
		move	(a3)+,d5
		movem	(a4,d4.w),d0/d1
		movem	(a4,d5.w),d2/d3
		bsr.L	o_drawline
		bra.s	o_rys1

o_rys2:

		waitblt					;fill right
		move	#o_row-4+o_row,$60(a0)
		move	#o_row-4+o_row,$62(a0)
		move	#o_row-4+o_row,$66(a0)
		move.l	#1,$44(a0)
		move.l	#$076a0000,$40(a0)
		move.l	a5,d4
		addi.l	#[o_row]-2,d4
		move.l	d4,$4c(a0)		;B
		addi.l	#2*o_row,d4
		move.l	d4,$48(a0)		;C
		move.l	d4,$54(a0)		;D
		move	#1,$74(a0)		;Adat
		move	#[[o_heith-1]*64]+2,$58(a0)
		waitblt					;fill
		move.l	#-1,$44(a0)
		move	#o_row,$64(a0)
		move	#o_row,$66(a0)
		move.l	#$09f00012,$40(a0)
		move.l	a5,d4
		addi.l	#[o_heith*o_row*2]-o_row,d4
		move.l	d4,$54(a0)
		move.l	d4,$50(a0)
		move	#[o_heith*64]+[o_row/2],$58(a0)
		rts

;-------------------------------------------------------------------
;d3-kat, d0-x , d1-y

o_rotate:	move	16(a6),d3	;wez kat (*2)
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
o_drawline:	cmp	d0,d2
		bpl.s	o_okx
		exg	d0,d2
		exg	d1,d3
;o_okx:		cmpi	#[o_row*8]-2,d2
o_okx:		cmpi	#[[o_row+2]*8]-2,d2
		bmi.s	o_left
;		cmpi	#[o_row*8]-2,d0
		cmpi	#[[o_row+2]*8]-2,d0
		bpl.L	o_noline2
;		move	#[o_row*8]-2,d4
		move	#[[o_row+2]*8]-2,d4
		sub	d0,d4
		sub	d0,d2
		sub	d1,d3
		muls	d4,d3
		divs	d2,d3
		add	d1,d3
;		move	#[o_row*8]-2,d2
		move	#[[o_row+2]*8]-2,d2

		move	d3,d4
		addq	#1,d4
		bgt.s	o_right2
		moveq	#1,d4
o_right2:	cmpi	#o_heith,d4
		bpl.s	o_left
		add	d4,d4
		move	(a2,d4.w),d4		;mulu #o_row,d4
		waitblt
;		eori	#1,o_row-2(a5,d4.w)
		eori	#1,o_row(a5,d4.w)

o_left:		tst	d0
		bpl.s	o_up
		tst	d2
		bmi.L	o_noline2
		sub	d2,d0
		sub	d3,d1
		muls	d2,d1
		divs	d0,d1
		neg	d1
		add	d3,d1
		moveq	#0,d0

o_up:		cmp	d1,d3
		bpl.s	o_oky
		exg	d0,d2
		exg	d1,d3
o_oky:		tst	d1
		bpl.s	o_down
		tst	d3
		bmi.L	o_noline2
		sub	d2,d0
		sub	d3,d1
		muls	d3,d0
		divs	d1,d0
		neg	d0
		add	d2,d0
		moveq	#0,d1

o_down:		cmpi	#o_heith-1,d3
		bmi.s	o_line
		cmpi	#o_heith-1,d1
		bpl.L	o_noline2
		move	#o_heith-1,d4
		sub	d1,d4
		sub	d0,d2
		sub	d1,d3
		muls	d4,d2
		divs	d3,d2
		add	d0,d2
		move	#o_heith-1,d3

o_line:		movem	d4-d7,-(sp)
		cmpi	d1,d3
		beq.L	o_noline
		bpl.s	o_lineok
		exg	d0,d2
		exg	d1,d3
o_lineok:	moveq	#3,d4
		addq	#1,d1
		move	d0,d5
		move	d1,d6
		subi	d3,d1
		bpl.s	o_dr1
		neg	d1
o_dr1:		subi	d2,d0
		bpl.s	o_dr2
		eori	#%01,d4
		neg	d0
o_dr2:		cmpi	d0,d1
		bmi.s	o_dr3
		exg	d0,d1
		eori	#%10,d4
o_dr3:		move	d5,d7
		and.l	#$f,d7
		ror	#4,d7
		ori	#$0b4a,d7
		swap	d7
		move.b	o_octant(pc,d4.w),d7
		lsl	#1,d1
		add	d6,d6
		move	o_ytable(pc,d6.w),d6
		and.l	#$fff0,d5
		lsr	#3,d5
		addi	d6,d5
		add.l	a5,d5
		waitblt
		move.l	#$ffff8000,$72(a0)
		move	#o_row*2,$60(a0)
		move	d1,$62(a0)
		move.l	d5,$48(a0)
		move.l	d5,$54(a0)
		subi	d0,d1
		bpl.s	o_dr4
		ori	#$40,d7
o_dr4:		move	d1,$52(a0)
		move.l	d7,$40(a0)
		subi	d0,d1
		move	d1,$64(a0)
		addq	#1,d0
		lsl	#6,d0
		addq	#2,d0
		move	d0,$58(a0)
o_noline:	movem	(sp)+,d4-d7
o_noline2:	rts

o_octant:	dc.b	3,8+3,16+3,20+3

;---------------------------------------------------------------------
o_heith=272			;screen dimensions
o_row=44

;---------------------------------------------------------------------
o_ytable:	ds.w	o_heith,0
;---------------------------------------------------------------------
o_maketab:	lea	o_ytable(pc),a1
		moveq	#0,d0
		move	#o_heith-1,d7
o_tabloop:	move	d0,(a1)+
		addi	#o_row*2,d0
		dbf	d7,o_tabloop
		rts

;---------------------------------------------------------------------
;---------------------------------------------------------------------
c_coltab:
dc.w	$101,$101,$101,$101
dc.w	$0a0,$440,$060,$888

c_copper:
dc.l	$1800000,$1820000,$1840000,$1860000
dc.l	$1880000,$18a0000,$18c0000,$18e0000

dc.l	$920028,$9400d8,$8e2471,$9034c9
dc.l	$1040000
dc.w	$108,c_row-2,$10a,c_row-2
c_scr:
dc.l	$e00007,$e20000
dc.l	$e40007,$e60000
dc.l	$1020000
o_scr:
dc.l	$e80007,$ea0000

dc.l	$2401ff00,$01003300
dc.l	$ffdffffe
dc.l	$3401ff00,$01000300
dc.l	-2

c_copper0:
dc.l	$1000300,$1800101,-2

;---------------------------------------------------------------------
o_screen=$61000
c_scron:		dc.l	o_screen
c_scroff:		dc.l	o_screen+[2*o_heith*o_row]

o_matrix=$60000
;-------------------------------------------------------------------
o_szescian:
dc.w	184,128,-700			;x,y,z mid
dc.w	4,0,0
dc.w	128,128,128
dc.w	11				;ilosc punktow-1
dc.l	o_szdots,o_szline		;20, 24

o_szdots:
dc.w	-480,-480
dc.w	480,-480
dc.w	480,480
dc.w	-480,480
dc.w	-330,-330
dc.w	330,-330
dc.w	330,330
dc.w	-330,330
dc.w	-130,-130
dc.w	130,-130
dc.w	130,130
dc.w	-130,130
o_szline:
dc.w	0*4,1*4,1*4,2*4,2*4,3*4,3*4,0*4
dc.w	4*4,5*4,5*4,6*4,6*4,7*4,7*4,4*4
dc.w	8*4,9*4,9*4,10*4,10*4,11*4,11*4,8*4
dc.w	-1

;-------------------------------------------------------------------
l_plasm:			;426
o_sinus=l_plasm+426		;640
l_picl=l_plasm+426+640		;30720

>extern	"df0:.Vector anims/plasm.vec",l_plasm,-1
>extern	"df0:.store/vec_sine(7fff).dat",o_sinus,-1
>extern	"df0:.store/running.raw",l_picl,-1

end=l_picl+30720
