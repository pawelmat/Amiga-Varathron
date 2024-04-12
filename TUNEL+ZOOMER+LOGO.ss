;		*****************************************
;		*	VECTOR TUNNEL+ZOOMER+LOGO	*
;		*	-------------------------	*
;		*Coding on 05.04.1993 by KANE of SUSPECT*
;		*****************************************

org $48000
load $48000

exe=0

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
if	exe=0
		move	#$4000,$9a(a0)
endif
		move	#$83e0,$96(a0)
		move	#$20,$96(a0)

		bsr.s	v_tunnel
		bsr	l_zoomer
		move.l	#lo_copper4,$80(a0)
		move	#20,d7
lo_wait2:	raster
		dbf	d7,lo_wait2
		bsr	lo_rozchlap

if	exe=0
		move	#$c000,$9a(a0)
endif
		move	#$8020,$96(a0)
		rts

;-------------------------------------------------------------------

v_tunnel:
		waitblt
		move.l	#-1,$44(a0)
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#v_screen,$54(a0)
		move	#[3*v_heith*64]+[v_row],$58(a0)
		bsr.L	v_maketab
		waitblt
		move.l	#v_copper,$80(a0)

		lea	v_szescian(pc),a6
v_control1:	raster
		bsr.s	v_vector
		addi	#20,4(a6)
		cmpi	#350,4(a6)
		bne.s	v_con2
		subi	#100,4(a6)
v_con2:		subi	#1,v_counter
		bne.s	v_control1
v_control2:	raster
		bsr.s	v_vector
		addi	#20,4(a6)
		cmpi	#1850,4(a6)
		bne.s	v_control2

		rts

v_counter:	dc.w	150
;-------------------------------------------------------------------

v_vector:
		lea	v_scrtab(pc),a1
		lea	v_scr+24(pc),a2
		subq	#4,(a1)
		bpl.s	v_addok1
		move	#5*4,(a1)
v_addok1:	move	(a1),d0
		moveq	#3,d7
v_scrloop:	move.l	2(a1,d0.w),d1
		move	d1,6(a2)
		swap	d1
		move	d1,2(a2)
		lea	-8(a2),a2
		subq	#4,d0
		bpl.s	v_addok2
		moveq	#5*4,d0
v_addok2:	dbf	d7,v_scrloop
		move.l	2(a1,d0.w),v_scroff
		subq	#4,d0
		bpl.s	v_addok3
		moveq	#5*4,d0
v_addok3:	waitblt
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	2(a1,d0.w),$54(a0)
		move	#[v_heith*64]+[v_row/2],$58(a0)


		move	6(a6),d0
		add	d0,8(a6)
		andi	#$1fe,8(a6)
		lea	v_sinus,a1
		lea	v_sinus+$80,a2		;cosinus
		lea	v_matrix,a3
		move.l	10(a6),a4		;point tab
		move	8(a6),d3	;wez kat (*2)
v_rotate:
		move	(a4),d0
		cmpi	#$aaaa,d0
		beq.s	v_draw
		move	2(a4),d1	;xy
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

		move	4(a4),d2
		add	4(a6),d2	;dod.srodek z
		cmpi	#1024,d2
		bmi.s	v_rot1
		move	#1023,d2
v_rot1:
		move	#1024,d4	;zooming - calosc
		sub	d2,d4
		muls	d4,d0
		asr.l	#8,d0
		asr.l	#2,d0
		muls	d4,d1
		asr.l	#8,d1
		asr.l	#2,d1

		add	(a6),d0		;dodaj srodek x,y
		add	2(a6),d1
		move	d0,(a3)+
		move	d1,(a3)+
		lea	6(a4),a4
		bra.s	v_rotate

v_draw:		lea	v_matrix,a4
		move.l	14(a6),a3		;line tab
		move.l	v_scroff(pc),a5
		lea	v_ytable(pc),a2
v_rys1:		move	(a3)+,d4
		bmi.s	v_rys2
		move	(a3)+,d5
		movem	(a4,d4.w),d0/d1
		movem	(a4,d5.w),d2/d3
		bsr.L	v_drawline
		bra.s	v_rys1

v_rys2:
		waitblt					;fill right
		move	#v_row-4,$60(a0)
		move	#v_row-4,$62(a0)
		move	#v_row-4,$66(a0)
		move.l	#1,$44(a0)
		move.l	#$076a0000,$40(a0)
		move.l	a5,d4
		addi.l	#v_row-4,d4
		move.l	d4,$4c(a0)		;B
		addi.l	#v_row,d4
		move.l	d4,$48(a0)		;C
		move.l	d4,$54(a0)		;D
		move	#1,$74(a0)		;Adat
		move	#[[v_heith-1]*64]+2,$58(a0)
		waitblt					;fill
		move.l	#-1,$44(a0)
		move.l	#0,$64(a0)
		move.l	#$09f00012,$40(a0)
		move.l	a5,d4
		addi.l	#[v_heith*v_row]-2,d4
		move.l	d4,$54(a0)
		move.l	d4,$50(a0)
		move	#[v_heith*64]+[v_row/2],$58(a0)
		rts

;-------------------------------------------------------------------
v_drawline:	cmp	d0,d2
		bpl.s	v_okx
		exg	d0,d2
		exg	d1,d3
v_okx:		cmpi	#[v_row*8]-2,d2
		bmi.s	v_left
		cmpi	#[v_row*8]-2,d0
		bpl.L	v_noline2
		move	#[v_row*8]-2,d4
		sub	d0,d4
		sub	d0,d2
		sub	d1,d3
		muls	d4,d3
		divs	d2,d3
		add	d1,d3
		move	#[v_row*8]-2,d2

		move	d3,d4
		addq	#1,d4
		bgt.s	v_right2
		moveq	#1,d4
v_right2:	cmpi	#v_heith,d4
		bpl.s	v_left
		add	d4,d4
		move	(a2,d4.w),d4		;mulu #v_row,d4
		waitblt
		eori	#1,v_row-2(a5,d4.w)

v_left:		tst	d0
		bpl.s	v_up
		tst	d2
		bmi.L	v_noline2
		sub	d2,d0
		sub	d3,d1
		muls	d2,d1
		divs	d0,d1
		neg	d1
		add	d3,d1
		moveq	#0,d0

v_up:		cmp	d1,d3
		bpl.s	v_oky
		exg	d0,d2
		exg	d1,d3
v_oky:		tst	d1
		bpl.s	v_down
		tst	d3
		bmi.L	v_noline2
		sub	d2,d0
		sub	d3,d1
		muls	d3,d0
		divs	d1,d0
		neg	d0
		add	d2,d0
		moveq	#0,d1

v_down:		cmpi	#v_heith-1,d3
		bmi.s	v_line
		cmpi	#v_heith-1,d1
		bpl.L	v_noline2
		move	#v_heith-1,d4
		sub	d1,d4
		sub	d0,d2
		sub	d1,d3
		muls	d4,d2
		divs	d3,d2
		add	d0,d2
		move	#v_heith-1,d3

v_line:		movem	d4-d7,-(sp)
		cmpi	d1,d3
		beq.L	v_noline
		bpl.s	v_lineok
		exg	d0,d2
		exg	d1,d3
v_lineok:	moveq	#3,d4
		addq	#1,d1
		move	d0,d5
		move	d1,d6
		subi	d3,d1
		bpl.s	v_dr1
		neg	d1
v_dr1:		subi	d2,d0
		bpl.s	v_dr2
		eori	#%01,d4
		neg	d0
v_dr2:		cmpi	d0,d1
		bmi.s	v_dr3
		exg	d0,d1
		eori	#%10,d4
v_dr3:		move	d5,d7
		and.l	#$f,d7
		ror	#4,d7
		ori	#$0b4a,d7
		swap	d7
		move.b	v_octant(pc,d4.w),d7
		lsl	#1,d1
		add	d6,d6
		move	v_ytable(pc,d6.w),d6
		and.l	#$fff0,d5
		lsr	#3,d5
		addi	d6,d5
		add.l	a5,d5
		waitblt
		move.l	#$ffff8000,$72(a0)
		move	#v_row,$60(a0)
		move	d1,$62(a0)
		move.l	d5,$48(a0)
		move.l	d5,$54(a0)
		subi	d0,d1
		bpl.s	v_dr4
		ori	#$40,d7
v_dr4:		move	d1,$52(a0)
		move.l	d7,$40(a0)
		subi	d0,d1
		move	d1,$64(a0)
		addq	#1,d0
		lsl	#6,d0
		addq	#2,d0
		move	d0,$58(a0)
v_noline:	movem	(sp)+,d4-d7
v_noline2:	rts

v_octant:	dc.b	3,8+3,16+3,20+3

;---------------------------------------------------------------------
v_heith=256			;screen dimensions
v_row=44

;---------------------------------------------------------------------
v_ytable:	ds.w	v_heith,0
;---------------------------------------------------------------------
v_maketab:	lea	v_ytable(pc),a1
		moveq	#0,d0
		move	#v_heith-1,d7
v_tabloop:	move	d0,(a1)+
		addi	#v_row,d0
		dbf	d7,v_tabloop
		rts

;---------------------------------------------------------------------
v_copper:

dc.w	$0182,$0a00 ,$0184,$0811 ,$0186,$0b30
dc.w	$0188,$0900 ,$018a,$0b40 ,$018c,$0b40 ,$018e,$0d73
dc.w	$0190,$0940 ,$0192,$0b60 ,$0194,$0b60 ,$0196,$0d90
dc.w	$0198,$0b80 ,$019a,$0c50 ,$019c,$0db4 ,$019e,$0ff3

dc.l	$1800000

dc.l	$920030,$9400d8,$8e2c71,$902cd1
dc.l	$1020000,$1040000
dc.w	$108,0,$10a,0
v_scr:
dc.w	$e0,v_screen/$10000,$e2,v_screen&$ffff
dc.w	$e4,v_screen+[v_heith*v_row]/$10000,$e6,v_screen+[v_heith*v_row]&$ffff
dc.w	$e8,v_screen+[2*v_heith*v_row]/$10000,$ea,v_screen+[2*v_heith*v_row]&$ffff
dc.w	$ec,v_screen+[3*v_heith*v_row]/$10000,$ee,v_screen+[3*v_heith*v_row]&$ffff

dc.l	$2c01ff00,$01004300
dc.l	$ffdffffe
dc.l	$2c01ff00,$01000300
dc.l	-2

;-------------------------------------------------------------------
v_screen=$6c000
v_scroff:	dc.l	v_screen+[v_heith*v_row]

v_scrtab:	dc.w	0	;pointer,:   clr, draw, scr 1-4
dc.l	v_screen,v_screen+[v_heith*v_row],v_screen+[2*v_heith*v_row]
dc.l	v_screen+[3*v_heith*v_row],v_screen+[4*v_heith*v_row]
dc.l	v_screen+[5*v_heith*v_row]

v_matrix=$6a000

;-------------------------------------------------------------------
v_szescian:
dc.w	176,128,-990			;x,y,z mid
dc.w	12				;6
dc.w	128				;angle	;8
dc.l	v_szdots,v_szline			;10, 14

v_szdots:
dc.w	-500,-260,0
dc.w	500,-260,0
dc.w	0,500,0

dc.w	-500,-260,100
dc.w	500,-260,100
dc.w	0,500,100

dc.w	-500,-260,200
dc.w	500,-260,200
dc.w	0,500,200

dc.w	-500,-260,300
dc.w	500,-260,300
dc.w	0,500,300

dc.w	-500,-260,400
dc.w	500,-260,400
dc.w	0,500,400

dc.w	-500,-260,500
dc.w	500,-260,500
dc.w	0,500,500

dc.w	-500,-260,600
dc.w	500,-260,600
dc.w	0,500,600

dc.w	-500,-260,700
dc.w	500,-260,700
dc.w	0,500,700

dc.w	-500,-260,800
dc.w	500,-260,800
dc.w	0,500,800

dc.w	$aaaa

v_szline:
dc.w	0*4,1*4,1*4,3*4,0*4,4*4
dc.w	0*4,2*4,2*4,3*4,0*4,5*4
dc.w	1*4,2*4,2*4,4*4,1*4,5*4

dc.w	3*4,8*4,4*4,6*4,5*4,6*4
dc.w	3*4,7*4,4*4,8*4,5*4,7*4

dc.w	6*4,11*4,7*4,9*4,8*4,9*4
dc.w	6*4,10*4,7*4,11*4,8*4,10*4

dc.w	9*4,14*4,10*4,12*4,11*4,12*4
dc.w	9*4,13*4,10*4,14*4,11*4,13*4

dc.w	12*4,17*4,13*4,15*4,14*4,15*4
dc.w	12*4,16*4,13*4,17*4,14*4,16*4

dc.w	15*4,20*4,16*4,18*4,17*4,18*4
dc.w	15*4,19*4,16*4,20*4,17*4,19*4

dc.w	18*4,23*4,19*4,21*4,20*4,21*4
dc.w	18*4,22*4,19*4,23*4,20*4,22*4

dc.w	21*4,26*4,22*4,24*4,23*4,24*4
dc.w	21*4,25*4,22*4,26*4,23*4,25*4

dc.w	24*4,25*4,25*4,26*4,26*4,24*4
dc.w	-1
;-------------------------------------------------------------------
;-------------------------------------------------------------------
;		*****************************************
;		*	2D VECTOR ZOOMER (1 btpl)	*
;		*	-------------------------	*
;		*Coding on 02.04.1993 by KANE of SUSPECT*
;		*****************************************

zoom:	macro
	lea	l_table+[\1],a6
	move	#\2,4(a6)
	bsr	l_out
	endm


l_zoomer:	bsr.L	l_maketab
		waitblt
		move.l	#-1,$44(a0)
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#l_screen,$54(a0)
		move	#[l_heith*64]+[l_row],$58(a0)
		waitblt
		move.l	#l_copper,$80(a0)

		zoom	0,-13000
		zoom	920,-13000
		zoom	920+396,-13000
		zoom	920+396+306,-13000
		zoom	920+396+306+510,-13000
		zoom	920+396+306+510+700,-13000

;l_control2:	raster
;		bsr.L	l_vector
;		btst.b	#6,$bfe001
;		bne.s	l_control2
		rts

;-------------------------------------------------------------------
l_out:		raster
		bsr.L	l_vector
		raster
	cmp.b	#$ff,6(a0)
	bne.s	*-8
	cmp.b	#$34,6(a0)
	bne.s	*-8
		eori	#col,l_cols+2
		eori	#col,l_cols+6
		bsr.L	l_vector
		lea	l_distsine(pc),a1
		moveq	#79,d7
l_out2:		raster
		move	(a1)+,d0
		subi	#11950,d0
		move	d0,4(a6)
		movem.l	a1/d7,-(sp)
		bsr.L	l_vector
		movem.l	(sp)+,a1/d7
		dbf	d7,l_out2
		rts

l_distsine:
	dc.w	$FC00,$FD13,$FE27,$FF3A,$004D,$0160,$0272,$0384
	dc.w	$0494,$05A4,$06B3,$07C1,$08CE,$09D9,$0AE3,$0BEC
	dc.w	$0CF3,$0DF8,$0EFC,$0FFD,$10FD,$11FB,$12F6,$13EF
	dc.w	$14E6,$15DA,$16CC,$17BB,$18A7,$1990,$1A77,$1B5A
	dc.w	$1C3B,$1D18,$1DF2,$1EC8,$1F9B,$206B,$2137,$21FF
	dc.w	$22C3,$2384,$2441,$24F9,$25AE,$265E,$270A,$27B2
	dc.w	$2856,$28F5,$2990,$2A26,$2AB8,$2B45,$2BCD,$2C51
	dc.w	$2CCF,$2D49,$2DBE,$2E2E,$2E9A,$2F00,$2F61,$2FBC
	dc.w	$3013,$3065,$30B1,$30F8,$313A,$3177,$31AE,$31E0
	dc.w	$320D,$3234,$3256,$3272,$3289,$329B,$32A7,$32AE
;-------------------------------------------------------------------

l_vector:	move.l	scron(pc),d0
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
col=$fff

l_copper:
dc.l	$920028,$9400d8,$8e2071,$9035e1
dc.l	$1020044,$1040000
dc.w	$108,0,$10a,0
l_scr:
dc.l	$e00007,$e20000

dc.l	$2001ff00,$01001300
dc.l	$2101ff00
l_cols:
dc.w	$182,col
dc.l	$1800000

dc.l	$ffdffffe
dc.l	$3401ff00,$01000300
dc.l	$1820000,$1800000
dc.l	-2

;-------------------------------------------------------------------
l_screen=$78000
scron:		dc.l	l_screen
scroff:		dc.l	l_screen+[l_heith*l_row]

l_matrix=$72000

;-------------------------------------------------------------------
;		*****************************************
;		*	    PICTURE BLA BLA !!!		*
;		*    ------------------------------	*
;		*Coded on 04.04.1993  by KANE of SUSPECT*
;		*****************************************

lo_rozchlap:
		waitblt
		move.l	#-1,$44(a0)
		move.l	#0,$64(a0)
		move.l	#$1000000,$40(a0)
		move.l	#lo_plane,$54(a0)
		move	#[608*64],$58(a0)
		waitblt
		move.l	#$9f00000,$40(a0)		;copy picture
		move.l	#lo_pic1,$50(a0)
		move.l	#lo_plane+[272*lo_row],$54(a0)
		move	#[74*64]+15,$58(a0)
		waitblt
		move.l	#lo_pic1+[74*30],$50(a0)
		move.l	#lo_plane+[lo_heith2*lo_row]+[272*30],$54(a0)
		move	#[74*64]+15,$58(a0)
		waitblt
		move.l	#lo_pic1+[2*74*30],$50(a0)
		move.l	#lo_plane+[2*lo_heith2*lo_row]+[272*30],$54(a0)
		move	#[74*64]+15,$58(a0)
		waitblt
		move.l	#lo_pic1+[3*74*30],$50(a0)
		move.l	#lo_plane+[3*lo_heith2*lo_row]+[272*30],$54(a0)
		move	#[74*64]+15,$58(a0)

		lea	lo_modtab(pc),a2
		bsr.L	lo_makecop
		move.l	#lo_copper,$80(a0)

lo_control:	raster
		bsr.L	lo_setadr
		subq	#1,lo_offset
		cmpi	#190,lo_offset
		bne.s	lo_control

		lea	lo_modtab2(pc),a2
		bsr.L	lo_makecop
lo_control2:	raster
		bsr.L	lo_setadr
		addq	#1,lo_offset
		cmpi	#380,lo_offset
		bne.s	lo_control2

;---------------------------------------------------------------------
		move.l	#lo_copper2,$80(a0)

		moveq	#14,d7
lo_con3:	raster
		lea	lo_copper2,a1
		moveq	#6,d6
lo_c3_1:	addi	#$111,2(a1)
		lea	4(a1),a1
		dbf	d6,lo_c3_1
		dbf	d7,lo_con3

		move	#16,d7
lo_set:		raster
se_setcol:	lea	lo_copper2(pc),a1
		lea	lo_coltab2(pc),a2
		moveq	#6,d3
se_scol1:	move	(a2),d1
		andi	#$f,d1
		move	2(a1),d2
		andi	#$f,d2
		cmpi	d1,d2
		beq.s	se_scol2
		subi	#1,2(a1)
se_scol2:	move	(a2),d1
		andi	#$f0,d1
		move	2(a1),d2
		andi	#$f0,d2
		cmpi	d1,d2
		beq.s	se_scol3
		subi	#$10,2(a1)
se_scol3:	move	(a2)+,d1
		andi	#$f00,d1
		move	2(a1),d2
		andi	#$f00,d2
		cmpi	d1,d2
		beq.s	se_scol4
		subi	#$100,2(a1)
se_scol4:	addi.l	#4,a1
		dbf	d3,se_scol1
		dbf	d7,lo_set

		move	#180,d7
lo_waait:	raster
		dbf	d7,lo_waait

		moveq	#16,d7
lo_fad:		raster
		raster
fa_fadcol:	lea	lo_copper2(pc),a1
		moveq	#7,d3			;no. of colors - 1
fa_fad1:	move	2(a1),d1
		andi	#$f,d1
		cmpi	#$f,d1
		beq.s	fa_fad2
		addi	#1,2(a1)
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
		dbf	d7,lo_fad

		move.l	#lo_copper3,$80(a0)
		rts

;---------------------------------------------------------------------
lo_setadr:
		move	lo_offset(pc),d0
		mulu	#lo_row,d0
		add.l	#lo_plane,d0
		lea	lo_scr(pc),a1
		moveq	#3,d1
lo_adrloop:	move	d0,6(a1)
		swap	d0
		move	d0,2(a1)
		swap	d0
		addi.l	#lo_heith2*lo_row,d0
		lea	8(a1),a1
		dbf	d1,lo_adrloop
		rts

lo_offset:	dc.w	380
;---------------------------------------------------------------------
lo_makecop:
		lea	lo_coptab(pc),a1
		move.l	#$22dffffe,d0
		move	#lo_heith-1,d7
lo_mcop:
		move	(a2)+,d6
		beq.s	lo_m3
		subq	#1,d6
lo_m2:
		move.l	d0,(a1)+
		addi.l	#$1000000,d0
		move	#$108,(a1)+
		move	#-lo_row,(a1)+
		move	#$10a,(a1)+
		move	#-lo_row,(a1)+
		subq	#1,d7
		bmi.s	lo_out
		dbf	d6,lo_m2
lo_m3:
		move.l	d0,(a1)+
		addi.l	#$1000000,d0
		move.l	#$1080000,(a1)+
		move.l	#$10a0000,(a1)+
		subq	#1,d7
		bpl.s	lo_mcop
lo_out:		rts


lo_modtab2:
dc.w	0,0,1,1,1,1,1,2,2,2,2,3,3,3,4,4,4,5,5,6,6,7,7,6,6,5,5
dc.w	5,4,4,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,3
dc.w	3,3,3,3,3,3,4,4,4,5,5,6,7,8,9,9,9,9


lo_modtab:
dc.w	0,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2
dc.w	3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5
dc.w	4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,
dc.w	1,1,1,1,1,1,1,1,1,1,1

;---------------------------------------------------------------------
lo_plane=$60000

lo_heith=272			;screen heith
lo_row=30

lo_heith2=624			;screen heith
;---------------------------------------------------------------------
lo_copper:
dc.w	$0182,$0c8b ,$0184,$0302 ,$0186,$0403
dc.w	$0188,$0411 ,$018a,$0514 ,$018c,$0625 ,$018e,$0736
dc.w	$0190,$0847 ,$0192,$0958 ,$0194,$0b7a ,$0196,$0201
dc.w	$0198,$0d9c ,$019a,$0ead ,$019c,$0fbe ,$019e,$0fcf
dc.l	$1800000

dc.l	$920050,$9400c0,$8e2271,$9032d1,$1020000,$1080000,$10a0000
dc.l	$1040000
lo_scr:
dc.l	$e00000+[lo_plane/$10000],$e20000+[lo_plane&$ffff]
dc.l	$e40000+[lo_plane/$10000],$e60000+[lo_plane&$ffff]
dc.l	$e80000+[lo_plane/$10000],$ea0000+[lo_plane&$ffff]
dc.l	$ec0000+[lo_plane/$10000],$ee0000+[lo_plane&$ffff]
dc.l	$2201ff00,$1004300
lo_coptab:
blk.l	3*lo_heith,0
dc.l	$3201ff00,$1000300
dc.l	-2

;---------------------------------------------------------------------
lo_h2=86
lo_r2=40

lo_coltab2:
dc.w	$0f74,$0f22,$0bc7
dc.w	$0896,$0674,$0453,$0131

lo_copper2:
dc.w	$0182,0,$0184,0,$0186,0
dc.w	$0188,0,$018a,0,$018c,0,$018e,0
dc.l	$1800000

dc.l	$920038,$9400d0,$8e3071,$9020d1,$1020000,$1080000,$10a0000
dc.l	$1040000
dc.l	$e00000+[lo_pic2/$10000],$e20000+[lo_pic2&$ffff]
dc.l	$e40000+[[lo_pic2+[lo_h2*lo_r2]]/$10000],$e60000+[[lo_pic2+[lo_h2*lo_r2]]&$ffff]
dc.l	$e80000+[[lo_pic2+[2*lo_h2*lo_r2]]/$10000],$ea0000+[[lo_pic2+[2*lo_h2*lo_r2]]&$ffff]

dc.l	$7a01ff00,$1003300
dc.l	$d001ff00,$1000300
dc.l	-2

lo_copper3:
dc.l	$1000300,$180000f,-2
lo_copper4:
dc.l	$1000300,$1800000,-2

;---------------------------------------------------------------------
;-------------------------------------------------------------------
v_sinus:			;640
l_table=v_sinus+640		;3544
lo_pic1=v_sinus+640+3544	;8880	($2260)
lo_pic2=v_sinus+640+3544+8880	;10320


>extern	"df0:.store/vec_sine(7fff).dat",v_sinus,-1
>extern	"df0:.Vector anims/brings_ya_a....vec",l_table,-1
>extern	"df0:.store/varathron.raw",lo_pic1,-1
>extern	"df0:.store/varathron2.raw",lo_pic2,-1

end=lo_pic2+10320
