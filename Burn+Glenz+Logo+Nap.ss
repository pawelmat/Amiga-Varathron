;		*****************************************
;		*  BURNING VEC + logo + GLENZ +napisy	*
;		*	-------------------------	*
;		*   Coding on 31.03.1993 - 04.04.1993	*
;		*	   by  KANE  of SUSPECT		*
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
if	exe=0
		move	#$4000,$9a(a0)
endif
		move	#$83e0,$96(a0)		;sprites on

		bsr.L	l_burning
		waitblt
		moveq	#20,d7
l_waiit:	raster
		dbf	d7,l_waiit
		bsr	n_napisy
		moveq	#20,d7
l_waiit2:	raster
		dbf	d7,l_waiit2
		bsr	g_glenz_192

		waitblt
		move.l	#-1,$44(a0)
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#l_screen,$54(a0)
		move	#[2*l_heiths*64]+l_rows,$58(a0)
		waitblt
		move.l	#l_copper,$80(a0)

		lea	l_jumpsine+6(pc),a2
		moveq	#0,d7
l_cont2_1:	raster
		moveq	#0,d0
		move.b	(a2,d7.w),d0
		add	d0,d0
		addi	#16,d0
		move	d0,spr_posX
		addi	#4,spr_posY
		bsr	put_sprite
		addq	#1,d7
		cmpi	#64,d7
		bmi.s	l_cont2_1

		move	#5,d7
		lea	l_cols(pc),a1
		lea	l_tab(pc),a2
l_cont_0_1:	raster
		raster
		raster
		raster
		raster
		move	d7,d6
		moveq	#0,d1
l_c_002:	move.b	(a2,d6.w),d1
		subi	#$100,(a1,d1.w)
		dbf	d6,l_c_002
		dbf	d7,l_cont_0_1

;---------------------------------------------------
if	exe=0
		move	#$c000,$9a(a0)
endif
		rts

;------------------------------------------------------------------
;			ROUTINES
;------------------------------------------------------------------
l_burning:
		waitblt
		move.l	#-1,$44(a0)
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#l_screen,$54(a0)
		move	#[2*l_heiths*64]+l_rows,$58(a0)
		waitblt
		move.l	#l_plane,$54(a0)
		move	#[445*64]+64,$58(a0)
		bsr.L	l_maketab
		bsr	put_sprite
		waitblt
;		move	#$8020,$96(a0)
		move.l	#l_copper,$80(a0)


;---------------------------------------------------
		move	#5,d7
		lea	l_cols(pc),a1
		lea	l_tab(pc),a2
l_cont_0_0:	raster
		raster
		raster
		raster
		raster
		move	d7,d6
		moveq	#0,d1
l_c_001:	move.b	(a2,d6.w),d1
		addi	#$100,(a1,d1.w)
		dbf	d6,l_c_001
		dbf	d7,l_cont_0_0

		lea	l_jumpsine+6(pc),a2
		moveq	#63,d7
l_cont0_1:	raster
		moveq	#0,d0
		move.b	(a2,d7.w),d0
		move	d0,d1
		add	d0,d0
		add	d1,d0
		addi	#16,d0
		move	d0,spr_posX
		subi	#8,spr_posY
		bsr	put_sprite
		subq	#2,d7
		bpl.s	l_cont0_1


		lea	l_szescian(pc),a6
l_control:	raster
		bsr.L	l_vector
		subi	#8,4(a6)
		cmpi	#2,4(a6)
		bpl.s	l_control

l_control2:	raster
		bsr.L	l_vector
		bsr.L	l_zmove
		subi	#1,l_licz
		bne.s	l_control2

l_control3:	raster
		bsr.L	l_vector
		addi	#8,4(a6)
		cmpi	#265,4(a6)
		bmi.s	l_control3
		waitblt
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#l_screen,$54(a0)
		move	#[2*l_heiths*64]+l_rows,$58(a0)
		waitblt
		move.l	#l_plane,$54(a0)
		move	#[445*64]+64,$58(a0)
		waitblt
		rts


l_tab:	dc.b	50,42,34,22,14,6
;-------------------------------------------------------------------
l_Zmove		move	l_add,d0
		add	d0,l_dist
		bpl.s	l_con1_2
		neg	l_add
		move	#0,l_dist
l_con1_2:	cmpi	#100,l_dist
		bmi.s	l_con1_3
		neg	l_add
		move	#100,l_dist
l_con1_3:	move	l_dist,d0
		addq	#2,d0
		move	d0,4(a6)
		rts

l_add:		dc.w	2
l_dist:		dc.w	0
;-------------------------------------------------------------------

l_vector:	move.l	scron(pc),d0
		move.l	scroff(pc),d1
		move.l	d0,scroff
		move.l	d1,scron
		lea	l_scr(pc),a1
		move	d1,6(a1)
		swap	d1
		move	d1,2(a1)
		swap	d1
		addi.l	#l_rows,d1
		move	d1,14(a1)
		swap	d1
		move	d1,10(a1)

		lea	l_frames(pc),a1
		addi	#1,(a1)
		cmpi	#9,(a1)
		bne.s	l_skip1
		move	#0,(a1)
l_skip1:	move	(a1),d0
		add	d0,d0
		add	d0,d0
		move.l	6(a1,d0.w),d0
		move.l	d0,2(a1)		;draw screen

		waitblt
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	d0,$54(a0)
		move	#[l_heith*64]+[l_row/2],$58(a0)


		move	20(a6),d7		;ilosc plaszczyzn
		lea	l_matrix(pc),a4
		lea	l_szline(pc),a3		;line tab

l_rys1:		moveq	#5,d6
		move.l	l_frames+2(pc),a5

		movem	(a3),d3-d5
		movem	(a4,d3.w),d0/d1
		movem	(a4,d4.w),d2/d3
		sub	d0,d2
		sub	d1,d3			;d2,d3-wsp.wek.B
		movem	(a4,d4.w),d0/d1
		movem	(a4,d5.w),d4/d5
		sub	d0,d4
		sub	d1,d5			;d4,d5-wsp.wek.A
		muls	d4,d3
		muls	d5,d2
		sub.l	d2,d3
		bpl.s	l_rys2

		lea	28(a3),a3
		bra.s	l_pomin
l_rys2:
		move	(a3)+,d4
		move	(a3),d5
		movem	(a4,d4.w),d0/d1
		movem	(a4,d5.w),d2/d3
		bsr.L	l_drawline
		dbf	d6,l_rys2


		lea	2(a3),a3
		moveq	#5,d6
		lea	l_row/2(a5),a5
l_rys3:		move	(a3)+,d4
		move	(a3),d5
		movem	(a4,d4.w),d0/d1
		movem	(a4,d5.w),d2/d3
		bsr.L	l_drawline
		dbf	d6,l_rys3
		lea	2(a3),a3
l_pomin:	dbf	d7,l_rys1


		waitblt					;fill
		move.l	#0,$64(a0)
		move.l	#$09f00012,$40(a0)
		move.l	l_frames+2(pc),d4
		addi.l	#[l_heith*l_row]-2,d4
		move.l	d4,$54(a0)
		move.l	d4,$50(a0)
		move	#[l_heith*64]+[l_row/2],$58(a0)

l_turn:		movem	6(a6),d0-d2
		add	d0,12(a6)
		andi	#$1fe,12(a6)
		add	d1,14(a6)
		andi	#$1fe,14(a6)
		add	d2,16(a6)
		andi	#$1fe,16(a6)

		lea	l_sinus,a1
		lea	l_sinus+$80,a2		;cosinus
		lea	l_matrix(pc),a3
		lea	l_szdots(pc),a4
		move	18(a6),d7		;ilosc punktow-1

l_twodim:	bsr.L	l_rotate
		addi	4(a6),d2	;dod.srodek z
		move	#256,d3		;lame zooming
		sub	d2,d3
		mulu	d3,d0
		asr	#8,d0
		mulu	d3,d1
		asr	#8,d1

		add	(a6),d0		;dodaj srodek
		add	2(a6),d1
		move	d0,(a3)+
		move	d1,(a3)+
		lea	6(a4),a4
		dbf	d7,l_twodim

;-------------------------------------------------------------------
;-------------------------------------------------------------------
;-------------------------------------------------------------------
;-------------------------------------------------------------------
;-------------------------------------------------------------------
;-------------------------------------------------------------------
;copy frame to screen

		waitblt
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	scroff,$54(a0)
		move	#[l_heiths*64]+l_row,$58(a0)

;-------------------------------------------------------------------

		lea	l_jumpsine(pc),a2
		lea	6(a2),a3
		move	(a2),d0
		add	d0,2(a2)
		bpl.s	l_noch1
		neg	(a2)
		move	#0,2(a2)
l_noch1:	cmpi	#[l_rows*8]-160,2(a2)
		bmi.s	l_noch2
		neg	(a2)
		move	#[l_rows*8]-161,2(a2)
l_noch2:
		lea	l_oldposs(pc),a4	;old positions
		move	l_frames(pc),d1
		add	d1,d1
		add	d1,d1
		move	2(a2),(a4,d1.w)		;x

		addi	#5,4(a2)
		andi	#$7f,4(a2)
		moveq	#0,d0
		move	4(a2),d0
		move.b	(a3,d0.w),d0
		move	d0,2(a4,d1.w)		;y

;-------------------------------------------------------------------
;copy last, full frame


		lea	l_frames(pc),a1
		move	(a1),d0			;last frame
		move.l	#l_buffer,d5
		bsr.L	l_makevar

		waitblt
		move	#0,$64(a0)
		move	#l_rows-[l_row/2],$62(a0)	;b mod
		move	#l_rows-[l_row/2],$66(a0)
		move.l	d2,$40(a0)
		move.l	l_frames+2,$50(a0)
		move.l	d1,$4c(a0)			;B
		move.l	d1,$54(a0)
		move	#[2*l_heith*64]+[l_row/4],$58(a0)

;copy shades

		moveq	#1,d7
l_trailloop:	subq	#1,d0
		bpl.s	l_skip2
		addi	#9,d0
l_skip2:
		bsr.L	l_makevar
		move	d0,d4
		add	d4,d4
		add	d4,d4
		move.l	6(a1,d4.w),d4

		waitblt
		move	#0,$64(a0)			;a
		move	#0,$60(a0)			;c
		move	#l_rows-[l_row/2],$62(a0)	;b mod
		move	#l_rows-[l_row/2],$66(a0)	;d
		move.l	d3,$40(a0)

		move.l	d4,$50(a0)			;A
		move.l	d1,$4c(a0)			;B
		move.l	d5,$48(a0)			;C
		move.l	d1,$54(a0)			;D
		move	#[2*l_heith*64]+[l_row/4],$58(a0)

		addi.l	#$1900,d5
		dbf	d7,l_trailloop
		waitblt
		rts

;-------------------------------------------------------------------
l_makevar:	move	d0,d2
		add	d2,d2
		add	d2,d2
		move	(a4,d2.w),d4
		move	#l_heiths-137,d1
		sub	2(a4,d2.w),d1
		mulu	#l_rows*2,d1
		moveq	#0,d2
		move	d4,d2
		lsr	#4,d4
		lsl	#1,d4
		add	d4,d1
		add.l	scroff,d1

		andi	#$f,d2
		ror.l	#4,d2
		move.l	d2,d3
		ori.l	#$0dfc0000,d2		;start x point
		ori.l	#$0fec0000,d3
		rts

;-------------------------------------------------------------------
;-------------------------------------------------------------------
;d3-kat, d0-x , d1-y

l_rotate:	move	16(a6),d3	;wez kat (*2)
		move	4(a4),d0
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
l_drawline:	movem	d0-d7,-(sp)
		cmpi	d1,d3
		beq.L	l_noline
		bpl.s	l_line
		exg	d0,d2
		exg	d1,d3
l_line:		moveq	#3,d4
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
l_noline:	movem	(sp)+,d0-d7
		rts

l_octant:	dc.b	3,8+3,16+3,20+3

;---------------------------------------------------------------------
l_heiths=256			;screen dimensions
l_rows=40

l_heith=144			;frame dimensions
l_row=22*2
;---------------------------------------------------------------------
l_ytable:	ds.w	l_heith,0
;---------------------------------------------------------------------
l_maketab:	lea	l_ytable(pc),a1
		moveq	#0,d0
		move	#l_heith-1,d7
l_tabloop:	move	d0,(a1)+
		addi	#l_row,d0
		dbf	d7,l_tabloop

		lea	l_buffer,a1
		lea	$fa0000,a2
		lea	$fb0000,a3
		lea	$fc0000,a4
		move	#$18f0/4,d7
l_makbuf:	move.l	(a2)+,(a1)
		move.l	(a3)+,d1
		not	d1
		and.l	d1,(a1)+

		move.l	(a3)+,d0
		and.l	(a4)+,d0
;		not	d0
		move.l	d0,$1900-2(a1)

;		not.l	d0
		move.l	(a4)+,d1
		ror.l	#5,d1
		and.l	d1,d0
		move.l	d0,[2*$1900]-2(a1)

		dbf	d7,l_makbuf
		rts

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;draw 16 col sprite routine (sprite, at x,y) by KANE of SUSPECT
wys=[256*4]+8			;256-wysokosc sprita

put_sprite:	lea	sprite0,a1
		move	spr_posY(pc),d0
		addi	#21,d0		;y to border
		move	d0,d1
		addi	#[wys-8]/4,d1	;add heith
		ror	#8,d1
		lsl.b	#1,d1		;vstop
		rol	#8,d0
		tst.b	d0
		beq.s	spr_2
		ori	#4,d1
		clr.b	d0
spr_2:		move	spr_posX(pc),d2
		addi	#116,d2		;x to border
		ori	#$80,d1

		moveq	#3,d6
spr_loop:	move	d2,d3
		andi	#-2,d1
		lsr	#1,d3
		bcc.s	spr_3
		ori	#1,d1
spr_3:		move.b	d3,d0
		move	d0,(a1)
		move	d0,wys(a1)
		move	d1,2(a1)
		move	d1,wys+2(a1)

		lea	2*wys(a1),a1
		addi	#16,d2
		dbf	d6,spr_loop

		rts

spr_posX:	dc.w	119
spr_posY:	dc.w	226+22+30
;---------------------------------------------------------------------
l_copper:
dc.w	$120,sprite0/$10000,$122,sprite0&$ffff
dc.w	$124,sprite0+[wys]/$10000,$126,sprite0+[wys]&$ffff
dc.w	$128,sprite0+[2*wys]/$10000,$12a,sprite0+[2*wys]&$ffff
dc.w	$12c,sprite0+[3*wys]/$10000,$12e,sprite0+[3*wys]&$ffff
dc.w	$130,sprite0+[4*wys]/$10000,$132,sprite0+[4*wys]&$ffff
dc.w	$134,sprite0+[5*wys]/$10000,$136,sprite0+[5*wys]&$ffff
dc.w	$138,sprite0+[6*wys]/$10000,$13a,sprite0+[6*wys]&$ffff
dc.w	$13c,sprite0+[7*wys]/$10000,$13e,sprite0+[7*wys]&$ffff

dc.w	$01a0,$0000 ,$01a2,$06bd ,$01a4,$07cd ,$01a6,$08dd
dc.w	$01a8,$04bd ,$01aa,$03ad ,$01ac,$0078 ,$01ae,$0035

dc.l	$1820cc0,$1840f00,$1860cc0
	dc.l	$1800101

dc.l	$920038,$9400d0,$8e2871,$9028d1
dc.l	$1020000,$1040000
dc.w	$108,l_rows,$10a,l_rows
l_scr:
dc.w	$e0,l_screen/$10000,$e2,l_screen&$ffff
dc.w	$e4,l_screen/$10000,$e6,l_screen&$ffff

dc.l	$2801ff00,$01002300
l_cols:
dc.l	$d801ff00,$1800101
dc.l	$e801ff00,$1800101
dc.l	$f801ff00,$1800101
dc.l	$ffdffffe
dc.l	$0801ff00,$1800101
dc.l	$1801ff00,$1800101
dc.l	$2801ff00,$1800101,$01000300
dc.l	-2

;-------------------------------------------------------------------
l_screen=$74000
scron:		dc.l	l_screen
scroff:		dc.l	l_screen+[2*[l_heiths+32]*l_rows]

l_plane=$66000
l_frames:
dc.w	0
dc.l	l_plane			;rys plane
dc.l	l_plane,l_plane+[l_heith*l_row],l_plane+[2*l_heith*l_row]
dc.l	l_plane+[3*l_heith*l_row],l_plane+[4*l_heith*l_row],l_plane+[5*l_heith*l_row]
dc.l	l_plane+[6*l_heith*l_row],l_plane+[7*l_heith*l_row],l_plane+[8*l_heith*l_row]

l_oldposs:
ds.l	9,0

l_buffer=$60000			;ok. $6000
l_matrix:	ds.w	22*2,0
l_licz:		dc.w	350
;-------------------------------------------------------------------
l_szescian:
dc.w	80,73,248				;x,y,z mid
dc.w	8,-4,-6
;dc.w	0,0,0
dc.w	128,158,128
dc.w	19,5
l_szdots:
dc.w	-40,40,-40,0,40,-40,40,40,-40,40,0,-40
dc.w	40,-40,-40,0,-40,-40,-40,-40,-40,-40,0,-40
dc.w	-40,40,40,0,40,40,40,40,40,40,0,40
dc.w	40,-40,40,0,-40,40,-40,-40,40,-40,0,40
dc.w	-40,40,0,40,40,0,40,-40,0,-40,-40,0

l_szline:
dc.w	1*4,2*4,3*4,7*4,6*4,5*4,1*4		,0*4,1*4,5*4,4*4,3*4,7*4,0*4		;przod
dc.w	8*4,15*4,11*4,12*4,13*4,9*4,8*4		,10*4,9*4,13*4,14*4,15*4,11*4,10*4	;tyl
dc.w	6*4,5*4,13*4,12*4,18*4,19*4,6*4		,4*4,5*4,13*4,14*4,19*4,18*4,4*4	;gora
dc.w	8*4,9*4,1*4,2*4,17*4,16*4,8*4		,10*4,9*4,1*4,0*4,16*4,17*4,10*4	;dol
dc.w	8*4,16*4,19*4,6*4,7*4,15*4,8*4		,16*4,0*4,7*4,15*4,14*4,19*4,16*4	;lewa
dc.w	2*4,17*4,18*4,12*4,11*4,3*4,2*4		,17*4,10*4,11*4,3*4,4*4,18*4,17*4	;prawa

;-------------------------------------------------------------------
l_jumpsine:
dc.w	4,0,0		;x add,x,y
dc.b	$00,$02,$05,$07,$0A,$0C,$0F,$11,$14,$16,$19,$1B,$1E,$20,$23,$25
dc.b	$27,$2A,$2C,$2E,$31,$33,$35,$37,$39,$3C,$3E,$40,$42,$44,$46,$47
dc.b	$49,$4B,$4D,$4E,$50,$52,$53,$55,$56,$58,$59,$5A,$5B,$5D,$5E,$5F
dc.b	$60,$61,$62,$62,$63,$64,$65,$65,$66,$66,$66,$67,$67,$67,$67,$67
dc.b	$67,$67,$67,$67,$67,$67,$66,$66,$65,$65,$64,$64,$63,$62,$61,$60
dc.b	$5F,$5E,$5D,$5C,$5B,$5A,$58,$57,$55,$54,$52,$51,$4F,$4E,$4C,$4A
dc.b	$48,$47,$45,$43,$41,$3F,$3D,$3B,$38,$36,$34,$32,$30,$2D,$2B,$29
dc.b	$26,$24,$21,$1F,$1D,$1A,$18,$15,$13,$10,$0E,$0B,$08,$06,$03,$01

;-------------------------------------------------------------------
;-------------------------------------------------------------------
;-------------------------------------------------------------------

g_glenz_192:	bsr.L	g_maketab
		waitblt
		move.l	#-1,$44(a0)
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#g_screen,$54(a0)
		move	#[420*64],$58(a0)
		waitblt
		move.l	#g_copper,$80(a0)

		lea	g_szescian(pc),a6
g_control:	raster
		bsr.L	g_vector
		raster
		bsr.L	g_vector
		bsr	le_setcol
		subi	#1,g_licz
		bne.s	g_control

		move	#300,g_licz
g_control2:	raster
		bsr.L	g_vector
		subi	#1,g_licz
		bne.s	g_control2

		move	#25,g_licz
g_control3:	raster
		bsr.L	g_vector
		raster
		bsr.L	g_vector
		bsr	la_fadcol
		subi	#1,g_licz
		bne.s	g_control3
		rts

g_licz:		dc.w	16
;-------------------------------------------------------------------

le_setcol:	lea	g_cols(pc),a1
		lea	g_coltab,a2
		moveq	#3,d3			;color nr. - 1
le_scol1:	move	(a2),d1
		andi	#$f,d1
		move	2(a1),d2
		andi	#$f,d2
		cmpi	d1,d2
		beq.s	le_scol2
		addi	#1,2(a1)
le_scol2:	move	(a2),d1
		andi	#$f0,d1
		move	2(a1),d2
		andi	#$f0,d2
		cmpi	d1,d2
		beq.s	le_scol3
		addi	#$10,2(a1)
le_scol3:	move	(a2)+,d1
		andi	#$f00,d1
		move	2(a1),d2
		andi	#$f00,d2
		cmpi	d1,d2
		beq.s	le_scol4
		addi	#$100,2(a1)
le_scol4:	addi.l	#4,a1
		dbf	d3,le_scol1
		rts

la_fadcol:	lea	g_cols(pc),a1
		moveq	#3,d3			;no. of colors - 1
la_fad1:	move	2(a1),d1
		andi	#$f,d1
		cmpi	#1,d1
		beq.s	la_fad2
		subi	#1,2(a1)
la_fad2:	move	2(a1),d1
		andi	#$f0,d1
		beq.s	la_fad3
		subi	#$10,2(a1)
la_fad3:	move	2(a1),d1
		andi	#$f00,d1
		cmpi	#$100,d1
		beq.s	la_fad4
		subi	#$100,2(a1)
la_fad4:	addi.l	#4,a1
		dbf	d3,la_fad1
		rts


;-------------------------------------------------------------------

g_vector:	move.l	g_scron(pc),d0
		move.l	g_scroff(pc),d1
		move.l	d0,g_scroff
		addi.l	#[10*g_row]+12,d0	;position shift
		move.l	d0,g_scruse
		move.l	d1,g_scron
		lea	g_scr(pc),a1
		move	d1,6(a1)
		swap	d1
		move	d1,2(a1)
		swap	d1
		addi.l	#g_row/3,d1
		move	d1,14(a1)
		swap	d1
		move	d1,10(a1)
		swap	d1
		addi.l	#g_row/3,d1
		move	d1,22(a1)
		swap	d1
		move	d1,18(a1)

		waitblt
		move	#[g_row/3]-g_rows,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	d0,$54(a0)
		move	#[3*g_heiths*64]+[g_rows/2],$58(a0)

;---------------------------------------
g_turn:		movem	6(a6),d0-d2
		add	d0,12(a6)
		andi	#$1fe,12(a6)
		add	d1,14(a6)
		andi	#$1fe,14(a6)
		add	d2,16(a6)
		andi	#$1fe,16(a6)

		lea	l_sinus,a1
		lea	l_sinus+$80,a2		;cosinus
		lea	g_matrix,a3
		lea	g_szdots(pc),a4
		move	18(a6),d7		;ilosc punktow-1

g_twodim:	bsr.L	g_rotate
		addi	4(a6),d2	;dod.srodek z
		move	#256,d3		;lame zooming
		sub	d2,d3
		mulu	d3,d0
		asr	#8,d0
		mulu	d3,d1
		asr	#8,d1

		add	(a6),d0		;dodaj srodek
		add	2(a6),d1
		move	d0,(a3)+
		move	d1,(a3)+
		lea	6(a4),a4
		dbf	d7,g_twodim


;---------------------------------------
		move	20(a6),d7		;ilosc plaszczyzn
		lea	g_matrix,a4
		lea	g_szline(pc),a3		;line tab

g_drawit:	moveq	#13,d6			;ile linii -2
		move.l	g_scruse(pc),a5

		movem	(a3),d3-d5		;wylicz normalna
		movem	(a4,d4.w),d0/d1
		sub	(a4,d3.w),d0
		sub	2(a4,d3.w),d1
		movem	(a4,d5.w),d2/d3
		sub	(a4,d4.w),d2
		sub	2(a4,d4.w),d3
		mulu	d2,d1
		mulu	d3,d0
		sub	d0,d1
		bpl.s	g_rys0

		lea	[g_row/3](a5),a5
g_rys1:		move	(a3)+,d4		;btpl 1
		move	(a3),d5
		movem	(a4,d4.w),d0/d1
		movem	(a4,d5.w),d2/d3
		bsr.L	g_drawline
		dbf	d6,g_rys1
		lea	2(a3),a3

		moveq	#3,d6
g_rys1_1:	move	(a3)+,d4		;figure 2
		move	(a3),d5
		movem	(a4,d4.w),d0/d1
		movem	(a4,d5.w),d2/d3
		bsr.L	g_drawline
		dbf	d6,g_rys1_1
		lea	2(a3),a3

		moveq	#3,d6
g_rys1_2:	move	(a3)+,d4		;figure 3
		move	(a3),d5
		movem	(a4,d4.w),d0/d1
		movem	(a4,d5.w),d2/d3
		bsr.L	g_drawline
		dbf	d6,g_rys1_2

		lea	10+2(a3),a3
		bra.s	g_pomin

;===============================
g_rys0:		move	(a3)+,d4		;btpl 0
		move	(a3),d5
		movem	(a4,d4.w),d0/d1
		movem	(a4,d5.w),d2/d3
		bsr.L	g_drawline
		dbf	d6,g_rys0
		lea	2(a3),a3

		moveq	#3,d6
g_rys0_1:	move	(a3)+,d4		;figure 2
		move	(a3),d5
		movem	(a4,d4.w),d0/d1
		movem	(a4,d5.w),d2/d3
		bsr.L	g_drawline
		dbf	d6,g_rys0_1
		lea	2(a3),a3

		moveq	#3,d6
g_rys0_2:	move	(a3)+,d4		;figure 3
		move	(a3),d5
		movem	(a4,d4.w),d0/d1
		movem	(a4,d5.w),d2/d3
		bsr.L	g_drawline
		dbf	d6,g_rys0_2
		lea	2(a3),a3

		moveq	#3,d6
		lea	2*[g_row/3](a5),a5
g_rys2:		move	(a3)+,d4		;btpl 2 - mask
		move	(a3),d5
		movem	(a4,d4.w),d0/d1
		movem	(a4,d5.w),d2/d3
		bsr.L	g_drawline
		dbf	d6,g_rys2
		lea	2(a3),a3

g_pomin:	dbf	d7,g_drawit




		waitblt					;fill
		move	#[g_row/3]-g_rows,$64(a0)
		move	#[g_row/3]-g_rows,$66(a0)
		move.l	#$09f00012,$40(a0)
		move.l	g_scruse(pc),d4
		addi.l	#[[g_heiths-1]*g_row]+g_rows-2,d4
		move.l	d4,$54(a0)
		move.l	d4,$50(a0)
		move	#[3*g_heiths*64]+[g_rows/2],$58(a0)
		rts

;-------------------------------------------------------------------
;-------------------------------------------------------------------
;d3-kat, d0-x , d1-y

g_rotate:	move	16(a6),d3	;wez kat (*2)
		move	4(a4),d0
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
g_drawline:	movem	d0-d7,-(sp)
		cmpi	d1,d3
		beq.L	g_noline
		bpl.s	g_line
		exg	d0,d2
		exg	d1,d3
g_line:		moveq	#3,d4
		addq	#1,d1
		move	d0,d5
		move	d1,d6
		subi	d3,d1
		bpl.s	g_dr1
		neg	d1
g_dr1:		subi	d2,d0
		bpl.s	g_dr2
		eori	#%01,d4
		neg	d0
g_dr2:		cmpi	d0,d1
		bmi.s	g_dr3
		exg	d0,d1
		eori	#%10,d4
g_dr3:		move	d5,d7
		and.l	#$f,d7
		ror	#4,d7
		ori	#$0b4a,d7
		swap	d7
		move.b	g_octant(pc,d4.w),d7
		lsl	#1,d1
		add	d6,d6
		move	g_ytable(pc,d6.w),d6
		and.l	#$fff0,d5
		lsr	#3,d5
		addi	d6,d5
		add.l	a5,d5
		waitblt
		move.l	#$ffff8000,$72(a0)
		move	#g_row,$60(a0)
		move	d1,$62(a0)
		move.l	d5,$48(a0)
		move.l	d5,$54(a0)
		subi	d0,d1
		bpl.s	g_dr4
		ori	#$40,d7
g_dr4:		move	d1,$52(a0)
		move.l	d7,$40(a0)
		subi	d0,d1
		move	d1,$64(a0)
		addq	#1,d0
		lsl	#6,d0
		addq	#2,d0
		move	d0,$58(a0)
g_noline:	movem	(sp)+,d0-d7
		rts

g_octant:	dc.b	3,8+3,16+3,20+3

;---------------------------------------------------------------------
g_heiths=174			;frame dimensions
g_rows=22

g_heith=256			;screen dimensions
norm=34
g_row=norm*3
;---------------------------------------------------------------------
g_ytable:	ds.w	g_heith,0
;---------------------------------------------------------------------
g_maketab:	lea	g_ytable(pc),a1
		moveq	#0,d0
		move	#g_heith-1,d7
g_tabloop:	move	d0,(a1)+
		addi	#g_row,d0
		dbf	d7,g_tabloop
		rts


;---------------------------------------------------------------------
g_coltab:
dc.w	$f0f,$999,$c4c,$ccc
;---------------------------------------------------------------------
g_copper:
dc.w	$120,sprite0/$10000,$122,sprite0&$ffff
dc.w	$124,sprite0+[wys]/$10000,$126,sprite0+[wys]&$ffff
dc.w	$128,sprite0+[2*wys]/$10000,$12a,sprite0+[2*wys]&$ffff
dc.w	$12c,sprite0+[3*wys]/$10000,$12e,sprite0+[3*wys]&$ffff
dc.w	$130,sprite0+[4*wys]/$10000,$132,sprite0+[4*wys]&$ffff
dc.w	$134,sprite0+[5*wys]/$10000,$136,sprite0+[5*wys]&$ffff
dc.w	$138,sprite0+[6*wys]/$10000,$13a,sprite0+[6*wys]&$ffff
dc.w	$13c,sprite0+[7*wys]/$10000,$13e,sprite0+[7*wys]&$ffff

	dc.l	$1800101
dc.l	$1820101,$1840101,$1860101
g_cols:
dc.l	$1880101,$18a0101,$18c0101,$18e0101

dc.l	$920038,$9400b8,$8e2880,$90288e
dc.l	$1020000,$1040000
dc.w	$108,2*norm,$10a,2*norm
g_scr:
dc.w	$e0,g_screen/$10000,$e2,g_screen&$ffff
dc.w	$e4,g_screen+[g_row]/$10000,$e6,g_screen+[g_row]&$ffff
dc.w	$e8,g_screen+[2*g_row]/$10000,$ea,g_screen+[2*g_row]&$ffff

dc.l	$2801ff00,$01003300
dc.l	$d801ff00,$1800201
dc.l	$e801ff00,$1800301
dc.l	$f801ff00,$1800401
dc.l	$ffdffffe
dc.l	$0801ff00,$1800501
dc.l	$1801ff00,$1800601
dc.l	$2801ff00,$1800701,$01000300
dc.l	-2

;-------------------------------------------------------------------
g_matrix=$70000
g_screen=$72000
g_scron:		dc.l	g_screen
g_scroff:		dc.l	g_screen+[g_heith*g_row]
g_scruse:		dc.l	0

;-------------------------------------------------------------------
g_szescian:
dc.w	88,88,20				;x,y,z mid
dc.w	6,-4,-2
dc.w	128,158,128
dc.w	43,5
g_szdots:

dc.w	-25,-50,-50,-25,50,-50,0,50,-50,0,-50,-50	;front
dc.w	25,-50,-50,25,50,-50,50,50,-50,50,25,-50,-50,25,-50
dc.w	-50,0,-50,50,0,-50,50,-25,-50,-50,-25,-50,-50,-50,-50
dc.w	-25,-50,50,-25,50,50,0,50,50,0,-50,50		;back
dc.w	25,-50,50,25,50,50,50,-50,50,50,25,50,-50,25,50,-50,0,50
dc.w	50,0,50,50,-25,50,-50,-25,50,-50,50,50
dc.w	50,-50,-50,50,-50,-25,50,-50,0,50,-50,25	;right
dc.w	50,50,-25,50,50,0,50,50,25,50,50,50
dc.w	-50,-50,-25,-50,-50,0,-50,-50,25,-50,-50,50	;left
dc.w	-50,50,-50,-50,50,-25,-50,50,0,-50,50,25


g_szline:
dc.w	4*0,4*1,4*2,4*3,4*4,4*5,4*6,4*7,4*8,4*9,4*10,4*11,4*12,4*13,4*0			;front
dc.w	4*13,4*6,4*28,4*40,4*13		;figure 1
dc.w	4*3,4*10,4*2,4*9,4*3		;figure 2
dc.w	4*13,4*28,4*6,4*40,4*13		;mask

dc.w	4*14,4*17,4*16,4*19,4*18,4*20,4*25,4*26,4*23,4*24,4*21,4*22,4*27,4*15,4*14	;back
dc.w	4*39,4*35,4*20,4*27,4*39
dc.w	4*17,4*24,4*16,4*23,4*17
dc.w	4*39,4*20,4*35,4*27,4*39

dc.w	4*29,4*32,4*33,4*30,4*31,4*34,4*35,4*21,4*7,4*10,4*24,4*25,4*11,4*28,4*29	;right
dc.w	4*28,4*35,4*20,4*6,4*28
dc.w	4*30,4*24,4*33,4*10,4*30
dc.w	4*28,4*20,4*35,4*6,4*28

dc.w	4*36,4*37,4*42,4*43,4*38,4*39,4*26,4*12,4*9,4*23,4*22,4*8,4*40,4*41,4*36	;left
dc.w	4*13,4*27,4*39,4*40,4*13
dc.w	4*37,4*23,4*42,4*9,4*37
dc.w	4*13,4*39,4*27,4*40,4*13

dc.w	4*37,4*30,4*31,4*38,4*39,4*14,4*0,4*3,4*17,4*18,4*4,4*28,4*29,4*36,4*37		;top
dc.w	4*39,4*28,4*20,4*13,4*39
dc.w	4*30,4*17,4*37,4*3,4*30
dc.w	4*39,4*20,4*28,4*13,4*39

dc.w	4*40,4*41,4*32,4*33,4*42,4*43,4*34,4*35,4*19,4*5,4*2,4*16,4*15,4*1,4*40		;bottom
dc.w	4*40,4*35,4*27,4*6,4*40
dc.w	4*42,4*16,4*33,4*2,4*42
dc.w	4*40,4*27,4*35,4*6,4*40

;-------------------------------------------------------------------
*		glenz 192 - vector logos

n_napisy:
		bsr.L	n_maketab
		waitblt
		move.l	#-1,$44(a0)
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#n_screen,$54(a0)
		move	#[n_heith*64]+[n_row],$58(a0)
		waitblt
		move.l	#n_copper,$80(a0)

		lea	n_glenz,a6
		move	#70,2(a6)
		move	#0,4(a6)
		move	#580,(a6)

n_control2:	raster
		bsr.L	n_vector
		subi	#5,(a6)
		addi	#10,4(a6)
		cmpi	#1024,4(a6)
		bmi.s	n_control2

		lea	n_192,a6
		move	#70,2(a6)
		move	#0,4(a6)
		move	#430,(a6)

		move	#$a33,n_cols+2
		move	#$411,n_cols+6
		move	#$722,n_cols+10

n_control4:	raster
		bsr.L	n_vector
		subi	#5,(a6)
		addi	#12,4(a6)
		cmpi	#1024,4(a6)
		bmi.s	n_control4

		rts

;-------------------------------------------------------------------

n_vector:	move.l	n_scron(pc),d0
		move.l	n_scroff(pc),d1
		move.l	d0,n_scroff
		move.l	d1,n_scron
		lea	n_scr(pc),a1
		move	d1,6(a1)
		move	d1,6+8(a1)
		swap	d1
		move	d1,2(a1)
		move	d1,2+8(a1)
		waitblt
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	d0,$54(a0)
		move	#[n_heith*64]+[n_row/2],$58(a0)


		move.l	22(a6),d0		;coord tab
		lea	(a6,d0.w),a1
		lea	n_matrix,a2
		move	18(a6),d7		;ilosc punktow-1
n_zoom:		move	(a1)+,d0
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
		dbf	d7,n_zoom

		move.l	26(a6),d0		;line tab
		lea	(a6,d0.w),a1
		lea	n_ytable(pc),a2
		lea	n_matrix,a3
		move.l	n_scroff(pc),a5
		move	20(a6),d7
n_draw1:	move	(a1)+,d6
n_draw2:	move	(a1)+,d4
		move	(a1)+,d5
		movem	(a3,d4.w),d0/d1
		movem	(a3,d5.w),d2/d3
		bsr.L	n_drawline
		dbf	d6,n_draw2
		dbf	d7,n_draw1

		waitblt					;fill right
		move	#n_row-4,$60(a0)
		move	#n_row-4,$62(a0)
		move	#n_row-4,$66(a0)
		move.l	#1,$44(a0)
		move.l	#$076a0000,$40(a0)
		move.l	a5,d4
		addi.l	#n_row-4,d4
		move.l	d4,$4c(a0)		;B
		addi.l	#n_row,d4
		move.l	d4,$48(a0)		;C
		move.l	d4,$54(a0)		;D
		move	#1,$74(a0)		;Adat
		move	#[[n_heith-1]*64]+2,$58(a0)
		waitblt					;fill
		move.l	#-1,$44(a0)
		move.l	#0,$64(a0)
		move.l	#$09f00012,$40(a0)
		move.l	a5,d4
		addi.l	#[n_heith*n_row]-2,d4
		move.l	d4,$54(a0)
		move.l	d4,$50(a0)
		move	#[n_heith*64]+[n_row/2],$58(a0)
		rts

;-------------------------------------------------------------------
n_drawline:	cmp	d0,d2
		bpl.s	n_okx
		exg	d0,d2
		exg	d1,d3
n_okx:		cmpi	#[n_row*8]-2,d2
		bmi.s	n_left
		cmpi	#[n_row*8]-2,d0
		bpl.L	n_noline2
		move	#[n_row*8]-2,d4
		sub	d0,d4
		sub	d0,d2
		sub	d1,d3
		muls	d4,d3
		divs	d2,d3
		add	d1,d3
		move	#[n_row*8]-2,d2

		move	d3,d4
		addq	#1,d4
		bgt.s	n_right2
		moveq	#1,d4
n_right2:	cmpi	#n_heith,d4
		bpl.s	n_left
		add	d4,d4
		move	(a2,d4.w),d4		;mulu #n_row,d4
		waitblt
		eori	#1,n_row-2(a5,d4.w)

n_left:		tst	d0
		bpl.s	n_up
		tst	d2
		bmi.L	n_noline2
		sub	d2,d0
		sub	d3,d1
		muls	d2,d1
		divs	d0,d1
		neg	d1
		add	d3,d1
		moveq	#0,d0

n_up:		cmp	d1,d3
		bpl.s	n_oky
		exg	d0,d2
		exg	d1,d3
n_oky:		tst	d1
		bpl.s	n_down
		tst	d3
		bmi.L	n_noline2
		sub	d2,d0
		sub	d3,d1
		muls	d3,d0
		divs	d1,d0
		neg	d0
		add	d2,d0
		moveq	#0,d1

n_down:		cmpi	#n_heith-1,d3
		bmi.s	n_line
		cmpi	#n_heith-1,d1
		bpl.L	n_noline2
		move	#n_heith-1,d4
		sub	d1,d4
		sub	d0,d2
		sub	d1,d3
		muls	d4,d2
		divs	d3,d2
		add	d0,d2
		move	#n_heith-1,d3

n_line:		movem	d4-d7,-(sp)
		cmpi	d1,d3
		beq.L	n_noline
		bpl.s	n_lineok
		exg	d0,d2
		exg	d1,d3
n_lineok:	moveq	#3,d4
		addq	#1,d1
		move	d0,d5
		move	d1,d6
		subi	d3,d1
		bpl.s	n_dr1
		neg	d1
n_dr1:		subi	d2,d0
		bpl.s	n_dr2
		eori	#%01,d4
		neg	d0
n_dr2:		cmpi	d0,d1
		bmi.s	n_dr3
		exg	d0,d1
		eori	#%10,d4
n_dr3:		move	d5,d7
		and.l	#$f,d7
		ror	#4,d7
		ori	#$0b4a,d7
		swap	d7
		move.b	n_octant(pc,d4.w),d7
		lsl	#1,d1
		add	d6,d6
		move	n_ytable(pc,d6.w),d6
		and.l	#$fff0,d5
		lsr	#3,d5
		addi	d6,d5
		add.l	a5,d5
		waitblt
		move.l	#$ffff8000,$72(a0)
		move	#n_row,$60(a0)
		move	d1,$62(a0)
		move.l	d5,$48(a0)
		move.l	d5,$54(a0)
		subi	d0,d1
		bpl.s	n_dr4
		ori	#$40,d7
n_dr4:		move	d1,$52(a0)
		move.l	d7,$40(a0)
		subi	d0,d1
		move	d1,$64(a0)
		addq	#1,d0
		lsl	#6,d0
		addq	#2,d0
		move	d0,$58(a0)
n_noline:	movem	(sp)+,d4-d7
n_noline2:	rts

n_octant:	dc.b	3,8+3,16+3,20+3

;---------------------------------------------------------------------
n_heith=256			;screen dimensions
n_row=40

;---------------------------------------------------------------------
n_ytable:	ds.w	n_heith,0
;---------------------------------------------------------------------
n_maketab:	lea	n_ytable(pc),a1
		moveq	#0,d0
		move	#n_heith-1,d7
n_tabloop:	move	d0,(a1)+
		addi	#n_row,d0
		dbf	d7,n_tabloop
		rts

;---------------------------------------------------------------------
n_copper:
dc.w	$120,sprite0/$10000,$122,sprite0&$ffff
dc.w	$124,sprite0+[wys]/$10000,$126,sprite0+[wys]&$ffff
dc.w	$128,sprite0+[2*wys]/$10000,$12a,sprite0+[2*wys]&$ffff
dc.w	$12c,sprite0+[3*wys]/$10000,$12e,sprite0+[3*wys]&$ffff
dc.w	$130,sprite0+[4*wys]/$10000,$132,sprite0+[4*wys]&$ffff
dc.w	$134,sprite0+[5*wys]/$10000,$136,sprite0+[5*wys]&$ffff
dc.w	$138,sprite0+[6*wys]/$10000,$13a,sprite0+[6*wys]&$ffff
dc.w	$13c,sprite0+[7*wys]/$10000,$13e,sprite0+[7*wys]&$ffff

dc.w	$01a0,$0000 ,$01a2,$06bd ,$01a4,$07cd ,$01a6,$08dd
dc.w	$01a8,$04bd ,$01aa,$03ad ,$01ac,$0078 ,$01ae,$0035

n_cols:
dc.l	$1820772,$1840441,$1860662
dc.l	$1800101

dc.l	$920038,$9400d0,$8e2871,$9028e1
dc.l	$1020046,$1040024
dc.w	$108,0,$10a,0
n_scr:
dc.l	$e00007,$e20000
dc.l	$e40007,$e60000

n_soff:
dc.l	$2801ff00,$01001300
dc.l	$2901ff00,$01002300

dc.l	$d801ff00,$1800201
dc.l	$e801ff00,$1800301
dc.l	$f801ff00,$1800401
dc.l	$ffdffffe
dc.l	$0801ff00,$1800501
dc.l	$1801ff00,$1800601
dc.l	$2801ff00,$1800701,$01000300
dc.l	-2

;-------------------------------------------------------------------
n_screen=$79000
n_scron:		dc.l	n_screen
n_scroff:		dc.l	n_screen+[n_heith*n_row]

n_matrix=$78000

;-------------------------------------------------------------------
l_sinus:
n_glenz=l_sinus+640
n_192=n_glenz+520
sprite0=n_192+470		;($2040) - sprite data
>extern	"df0:.store/vec_sine(7fff).dat",l_sinus,-1
>extern	"df0:.Vector anims/glenz.vec",n_glenz,-1
>extern	"df0:.Vector anims/192.vec",n_192,-1
>extern	"df0:.store/vader_logo1.spr",sprite0,-1

end=sprite0+8256
