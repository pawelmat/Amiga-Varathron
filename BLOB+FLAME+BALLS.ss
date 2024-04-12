
;		*****************************************
;		*	FILLED BLOB + FLAME + BALLS	*
;		*	-------------------------	*
;		*Fixed on Prima Aprilis '93  by KANE/SCT*
;		*****************************************
;Part of VARATHRON demo.

exe=0

org	$4c000
load	$4c000

waitblt: macro
	btst.b	#14,2(a0)
	bne.s	*-8
	endm
raster: macro
	cmpi.b	#$ff,6(a0)
	bne.s	*-8
	cmpi.b	#$ff,6(a0)
	beq.s	*-8
	endm

s:		lea	$dff000,a0
if exe=0
		move	#$4000,$9a(a0)
endif
		move	#$20,$96(a0)
		move	#$83c0,$96(a0)

		move.l	#r_copper0,$80(a0)	;neutral copper
		bsr.s	e_blob
		move.l	#r_copper0,$80(a0)
		move	#20,d7
llloooppp:	raster
		dbf	d7,llloooppp
		bsr	t_dragon_flame
		move.l	#r_copper0,$80(a0)
		bsr	r_rubber_balls
		move.l	#r_copper0,$80(a0)

if exe=0
		move	#$8020,$96(a0)
		move	#$c000,$9a(a0)
endif
		rts


;----------------------------------------------------------------
;			ROUTINES
;----------------------------------------------------------------

e_blob:		waitblt
		move.l	#-1,$44(a0)
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#e_plane,$54(a0)
		move	#[510*64],$58(a0)	;clr $10000
		waitblt
		move.l	#e_copper,$80(a0)
		bsr.L	e_maketab

;-------------------------------------------------
e_control:	raster
		bsr.L	e_elypse
		addi.b	#2,e_wysw
		cmpi.b	#$aa,e_wysw
		bne.s	e_control

		move.l	#$1200000,e_wysw+4
e_control2:	raster
		bsr.L	e_elypse
		addi.b	#2,e_wysw+16
		cmpi.b	#$ff,e_wysw+16
		bne.s	e_control2

e_control3:	raster
		bsr.s	e_elypse
		subi	#1,e_licz
		bne.s	e_control3

e_control4:	raster
		bsr.s	e_elypse
		subi.b	#2,e_wysw+16
		cmpi.b	#$ab,e_wysw+16
		bne.s	e_control4

		move.l	#$1000300,e_wysw+4
e_control5:	raster
		bsr.s	e_elypse
		subi.b	#2,e_wysw
		cmpi.b	#$4c,e_wysw
		bne.s	e_control5

		rts

e_licz:		dc.w	420,0
;---------------------------------------------------------------------

e_elypse:	move	e_scrpnt,d0
		bpl.s	e_chg2
		move	#6,e_scrpnt
		moveq	#6,d0
e_chg2:		moveq	#4,d1
		lea	e_scr(pc),a1
e_chg3:		move	d0,d2
		mulu	#[e_row*e_heith],d2
		addi.l	#e_plane,d2
		move	d2,6(a1)
		swap	d2
		move	d2,2(a1)
		addq	#8,a1
		addq	#1,d0
		cmpi	#7,d0
		bne.s	e_chg4
		clr	d0
e_chg4:		dbf	d1,e_chg3
		subq	#1,e_scrpnt
		move	d0,d2
		mulu	#[e_row*e_heith],d2
		addi.l	#e_plane,d2		;clr
		addq	#1,d0
		cmpi	#7,d0
		bne.s	e_chg5
		clr	d0
e_chg5:		mulu	#[e_row*e_heith],d0
		addi.l	#e_plane,d0
		move.l	d0,a5			;drawplane

		waitblt
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	d2,$54(a0)
		move	#[e_heith*64]+[e_row/2],$58(a0)


;---------------------------------------------------------------------
		lea	e_ytable(pc),a6

		lea	e_angles(pc),a1
		lea	e_speeds(pc),a2
		moveq	#9,d7
e_addaxids:	move	(a2)+,d0
		add	d0,(a1)
		andi	#255,(a1)+
		dbf	d7,e_addaxids


		lea	e_sinus(pc),a1
		lea	e_angles(pc),a2


		bsr.L	e_setdelta
		add	d2,a3			;add to middle
		add	d5,a4
		sub	d2,d0			;set radius
		add	d4,d0
		add	d3,d1
		sub	d5,d1
		ext.l	d0
		ext.l	d1
		movem.l	a1/a2,-(sp)
		bsr.L	e_makevar
		bsr.L	e_draw1


		movem.l	(sp)+,a1/a2
		bsr.L	e_setdelta
		add	d3,a4			;add to middle
		add	d4,a3			;x
		add	d2,d0			;set radius
		sub	d4,d0
		add	d3,d1
		sub	d5,d1
		ext.l	d0
		ext.l	d1
		movem.l	a1/a2,-(sp)
		bsr.L	e_makevar
		bsr.L	e_draw2


		movem.l	(sp)+,a1/a2
		bsr.L	e_setdelta
		add	d2,a3			;add to middle
		add	d5,a4
		add	d2,d0			;set radius
		sub	d4,d0
		sub	d3,d1
		add	d5,d1
		ext.l	d0
		ext.l	d1
		movem.l	a1/a2,-(sp)
		bsr.L	e_makevar
		bsr.L	e_draw3


		movem.l	(sp)+,a1/a2
		bsr.s	e_setdelta
		add	d3,a4			;add to middle
		add	d4,a3			;x
		sub	d2,d0			;set radius
		add	d4,d0
		sub	d3,d1
		add	d5,d1
		ext.l	d0
		ext.l	d1
		bsr.L	e_makevar
		bsr.L	e_draw4

;---------------------------------------------------------------------
e_fill:		waitblt
		move	#0,$64(a0)
		move.l	#$09f00012,$40(a0)
		move.l	a5,d0
		addi.l	#[e_heith*e_row]-2,d0
		move.l	d0,$54(a0)
		move.l	d0,$50(a0)
		move	#[e_heith*64]+[e_row/2],$58(a0)
		rts

;---------------------------------------------------------------------
e_setdelta:	move	(a2)+,d2
		move	(a2)+,d3
		move	(a2),d4
		move	2(a2),d5
		move	(a1,d2.w),d2		;x1
		move	(a1,d3.w),d3		;y1 - sinusy
		move	(a1,d4.w),d4		;x2
		move	(a1,d5.w),d5		;y2

		move	#e_row*4,a3		;x middle
		move	#e_heith/2,a4		;y middle
		move	#e_radiusx,d0
		move	#e_radiusy,d1
		rts

;---------------------------------------------------------------------
e_makevar:	move	#0,a1		;x
		move	d1,a2		;y

		mulu	d1,d1			;a
		moveq	#1,d2
		sub.l	d0,d2
		add.l	d2,d2		;delta=2(1-b)
		mulu	d0,d0			;b
		move.l	d1,d3		;a2
		move	a2,d4
		add	d4,d4
		addq	#1,d4
		mulu	d0,d4		;b2
		rts

;---------------------------------------------------------------------
e_draw1_1:	move	a1,d5			;x
		move	a2,d6			;y
		add	a3,d5			;x middle
		add	a4,d6			;y middle
		move	d5,d7
		lsr	#3,d7
		not	d5
		add	d6,d6
		move	(a6,d6.w),d6		;y*40
		add	d7,d6
		bchg	d5,(a5,d6.w)		;down-right
		bsr.s	e_math

e_draw1:	cmpi	#0,a2			;don't end if y>0
		bpl.s	e_draw1_1		;dorob punkt zerowy
		rts


e_draw2_1:	move	a3,d5			;x middle
		move	a4,d6			;y middle
		add	a1,d5			;x
		sub	a2,d6			;y
		move	d5,d7
		lsr	#3,d7
		not	d5
		add	d6,d6
		move	(a6,d6.w),d6
		add	d7,d6
		bchg	d5,(a5,d6.w)		;up-right
		bsr.s	e_math

e_draw2:	cmpi	#0,a2
		bgt.s	e_draw2_1		;bez zerowego
		rts


e_draw3_1:	move	a3,d5			;x middle
		move	a4,d6			;y middle
		sub	a1,d5			;x
		sub	a2,d6			;y
		move	d5,d7
		lsr	#3,d7
		not	d5
		add	d6,d6
		move	(a6,d6.w),d6
		add	d7,d6
		bchg	d5,(a5,d6.w)		;up-left
		bsr.s	e_math

e_draw3:	cmpi	#0,a2
		bgt.s	e_draw3_1		;zerowy nie
		rts


e_draw4_1:	move	a3,d5			;x middle
		move	a4,d6			;y middle
		sub	a1,d5			;x
		add	a2,d6			;y
		move	d5,d7
		lsr	#3,d7
		not	d5
		add	d6,d6
		move	(a6,d6.w),d6
		add	d7,d6
		bchg	d5,(a5,d6.w)		;up-left
		bsr.s	e_math

e_draw4:	cmpi	#0,a2
		bpl.s	e_draw4_1		;zerowy tak
		rts

;---------------------------------------------------------------------
e_math:		tst.l	d2			;test delta
		beq.s	e_krok_ukosny
		bpl.s	e_decyzja

		move.l	d2,d5			;krok pionowy
		add.l	d2,d5
		add.l	d4,d5			;decyzja=2*delta+b2
		tst.l	d5

		bpl.s	e_krok_ukosny
		addq	#1,a1			;x=x+1
		add.l	d1,d3
		add.l	d1,d3			;a2=a2+a+a
		add.l	d3,d2			;delta=delta+a2
		bra.s	e_math

e_krok_ukosny:	addq	#1,a1
		subq	#1,a2
		add.l	d1,d3
		add.l	d1,d3			;a2=a2+a+a
		sub.l	d0,d4
		sub.l	d0,d4			;b2=b2-b-b
		add.l	d3,d2
		sub.l	d4,d2			;delta=delta+a2-b2
		rts

e_decyzja:	move.l	d2,d5
		add.l	d2,d5
		sub.l	d3,d5			;decyzja=2*delta-a2
		tst.l	d5

		ble.s	e_krok_ukosny

		subq	#1,a2
		sub.l	d0,d4
		sub.l	d0,d4			;b2=b2-b-b
		sub.l	d4,d2			;delta=delta-b2
		rts

;---------------------------------------------------------------------
e_heith=180
e_row=26
;---------------------------------------------------------------------
e_ytable:	ds.w	e_heith,0
;---------------------------------------------------------------------
e_maketab:	lea	e_ytable,a1
		moveq	#0,d0
		move	#e_heith-1,d7
e_tabloop:	move	d0,(a1)+
		addi	#e_row,d0
		dbf	d7,e_tabloop
		rts
;---------------------------------------------------------------------
e_copper:
dc.l	$1800003,$1820521,$1840622,$1860823,$1880925
dc.l	$18A0B28,$18C0C2B,$18E0D2E,$1900E2C,$1920E29
dc.l	$1940E26,$1960E33,$1980E63,$19A0E93,$19C0EC3
dc.l	$19E0DE3,$1A00CD3,$1A20AD2,$1A408C2,$1A607C1
dc.l	$1A805B1,$1AA04B1,$1AC02A0,$1AE01A0,$1B00090
dc.l	$1B20081,$1B40072,$1B60063,$1B80053,$1BA0043
dc.l	$1BC0033,$1be0023

dc.l	$920050,$9400b0,$8e40b1,$900181
dc.l	$1020000,$1040000,$1080000,$10a0000
e_scr:
dc.l	$e00006,$e20000
dc.l	$e40006,$e60000
dc.l	$e80006,$ea0000
dc.l	$ec0006,$ee0000
dc.l	$f00006,$f20000

dc.l	$4c01ff00,$01005300
e_wysw:
dc.l	$4c01ff00,$01000300
dc.l	$ab01ff00,$1800005
dc.l	$ab01ff00,$01000300
dc.l	-2
;---------------------------------------------------------------------
e_plane=$60000
e_scrpnt:	dc.w	0
;----------------------------------------------------------------------
e_angles:	dc.w	8,130,84,26,210,0,34,6,  8,130
e_speeds:	dc.w	4,2,8,6,2,4,2,8,  4,2

e_radiusx=75
e_radiusy=65

e_sinus:		;22
	dc.w	$0000,$0001,$0002,$0003,$0004,$0005,$0006,$0007
	dc.w	$0008,$0009,$000A,$000B,$000C,$000D,$000E,$000E
	dc.w	$000F,$0010,$0011,$0011,$0012,$0012,$0013,$0013
	dc.w	$0014,$0014,$0015,$0015,$0015,$0015,$0015,$0015,$15
	dc.w	$0015,$0015,$0015,$0015,$0015,$0015,$0014,$0014
	dc.w	$0014,$0013,$0013,$0012,$0012,$0011,$0010,$0010
	dc.w	$000F,$000E,$000D,$000C,$000B,$000A,$0009,$0008
	dc.w	$0007,$0006,$0005,$0004,$0003,$0002,$0001
	dc.w	$0000,$FFFF,$FFFE,$FFFD,$FFFC,$FFFB,$FFFA,$FFF9
	dc.w	$FFF8,$FFF7,$FFF6,$FFF5,$FFF4,$FFF3,$FFF2,$FFF1
	dc.w	$FFF0,$FFF0,$FFEF,$FFEE,$FFEE,$FFED,$FFED,$FFEC
	dc.w	$FFEC,$FFEC,$FFEB,$FFEB,$FFEB,$FFEB,$FFEB,$FFEB,$ffeb
	dc.w	$FFEB,$FFEB,$FFEB,$FFEB,$FFEB,$FFEB,$FFEC,$FFEC
	dc.w	$FFED,$FFED,$FFEE,$FFEE,$FFEF,$FFEF,$FFF0,$FFF1
	dc.w	$FFF2,$FFF2,$FFF3,$FFF4,$FFF5,$FFF6,$FFF7,$FFF8
	dc.w	$FFF9,$FFFA,$FFFB,$FFFC,$FFFD,$FFFE,$FFFF

;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;		*****************************************
;		*	     DRAGON FLAME		*
;		*	-------------------------	*
;		* Coding 01.02.1993  by KANE of SUSPECT	*
;		*****************************************

t_dragon_flame:
		waitblt
		move.l	#-1,$44(a0)
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	#t_plane,$54(a0)
		move	#[1020*64],$58(a0)
		waitblt
		move.l	#t_copper,$80(a0)

		bsr.L	t_maketab
t_control1:	raster
		bsr.L	t_trails
		raster
		bsr.L	t_trails
		bsr.s	se_setcol
		subi	#1,t_licz
		bne.s	t_control1

		move	#500,t_licz
t_control2:	raster
		bsr.L	t_trails
		subi	#1,t_licz
		bne.s	t_control2

		move	#30,t_licz
t_control3:	raster
		bsr.L	t_trails
		raster
		bsr.L	t_trails
		bsr.L	fa_fadcol
		bsr.L	fb_fadcol
		subi	#1,t_licz
		bne.s	t_control3

		rts

t_licz:		dc.w	20
;---------------------------------------------------------------------

se_setcol:	lea	t_cols1(pc),a1
		lea	t_coltab(pc),a2
		bsr.s	se_sese2
		lea	t_cols2(pc),a1
		lea	t_coltab(pc),a2
se_sese2:	moveq	#30,d3				;color nr. - 1
se_scol1:	move	(a2),d1
		andi	#$f,d1
		move	2(a1),d2
		andi	#$f,d2
		cmpi	d1,d2		;d1 nowy ,d2 stary
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
		rts

fa_fadcol:	lea	t_cols1(pc),a1
		moveq	#30,d3			;no. of colors - 1
fa_fad1:	move	2(a1),d1
		andi	#$f,d1
		cmpi	#3,d1
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
		rts

fb_fadcol:	lea	t_cols2(pc),a1
		moveq	#30,d3			;no. of colors - 1
fb_fad1:	move	2(a1),d1
		andi	#$f,d1
		cmpi	#5,d1
		beq.s	fb_fad2
		subi	#1,2(a1)
fb_fad2:	move	2(a1),d1
		andi	#$f0,d1
		beq.s	fb_fad3
		subi	#$10,2(a1)
fb_fad3:	move	2(a1),d1
		andi	#$f00,d1
		beq.s	fb_fad4
		subi	#$100,2(a1)
fb_fad4:	addi.l	#4,a1
		dbf	d3,fb_fad1
		rts

;---------------------------------------------------------------------

t_trails:
		move	t_scrpnt,d0
		bpl.s	t_chg2
		move	#6,t_scrpnt
		moveq	#6,d0
t_chg2:		moveq	#4,d1
		lea	t_scr(pc),a1
t_chg3:		move	d0,d2
		mulu	#[t_row*t_heith],d2
		addi.l	#t_plane,d2
		move	d2,6(a1)
		swap	d2
		move	d2,2(a1)
		addq	#8,a1
		addq	#1,d0
		cmpi	#7,d0
		bne.s	t_chg4
		clr	d0
t_chg4:		dbf	d1,t_chg3
		subq	#1,t_scrpnt
		move	d0,d2
		mulu	#[t_row*t_heith],d2
		addi.l	#t_plane,d2		;clr
		addq	#1,d0
		cmpi	#7,d0
		bne.s	t_chg5
		clr	d0
t_chg5:		mulu	#[t_row*t_heith],d0
		addi.l	#t_plane,d0
		move.l	d0,a5			;drawplane

		waitblt
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	d2,$54(a0)
		move	#[t_heith*64]+[t_row/2],$58(a0)

;---------------------------------------------------------------------

		lea	t_angles(pc),a1
		lea	t_speeds(pc),a2
		lea	t_sinus(pc),a3
		lea	t_addsin(pc),a4

		movem	(a2),d0-d3
		lea	t_buffer,a6
		moveq	#7,d7
t_dodaj1:	add	d0,(a1)
		move	(a1)+,(a6)+
		add	d1,(a1)
		move	(a1)+,(a6)+
		add	d2,(a1)
		move	(a1)+,(a6)+
		add	d3,(a1)
		move	(a1)+,(a6)+
		dbf	d7,t_dodaj1

		lea	t_buffer,a6
		moveq	#3,d7
t_ogon:
		moveq	#t_repeat,d6
t_ogon2:	movem	8(a2),d0-d3		;dodaj pojedyncze
		add	d0,(a6)
		andi	#$ff,(a6)
		add	d1,2(a6)
		andi	#$ff,2(a6)
		add	d2,4(a6)
		andi	#$ff,4(a6)
		add	d3,6(a6)
		andi	#$ff,6(a6)
		add	d0,8(a6)
		andi	#$ff,8(a6)
		add	d1,10(a6)
		andi	#$ff,10(a6)
		add	d2,12(a6)
		andi	#$ff,12(a6)
		add	d3,14(a6)
		andi	#$ff,14(a6)

		movem	(a6),d0-d3
		move	(a3,d0.w),d0
		move	(a3,d1.w),d1
		add	(a4,d2.w),d0
		add	(a4,d3.w),d1
		movem	8(a6),d2-d5
		move	(a3,d2.w),d2
		move	(a3,d3.w),d3
		add	(a4,d4.w),d2
		add	(a4,d5.w),d3
		lsr	#1,d1
		lsr	#1,d3

		bsr.s	t_drawline
		dbf	d6,t_ogon2

		lea	16(a6),a6
		dbf	d7,t_ogon

;---------------------------------------------------------------------
t_fill:		waitblt
		move	#0,$64(a0)
		move.l	#$09f00012,$40(a0)
		move.l	a5,d0
		addi.l	#[t_heith*t_row]-2,d0
		move.l	d0,$54(a0)
		move.l	d0,$50(a0)
		move	#[t_heith*64]+[t_row/2],$58(a0)
		rts

;---------------------------------------------------------------------

t_drawline:	movem	d0-d7,-(sp)
		cmpi	d1,d3
		beq.L	t_noline
		bpl.s	t_line
		exg	d0,d2
		exg	d1,d3
t_line:		moveq	#3,d4
		addq	#1,d1
		move	d0,d5
		move	d1,d6
		subi	d3,d1
		bpl.s	t_dr1
		neg	d1
t_dr1:		subi	d2,d0
		bpl.s	t_dr2
		eori	#%01,d4
		neg	d0
t_dr2:		cmpi	d0,d1
		bmi.s	t_dr3
		exg	d0,d1
		eori	#%10,d4
t_dr3:		move	d5,d7
		and.l	#$f,d7
		ror	#4,d7
		ori	#$0b4a,d7
		swap	d7
		move.b	t_octant(pc,d4.w),d7
		lsl	#1,d1
		add	d6,d6
		move	t_ytable(pc,d6.w),d6
		and.l	#$fff0,d5
		lsr	#3,d5
		addi	d6,d5
		add.l	a5,d5
		waitblt
		move.l	#$ffff8000,$72(a0)
		move	#t_row,$60(a0)
		move	d1,$62(a0)
		move.l	d5,$48(a0)
		move.l	d5,$54(a0)
		subi	d0,d1
		bpl.s	t_dr4
		ori	#$40,d7
t_dr4:		move	d1,$52(a0)
		move.l	d7,$40(a0)
		subi	d0,d1
		move	d1,$64(a0)
		addq	#1,d0
		lsl	#6,d0
		addq	#2,d0
		move	d0,$58(a0)
t_noline:	movem	(sp)+,d0-d7
		rts

t_octant:	dc.b	3,8+3,16+3,20+3

;---------------------------------------------------------------------
t_heith=160
t_row=40
;---------------------------------------------------------------------
t_ytable:	ds.w	t_heith,0
;---------------------------------------------------------------------
t_maketab:	lea	t_ytable,a1
		moveq	#0,d0
		move	#t_heith-1,d7
t_tabloop:	move	d0,(a1)+
		addi	#t_row,d0
		dbf	d7,t_tabloop
		rts
;---------------------------------------------------------------------
t_coltab:	;(31)
dc.w	$305,$405,$505,$615,$725,$835,$945,$A55,$B65
dc.w	$C75,$D85,$E95,$EA5,$EB5,$EC5,$ED5,$EE5,$DE5,$CE5
dc.w	$BE5,$AE5,$9E5,$8E6,$7E7,$6D6,$5C5,$4B5,$4A5,$495
dc.w	$485,$475
;---------------------------------------------------------------------
t_copper:
t_cols1:
dc.l	$1820003,$1840003,$1860003,$1880003
dc.l	$18A0003,$18C0003,$18E0003,$1900003,$1920003
dc.l	$1940003,$1960003,$1980003,$19A0003,$19C0003
dc.l	$19E0003,$1A00003,$1A20003,$1A40003,$1A60003
dc.l	$1A80003,$1AA0003,$1AC0003,$1AE0003,$1B00003
dc.l	$1B20003,$1B40003,$1B60003,$1B80003,$1BA0003
dc.l	$1BC0003,$1be0003

dc.l	$1800003

dc.l	$920038,$9400d0,$8e3080,$9030d0
dc.l	$1020000,$1040000,$1080000,$10a0000
t_scr:
dc.l	$e00006,$e20000
dc.l	$e40006,$e60000
dc.l	$e80006,$ea0000
dc.l	$ec0006,$ee0000
dc.l	$f00006,$f20000

dc.l	$5001ff00,$01005300
dc.l	$ab01ff00,$1800005

t_cols2:
dc.l	$1820005,$1840005,$1860005,$1880005
dc.l	$18A0005,$18C0005,$18E0005,$1900005,$1920005
dc.l	$1940005,$1960005,$1980005,$19A0005,$19C0005
dc.l	$19E0005,$1A00005,$1A20005,$1A40005,$1A60005
dc.l	$1A80005,$1AA0005,$1AC0005,$1AE0005,$1B00005
dc.l	$1B20005,$1B40005,$1B60005,$1B80005,$1BA0005
dc.l	$1BC0005,$1be0005

dc.l	$f001ff00,$01000300
dc.l	-2
;---------------------------------------------------------------------
t_plane=$60000
t_scrpnt:	dc.w	0
;----------------------------------------------------------------------
t_repeat=17-1

t_buffer:	ds.l	16,0

t_angles:				;sinus glowny i addsine
	dc.w	90,20	,10,10
	dc.w	98,26	,20,26

	dc.w	98,26	,20,26
	dc.w	50,70	,50,40

	dc.w	50,70	,50,40
	dc.w	58,90	,70,56

	dc.w	58,90	,70,56
	dc.w	90,20	,10,10
t_speeds:
	dc.w	2,4	,6,-2		;calosc
	dc.w	18,20	,16,4		;kolejne segmenty

t_sinus:		;A=100 , ilosc=128
	dc.w	$00A0,$00A4,$00A9,$00AE,$00B3,$00B8,$00BD,$00C1
	dc.w	$00C6,$00CB,$00CF,$00D3,$00D7,$00DB,$00DF,$00E3
	dc.w	$00E7,$00EA,$00ED,$00F0,$00F3,$00F6,$00F8,$00FA
	dc.w	$00FC,$00FE,$00FF,$0101,$0102,$0103,$0103,$0103
	dc.w	$0103,$0103,$0103,$0102,$0101,$0100,$00FF,$00FD
	dc.w	$00FB,$00F9,$00F7,$00F4,$00F2,$00EF,$00EC,$00E8
	dc.w	$00E5,$00E1,$00DD,$00D9,$00D5,$00D1,$00CD,$00C8
	dc.w	$00C4,$00BF,$00BA,$00B6,$00B1,$00AC,$00A7,$00A2
	dc.w	$009E,$0099,$0094,$008F,$008A,$0086,$0081,$007C
	dc.w	$0078,$0073,$006F,$006B,$0067,$0063,$005F,$005B
	dc.w	$0058,$0054,$0051,$004E,$004C,$0049,$0047,$0045
	dc.w	$0043,$0041,$0040,$003F,$003E,$003D,$003D,$003D,$3d
	dc.w	$003D,$003D,$003D,$003E,$003F,$0041,$0042,$0044
	dc.w	$0046,$0048,$004A,$004D,$0050,$0053,$0056,$0059
	dc.w	$005D,$0061,$0065,$0069,$006D,$0071,$0075,$007A
	dc.w	$007F,$0083,$0088,$008D,$0092,$0097,$009C
t_addsin:
	dc.w	$0000,$0002,$0005,$0008,$000B,$000E,$0011,$0014
	dc.w	$0017,$0019,$001C,$001F,$0021,$0023,$0026,$0028
	dc.w	$002A,$002C,$002E,$0030,$0032,$0033,$0035,$0036
	dc.w	$0037,$0038,$0039,$003A,$003A,$003B,$003B,$003B,$3b
	dc.w	$003B,$003B,$003B,$003B,$003A,$0039,$0039,$0038
	dc.w	$0037,$0035,$0034,$0032,$0031,$002F,$002D,$002B
	dc.w	$0029,$0027,$0025,$0022,$0020,$001D,$001B,$0018
	dc.w	$0015,$0012,$0010,$000D,$000A,$0007,$0004
	dc.w	$FFFF,$FFFC,$FFF9,$FFF6,$FFF3,$FFF0,$FFEE,$FFEB
	dc.w	$FFE8,$FFE5,$FFE3,$FFE0,$FFDE,$FFDB,$FFD9,$FFD7
	dc.w	$FFD5,$FFD3,$FFD1,$FFCF,$FFCE,$FFCC,$FFCB,$FFC9
	dc.w	$FFC8,$FFC7,$FFC7,$FFC6,$FFC5,$FFC5,$FFC5,$FFC5,$ffc5
	dc.w	$FFC5,$FFC5,$FFC5,$FFC6,$FFC6,$FFC7,$FFC8,$FFC9
	dc.w	$FFCA,$FFCB,$FFCD,$FFCE,$FFD0,$FFD2,$FFD4,$FFD6
	dc.w	$FFD8,$FFDA,$FFDD,$FFDF,$FFE1,$FFE4,$FFE7,$FFE9
	dc.w	$FFEC,$FFEF,$FFF2,$FFF5,$FFF8,$FFFB,$FFFE


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;		*****************************************
;		*	      RUBBER BALLS		*
;		*	-------------------------	*
;		*	Coded on Prima Aprilis '93	*
;		*	    by KANE of SUSPECT		*
;		*****************************************

r_rubber_balls:
		bsr.L	r_maketab
		bsr.L	r_makeanim

;---------------------------------------------------------------------

r_control0:	raster
		addi.b	#1,r_bar
		cmpi.b	#$e0,r_bar
		bne.s	r_control0
		raster
		move.l	#r_copper,$80(a0)


r_control1:	raster
		bsr.L	r_animball
		bsr.L	r_movein
		addi	#2,r_dist
		cmpi	#321,r_dist
		bmi.s	r_control1

		move	#$2679,r_mov+18
r_control2:	raster
		bsr.L	r_animball
;	btst.b	#6,$bfe001			;kick this out !!!
;	beq.s	quit
		subi	#1,r_licz
		bne.s	r_control2

		subi	#2,r_dist
		move	#$2680,r_mov+18
r_control3:	raster
		bsr.L	r_animball
		bsr.s	r_movein
		subi	#2,r_dist
		bpl.s	r_control3
		rts

;---------------------------------------------------------------------
;Copper shifting (only to the right)... Prima Aprilis '93 by Kane

r_MoveIn:
		lea	r_mov(pc),a1
		lea	r_scr(pc),a2
		move	r_dist(pc),d0

		move	d0,d1
		andi	#$f,d1
		move	d1,d2
		lsl	#4,d2
		or	d2,d1
		move	d1,6(a1)		;shift

		move	d0,d1
		lsr	#4,d1
		add	d1,d1
		addi	#4,d1

		move	#r_row,d2
		sub	d1,d2
		move	d2,10(a1)		;modulos
		move	d2,14(a1)

		add	d2,6(a2)		;center screens
		add	d2,14(a2)
		add	d2,22(a2)

		lsr	#4,d0
		lsl	#3,d0
		addi	#$38,d0
		move	d0,2(a1)		;screen size
		rts

r_dist:		dc.w	0
;---------------------------------------------------------------------
;---------------------------------------------------------------------
r_animball:	lea	r_animtab(pc),a1
		lea	r_jumpsin(pc),a4
		lea	r_animdata(pc),a2
		lea	r_scr(pc),a3
		moveq	#8,d7
		bsr.s	r_anim2
		lea	8(a3),a3		;next screen
		lea	8(a2),a2		;next data
		moveq	#0,d7
		bsr.s	r_anim2
		lea	8(a3),a3		;next screen
		lea	8(a2),a2		;next data
		moveq	#-8,d7

r_anim2:	move	(a2),d3
		move	4(a2),d0
		add	d0,(a2)
		andi	#$1f,(a2)
		move.b	(a1,d3.w),d3
		mulu	#r_heith*r_row,d3

		move	2(a2),d2
		andi	#$7e,d2
		move	6(a2),d0
		addi	d0,2(a2)
		andi	#$7f,2(a2)
		move	(a4,d2.w),d2
		mulu	#r_row,d2
		addi.l	d2,d3
		add	d7,d3

		addi.l	#r_animation,d3
		move	d3,6(a3)
		swap	d3
		move	d3,2(a3)
		rts

;---------------------------------------------------------------------
;---------------------------------------------------------------------

r_makeanim:	lea	r_animation,a5
		waitblt
		move	#0,$66(a0)
		move.l	#$1000000,$40(a0)
		move.l	a5,$54(a0)
		move	#[1024*64],$58(a0)
		waitblt
		move.l	a5,d0
		addi.l	#1024*128,d0
		move.l	d0,$54(a0)
		move	#[500*64],$58(a0)
		waitblt

r_animloop:	lea	r_sinus(pc),a4
		move	r_angle(pc),d2
		addi	#1,r_angle
		moveq	#0,d0
		moveq	#0,d1
		move.b	(a4,d2.w),d0		;b
		move.b	16(a4,d2.w),d1		;a
		move	#r_heith-102,r_yadd		;keep in one place
		bsr.s	r_elypse

		adda.l	#r_row*r_heith,a5
		cmpa.l	#r_animation+[16*r_row*r_heith],a5
		bne.s	r_animloop
		rts

r_elypse:	lea	r_ytable(pc),a4
		move	#0,a1		;x
		move	d1,a2		;y
		mulu	d1,d1			;a
		moveq	#1,d2
		sub.l	d0,d2
		add.l	d2,d2		;delta=2(1-b)
		mulu	d0,d0			;b
		move.l	d1,d3		;a2
		move	a2,d4
		add	d4,d4
		addq	#1,d4
		mulu	d0,d4		;b2

r_begin:	cmpi	#0,a2			;don't end if y>0
		bgt.s	r_elypseloop

		move	a1,d5			;x
		move	a2,d6			;y
		addi	#r_row*4,d5
		add	r_yadd,d6
		move	d5,d7
		lsr	#3,d7
		not	d5
		add	d6,d6
		move	(a4,d6.w),d6		;y*40
		add	d6,d7
		bchg	d5,(a5,d7.w)		;down-right

		move	#r_row*4,d5
		sub	a1,d5
		move	d5,d7
		lsr	#3,d7
		not	d5
		add	d7,d6
		bchg	d5,(a5,d6.w)		;up-left

c_fill:		waitblt
		move	#0,$64(a0)
		move.l	#$09f00012,$40(a0)
		move.l	a5,d0
		addi.l	#[r_heith*r_row]-2,d0
		move.l	d0,$54(a0)
		move.l	d0,$50(a0)
		move	#[r_heith*64]+[r_row/2],$58(a0)
		rts
;---------------------------------------------------------------------

r_elypseloop:					;set pixels
		move	a1,d5			;x
		move	a2,d6			;y
		addi	#r_row*4,d5
		addi	r_yadd,d6
		move	d5,d7
		lsr	#3,d7
		not	d5
		add	d6,d6
		move	(a4,d6.w),d6		;y*40
		move	d6,a3
		add	d7,d6
		bchg	d5,(a5,d6.w)		;down-right
		move	r_yadd,d6
		sub	a2,d6
		add	d6,d6
		move	(a4,d6.w),d6
		add	d6,d7
		bchg	d5,(a5,d7.w)		;up-right
		move	#r_row*4,d5
		sub	a1,d5
		move	d5,d7
		lsr	#3,d7
		not	d5
		add	d7,d6
		bchg	d5,(a5,d6.w)		;up-left
		add	a3,d7
		bchg	d5,(a5,d7.w)		;down-left
r_math:		tst.l	d2			;test delta
		beq.s	r_krok_ukosny
		bpl.s	r_decyzja
		move.l	d2,d5			;krok pionowy
		add.l	d2,d5
		add.l	d4,d5			;decyzja=2*delta+b2
		bpl.s	r_krok_ukosny
		addq	#1,a1			;x=x+1
		add.l	d1,d3
		add.l	d1,d3			;a2=a2+a+a
		add.l	d3,d2			;delta=delta+a2
		bra.s	r_math
r_krok_ukosny:	addq	#1,a1
		subq	#1,a2
		add.l	d1,d3
		add.l	d1,d3			;a2=a2+a+a
		sub.l	d0,d4
		sub.l	d0,d4			;b2=b2-b-b
		add.l	d3,d2
		sub.l	d4,d2			;delta=delta+a2-b2
		bra.L	r_begin
r_decyzja:	move.l	d2,d5
		add.l	d2,d5
		sub.l	d3,d5			;decyzja=2*delta-a2
		ble.s	r_krok_ukosny
		subq	#1,a2
		sub.l	d0,d4
		sub.l	d0,d4			;b2=b2-b-b
		sub.l	d4,d2			;delta=delta-b2
		bra.L	r_begin

;---------------------------------------------------------------------
r_heith=256
r_row=44
;---------------------------------------------------------------------
r_ytable:	ds.w	r_heith,0
;---------------------------------------------------------------------
r_maketab:	lea	r_ytable,a1
		moveq	#0,d0
		move	#r_heith-1,d7
r_tabloop:	move	d0,(a1)+
		addi	#r_row,d0
		dbf	d7,r_tabloop
		rts
;---------------------------------------------------------------------
r_copper0:
dc.l	$1000300,$1800003
r_bar:
dc.l	$ab07fffe,$1800005
dc.l	-2

r_copper:
dc.l	$1820a00,$18400a0,$1860aa0,$188000a,$18a0a0a,$18c00aa,$18e0aaa
dc.l	$1800003

dc.l	$920030,$90fdc7
dc.l	$1040000
r_mov:				;space to move screen in
dc.w	$94,$30
dc.w	$102,0
dc.l	$1080000,$10a0000
dc.l	$8e2680

r_scr:
dc.w	$e0,[r_animation+[15*r_heith*r_row]]/$10000
dc.w	$e2,[r_animation+[15*r_heith*r_row]]&$ffff
dc.w	$e4,[r_animation+[15*r_heith*r_row]]/$10000
dc.w	$e6,[r_animation+[15*r_heith*r_row]]&$ffff
dc.w	$e8,[r_animation+[15*r_heith*r_row]]/$10000
dc.w	$ea,[r_animation+[15*r_heith*r_row]]&$ffff

dc.l	$2601ff00,$01003300

dc.l	$e007fffe,$1800005
dc.l	$fe01ff00,$01000300
dc.l	-2
;---------------------------------------------------------------------
r_animation=$50000
r_animtab:	dc.b	0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15
		dc.b	15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0

r_animdata:	dc.w	4,0*2	,1,2	;actual frame, jump offset
		dc.w	30,34*2	,1,2
		dc.w	12,16*2	,1,4

r_licz:		dc.w	500
;---------------------------------------------------------------------
;variables for Makr_anim
r_angle:	dc.w	0
r_yadd:		dc.w	0
r_sinus:			;102 / 41
dc.b	$66,$66,$64,$61,$5E,$5A,$55,$50,$4C,$47,$42,$3E,$3B,$38,$36,$36
dc.b	$29,$29,$2A,$2D,$31,$35,$3B,$40,$46,$4C,$51,$57,$5B,$5F,$62,$63
;---------------------------------------------------------------------
r_jumpsin:		;(A=80, len=64)
	dc.w	$0000,$0003,$0007,$000B,$000F,$0013,$0016,$001A
	dc.w	$001E,$0021,$0025,$0028,$002B,$002E,$0031,$0034
	dc.w	$0037,$003A,$003C,$003E,$0041,$0043,$0045,$0046
	dc.w	$0048,$0049,$004A,$004B,$004C,$004D,$004D,$004D
	dc.w	$004D,$004D,$004D,$004C,$004C,$004B,$004A,$0049
	dc.w	$0047,$0045,$0044,$0042,$0040,$003D,$003B,$0038
	dc.w	$0036,$0033,$0030,$002D,$002A,$0026,$0023,$001F
	dc.w	$001C,$0018,$0014,$0011,$000D,$0009,$0005,$0001

;---------------------------------------------------------------------

end:
