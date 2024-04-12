

;Geralt + transforming points - code by PILLAR & KANE/SUSPECT 9.04.93
;--------------------------------------------------------------------

exe=0

raster:	macro
	cmpi.b	#$ff,6(a0)
	bne.s	*-8
	cmpi.b	#$ff,6(a0)
	beq.s	*-8
	endm

org	$48000
load	$48000

s:
		lea	$dff000,a0
if exe=0
		move	#$4000,$9a(a0)
endif
		move	#$20,$96(a0)
		raster
		move.l	#q_cop0,$80(a0)

		moveq	#10,d7
q_waitfo:	raster
		dbf	d7,q_waitfo

q_wait:		btst	#15,4(a0)		;czy long frame? (1)
		beq.s	q_wait
		move.l	#q_copper2,$80(a0)

		bsr.s	qe_set
		move	#350,d7
q_waitfo2:	raster
		dbf	d7,q_waitfo2
		bsr.L	qa_fade

		raster
		move.l	#q_cop0,$80(a0)
		move	#0,$88(a0)
		raster
		bsr	px_dotsphere

if exe=0
		move	#$c000,$9a(a0)
endif
		move	#$8020,$96(a0)
		rts

;----------------------------------------------------------------------

qe_set:		move	#17,d0
qe_setcol:	lea	q_copper1(pc),a1
		lea	q_copper2(pc),a3
		lea	q_coltab(pc),a2
		moveq	#7,d3			;color nr. - 1
qe_scol1:	move	(a2),d1
		andi	#$f,d1
		move	2(a1),d2
		andi	#$f,d2
		cmpi	d1,d2
		beq.s	qe_scol2
		addi	#1,2(a1)
		addi	#1,2(a3)
qe_scol2:	move	(a2),d1
		andi	#$f0,d1
		move	2(a1),d2
		andi	#$f0,d2
		cmpi	d1,d2
		beq.s	qe_scol3
		addi	#$10,2(a1)
		addi	#$10,2(a3)
qe_scol3:	move	(a2)+,d1
		andi	#$f00,d1
		move	2(a1),d2
		andi	#$f00,d2
		cmpi	d1,d2
		beq.s	qe_scol4
		addi	#$100,2(a1)
		addi	#$100,2(a3)
qe_scol4:	addi.l	#4,a1
		addi.l	#4,a3
		dbf	d3,qe_scol1
		raster
		raster
		raster
		raster
		dbf	d0,qe_setcol
		rts

qa_fade:	move	#19,d0
qa_fadcol:	lea	q_copper1(pc),a1
		lea	q_copper2(pc),a2
		moveq	#7,d3			;no. of colors - 1
qa_fad1:	move	2(a1),d1
		andi	#$f,d1
		beq.s	qa_fad2
		subi	#1,2(a1)
		subi	#1,2(a2)
qa_fad2:	move	2(a1),d1
		andi	#$f0,d1
		beq.s	qa_fad3
		subi	#$10,2(a1)
		subi	#$10,2(a2)
qa_fad3:	move	2(a1),d1
		andi	#$f00,d1
		beq.s	qa_fad4
		subi	#$100,2(a1)
		subi	#$100,2(a2)
qa_fad4:	addi.l	#4,a1
		addi.l	#4,a2
		dbf	d3,qa_fad1
		raster
		raster
		raster
		dbf	d0,qa_fadcol
		rts

;----------------------------------------------------------------------
;----------------------------------------------------------------------

q_heigth=416			;wymiary obrazka
q_row=68
q_bpl=q_heigth*q_row			;wymiary - 1 bitplan
;----------------------------------------------------------------------
q_coltab:
dc.w	0,$fff,$321,$543
dc.w	$865,$a97,$dcb,$fff

q_cop0:	dc.l	$1000304,$1800000,-2

q_copper1:
dc.l	$1800000,$1820000,$1840000,$1860000
dc.l	$1880000,$18a0000,$18c0000,$18e0000

dc.l	$920048,$9400c8,$8e0171,$9037d1
dc.l	$1020000,$1080000+q_row,$10a0000+q_row	;modulo interlace'u

dc.l	$e00000+[q_obrazek/$10000],$e20000+[q_obrazek&$ffff]	;ustaw plany
dc.l	$e40000+[[q_obrazek+q_bpl]/$10000],$e60000+[[q_obrazek+q_bpl]&$ffff]
dc.l	$e80000+[[q_obrazek+[2*q_bpl]]/$10000],$ea0000+[[q_obrazek+[2*q_bpl]]&$ffff]

dc.l	$4001ff00,$0100b304			;obraz on + lace
dc.l	$ffdffffe
dc.l	$1001ff00,$01000304
dc.w	$80,q_copper2/$10000,$82,q_copper2&$ffff	;ust.drugi copperlist
dc.l	-2

q_copper2:
dc.l	$1800000,$1820000,$1840000,$1860000
dc.l	$1880000,$18a0000,$18c0000,$18e0000

dc.l	$920048,$9400c8,$8e0171,$9037d1
dc.l	$1020000,$1080000+q_row,$10a0000+q_row

dc.l	$e00000+[q_obrazek+q_row/$10000],$e20000+[[q_obrazek+q_row]&$ffff]
dc.l	$e40000+[[q_obrazek+q_row+q_bpl]/$10000],$e60000+[[q_obrazek+q_row+q_bpl]&$ffff]
dc.l	$e80000+[[q_obrazek+q_row+[2*q_bpl]]/$10000],$ea0000+[[q_obrazek+q_row+[2*q_bpl]]&$ffff]

dc.l	$4001ff00,$0100b304
dc.l	$ffdffffe
dc.l	$1001ff00,$01000304
dc.w	$80,q_copper1/$10000,$82,q_copper1&$ffff
dc.l	-2

;----------------------------------------------------------------------

;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
; TRAILED DOT SPHERE .
; CODING ON 14.III.93. BY PILLAR OF SUSPECT .
; SPECIALY PREPARED TO NEW SUSPECT DEMO. 
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
waitblt2:MACRO
	BTST	#$E,$02(A6)
	BNE.S	*-8
	ENDM

VBLANK:	MACRO
	CMPI.B	#$FF,$06(A6)
	BNE.S	*-8
	CMPI.B	#$FF,$06(A6)
	BEQ.S	*-8
	ENDM

px_dotsphere:
	MOVEM.L	D0-D7/A0-A6,-(A7)
	LEA	$DFF000,A6
	MOVE	#$0020,$96(A6)
	MOVE	#$83C0,$96(A6)
	MOVE.L	#PX_COPPER,$80(A6)
	BSR	PX_INIT
	LEA	PX_TABLE,A0
	MOVE.L	(A0)+,PX_INIT_3D
	MOVE.L	(A0)+,PX_NUM
	waitblt2
	MOVE.L	#$01000000,$40(A6)
	MOVE	#$0,$66(A6)

	moveq	#20,d7
px_logon:vblank
	vblank
	vblank
	bsr	pe_setcol1
	dbf	d7,px_logon


	MOVE.L	#16,PX_CON
PX_LOOP0:VBLANK
	ADDQ	#8,PX_YANGLE
	ANDI	#$3FF,PX_YANGLE
	ADDQ	#6,PX_XANGLE
	ANDI	#$3FF,PX_XANGLE
	BSR	PX_CHANGESCREEN
	BSR	PX_MAIN
	VBLANK
	ADDQ	#8,PX_YANGLE
	ANDI	#$3FF,PX_YANGLE
	ADDQ	#6,PX_XANGLE
	ANDI	#$3FF,PX_XANGLE
	BSR	PX_CHANGESCREEN
	BSR	PX_MAIN
	VBLANK
	ADDQ	#8,PX_YANGLE
	ANDI	#$3FF,PX_YANGLE
	ADDQ	#6,PX_XANGLE
	ANDI	#$3FF,PX_XANGLE
	BSR	PX_CHANGESCREEN
	BSR	PX_MAIN
	bsr	se_setcol1
	SUBQ.L	#1,PX_CON
	BNE.L	PX_LOOP0


	MOVE.L	#250,PX_CON
PX_LOOP:VBLANK
	ADDQ	#8,PX_YANGLE
	ANDI	#$3FF,PX_YANGLE
	ADDQ	#6,PX_XANGLE
	ANDI	#$3FF,PX_XANGLE
	BSR	PX_CHANGESCREEN
	BSR	PX_MAIN
	SUBQ.L	#1,PX_CON
	BNE.S	PX_LOOP

	LEA	PX_PIX3P,A0
	LEA	PX_WORK1,A1
	MOVEQ	#90,D7
	BSR	PX_ROTTEN
	LEA	PX_PIX2P,A0
	LEA	PX_WORK2,A1
	MOVEQ	#90,D7
	BSR	PX_ROTTEN
	LEA	PX_WORK1+$222,A0
	LEA	PX_WORK1,A1
	MOVEQ	#90,D7
	BSR	PX_NGT
	LEA	PX_WORK2+$222,A0
	LEA	PX_WORK2,A1
	MOVEQ	#90,D7
	BSR	PX_NGT
	LEA	PX_WORK1,A0
	LEA	PX_WORK3,A1
	MOVE.L	#181,D7
	BSR	PX_TWOD
	LEA	PX_WORK2,A0
	LEA	PX_WORK4,A1
	MOVE.L	#181,D7
	BSR	PX_TWOD
PX_LOOP2:
	VBLANK
	BSR	PX_CHANGESCREEN
	LEA	PX_WORK3,A0
	LEA	PX_WORK4,A1
	MOVE.L	PX_PLANE,A2
	MOVE.L	#181,D7
	BSR	PX_META
	ADDQ	#2,PX_STEP
	CMPI	#62,PX_STEP
	BNE.S	PX_LOOP2
	MOVE	#$0,PX_STEP

	LEA	PX_TABLE2,A0
	MOVE.L	(A0)+,PX_INIT_3D
	MOVE.L	(A0)+,PX_NUM
	MOVE.L	#250,PX_CON
PX_LOOP3:
	VBLANK
	ADDQ	#6,PX_YANGLE
	ANDI	#$3FF,PX_YANGLE
	ADDQ	#8,PX_XANGLE
	ANDI	#$3FF,PX_XANGLE
	BSR.L	PX_CHANGESCREEN
	BSR	PX_MAIN
	SUBQ.L	#1,PX_CON
	BNE.S	PX_LOOP3


	LEA	PX_PIX2P,A0
	LEA	PX_WORK1,A1
	MOVEQ	#90,D7
	BSR	PX_ROTTEN
	LEA	PX_PIX1P,A0
	LEA	PX_WORK2,A1
	MOVEQ	#89,D7
	BSR	PX_ROTTEN
	LEA	PX_WORK1+$222,A0
	LEA	PX_WORK1,A1
	MOVEQ	#90,D7
	BSR	PX_NGT
	LEA	PX_WORK2+$21C,A0
	LEA	PX_WORK2,A1
	MOVEQ	#89,D7
	BSR	PX_NGT
	LEA	PX_WORK1,A0
	LEA	PX_WORK3,A1
	MOVE.L	#181,D7
	BSR	PX_TWOD
	LEA	PX_WORK2,A0
	LEA	PX_WORK4,A1
	MOVE.L	#179,D7
	BSR	PX_TWOD
PX_LOOP4:
	VBLANK
	BSR	PX_CHANGESCREEN
	LEA	PX_WORK3,A0
	LEA	PX_WORK4,A1
	MOVE.L	PX_PLANE,A2
	MOVE.L	#181,D7
	BSR	PX_META
	ADDQ	#2,PX_STEP
	CMPI	#62,PX_STEP
	BNE.S	PX_LOOP4
	MOVE	#$0,PX_STEP


	LEA	PX_TABLE3,A0
	MOVE.L	(A0)+,PX_INIT_3D
	MOVE.L	(A0)+,PX_NUM
	MOVE.L	#250,PX_CON
PX_LOOP5:
	VBLANK
	ADDQ	#6,PX_YANGLE
	ANDI	#$3FF,PX_YANGLE
	ADDQ	#8,PX_XANGLE
	ANDI	#$3FF,PX_XANGLE
	BSR.L	PX_CHANGESCREEN
	BSR	PX_MAIN
	SUBQ.L	#1,PX_CON
	BNE.S	PX_LOOP5


	LEA	PX_PIX1P,A0
	LEA	PX_WORK1,A1
	MOVEQ	#89,D7
	BSR	PX_ROTTEN
	LEA	PX_PIX0P,A0
	LEA	PX_WORK2,A1
	MOVEQ	#89,D7
	BSR	PX_ROTTEN
	LEA	PX_WORK1+$21C,A0
	LEA	PX_WORK1,A1
	MOVEQ	#90,D7
	BSR	PX_NGT
	LEA	PX_WORK2+$21C,A0
	LEA	PX_WORK2,A1
	MOVEQ	#89,D7
	BSR	PX_NGT
	LEA	PX_WORK1,A0
	LEA	PX_WORK3,A1
	MOVE.L	#179,D7
	BSR	PX_TWOD
	LEA	PX_WORK2,A0
	LEA	PX_WORK4,A1
	MOVE.L	#179,D7
	BSR	PX_TWOD
PX_LOOP6:
	VBLANK
	BSR	PX_CHANGESCREEN
	LEA	PX_WORK3,A0
	LEA	PX_WORK4,A1
	MOVE.L	PX_PLANE,A2
	MOVE.L	#179,D7
	BSR	PX_META
	ADDQ	#2,PX_STEP
	CMPI	#62,PX_STEP
	BNE.S	PX_LOOP6
	MOVE	#$0,PX_STEP


	LEA	PX_TABLE4,A0
	MOVE.L	(A0)+,PX_INIT_3D
	MOVE.L	(A0)+,PX_NUM
	MOVE.L	#250,PX_CON
PX_LOOP7:
	VBLANK
	ADDQ	#6,PX_YANGLE
	ANDI	#$3FF,PX_YANGLE
	ADDQ	#8,PX_XANGLE
	ANDI	#$3FF,PX_XANGLE
	BSR.L	PX_CHANGESCREEN
	BSR	PX_MAIN
	SUBQ.L	#1,PX_CON
	BNE.S	PX_LOOP7

	MOVE.L	#20,PX_CON
PX_LOOP70:
	VBLANK
	ADDQ	#6,PX_YANGLE
	ANDI	#$3FF,PX_YANGLE
	ADDQ	#8,PX_XANGLE
	ANDI	#$3FF,PX_XANGLE
	BSR.L	PX_CHANGESCREEN
	BSR	PX_MAIN
	VBLANK
	ADDQ	#6,PX_YANGLE
	ANDI	#$3FF,PX_YANGLE
	ADDQ	#8,PX_XANGLE
	ANDI	#$3FF,PX_XANGLE
	BSR.L	PX_CHANGESCREEN
	BSR	PX_MAIN
	VBLANK
	ADDQ	#6,PX_YANGLE
	ANDI	#$3FF,PX_YANGLE
	ADDQ	#8,PX_XANGLE
	ANDI	#$3FF,PX_XANGLE
	BSR.L	PX_CHANGESCREEN
	BSR	PX_MAIN
	bsr	fa_fadcol1
	bsr	pa_fadcol1
	SUBQ.L	#1,PX_CON
	BNE.L	PX_LOOP70

	move.l	#whitecop,$dff080

	MOVEM.L	(A7)+,D0-D7/A0-A6
	RTS
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн

se_setcol1:	movem.l	a1/a2/d1-d3,-(sp)
		lea	p_copper1,a1
		lea	p_coltab1,a2
		moveq	#14,d3				;color nr. - 1
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
		movem.l	(sp)+,a1/a2/d1-d3
		rts

fa_fadcol1:	movem.l	a1/d1/d3,-(sp)
		lea	p_copper1(pc),a1
		moveq	#14,d3			;no. of colors - 1
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
		cmpi	#$f00,d1
		beq.s	fa_fad4
		addi	#$100,2(a1)
fa_fad4:	addi.l	#4,a1
		dbf	d3,fa_fad1
		movem.l	(sp)+,a1/d1/d3
		rts

;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
pe_setcol1:	movem.l	a1/a2/d1-d3,-(sp)
		lea	p_copper0(pc),a1
		lea	p_coltab0(pc),a2
		moveq	#31,d3				;color nr. - 1
pe_scol1:	move	(a2),d1
		andi	#$f,d1
		move	2(a1),d2
		andi	#$f,d2
		cmpi	d1,d2
		beq.s	pe_scol2
		addi	#1,2(a1)
pe_scol2:	move	(a2),d1
		andi	#$f0,d1
		move	2(a1),d2
		andi	#$f0,d2
		cmpi	d1,d2
		beq.s	pe_scol3
		addi	#$10,2(a1)
pe_scol3:	move	(a2)+,d1
		andi	#$f00,d1
		move	2(a1),d2
		andi	#$f00,d2
		cmpi	d1,d2
		beq.s	pe_scol4
		addi	#$100,2(a1)
pe_scol4:	addi.l	#4,a1
		dbf	d3,pe_scol1
		movem.l	(sp)+,a1/a2/d1-d3
		rts

pa_fadcol1:	movem.l	a1/d1/d3,-(sp)
		lea	p_copper0(pc),a1
		moveq	#31,d3			;no. of colors - 1
pa_fad1:	move	2(a1),d1
		andi	#$f,d1
		beq.s	pa_fad2
		subi	#1,2(a1)
pa_fad2:	move	2(a1),d1
		andi	#$f0,d1
		beq.s	pa_fad3
		subi	#$10,2(a1)
pa_fad3:	move	2(a1),d1
		andi	#$f00,d1
		cmpi	#$f00,d1
		beq.s	pa_fad4
		addi	#$100,2(a1)
pa_fad4:	addi.l	#4,a1
		dbf	d3,pa_fad1
		movem.l	(sp)+,a1/d1/d3
		rts

whitecop:
dc.l	$1000300,$1800f00,-2

;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн

PX_CHANGESCREEN:
	MOVE	PX_SCRPNT,D0
	BPL.S	PX_CHG2
	MOVE	#4,PX_SCRPNT
	MOVEQ	#4,D0
PX_CHG2:MOVEQ	#3,D1
	LEA	PX_PL+2,A1
PX_CHG3:MOVE	D0,D2
	MULU	#$2800,D2
	ADD.L	#$70000+[10*p_row],D2
	MOVE	D2,4(A1)
	SWAP	D2
	MOVE	D2,(A1)
	ADDQ	#8,A1
	ADDQ	#1,D0
	CMP	#5,D0
	BNE.S	PX_CHG4
	MOVEQ	#0,D0
PX_CHG4:DBF	D1,PX_CHG3
	MULU	#$2800,D0
	ADD.L	#$70000,D0
	MOVE.L	D0,PX_PLANE
	SUBQ	#1,PX_SCRPNT
	addi.l	#10*p_row,d0
	waitblt2
	MOVE.L	D0,$54(A6)
	MOVE	#$3800+[p_row/2],$58(A6)
	waitblt2
	RTS
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
; PETLA OBRACAJACA .

PX_MAIN:MOVE.L	PX_INIT_3D,A0
	LEA	PX_SIN,A2
	LEA	PX_SIN+$100,A3
	LEA	PX_YPOSES,A4
	MOVE.L	PX_PLANE,A5
	MOVE	PX_XANGLE,D5
	MOVE	PX_YANGLE,D6
	MOVE.L	PX_NUM,D7
PX_ROT:	MOVEM	(A0)+,D0-D2
	MOVE	D0,D3
	MOVE	D2,D4
	MULS	0(A3,D6.w),D3
	MULS	0(A2,D6.w),D4
	SUB.L	D4,D3
	ADD.L	D3,D3
	SWAP	D3
	MULS	0(A2,D6.W),D0
	MULS	0(A3,D6.W),D2
	ADD.L	D0,D2
	ADD.L	D2,D2
	SWAP	D2
	MOVE	D3,D0
	MOVE	D1,D3
	MOVE	D2,D4
	MULS	0(A3,D5.W),D3
	MULS	0(A2,D5.W),D4
	ADD.L	D4,D3
	ADD.L	D3,D3
	SWAP	D3
	MULS	0(A2,D5.W),D1
	MULS	0(A3,D5.W),D2
	SUB.L	D1,D2
	ADD.L	D2,D2
	SWAP	D2
	MOVEM	D0/D2/D3,-(A7)
	SUB	#$100,D2
	MULS	D2,D0
	MULS	D2,D3
	ASR	#8,D0
	ASR	#8,D3
	ADD	#p_row*4,D0
	ADD	#128,D3
	MOVE	D0,D2
	LSR	#3,D0
	ADD	D3,D3
	MOVE	0(A4,D3.W),D3
	ADD	D3,D0
	NOT	D2
	BSET.B	D2,0(A5,D0.W)
	MOVEM	(A7)+,D0/D2/D3
	NEG	D0
	NEG	D2
	NEG	D3
	SUB	#$100,D2
	MULS	D2,D0
	MULS	D2,D3
	ASR	#8,D0
	ASR	#8,D3
	ADD	#p_row*4,D0
	ADD	#128,D3
	MOVE	D0,D2
	LSR	#3,D0
	ADD	D3,D3
	MOVE	0(A4,D3.W),D3
	ADD	D3,D0
	NOT	D2
	BSET.B	D2,0(A5,D0.W)
	DBF	D7,PX_ROT
	RTS
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
; Dazenie punktu do punktu / PILLAR OF SUSPECT /
; a0 - pierwsza tablica punktow
; a1 - druga tablica punktow
; a2 - adres ekranu do wyswietlenia
; d7 - liczba punktow metarphozowanych

PX_META:LEA	PX_YPOSES,A3
	LEA	PX_STEPEN,A4
	MOVE	PX_STEP,D4
PX_DEST:MOVEM	(A0)+,D0-D1
	MOVEM	(A1)+,D2-D3
	SUB	D0,D2
	SUB	D1,D3
	MULS	0(A4,D4.W),D2
	ASR	#7,D2
	MULS	0(A4,D4.W),D3
	ASR	#7,D3
	ADD	D2,D0
	ADD	D3,D1
	ADD	D1,D1
	MOVE	0(A3,D1.W),D1
	MOVE	D0,D2
	LSR	#3,D2
	ADD	D2,D1
	NOT	D0
	BSET.B	D0,0(A2,D1.W)
	DBF	D7,PX_DEST
	RTS
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
PX_TWOD:MOVEM	(A0)+,D0-D2
	SUB	#$100,D2
	MULS	D2,D0
	ASR	#8,D0
	MULS	D2,D1
	ASR	#8,D1
	ADD	#p_row*4,D0
	ADD	#128,D1
	MOVE	D0,(A1)+
	MOVE	D1,(A1)+
	DBF	D7,PX_TWOD
	RTS
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
PX_NGT:	MOVEM	(A1)+,D0-D2
	NEG	D0
	NEG	D1
	NEG	D2
	MOVE	D0,(A0)+
	MOVE	D1,(A0)+
	MOVE	D2,(A0)+
	DBF	D7,PX_NGT
	RTS
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
; OBROT OBIEKTU O KAT .

PX_ROTTEN:
	LEA	PX_SIN,A2
	LEA	PX_SIN+$100,A3
	MOVE	PX_XANGLE,D5
	MOVE	PX_YANGLE,D6
PX_RN1:	MOVEM	(A0)+,D0-D2
	MOVE	D0,D3
	MOVE	D2,D4
	MULS	0(A3,D6.w),D3
	MULS	0(A2,D6.w),D4
	SUB.L	D4,D3
	ADD.L	D3,D3
	SWAP	D3
	MULS	0(A2,D6.W),D0
	MULS	0(A3,D6.W),D2
	ADD.L	D0,D2
	ADD.L	D2,D2
	SWAP	D2
	MOVE	D3,D0
	MOVE	D1,D3
	MOVE	D2,D4
	MULS	0(A3,D5.W),D3
	MULS	0(A2,D5.W),D4
	ADD.L	D4,D3
	ADD.L	D3,D3
	SWAP	D3
	MULS	0(A2,D5.W),D1
	MULS	0(A3,D5.W),D2
	SUB.L	D1,D2
	ADD.L	D2,D2
	SWAP	D2
	MOVE	D0,(A1)+
	MOVE	D3,(A1)+
	MOVE	D2,(A1)+
	DBF	D7,PX_RN1
	RTS
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
; INICJACJA BRYL DO OBROTU I KILKU TABLIC . 

PX_INIT:LEA	PX_YPOSES,A0
	MOVEQ	#0,D0
	MOVEQ	#p_row,D1
	MOVE.L	#255,D2
PX_IN1:	MOVE	D0,(A0)+
	ADD	D1,D0
	DBF	D2,PX_IN1
	LEA	$70000,A0
	LEA	$7FFF0,A1
	MOVEQ	#0,D0
PX_IN2:	MOVE.L	D0,(A0)+
	CMPA.L	A1,A0
	BNE.S	PX_IN2

	LEA	PX_CIRCLE,A0
	LEA	PX_SIN,A1
	LEA	PX_SIN+$100,A2
	MOVEQ	#12,D7
	MOVEQ	#8,D6
PX_IN3:	MOVEQ	#70,D0
	MOVEQ	#70,D1
	MULS	0(A2,D6.W),D0
	ADD.L	D0,D0
	SWAP	D0
	MULS	0(A1,D6.W),D1
	ADD.L	D1,D1
	SWAP	D1
	MOVE	D0,(A0)+
	MOVE	D1,(A0)+
	ADD	#40,D6
	DBF	D7,PX_IN3

; OPIS SZESCIANU DO OBROTU.

	LEA	PX_PIX3P,A0
	MOVEQ	#-60,D0
	MOVEQ	#-60,D1
	MOVEQ	#-60,D2
	MOVEQ	#24,D3
PX_CB2:	MOVE	D0,(A0)+
	MOVE	D1,(A0)+
	MOVE	D2,(A0)+
	ADD	D3,D0
	CMP	#59,D0
	BLE.S	PX_CB2
	MOVEQ	#-60,D0
	ADD	D3,D1
	CMP	#59,D1
	BLE.S	PX_CB2

	MOVEQ	#-60,D0
	MOVEQ	#-60,D1
	MOVEQ	#-60,D2
	MOVEQ	#59,D4
PX_CB3:	MOVE	D0,(A0)+
	MOVE	D1,(A0)+
	MOVE	D2,(A0)+
	MOVE	D0,(A0)+
	MOVE	D4,(A0)+
	MOVE	D2,(A0)+
	ADD	D3,D0
	CMP	#83,D0
	BLE.S	PX_CB3
	MOVEQ	#-60,D0
	ADD	D3,D2
	TST	D2
	BLE.S	PX_CB3

	MOVEQ	#-60,D0
	MOVEQ	#-60,D1
	MOVEQ	#-60,D2
PX_CB4:	MOVE	D0,(A0)+
	MOVE	D1,(A0)+
	MOVE	D2,(A0)+
	MOVE	D4,(A0)+
	MOVE	D1,(A0)+
	MOVE	D2,(A0)+
	ADD	D3,D1
	CMP	#59,D1
	BLE.S	PX_CB4
	MOVEQ	#-60,D1
	ADD	D3,D2
	TST	D2
	BLE.S	PX_CB4

; OPIS WALCA DO OBROTU .

	LEA	PX_PIX2P,A0
	LEA	PX_CIRCLE,A1
	LEA	PX_CIRCLE+52,a2
	MOVEQ	#-70,D2
	MOVEQ	#28,D3
PX_CB5:	MOVEM	(A1)+,D0/D1
	MOVE	D0,(A0)+
	MOVE	D1,(A0)+
	MOVE	D2,(A0)+
	CMPA.L	A2,A1
	BNE.S	PX_CB5
	LEA	PX_CIRCLE,A1
	ADD	D3,D2
	CMP	#70,D2
	BLE.S	PX_CB5

	LEA	PX_SIN,A1
	LEA	PX_SIN+$100,A2
	MOVEQ	#-70,D2
	MOVEQ	#28,D3
PX_CB7:	MOVEQ	#40,D6
	MOVEQ	#7,D7
PX_CB6:	MOVEQ	#32,D0
	MOVEQ	#32,D1
	MULS	0(A2,D6.W),D0
	ADD.L	D0,D0
	SWAP	D0
	MULS	0(A1,D6.W),D1
	ADD.L	D1,D1
	SWAP	D1
	MOVE	D0,(A0)+
	MOVE	D1,(A0)+
	MOVE	D2,(A0)+
	NEG	D1
	MOVE	D0,(A0)+
	MOVE	D1,(A0)+
	MOVE	D2,(A0)+
	NEG	D1
	ADD	#80,D6
	DBF	D7,PX_CB6
	ADD	D3,D2
	CMP	#70,D2
	BLE.S	PX_CB7

; OPIS KIELISZKA DWU-STRONNEGO

	LEA	PX_PIX1P,A0
	LEA	PX_SHAPE,A1
	LEA	PX_SHAPE+60,A2
	LEA	PX_SIN,A3
	LEA	PX_SIN+$100,A4
	MOVEQ	#40,D6
	MOVEQ	#5,D7
PX_GL1:	MOVEM	(A1)+,D0/D2
	MOVEQ	#0,D1
	MOVE	D0,D3
	MOVE	D1,D4
	MULS	0(A4,D6.W),D3
	MULS	0(A3,D6.W),D4
	SUB.L	D4,D3
	ADD.L	D3,D3
	SWAP	D3
	MULS	0(A3,D6.W),D0
	MULS	0(A4,D6.W),D1
	ADD.L	D0,D1
	ADD.L	D1,D1
	SWAP	D1
	MOVE	D3,(A0)+
	MOVE	D1,(A0)+
	MOVE	D2,(A0)+
	CMPA.L	A2,A1
	BNE.S	PX_GL1
	LEA	PX_SHAPE,A1
	ADD	#80,D6
	DBF	D7,PX_GL1

; OPIS KIELISZKA NR 2

	LEA	PX_PIX0P,A0
	LEA	PX_CIRCLE2,A1
	LEA	PX_CIRCLE2+32,A2
	LEA	PX_SIN,A3
	LEA	PX_SIN+$100,A4
	MOVEQ	#10,D6
	MOVEQ	#10,D7
PX_GL2:	MOVEM	(A1)+,D0/D2
	SUB	#65,D0
	MOVEQ	#0,D1
	MOVE	D0,D3
	MOVE	D1,D4
	MULS	0(A4,D6.W),D3
	MULS	0(A3,D6.W),D4
	SUB.L	D4,D3
	ADD.L	D3,D3
	SWAP	D3
	MULS	0(A3,D6.W),D0
	MULS	0(A4,D6.W),D1
	ADD.L	D0,D1
	ADD.L	D1,D1
	SWAP	D1
	MOVE	D3,(A0)+
	MOVE	D1,(A0)+
	MOVE	D2,(A0)+
	CMPA.L	A2,A1
	BNE.S	PX_GL2
	LEA	PX_CIRCLE2,A1
	ADD	#46,D6
	DBF	D7,PX_GL2
	RTS
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
PX_PIX3P:	EQU	$6FB50
PX_PIX2P:	EQU	$6F6A0
PX_PIX1P:	EQU	$6F1F0
PX_PIX0P:	EQU	$6ED40
PX_WORK1:	EQU	$6E3E0
PX_WORK2:	EQU	$6DA80
PX_WORK3:	EQU	$6D120
PX_WORK4:	EQU	$6C7C0
PX_INIT_3D:	DC.L	0
PX_YANGLE:	DC.W	0
PX_XANGLE:	DC.W	0
PX_STEP:	DC.W	0
PX_NUM:		DC.L	0
PX_CON:		DC.L	0
PX_SCRPNT:	DC.W	3
PX_PLANE:	DC.L	$75000
PX_YPOSES:	DS.W	256

l_heith=44
l_row=40

p_row=32

p_coltab0:			;32
dc.w	$0102 ,$0fff ,$0fee ,$0fed,$0fdc ,$0fdb ,$0fca ,$0fc9
dc.w	$0fb8 ,$0fa7 ,$0fa6 ,$0f95,$0f94 ,$0f83 ,$0f82 ,$0f71
dc.w	$0e61 ,$0e51 ,$0d51 ,$0c41,$0c31 ,$0b32 ,$0a22 ,$0921
dc.w	$0911 ,$0812 ,$0712 ,$0712,$0612 ,$0512 ,$0512 ,$0412

p_coltab1:			;16
DC.W $00EE,$00EE,$00EE,$00AA,$00AA,$00AA,$00AA
DC.W $0066,$0066,$0066,$0066,$0066,$0066,$0066,$0066

PX_COPPER:
p_copper0:			;32
	DC.W $0180,$0000,$0182,$0000,$0184,$0000,$0186,$0000
	DC.W $0188,$0000,$018A,$0000,$018C,$0000,$018E,$0000
	DC.W $0190,$0000,$0192,$0000,$0194,$0000,$0196,$0000
	DC.W $0198,$0000,$019A,$0000,$019C,$0000,$019E,$0000
	DC.W $01a0,$0000,$01a2,$0000,$01a4,$0000,$01a6,$0000
	DC.W $01a8,$0000,$01aA,$0000,$01aC,$0000,$01aE,$0000
	DC.W $01b0,$0000,$01b2,$0000,$01b4,$0000,$01b6,$0000
	DC.W $01b8,$0000,$01bA,$0000,$01bC,$0000,$01bE,$0000

	DC.W $0092,$0038,$0094,$00D0

dc.w	$e0,l_logo/$10000,$e2,l_logo&$ffff
dc.w	$e4,l_logo+[l_heith*l_row]/$10000,$e6,l_logo+[l_heith*l_row]&$ffff
dc.w	$e8,l_logo+[2*l_heith*l_row]/$10000,$ea,l_logo+[2*l_heith*l_row]&$ffff
dc.w	$ec,l_logo+[3*l_heith*l_row]/$10000,$ee,l_logo+[3*l_heith*l_row]&$ffff
dc.w	$f0,l_logo+[4*l_heith*l_row]/$10000,$f2,l_logo+[4*l_heith*l_row]&$ffff

	dc.l	$2001ff00,$01005300
	dc.l	$4c01ff00,$01000300

p_copper1:			;16
	DC.W $0182,$0000,$0184,$0000,$0186,$0000
	DC.W $0188,$0000,$018A,$0000,$018C,$0000,$018E,$0000
	DC.W $0190,$0000,$0192,$0000,$0194,$0000,$0196,$0000
	DC.W $0198,$0000,$019A,$0000,$019C,$0000,$019E,$0000

	DC.W $0092,$0048,$0094,$00c0,$0102,$0000,$0104,$0000
	DC.W $0108,$0000,$010A,$0000,$008E,$2081,$0090,$34d1

PX_PL:	DC.W $00E0,$0007,$00E2,$0000,$00E4,$0007,$00E6,$0000
	DC.W $00E8,$0007,$00EA,$0000,$00EC,$0007,$00EE,$0000

	dc.l	$5001ff00,$01004300
	dc.l	$ffdffffe
	dc.l	$3001ff00,$01000300
	dc.l	-2
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
PX_TABLE:	DC.L	PX_PIX3P,90
PX_TABLE2:	DC.L	PX_PIX2P,90
PX_TABLE3:	DC.L	PX_PIX1P,89
PX_TABLE4:	DC.L	PX_PIX0P,87
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
PX_CIRCLE:	DS.W	26
PX_CIRCLE2:	DC.W	20,20, -20,-20, 20,-20, -20,20
		DC.W	0,32, 0,-32, 32,0, -32,0
PX_STEPEN:
	DC.W	$0004,$0008,$000C,$0010,$0014,$0018,$001C
	DC.W	$0020,$0024,$0028,$002C,$0030,$0034,$0038
	DC.W	$003C,$0040,$0044,$0048,$004C,$0050,$0054
	DC.W	$0058,$005C,$0060,$0064,$0068,$006C,$0070
	DC.W	$0074,$0078,$007C
PX_SHAPE:
	DC.W	-57,-80,-39,-80,-19,-80
	DC.W	-75,-80, -60,-60
	DC.W	-45,-40, -30,-20
	DC.W	-20,0
	DC.W	-30,20, -45,40, -60,60
	DC.W	-75,80, -57,80, -39,80, -19,80
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн
PX_SIN:	DC.W	$0000,$0192,$0325,$04B8,$064A,$07DD,$096F,$0B00
	DC.W	$0C92,$0E22,$0FB2,$1142,$12D1,$145F,$15EC,$1779
	DC.W	$1905,$1A8F,$1C19,$1DA1,$1F29,$20AF,$2234,$23B7
	DC.W	$253A,$26BA,$283A,$29B7,$2B33,$2CAE,$2E27,$2F9D
	DC.W	$3113,$3286,$33F7,$3566,$36D3,$383E,$39A7,$3B0E
	DC.W	$3C72,$3DD4,$3F34,$4091,$41EB,$4343,$4499,$45EC
	DC.W	$473C,$4889,$49D4,$4B1B,$4C60,$4DA2,$4EE1,$501D
	DC.W	$5155,$528B,$53BD,$54EC,$5618,$5741,$5866,$5987
	DC.W	$5AA6,$5BC0,$5CD7,$5DEB,$5EFB,$6007,$6110,$6214
	DC.W	$6315,$6413,$650C,$6601,$66F3,$67E0,$68C9,$69AF
	DC.W	$6A90,$6B6D,$6C46,$6D1B,$6DEB,$6EB8,$6F80,$7044
	DC.W	$7103,$71BE,$7274,$7327,$73D4,$747E,$7522,$75C2
	DC.W	$765E,$76F5,$7787,$7815,$789E,$7923,$79A3,$7A1E
	DC.W	$7A94,$7B06,$7B73,$7BDB,$7C3E,$7C9D,$7CF6,$7D4B
	DC.W	$7D9B,$7DE6,$7E2C,$7E6E,$7EAA,$7EE2,$7F14,$7F42
	DC.W	$7F6B,$7F8F,$7FAE,$7FC8,$7FDD,$7FED,$7FF8,$7FFE
	DC.W	$7FFF,$7FFC,$7FF3,$7FE5,$7FD3,$7FBB,$7F9F,$7F7D
	DC.W	$7F57,$7F2C,$7EFC,$7EC7,$7E8C,$7E4E,$7E0A,$7DC1
	DC.W	$7D74,$7D21,$7CCA,$7C6E,$7C0D,$7BA7,$7B3D,$7ACE
	DC.W	$7A5A,$79E1,$7963,$78E1,$785A,$77CF,$773F,$76AA
	DC.W	$7611,$7573,$74D0,$742A,$737E,$72CE,$721A,$7161
	DC.W	$70A4,$6FE2,$6F1C,$6E52,$6D84,$6CB1,$6BDA,$6AFF
	DC.W	$6A20,$693D,$6855,$676A,$667A,$6587,$6490,$6394
	DC.W	$6295,$6193,$608C,$5F81,$5E73,$5D62,$5C4C,$5B33
	DC.W	$5A17,$58F7,$57D4,$56AD,$5583,$5455,$5324,$51F0
	DC.W	$50B9,$4F7F,$4E42,$4D01,$4BBE,$4A78,$492F,$47E3
	DC.W	$4694,$4543,$43EE,$4298,$413E,$3FE2,$3E84,$3D23
	DC.W	$3BC0,$3A5B,$38F3,$3789,$361D,$34AF,$333E,$31CC
	DC.W	$3058,$2EE2,$2D6A,$2BF1,$2A75,$28F9,$277A,$25FA
	DC.W	$2479,$22F6,$2171,$1FEC,$1E65,$1CDD,$1B54,$19CA
	DC.W	$183F,$16B3,$1526,$1398,$1209,$107A,$0EEA,$0D5A
	DC.W	$0BC9,$0A37,$08A6,$0713,$0581,$03EE,$025C,$00C9
	DC.W	$FF37,$FDA4,$FC11,$FA7F,$F8EC,$F75A,$F5C8,$F437
	DC.W	$F2A6,$F115,$EF85,$EDF6,$EC68,$EADA,$E94D,$E7C1
	DC.W	$E636,$E4AC,$E322,$E19B,$E014,$DE8E,$DD0A,$DB87
	DC.W	$DA06,$D885,$D707,$D58A,$D40F,$D295,$D11D,$CFA7
	DC.W	$CE33,$CCC1,$CB51,$C9E3,$C877,$C70D,$C5A5,$C440
	DC.W	$C2DC,$C17C,$C01D,$BEC1,$BD68,$BC11,$BABD,$B96C
	DC.W	$B81D,$B6D1,$B588,$B441,$B2FE,$B1BE,$B081,$AF46
	DC.W	$AE0F,$ACDB,$ABAB,$AA7D,$A953,$A82C,$A709,$A5E9
	DC.W	$A4CC,$A3B3,$A29E,$A18C,$A07E,$9F74,$9E6D,$9D6A
	DC.W	$9C6B,$9B70,$9A79,$9985,$9896,$97AB,$96C3,$95E0
	DC.W	$9501,$9426,$934F,$927C,$91AE,$90E3,$901E,$8F5C
	DC.W	$8E9F,$8DE6,$8D32,$8C82,$8BD6,$8B2F,$8A8D,$89EF
	DC.W	$8956,$88C1,$8831,$87A5,$871F,$869C,$861F,$85A6
	DC.W	$8532,$84C3,$8459,$83F3,$8392,$8336,$82DF,$828C
	DC.W	$823F,$81F6,$81B2,$8173,$8139,$8104,$80D4,$80A9
	DC.W	$8083,$8061,$8045,$802D,$801B,$800D,$8004,$8001
	DC.W	$8002,$8008,$8013,$8023,$8038,$8052,$8071,$8095
	DC.W	$80BE,$80EC,$811E,$8156,$8192,$81D4,$821A,$8265
	DC.W	$82B5,$830A,$8364,$83C2,$8425,$848D,$84FA,$856C
	DC.W	$85E2,$865D,$86DD,$8762,$87EB,$8879,$890B,$89A2
	DC.W	$8A3E,$8ADE,$8B82,$8C2C,$8CD9,$8D8C,$8E42,$8EFD
	DC.W	$8FBC,$9080,$9148,$9215,$92E5,$93BA,$9493,$9570
	DC.W	$9651,$9737,$9820,$990D,$99FF,$9AF4,$9BED,$9CEB
	DC.W	$9DEC,$9EF0,$9FF9,$A105,$A215,$A329,$A440,$A55A
	DC.W	$A679,$A79A,$A8BF,$A9E8,$AB14,$AC43,$AD75,$AEAB
	DC.W	$AFE3,$B11F,$B25E,$B3A0,$B4E4,$B62C,$B777,$B8C4
	DC.W	$BA14,$BB67,$BCBC,$BE15,$BF6F,$C0CC,$C22C,$C38E
	DC.W	$C4F2,$C659,$C7C2,$C92D,$CA9A,$CC09,$CD7A,$CEED
	DC.W	$D062,$D1D9,$D352,$D4CC,$D648,$D7C6,$D945,$DAC6
	DC.W	$DC48,$DDCC,$DF51,$E0D7,$E25E,$E3E7,$E570,$E6FB
	DC.W	$E887,$EA13,$EBA1,$ED2F,$EEBE,$F04D,$F1DD,$F36E
	DC.W	$F4FF,$F691,$F823,$F9B5,$FB48,$FCDA,$FE6D,$0000
	DC.W	$0000,$0195,$032A,$04BF,$0654,$07E9,$097D,$0B11
	DC.W	$0CA5,$0E38,$0FCA,$115C,$12ED,$147E,$160D,$179C
	DC.W	$192A,$1AB7,$1C43,$1DCD,$1F57,$20DF,$2266,$23EC
	DC.W	$2570,$26F3,$2874,$29F4,$2B72,$2CEF,$2E69,$2FE2
	DC.W	$3159,$32CE,$3441,$35B2,$3721,$388D,$39F8,$3B60
	DC.W	$3CC6,$3E29,$3F8A,$40E9,$4245,$439E,$44F5,$4649
	DC.W	$479B,$48E9,$4A35,$4B7E,$4CC3,$4E06,$4F46,$5083
	DC.W	$51BC,$52F2,$5425,$5555,$5682,$57AB,$58D0,$59F2
	DC.W	$5B11,$5C2C,$5D43,$5E57,$5F67,$6073,$617C,$6281
	DC.W	$6382,$647F,$6578,$666D,$675E,$684B,$6934,$6A18
	DC.W	$6AF9,$6BD5,$6CAE,$6D82,$6E51,$6F1D,$6FE4,$70A6
	DC.W	$7165,$721E,$72D4,$7385,$7431,$74D9,$757C,$761A
	DC.W	$76B4,$7749,$77DA,$7866,$78ED,$7970,$79ED,$7A66
	DC.W	$7ADB,$7B4A,$7BB4,$7C1A,$7C7B,$7CD7,$7D2E,$7D80
	DC.W	$7DCD,$7E15,$7E59,$7E97,$7ED1,$7F05,$7F35,$7F5F
	DC.W	$7F85,$7FA5,$7FC1,$7FD7,$7FE9,$7FF5,$7FFD,$7FFF
;*нннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннннн

l_logo:				;8800
q_obrazek=l_logo+8800		;84864

>extern	"df0:.store/suspect_pillar.raw",l_logo,-1
>extern	"df0:.store/geralt.raw",q_obrazek,-1

end=q_obrazek+84864
