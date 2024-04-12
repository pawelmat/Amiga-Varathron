
;		' GUY SPY ' fucken stupid game loader

org	$30000
load	$30000

q:
dc.b	"DOS",0
dc.l	0
dc.l	880

s:
	move.l	#514*512,$24(a1)		;ile
	move.l	#$28800,$28(a1)
	move.l	#100*512,$2c(a1)	;skad
	jsr	-456(a6)
	jmp	$28800


blk.l	1024,"SCT "
