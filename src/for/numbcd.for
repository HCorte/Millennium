C
C SUBROUTINE NUMBCD
C $Log:   GXAFXT:[GOLS]NUMBCD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:15:30   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:10:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - numbcd.for **
C
C NUMBCD.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C V01 29-AUG-90 XXX RELEASED FOR EURO-SYSTEM
C
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE NUMBCD(GAME,POOL,NUM,BCDNUM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INTEGER*4 GAME, POOL, NUM, BCDNUM, N, I
	INTEGER*4 DIG(4)
C
C
C
	N=NUM
	DO 10 I=1,4
	DIG(I)=MOD(N,10)
	N=N/10
10	CONTINUE
C
C
	IF(GAME.EQ.NB4TYP) GOTO 1000
	IF(POOL.EQ.TNB3F2) BCDNUM=4096*DIG(2)+256*DIG(1)+15*16+15
	IF(POOL.EQ.TNB3B2) BCDNUM=4096*15+256*DIG(2)+16*DIG(1)+15
	IF(POOL.EQ.TNB3S2) BCDNUM=4096*DIG(2)+256*15+16*DIG(1)+15
	IF(POOL.GE.TNB3ST) BCDNUM=4096*DIG(3)+256*DIG(2)+16*DIG(1)+15
	RETURN
C
C
1000	CONTINUE
        IF(POOL.EQ.TNB4F2) BCDNUM=4096*DIG(2)+256*DIG(1)+15*16+15
        IF(POOL.EQ.TNB4B2) BCDNUM=4096*15+256*15+16*DIG(2)+DIG(1)
        IF(POOL.EQ.TNB4M2) BCDNUM=4096*15+256*DIG(2)+16*DIG(1)+15
	IF(POOL.GE.TNB4ST) BCDNUM=4096*DIG(4)+256*DIG(3)+16*DIG(2)+DIG(1)
	RETURN
	END
