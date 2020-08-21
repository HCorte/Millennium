C
C SUBROUTINE KWINLOS
C
C V05 08-JUN-2000 UXN IMPLICIT NONE added to KWINLOS1
C V04 09-SEP-1998 RXK Changed for new Kicker
C V03 03-OCT-1993 HXK Catch second (0r more) joker winning maps.
C V02 22-AUG-1993 GXA Released for Finland Dec Conversion / Oddset.
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE KWINLOS(TRABUF,DIVWON,WIN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*4 NUMS(7)
	INTEGER*4 DIV
	INTEGER*4 I,J
	INTEGER*4 NUM
	INTEGER*4 GIND
	INTEGER*4 KGAM
	INTEGER*4 MATCH
	INTEGER*4 WIN
	INTEGER*4 DIVWON
	INTEGER*4 KOFF			!Kicker Game Offset.
C
C
	KOFF = WIN			!Input Argument
	IF(KOFF.LT.0.OR.KOFF.GT.1) KOFF = 0
C
	WIN=0
	MATCH=0
	DIVWON=0
	KGAM=TRABUF(TWKGME)
	GIND=GNTTAB(GAMIDX,KGAM)
	NUM=TRABUF(TWKICK+KOFF)
	DO 10 I=1,7
	NUMS(I)=MOD(NUM,10)
	NUM=NUM/10
10	CONTINUE
C
C
	DO 100 DIV=1,LKKDIV(GIND)                    !100
        DO 95 J=1,3                                  !90
        IF(LKKMAT(J,DIV,GIND).EQ.0) GOTO 100         !100
	DO 90 I=1,7                                  !80
	IF(KMATCH(I,J,DIV,GIND).NE.9) GOTO 90        !80
	IF(NUMS(I).NE.KWINNUM(I,GIND)) GOTO 95       !90
	MATCH=MATCH+1
90	CONTINUE                                     !80
	IF(MATCH.NE.0) THEN
	  WIN=1
	  DIVWON=DIV
	  RETURN
	ENDIF
95      CONTINUE                                     !90
100	CONTINUE                                     !100
	RETURN
	END
