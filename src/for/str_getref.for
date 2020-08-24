C
C SUBROUTINE STR_GETREF
C
C V03 03-JUL-2000 UXN Refund too late played tickets.
C V02 01-FEB-2000 UXN TNFRAC ADDED.
C V01 21-MAY-1999 UXN INITIAL RELEASE.
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE STR_GETREF(TRABUF,WINTAB,REFTAB,WIN,REFROWS,LATEFLG)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'

	LOGICAL*4 LATEFLG
	INTEGER*4 WINTAB(25)
	INTEGER*4 REFTAB(25)
	INTEGER*4 GIND
 	INTEGER*4 ROW
 	INTEGER*4 ROW2
 	INTEGER*4 ROW3
	INTEGER*4 REFROWS
 	INTEGER*4 J
 	INTEGER*4 AMT
 	INTEGER*4 WIN
 	INTEGER*4 BETS(3,500)
	INTEGER*4 COUNT

C----------------------------- Start of Code --------------------------

	GIND = TRABUF(TGAMIND)
	REFROWS = 0
	CALL STSEXP(TRABUF,BETS,COUNT)

	AMT = TRABUF(TWSTAMT)
	IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT/TRABUF(TNFRAC)

	IF(LSTSTS(GIND) .EQ. GAMCAN .OR. LATEFLG) THEN
	   WIN = WIN + 1
	   WINTAB(WIN) = COUNT*AMT
	   REFTAB(WIN) = 1
	   REFROWS = COUNT
	   IF(LATEFLG) THEN
              LSTLAT(LATCNT,GIND) = LSTLAT(LATCNT,GIND) + 1
	      LSTLAT(LATAMT,GIND) = LSTLAT(LATAMT,GIND) + COUNT*AMT
	   ENDIF
	   RETURN
	ENDIF

C---- Check if ticket already refunded

	DO 100 J = 1,COUNT
	   ROW  = BETS(1,J)
	   ROW2 = BETS(2,J)
	   ROW3 = BETS(3,J)

C---- Work of refunds.

	   IF(LSTSTA(ROW,GIND)  .EQ. GAMCAN .OR.
     *        LSTSTA(ROW2,GIND) .EQ. GAMCAN .OR.
     *        LSTSTA(ROW3,GIND) .EQ. GAMCAN) THEN
	      REFROWS = REFROWS + 1
	    ENDIF

100	CONTINUE

	IF(REFROWS.GT.0) THEN
	   WIN = WIN + 1
	   WINTAB(WIN) = REFROWS*AMT
	   REFTAB(WIN) = 1
        ENDIF
	RETURN

	END
