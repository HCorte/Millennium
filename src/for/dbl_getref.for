C
C SUBROUTINE DBL_GETREF
C
C V05 03-JUL-2000 UXN Refund too late played tickets.
C V04 01-FEB-2000 UXN TNFRAC added.
C V03 27-MAY-1999 UXN REFUNDS PUT TOGETHER.
C V02 21-JAN-1996 HXK Various fixes for Double / Couple for system bets and ties
C V01 28-NOV-1995 PXB Initial revision.
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE DBL_GETREF(TRABUF,WINTAB,REFTAB,WIN,REFROWS,LATEFLG)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'

	INTEGER*4 WINTAB(25)
	INTEGER*4 REFTAB(25)
	INTEGER*4 GIND
 	INTEGER*4 ROW
 	INTEGER*4 ROW2
 	INTEGER*4 I
 	INTEGER*4 J
 	INTEGER*4 AMT
 	INTEGER*4 WIN
 	INTEGER*4 BETS(2,100)
	INTEGER*4 COUNT
	INTEGER*4 REFAMT,REFROWS
	LOGICAL*4 LATEFLG
C----------------------------- Start of Code --------------------------

	GIND = TRABUF(TGAMIND)
	REFROWS = 0
	REFAMT  = 0
	CALL DSEXP(TRABUF,BETS,COUNT)

C---- Check for system bets.

	IF (TRABUF(TWSYST) .NE. NOSYS) GOTO 200

C---- Work of refunds.

	DO 110 I = 0,TRABUF(TWNBET)-1
	  ROW = TRABUF(TWDBROW1+I*TWDBBLEN)
	  ROW2 = TRABUF(TWDBROW2+I*TWDBBLEN)
	  IF (ROW .EQ. ROW2) GOTO 110
	  AMT = TRABUF(TWDBAMT+I*TWDBBLEN)
	  IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / TRABUF(TNFRAC)
	  IF (LDBSTS(GIND) .EQ. GAMCAN .OR. LATEFLG) THEN
            REFROWS = REFROWS + 1
	    REFAMT  = REFAMT  + AMT
	  ELSE
	    IF(LDBSTA(ROW,GIND)  .EQ. GAMCAN .OR.
     *	       LDBSTA(ROW2,GIND) .EQ. GAMCAN) THEN
              REFROWS = REFROWS + 1
	      REFAMT  = REFAMT  + AMT
	    ENDIF
	  ENDIF
110	CONTINUE

        IF(REFROWS.GT.0) THEN
           WIN = WIN + 1
           WINTAB(WIN) = REFAMT
           REFTAB(WIN) = 1
	   IF(LATEFLG) THEN
	      LDBLAT(LATCNT,GIND) = LDBLAT(LATCNT,GIND) + 1
	      LDBLAT(LATAMT,GIND) = LDBLAT(LATAMT,GIND) + REFAMT
	   ENDIF
        ENDIF

	RETURN

200	CONTINUE

C---- Check if ticket already refunded

	AMT = TRABUF(TWDBAMT)
	IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / TRABUF(TNFRAC)
	IF(LDBSTS(GIND) .EQ. GAMCAN .OR. LATEFLG) THEN
           WIN = WIN + 1
           WINTAB(WIN) = COUNT*AMT
           REFTAB(WIN) = 1
           REFROWS = COUNT
	   IF(LATEFLG) THEN
              LDBLAT(LATCNT,GIND) = LDBLAT(LATCNT,GIND) + 1
              LDBLAT(LATAMT,GIND) = LDBLAT(LATAMT,GIND) + COUNT*AMT
           ENDIF
           RETURN
        ENDIF

C---- Work of refunds.

	DO 220 J = 1,COUNT
	   ROW  = BETS(1,J)
	   ROW2 = BETS(2,J)
           IF(LDBSTA(ROW,GIND)  .EQ. GAMCAN .OR.
     *        LDBSTA(ROW2,GIND) .EQ. GAMCAN) THEN
                REFROWS = REFROWS + 1
	        REFAMT  = REFAMT  + AMT
	   ENDIF
220     CONTINUE
	IF(REFROWS.GT.0) THEN
           WIN = WIN + 1
           WINTAB(WIN) = REFAMT
           REFTAB(WIN) = 1
	ENDIF
	RETURN

	END
