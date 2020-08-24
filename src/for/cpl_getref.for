C
C SUBROUTINE CPL_GETREF
C
C V07 03-JUL-2000 UXN LATEFLG added.
C V06 01-FEB-2000 UXN TNFRAC added.
C V05 27-MAY-1999 UXN REFUNDS PUT TOGETHER.
C V04 21-JAN-1996 HXK Various fixes for Double / Couple for system bets and ties
C V03 11-JAN-1996 PXB Changed twdbamt to twcpamt
C V02 04-JAN-1996 HXK Further changes for Double / Couple batch
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
	SUBROUTINE CPL_GETREF(TRABUF,WINTAB,REFTAB,WIN,REFROWS,LATEFLG)
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
 	INTEGER*4 NOREF
 	INTEGER*4 ROW
 	INTEGER*4 ROW2
 	INTEGER*4 I
 	INTEGER*4 K
 	INTEGER*4 AMT
 	INTEGER*4 WIN
 	INTEGER*4 BETS(2,100)
	INTEGER*4 COUNT
	INTEGER*4 REFROWS,REFAMT
	LOGICAL*4 LATEFLG

C----------------------------- Start of Code --------------------------

	GIND = TRABUF(TGAMIND)
	REFROWS = 0
	REFAMT  = 0
	CALL CSEXP(TRABUF,BETS,COUNT)

C---- Check for system bets.

	IF (TRABUF(TWSYST) .NE. NOSYS) GOTO 200

C---- Work of refunds.

	DO 110 I = 0,TRABUF(TWNBET)-1
	  ROW = TRABUF(TWCPROW1+I*TWCPBLEN)
	  ROW2 = (TRABUF(TWCPROW2+I*TWCPBLEN)+MAXCPLRW/2)
	  AMT = TRABUF(TWCPAMT+I*TWCPBLEN)
	  IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / TRABUF(TNFRAC)
 	  IF (LCPSTS(GIND).EQ.GAMCAN .OR. LATEFLG .OR.
     *        LCPEST(1,GIND).EQ.GAMCAN .OR.
     *        LCPEST(2,GIND).EQ.GAMCAN) THEN
	    REFROWS = REFROWS + 1
            REFAMT  = REFAMT  + AMT
	  ELSE
	    IF(LCPSTA(ROW,GIND)  .EQ. GAMCAN .OR.
     *		LCPSTA(ROW2,GIND) .EQ. GAMCAN) THEN
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
              LCPLAT(LATCNT,GIND) = LCPLAT(LATCNT,GIND) + 1
	      LCPLAT(LATAMT,GIND) = LCPLAT(LATAMT,GIND) + REFAMT
	   ENDIF
        ENDIF

	RETURN

200	CONTINUE

C---- Check if ticket already refunded
	
        AMT = TRABUF(TWCPAMT)
	IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / TRABUF(TNFRAC)
        IF(LCPSTS(GIND) .EQ. GAMCAN .OR. LATEFLG .OR.
     *     LCPEST(1,GIND).EQ.GAMCAN .OR.
     *     LCPEST(2,GIND).EQ.GAMCAN) THEN
	     WIN = WIN + 1
	     WINTAB(WIN) = COUNT*AMT
	     REFTAB(WIN) = 1
	     REFROWS = COUNT
	     IF(LATEFLG) THEN
                LCPLAT(LATCNT,GIND) = LCPLAT(LATCNT,GIND) + 1
	        LCPLAT(LATAMT,GIND) = LCPLAT(LATAMT,GIND) + COUNT*AMT
	     ENDIF
	     RETURN
	ENDIF

C---- Work of refunds.

	DO 220 K = 1,COUNT
	  NOREF = 0
	  ROW = BETS(1,K)
	  ROW2 = BETS(2,K)
	  IF(LCPSTA(ROW,GIND)  .EQ. GAMCAN .OR.
     *	     LCPSTA(ROW2,GIND) .EQ. GAMCAN) THEN
                REFROWS = REFROWS + 1
                REFAMT  = REFAMT  + AMT
          ENDIF
220	CONTINUE

        IF(REFROWS.GT.0) THEN
           WIN = WIN + 1
           WINTAB(WIN) = REFAMT
           REFTAB(WIN) = 1
        ENDIF

	RETURN
	END
