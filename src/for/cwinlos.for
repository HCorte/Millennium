C
C SUBROUTINE CWINLOS
C
C V09 08-FEB-2000 UXN Fractions changed.
C V08 12-JUL-1999 UXN Fix for fractions.
C V07 18-MAY-1999 UXN LCPCMB added. COUNTTAB restructured.
C V06 04-FEB-1999 UXN Fix for big winning amount.
C V05 24-JAN-1996 HXK Fix for WBT count, counttab
C V04 21-JAN-1996 HXK Various fixes for Double / Couple for system bets and ties
C V03 16-JAN-1996 HXK Further changes for ODS allocation at winsel
C V02 28-NOV-1995 PXB Now updates wcp,hst,wbt fields
C V01 23-NOV-1995 PXB Initial revision.
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

C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CWINLOS(TRABUF,WINTAB,WIN,COUNTTAB)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'

	INTEGER*4 WINTAB(25)
	INTEGER*4 BETS(2,100)
	INTEGER*4 WIN
	INTEGER*4 AMTWON
	INTEGER*4 COUNT
	INTEGER*4 GIND
	INTEGER*4 AMTBET
        INTEGER*4 NO_CENTS
	INTEGER*4 I
	INTEGER*4 J
	INTEGER*4 K
	INTEGER*4 COUNTTAB(2,MAXCPLTI)
        INTEGER*4 FCNT,MAXFRAC

C----------------------- Start of code. -----------------------------

	GIND     = TRABUF(TGAMIND)
	AMTWON   = 0
        NO_CENTS = (100/DYN_VALUNIT)

        FCNT     = TRABUF(TFRAC)
        IF(FCNT.EQ.0) FCNT = 10
        MAXFRAC  = MAXFRC(TRABUF(TGAM))
        IF(MAXFRAC.EQ.0) MAXFRAC = 10
        FCNT = MAXFRAC/FCNT

	CALL CSEXP(TRABUF,BETS,COUNT)

	IF (TRABUF(TWSYST) .NE. NOSYS) GOTO 1000

	DO 10 I = 1,COUNT   
	  DO 10 J = 1,LCPCMB(GIND)
	    IF (BETS(1,I) .NE. LCPWIN(1,J,GIND)) GOTO 10
	    IF (BETS(2,I) .NE. LCPWIN(2,J,GIND)) GOTO 10
	    AMTBET = TRABUF(TWCPAMT+(I-1)*TWCPBLEN)
	    IF(TRABUF(TFAMTFLG).EQ.1) AMTBET=AMTBET/FCNT
	    WIN = WIN + 1
	    WINTAB(WIN) = IDINT(DFLOAT(AMTBET*FCNT)*
     *                          DFLOAT(LCPODS(J,GIND)) / 100.0D0)
     	    WINTAB(WIN) = (WINTAB(WIN)/NO_CENTS*NO_CENTS)/FCNT !ROUND DOWN TO MARKS
	    COUNTTAB(1,J) = 1
	    COUNTTAB(2,J) = COUNTTAB(2,J) + AMTBET
10	CONTINUE

	RETURN

C---- Process system bets

1000	CONTINUE

	AMTBET = TRABUF(TWCPAMT)
	IF(TRABUF(TFAMTFLG).EQ.1) AMTBET=AMTBET/FCNT
	DO 1010 I = 1,COUNT
	  DO 1010 J = 1,LCPCMB(GIND)
	    IF (BETS(1,I) .NE. LCPWIN(1,J,GIND)) GOTO 1010
	    IF (BETS(2,I) .NE. LCPWIN(2,J,GIND)) GOTO 1010
	    WIN = WIN + 1
	    AMTWON = IDINT(DFLOAT(AMTBET*FCNT)*
     *                     DFLOAT(LCPODS(J,GIND)) / 100.0D0)
	    AMTWON = (AMTWON/NO_CENTS*NO_CENTS)/FCNT     !ROUND DOWN TO MARKS
	    WINTAB(WIN) = AMTWON
	    COUNTTAB(1,J) = 1
	    COUNTTAB(2,J) = COUNTTAB(2,J) + AMTBET
1010	CONTINUE

	RETURN
	END
