C
C SUBROUTINE DWINLOS
C
C V09 08-FEB-2000 UXN Fractions changed.
C V08 12-JUL-1999 UXN Fix for fractions.
C V07 18-MAY-1999 UXN LDBCMB added. COUNTTAB restructured - fix for multiboard
C                     tickets.
C V06 24-JAN-1996 HXK Fix for WBT count, counttab
C V05 21-JAN-1996 HXK Various fixes for Double / Couple for system bets and ties
C V04 11-JAN-1996 HXK Fix for loop on simple bet
C V03 05-JAN-1996 PXB Fixed bug in system bets (Jumping to wrong goto)
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
        SUBROUTINE DWINLOS(TRABUF,WINTAB,WIN,COUNTTAB)
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
        INTEGER*4 COUNT
        INTEGER*4 GIND
        INTEGER*4 AMTBET
        INTEGER*4 NO_CENTS
        INTEGER*4 I
        INTEGER*4 J
        INTEGER*4 K
        INTEGER*4 COUNTTAB(2,MAXDBLTI)
        REAL*8    RAMTWON
	INTEGER*4 FCNT,MAXFRAC

C----------------------- Start of code. -----------------------------

        GIND     = TRABUF(TGAMIND)

        FCNT     = TRABUF(TFRAC)
	IF(FCNT.EQ.0) FCNT = 10
	MAXFRAC  = MAXFRC(TRABUF(TGAM))
	IF(MAXFRAC.EQ.0) MAXFRAC = 10
	FCNT = MAXFRAC/FCNT

        RAMTWON   = 0.D0
        NO_CENTS = (100/DYN_VALUNIT)

        CALL DSEXP(TRABUF,BETS,COUNT)

        IF (TRABUF(TWSYST) .NE. NOSYS) GOTO 1000

        DO 10 I = 1,COUNT
          DO 11 J = 1,LDBCMB(GIND)
            IF (BETS(1,I) .NE. LDBWIN(1,J,GIND)) GOTO 11
            IF (BETS(2,I) .NE. LDBWIN(2,J,GIND)) GOTO 11
            AMTBET = TRABUF(TWDBAMT+(I-1)*TWDBBLEN)
	    IF(TRABUF(TFAMTFLG).EQ.1) AMTBET=AMTBET/FCNT
            WIN = WIN + 1
            RAMTWON = DFLOAT(AMTBET*FCNT)*DFLOAT(LDBODS(J,GIND)) / 1.D2
            COUNTTAB(1,J) = 1
	    COUNTTAB(2,J) = COUNTTAB(2,J) + AMTBET
            WINTAB(WIN) = (JIDINT(RAMTWON)/NO_CENTS*NO_CENTS)/FCNT !ROUND DOWN TO MARKS
11        CONTINUE
10      CONTINUE

        RETURN

C---- Process system bets

1000    CONTINUE

        AMTBET = TRABUF(TWDBAMT)
	IF(TRABUF(TFAMTFLG).EQ.1) AMTBET=AMTBET/FCNT
        DO 1010 I = 1,COUNT
          DO 1011 J = 1,LDBCMB(GIND)
            IF (BETS(1,I) .NE. LDBWIN(1,J,GIND)) GOTO 1011
            IF (BETS(2,I) .NE. LDBWIN(2,J,GIND)) GOTO 1011
            WIN = WIN + 1
            RAMTWON = DFLOAT(AMTBET*FCNT)*DFLOAT(LDBODS(J,GIND)) / 1.D2
            COUNTTAB(1,J) = 1
	    COUNTTAB(2,J) = COUNTTAB(2,J) + AMTBET	    
            WINTAB(WIN) = (JIDINT(RAMTWON) / NO_CENTS * NO_CENTS)/FCNT
1011      CONTINUE
1010    CONTINUE

        RETURN
        END
