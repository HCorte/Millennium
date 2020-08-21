C
C SUBROUTINE STWINLOS
C
C V03 08-FEB-2000 UXN TFAMTFLG added.
C V02 12-JUL-1999 UXN Fix for fractions.
C V01 18-MAY-1999 UXN Initial revision.
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

C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE STWINLOS(TRABUF,WINTAB,WIN,COUNTTAB)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'

        INTEGER*4 WINTAB(25)
        INTEGER*4 BETS(3,500)
        INTEGER*4 WIN
        INTEGER*4 COUNT
        INTEGER*4 GIND
        INTEGER*4 AMTBET
        INTEGER*4 NO_CENTS
        INTEGER*4 I
        INTEGER*4 J
        INTEGER*4 COUNTTAB(2,MAXSTRTI)

        INTEGER*4 FCNT,MAXFRAC
        REAL*8    RAMTWON

C----------------------- Start of code. -----------------------------

        GIND     = TRABUF(TGAMIND)

        FCNT     = TRABUF(TFRAC)
        IF(FCNT.EQ.0) FCNT = 10
        MAXFRAC  = MAXFRC(TRABUF(TGAM))
        IF(MAXFRAC.EQ.0) MAXFRAC = 10
        FCNT = MAXFRAC/FCNT

        RAMTWON   = 0.D0
        NO_CENTS = (100/DYN_VALUNIT)

        CALL STSEXP(TRABUF,BETS,COUNT)

        AMTBET = TRABUF(TWSTAMT)
        IF(TRABUF(TFAMTFLG).EQ.1) AMTBET=AMTBET/FCNT

        DO 10 I = 1,COUNT
          DO 11 J = 1,LSTCMB(GIND)
            IF (BETS(1,I) .NE. LSTWIN(1,J,GIND)) GOTO 11
            IF (BETS(2,I) .NE. LSTWIN(2,J,GIND)) GOTO 11
            IF (BETS(3,I) .NE. LSTWIN(3,J,GIND)) GOTO 11
            WIN = WIN + 1
            RAMTWON = DFLOAT(AMTBET*FCNT)*DFLOAT(LSTODS(J,GIND)) / 1.D2
            COUNTTAB(1,J) = 1
	    COUNTTAB(2,J) = COUNTTAB(2,J) + AMTBET
            WINTAB(WIN) = (JIDINT(RAMTWON)/NO_CENTS*NO_CENTS)/FCNT !ROUND DOWN TO MARKS
11        CONTINUE
10      CONTINUE

        RETURN
        END
