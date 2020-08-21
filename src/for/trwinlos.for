C
C SUBROUTINE TRWINLOS
C
C V04 08-FEB-2000 UXN Fractions changed.
C V03 12-JUL-1999 UXN Fix for fractions.
C V02 28-MAY-1999 UXN LTRWIN changed. Code rewritten. Also LTRWBT amount
C                     corrected.
C V01 XX-XXX-XXXX RXK INITIAL RELEASE.
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TRWINLOS(TRABUF,WINTAB,WIN,CXLED)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'

        INTEGER*4 WINTAB(65)
        INTEGER*4 BETS(3,729)
        INTEGER*4 WIN
        INTEGER*4 COUNT
        INTEGER*4 GIND
        INTEGER*4 AMTBET
        INTEGER*4 AMTWON
        INTEGER*4 NO_CENTS
        INTEGER*4 I,J

        REAL*8    RAMTWON
        
        LOGICAL   CXLED
        INTEGER*4 FCNT,MAXFRAC

C----------------------- Start of code. -----------------------------

        GIND     = TRABUF(TGAMIND)

        FCNT     = TRABUF(TFRAC)
        IF(FCNT.EQ.0) FCNT = 10
        MAXFRAC  = MAXFRC(TRABUF(TGAM))
        IF(MAXFRAC.EQ.0) MAXFRAC = 10
        FCNT = MAXFRAC/FCNT

        AMTWON   = 0
        AMTBET = TRABUF(TWAMT)
        IF(TRABUF(TFAMTFLG).EQ.1) AMTBET=AMTBET/FCNT
        IF(TRABUF(TWSYST).EQ.FULSYS) AMTBET=AMTBET/TRABUF(TWSYSN)
        NO_CENTS = (100/DYN_VALUNIT)
        CALL TRSEXP(TRABUF,BETS,COUNT)

        DO 10 I = 1,COUNT
	   DO 20 J=1,LTRCMB(GIND)  
              IF(BETS(1,I).NE.LTRWIN(1,J,GIND)) GOTO 20
              IF(BETS(2,I).NE.LTRWIN(2,J,GIND)) GOTO 20
              IF(BETS(3,I).NE.LTRWIN(3,J,GIND)) GOTO 20
              WIN=WIN+1
              RAMTWON = DFLOAT(AMTBET*FCNT)*DFLOAT(LTRODS(J,GIND))/100.D0
              AMTWON = (JIDINT(RAMTWON)/NO_CENTS*NO_CENTS)/FCNT 
              WINTAB(WIN)=AMTWON
              IF(.NOT.CXLED) THEN
                 LTRWBT(TRACNT,J,GIND) = LTRWBT(TRACNT,J,GIND) + 1
                 LTRWBT(DOLAMT,J,GIND) = LTRWBT(DOLAMT,J,GIND) + AMTBET
                 LTRWCP(J,GIND) = LTRWCP(J,GIND) + 1
                 IF(LTRHST(J,GIND).LT.AMTWON) LTRHST(J,GIND) = AMTWON
	      ENDIF
20         CONTINUE
10	CONTINUE	   
        RETURN
        END
