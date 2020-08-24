C
C SUBROUTINE SSWIN_SWINLOS
C
C V03 08-FEB-2000 UXN Fractions changed.
C V02 12-JUL-1999 UXN Fix for fractions.
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SSWIN_SWINLOS(TRABUF,WINTAB,WIN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*4 WINTAB,BETS(2,3,16000)
	INTEGER*4 WIN,AMTWON,I,COUNT,GIND,AMTBET
        INTEGER*4 NO_CENTS

        REAL*8    RAMTWON 
        INTEGER*4 FCNT,MAXFRAC
C
	AMTWON=0
	WIN=0
        NO_CENTS = (100/DYN_VALUNIT)
	GIND=TRABUF(TGAMIND)

        FCNT     = TRABUF(TFRAC)
        IF(FCNT.EQ.0) FCNT = 10
        MAXFRAC  = MAXFRC(TRABUF(TGAM))
        IF(MAXFRAC.EQ.0) MAXFRAC = 10
        FCNT = MAXFRAC/FCNT

	CALL SSSEXP(TRABUF,BETS,COUNT)
	DO 10 I=1,COUNT
	   IF(BETS(1,1,I).NE.LSSWIN(1,1,GIND)) GOTO 10
	   IF(BETS(2,1,I).NE.LSSWIN(2,1,GIND)) GOTO 10
           IF(LSSEST(2,GIND).NE.GAMOPN) GOTO 5
	   IF(BETS(1,2,I).NE.LSSWIN(1,2,GIND)) GOTO 10
	   IF(BETS(2,2,I).NE.LSSWIN(2,2,GIND)) GOTO 10
           IF(LSSEST(3,GIND).NE.GAMOPN) GOTO 5
	   IF(BETS(1,3,I).NE.LSSWIN(1,3,GIND)) GOTO 10
	   IF(BETS(2,3,I).NE.LSSWIN(2,3,GIND)) GOTO 10
5          CONTINUE
	   WIN=1
	   AMTBET=TRABUF(TWAMT)
	   IF(TRABUF(TFAMTFLG).EQ.1) AMTBET=AMTBET/FCNT
           IF(TRABUF(TWSYST).EQ.FULSYS) AMTBET=AMTBET/TRABUF(TWSYSN)
           RAMTWON = (DFLOAT(AMTBET*FCNT)*DFLOAT(LSSODS(GIND))) / 100.D0
     	   WINTAB = (JIDINT(RAMTWON)/NO_CENTS*NO_CENTS)/FCNT !ROUND DOWN TO MARKS
10	CONTINUE
	RETURN

	END
