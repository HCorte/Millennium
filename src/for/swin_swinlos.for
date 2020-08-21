C
C SUBROUTINE SWIN_SWINLOS
C
C V05 08-FEB-2000 UXN Fractions changed.
C V04 12-JAN-1999 UXN Fix for fractions.
C V03 18-JAN-1994 HXK TRUNCATE TO MARKS.
C V02 11-JAN-1994 HXK FIXED SYSTEM BET AMOUNT BUG.
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
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
	SUBROUTINE SWIN_SWINLOS(TRABUF,WINTAB,WIN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*4 WINTAB(TWSBMAX),BETS(2,100)
	INTEGER*4 WIN,AMTWON,I,COUNT,GIND,AMTBET
        INTEGER*4 NO_CENTS
        INTEGER*4 FCNT,MAXFRAC
C

	AMTWON=0
	WIN=0
        NO_CENTS = (100/DYN_VALUNIT)
	GIND=TRABUF(TGAMIND)

        FCNT = TRABUF(TFRAC)
	IF(FCNT.EQ.0) FCNT = 10
        MAXFRAC  = MAXFRC(TRABUF(TGAM))
        IF(MAXFRAC.EQ.0) MAXFRAC = 10
        FCNT = MAXFRAC/FCNT

	CALL SSEXP(TRABUF,BETS,COUNT)
	IF(TRABUF(TWSYST).NE.NOSYS) GOTO 1000
	DO 10 I=1,COUNT
	IF(BETS(1,I).NE.LSCWIN(1,GIND)) GOTO 10
	IF(BETS(2,I).NE.LSCWIN(2,GIND)) GOTO 10
	AMTBET=TRABUF(TWSAMT+(I-1)*TWSBLEN)
        IF(TRABUF(TFAMTFLG).EQ.1) AMTBET=AMTBET/FCNT
	WIN=WIN+1
	WINTAB(WIN) = INT((DFLOAT(AMTBET*FCNT)*DFLOAT(LSCODS(GIND))) / 100.0D0)
     	WINTAB(WIN) = (WINTAB(WIN)/NO_CENTS*NO_CENTS)/FCNT !ROUND DOWN TO MARKS
10	CONTINUE
	RETURN
C
C PROCESS SYSTEM BETS
C
1000	CONTINUE
	WINTAB(1)=0
	AMTBET=TRABUF(TWSAMT)
        IF(TRABUF(TFAMTFLG).EQ.1) AMTBET=AMTBET/FCNT
	DO 1010 I=1,COUNT
	IF(BETS(1,I).NE.LSCWIN(1,GIND)) GOTO 1010
	IF(BETS(2,I).NE.LSCWIN(2,GIND)) GOTO 1010
	WIN=1
	AMTWON = INT((DFLOAT(AMTBET*FCNT)*DFLOAT(LSCODS(GIND))) / 100.0D0)
	AMTWON = (AMTWON / NO_CENTS * NO_CENTS)/FCNT !ROUND DOWN TO MARKS
	WINTAB(WIN) = WINTAB(WIN) + AMTWON
1010	CONTINUE
	RETURN
	END
