C
C SUBROUTINE WWINLOS
C
C V05 08-FEB-2000 UXN Fractions changed.
C V04 12-JUL-1999 UXN FIX FOR FRACTIONS.
C V03 10-FEB-1999 UXN FIX FOR BIG ODDS.
C V02 18-JAN-1994 HXK TRUNCATE PENNIES
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
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE WWINLOS(TRABUF,WINTAB,WIN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INTEGER*4 WINTAB(2,TWWBMAX)
	INTEGER*4 GIND, I, ROW, AMT, J, WIN
        INTEGER*4 NO_CENTS
        INTEGER*4 FCNT,MAXFRAC
C
        NO_CENTS = (100/DYN_VALUNIT)
	GIND=TRABUF(TGAMIND)

        FCNT     = TRABUF(TFRAC)
        IF(FCNT.EQ.0) FCNT = 10
        MAXFRAC  = MAXFRC(TRABUF(TGAM))
        IF(MAXFRAC.EQ.0) MAXFRAC = 10
        FCNT = MAXFRAC/FCNT

	DO 100 I=0,TRABUF(TWNBET)-1
	ROW=TRABUF(TWWROW+I*TWWBLEN)
	AMT=TRABUF(TWWAMT+I*TWWBLEN)
        IF(TRABUF(TFAMTFLG).EQ.1) AMT=AMT/FCNT
	DO 10 J=1,4
	IF(ROW.EQ.LWIWIN(J,GIND)) THEN
	  WIN=WIN+1
	  WINTAB(2,WIN)=0
	  WINTAB(1,WIN) = IDINT(DFLOAT(AMT*FCNT)*DFLOAT(LWIODS(J,GIND))/100.0D0)
	  WINTAB(1,WIN) = (WINTAB(1,WIN)/NO_CENTS*NO_CENTS)/FCNT !ROUND TO MARKS
	  GOTO 100
	ENDIF
10	CONTINUE
100	CONTINUE
	RETURN
	END
