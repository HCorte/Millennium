C
C SUBROUTINE GETREF
C
C V03 03-JUL-2000 UXN LATEFLG added.
C V02 01-FEB-2000 UXN TNFRAC added.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETREF(TRABUF,WINTAB,WIN,LATEFLG)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INTEGER*4 WINTAB(2,TWWBMAX)
	INTEGER*4 GIND, NOREF, ROW, I, AMT, WIN
	LOGICAL*4 LATEFLG
C
	GIND=TRABUF(TGAMIND)

	IF(LATEFLG) GOTO 105
C
C CHECK IF TICKET ALREADY REFUNDED
C
	NOREF=0
	DO 100 I=0,TRABUF(TWNBET)-1
	ROW=TRABUF(TWWROW+I*TWWBLEN)
	IF(LWISTA(ROW,GIND).NE.GAMREF) NOREF=1
100	CONTINUE
	IF(NOREF.EQ.0) RETURN
C
105	CONTINUE
C
	DO 110 I=0,TRABUF(TWNBET)-1
	ROW=TRABUF(TWWROW+I*TWWBLEN)
	AMT=TRABUF(TWWAMT+I*TWWBLEN)
        IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT/TRABUF(TNFRAC)
	IF(LWISTS(GIND).EQ.GAMCAN.OR.LATEFLG) THEN
	  WIN=WIN+1
	  WINTAB(1,WIN)=AMT
	  WINTAB(2,WIN)=1
	  IF(LATEFLG) THEN
	     LWILAT(LATCNT,GIND) = LWILAT(LATCNT,GIND) + 1
	     LWILAT(LATAMT,GIND) = LWILAT(LATAMT,GIND) + AMT
	  ENDIF
	ELSE
	  IF(LWISTA(ROW,GIND).EQ.GAMCAN.OR.
     *	     LWISTA(ROW,GIND).EQ.GAMREF) THEN
	    WIN=WIN+1
	    WINTAB(1,WIN)=AMT
	    WINTAB(2,WIN)=1
	  ELSE
	    IF(LWISTS(GIND).LT.GAMENV) THEN
	      WIN=0
	      RETURN
	    ENDIF
	  ENDIF
	ENDIF
110	CONTINUE
	RETURN
	END
