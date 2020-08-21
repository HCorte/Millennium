C SUBROUTINE BWIN_POST.FOR
C
C V03 07-FEB-2000 UXN ADD_PENNY added.
C V02 27-APR-1999 RXK STOPSYS optimization (CARYSCAN is now an array).
C V01 27-OCT-1994 HXK Initial revision.
C
C SUBROUTINE TO POST WINSEL SALES DATA
C
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE BWIN_POST(TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INTEGER*4 GIND, DATIND
	INTEGER*4 AMT, PENNY_TBNG(BGOENT,NUMBGO)    
	LOGICAL   FIRST/.TRUE./
C
	IF(FIRST) THEN
	    FIRST = .FALSE.
	    CALL FASTSET(0,PENNY_TBNG,BGOENT*NUMBGO)
	ENDIF
C
	IF(TRABUF(TTYP).NE.TWAG) RETURN
	IF(CARYSCAN(TBNG).AND.TRABUF(TSTAT).EQ.EXCH) GOTO 100
	IF(TRABUF(TSTAT).NE.GOOD.AND.TRABUF(TSTAT).NE.XCHD) RETURN
C
C
100	CONTINUE
	GIND=TRABUF(TGAMIND)
	DATIND = LBNDAT(CURDRW,GIND) - TRABUF(TCDC) + 3
	IF(CARYSCAN(TBNG)) DATIND = 1
	IF(MAILSCAN) DATIND = 2
	IF(TRABUF(TWBEG).GT.LBNDRW(GIND)) RETURN
	IF(TRABUF(TWEND).LT.LBNDRW(GIND)) RETURN
	AMT = TRABUF(TWAMT)
	IF(TRABUF(TFAMTFLG).EQ.1) THEN
           CALL ADD_PENNY(PENNY_TBNG(DATIND,GIND), AMT, TRABUF(TNFRAC))
	ENDIF
	LBNSAL(DATIND,GIND) = LBNSAL(DATIND,GIND) + AMT
	END
