C
C SUBROUTINE SSWIN_POST
C  
C
C V02 01-FEB-2000 UXN Fractions changed.
C V01 27-APR-1999 RXK STOPSYS optimization (CARYSCAN is now an array).
C
C SUBROUTINE TO POST WINSEL SALES DATA
C
C CALLING SEQUENCE:
C     CALL SSWIN_POST(TRABUF)
C INPUT
C     TRABUF - SUPERSCORE WAGER BODY
C OUTPUT
C     NONE
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
	SUBROUTINE SSWIN_POST(TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INTEGER*4 GIND,AMT
C
C
	IF(TRABUF(TTYP).NE.TWAG) RETURN
	IF(CARYSCAN(TSSC).AND.TRABUF(TSTAT).EQ.EXCH) GOTO 100
	IF(TRABUF(TSTAT).NE.GOOD) RETURN
C
C
100	CONTINUE
	GIND=TRABUF(TGAMIND)
	IF(TRABUF(TWBEG).GT.LSSDRW(GIND)) RETURN
	IF(TRABUF(TWEND).LT.LSSDRW(GIND)) RETURN
	
	AMT = TRABUF(TWAMT)
	IF(TRABUF(TFAMTFLG).EQ.1) AMT=AMT/TRABUF(TNFRAC)
	LSSSAL(GIND)=LSSSAL(GIND)+AMT

        IF(TRABUF(TWSYST).EQ.FULSYS) THEN
           LSSWPA(1,PRWON,GIND) = LSSWPA(1,PRWON,GIND) + TRABUF(TWSYSN)
	ELSE
           LSSWPA(1,PRWON,GIND) = LSSWPA(1,PRWON,GIND) + 1
        ENDIF
        LSSWPA(2,PRWON,GIND) = LSSWPA(2,PRWON,GIND) + AMT

	END
