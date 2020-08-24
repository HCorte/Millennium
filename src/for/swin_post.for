C
C SUBROUTINE SWIN_POST
C  
C V05 01-FEB-2000 UXN TNFRAC added
C V04 27-APR-1999 RXK STOPSYS optimization (CARYSCAN is now an array).
C V03 03-FEB-1994 HXK ACCUMULATE NUMBER OF BETS, NOT NUMBER OF COUPONS.
C V02 30-JAN-1994 HXK CHANGE TABLE USED FOR STATE REPORT FROM WPO TO WPA.
C V01 03-JAN-1994 HXK ADDED SALES COUNTER FOR STATE REPORT.
C  
C
C SUBROUTINE TO POST WINSEL SALES DATA
C
C CALLING SEQUENCE:
C     CALL SWIN_POST(TRABUF)
C INPUT
C     TRABUF - SCORE WAGER BODY
C OUTPUT
C     NONE
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
	SUBROUTINE SWIN_POST(TRABUF)
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
	IF(CARYSCAN(TSCR).AND.TRABUF(TSTAT).EQ.EXCH) GOTO 100
	IF(TRABUF(TSTAT).NE.GOOD) RETURN
C
C
100	CONTINUE
	GIND=TRABUF(TGAMIND)
	IF(TRABUF(TWBEG).GT.LSCDRW(GIND)) RETURN
	IF(TRABUF(TWEND).LT.LSCDRW(GIND)) RETURN
	
	AMT = TRABUF(TWAMT)
	IF(TRABUF(TFAMTFLG).EQ.1) AMT=AMT/TRABUF(TNFRAC)

	LSCSAL(GIND)=LSCSAL(GIND)+AMT

        IF(TRABUF(TWSYST).EQ.FULSYS) THEN
           LSCWPA(1,PRWON,GIND) = LSCWPA(1,PRWON,GIND) + TRABUF(TWSYSN)
	ELSE
           LSCWPA(1,PRWON,GIND) = LSCWPA(1,PRWON,GIND) + TRABUF(TWNBET)
        ENDIF
        LSCWPA(2,PRWON,GIND) = LSCWPA(2,PRWON,GIND) + AMT

	END
