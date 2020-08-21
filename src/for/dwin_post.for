C
C SUBROUTINE DWIN_POST
C 
C V03 01-FEB-2000 UXN Fractions changed. 
C V02 27-APR-1999 RXK STOPSYS optimization (CARYSCAN is now an array).
C V01 23-NOV-1995 PXB Initial revision.
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

	SUBROUTINE DWIN_POST(TRABUF)

	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'

	INTEGER*4 GIND, ROW, AMT, I


	IF (TRABUF(TTYP) .NE. TWAG) RETURN

	IF (CARYSCAN(TDBL) .AND. TRABUF(TSTAT) .EQ. EXCH) GOTO 100

	IF (TRABUF(TSTAT) .NE. GOOD) RETURN


100	CONTINUE

	GIND = TRABUF(TGAMIND)

	IF (TRABUF(TWBEG) .GT. LDBDRW(GIND)) RETURN

	IF (TRABUF(TWEND) .LT. LDBDRW(GIND)) RETURN

	LDBSAL(TRACNT,GIND) = LDBSAL(TRACNT,GIND) + 1	    !--- Sales count
	AMT = TRABUF(TWAMT)
	IF(TRABUF(TFAMTFLG).EQ.1) AMT=AMT/TRABUF(TNFRAC)
	LDBSAL(DOLAMT,GIND) = LDBSAL(DOLAMT,GIND) + AMT	    !--- Sales amount.
        LDBWPA(2,PRWON,GIND) = LDBWPA(2,PRWON,GIND) + AMT

	DO 110 I = 0,TRABUF(TWNBET)-1
	  ROW = TRABUF(TWDBROW1+I*TWDBBLEN)
	  AMT = TRABUF(TWDBAMT+I*TWDBBLEN)
	  IF(TRABUF(TFAMTFLG).EQ.1) AMT=AMT/TRABUF(TNFRAC)
	  IF (ROW .LE. 0 .OR. ROW .GT. 18) GOTO 110
	  LDBSBR(ROW,GIND) = LDBSBR(ROW,GIND) + AMT
110	CONTINUE

        IF (TRABUF(TWSYST) .EQ. FULSYS) THEN
          LDBWPA(1,PRWON,GIND) = LDBWPA(1,PRWON,GIND) + TRABUF(TWSYSN)
        ELSE
          LDBWPA(1,PRWON,GIND) = LDBWPA(1,PRWON,GIND) + TRABUF(TWNBET)
        END IF
 
	END
