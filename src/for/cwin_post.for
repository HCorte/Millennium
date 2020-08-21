C
C SUBROUTINE CWIN_POST
C
C
C V04 02-FEB-2000 UXN TNFRAC added.  
C V03 27-APR-1999 RXK STOPSYS optimization (CARYSCAN is now an array).
C V02 11-JAN-1996 HXK Use MAXCPLRW instead of 36
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

	SUBROUTINE CWIN_POST(TRABUF)

	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'

	INTEGER*4 GIND, ROW, AMT, I


	IF (TRABUF(TTYP) .NE. TWAG) RETURN

	IF (CARYSCAN(TCPL) .AND. TRABUF(TSTAT) .EQ. EXCH) GOTO 100

	IF (TRABUF(TSTAT) .NE. GOOD) RETURN


100	CONTINUE

	GIND = TRABUF(TGAMIND)

	IF (TRABUF(TWBEG) .GT. LCPDRW(GIND)) RETURN

	IF (TRABUF(TWEND) .LT. LCPDRW(GIND)) RETURN

	LCPSAL(TRACNT,GIND) = LCPSAL(TRACNT,GIND) + 1	    !--- Sales count
	AMT = TRABUF(TWAMT)
	IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / TRABUF(TNFRAC)
	LCPSAL(DOLAMT,GIND)  = LCPSAL(DOLAMT,GIND)  + AMT   !--- Sales amount.
        LCPWPA(2,PRWON,GIND) = LCPWPA(2,PRWON,GIND) + AMT

	DO 110 I = 0,TRABUF(TWNBET)-1
	  ROW = TRABUF(TWCPROW1+I*TWCPBLEN)
	  AMT = TRABUF(TWCPAMT+I*TWCPBLEN)
	  IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / TRABUF(TNFRAC)
	  IF (ROW .LE. 0 .OR. ROW .GT. MAXCPLRW) GOTO 110
	  LCPSBR(ROW,GIND) = LCPSBR(ROW,GIND) + AMT
110	CONTINUE

        IF (TRABUF(TWSYST) .EQ. FULSYS) THEN
          LCPWPA(1,PRWON,GIND) = LCPWPA(1,PRWON,GIND) + TRABUF(TWSYSN)
        ELSE
          LCPWPA(1,PRWON,GIND) = LCPWPA(1,PRWON,GIND) + TRABUF(TWNBET)
        END IF
 
	END
