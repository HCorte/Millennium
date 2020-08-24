C
C SUBROUTINE TRWIN_POST
C
C
C V02 01-FEB-2000 UXN TNFRAC added.
C V01 27-APR-1999 RXK STOPSYS optimization (CARYSCAN is now an array).
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

        SUBROUTINE TRWIN_POST(TRABUF)

        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'

        INTEGER*4 GIND, AMT


        IF (TRABUF(TTYP) .NE. TWAG) RETURN
        IF (CARYSCAN(TTRP) .AND. TRABUF(TSTAT) .EQ. EXCH) GOTO 100
        IF (TRABUF(TSTAT) .NE. GOOD) RETURN

100     CONTINUE

        GIND = TRABUF(TGAMIND)
        IF (TRABUF(TWBEG) .GT. LTRDRW(GIND)) RETURN
        IF (TRABUF(TWEND) .LT. LTRDRW(GIND)) RETURN

        LTRSAL(TRACNT,GIND) = LTRSAL(TRACNT,GIND) + 1       !--- Sales count
	AMT = TRABUF(TWAMT)
	IF(TRABUF(TFAMTFLG).EQ.1) AMT=AMT/TRABUF(TNFRAC)
        LTRSAL(DOLAMT,GIND) = LTRSAL(DOLAMT,GIND) + AMT     !--- Sales amount.

        IF (TRABUF(TWSYST) .EQ. FULSYS) THEN
          LTRWPA(1,PRWON,GIND) = LTRWPA(1,PRWON,GIND) + TRABUF(TWSYSN)
        ELSE
          LTRWPA(1,PRWON,GIND) = LTRWPA(1,PRWON,GIND) + 1
        END IF

        LTRWPA(2,PRWON,GIND) = LTRWPA(2,PRWON,GIND) + AMT
 
        END
