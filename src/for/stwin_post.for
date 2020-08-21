C
C SUBROUTINE STWIN_POST
C
C V02 01-FEB-2000 UXN Fractions changed.
C V01 17-MAY-1999 UXN Initial release.
C
C SUBROUTINE TO POST WINSEL SALES DATA
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

        SUBROUTINE STWIN_POST(TRABUF)

        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'

        INTEGER*4 GIND, AMT


        IF (TRABUF(TTYP) .NE. TWAG) RETURN
        IF (CARYSCAN(TSTR) .AND. TRABUF(TSTAT) .EQ. EXCH) GOTO 100
        IF (TRABUF(TSTAT) .NE. GOOD) RETURN

100     CONTINUE

        GIND = TRABUF(TGAMIND)
        IF (TRABUF(TWBEG) .GT. LSTDRW(GIND)) RETURN
        IF (TRABUF(TWEND) .LT. LSTDRW(GIND)) RETURN

        LSTSAL(TRACNT,GIND) = LSTSAL(TRACNT,GIND) + 1       !--- Sales count
	AMT = TRABUF(TWAMT)
	IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT/TRABUF(TNFRAC)
        LSTSAL(DOLAMT,GIND) = LSTSAL(DOLAMT,GIND) + AMT     !--- Sales amount.

        IF (TRABUF(TWSYST) .EQ. FULSYS) THEN
          LSTWPA(1,PRWON,GIND) = LSTWPA(1,PRWON,GIND) + TRABUF(TWSYSN)
        ELSE
          LSTWPA(1,PRWON,GIND) = LSTWPA(1,PRWON,GIND) + 1
        END IF

        LSTWPA(2,PRWON,GIND) = LSTWPA(2,PRWON,GIND) + AMT
 
        END
