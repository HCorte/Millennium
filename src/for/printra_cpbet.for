C
C SUBROUTINE PRINTRA_CPBET
C 
C V03 01-FEB-2000 UXN TNFRAC added.
C V02 06-FEB-1996 HXK Fix for system bet
C V01 18-DEC-1995 PXB Initial revision.
C
C
C BUILD BET IMAGE FOR COUPLE GAME TRANSACTIONS
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW /EXT

	SUBROUTINE PRINTRA_CPBET(TRABUF,BETS,LINES)

	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'

        CHARACTER*80 BETS(14)

        INTEGER*4 EXP_BETS(2,TWCPBMAX)	!For Expanded System bet.
	INTEGER*4 LINES,I,OFF
	INTEGER*4 EVENT1
	INTEGER*4 EVENT2
	INTEGER*4 AMT
C------------------------- Start of Code --------------------------------


	DO I = 1,TRABUF(TWNBET)
	  OFF = I - 1
	  EVENT1 = TRABUF(TWCPROW1+OFF*TWCPBLEN)
	  EVENT2 = TRABUF(TWCPROW2+OFF*TWCPBLEN)
	  CALL BINASC(EXP_BETS(1,I),1,2,EVENT1)
	  CALL BINASC(EXP_BETS(2,I),1,2,EVENT2)
	  IF (EVENT1 .EQ. 'FF'X) EXP_BETS(1,I) = '--'
	  IF (EVENT2 .EQ. 'FF'X) EXP_BETS(2,I) = '--'
	END DO

	DO I = 1,TRABUF(TWNBET)
	  OFF = I - 1
	  IF (I .NE. 1 .AND. TRABUF(TWSYST) .NE. NOSYS) THEN
	    WRITE(BETS(I),901) EXP_BETS(1,I),EXP_BETS(2,I)
	  ELSE
	    AMT = TRABUF(TWCPAMT+OFF*TWCPBLEN)
	    IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / TRABUF(TNFRAC)
	    WRITE(BETS(I),900) EXP_BETS(1,I),EXP_BETS(2,I),
     *		  CMONY(AMT,9,BETUNIT)
	  ENDIF
	END DO

	LINES = TRABUF(TWNBET) + 1

	RETURN


C--------------------------- Format statements. -------------------------

900	FORMAT(1X,'E1 ',A2,2X,'E2 ',A2,' Amount ',A9)

901	FORMAT(1X,'E1 ',A2,2X,'E2 ',A2)

	END


