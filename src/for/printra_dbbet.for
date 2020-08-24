C
C SUBROUTINE PRINTRA_DBBET
C
C V03 01-FEB-2000 UXN TNFRAC added.
C V02 06-FEB-1996 HXK Fix for system bet
C V01 18-DEC-1995 PXB Initial revision.
C
C
C BUILD BET IMAGE FOR DOUBLE GAME TRANSACTIONS
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

	SUBROUTINE PRINTRA_DBBET(TRABUF,BETS,LINES)

	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'

        CHARACTER*80 BETS(14)

        INTEGER*4 EXP_BETS(2,TWDBBMAX)	!For Expanded System bet.
	INTEGER*4 LINES,I,OFF
	INTEGER*4 FIRST
	INTEGER*4 SECOND
	INTEGER*4 AMT
C------------------------- Start of Code --------------------------------

	DO I = 1,TRABUF(TWNBET)
	  OFF = I - 1
	  FIRST  = TRABUF(TWDBROW1+OFF*TWDBBLEN)
	  SECOND = TRABUF(TWDBROW2+OFF*TWDBBLEN)
	  CALL BINASC(EXP_BETS(1,I),1,2,FIRST )
	  CALL BINASC(EXP_BETS(2,I),1,2,SECOND)
	  IF (FIRST  .EQ. 'FF'X) EXP_BETS(1,I) = '--'
	  IF (SECOND .EQ. 'FF'X) EXP_BETS(2,I) = '--'
	END DO

	DO I = 1,TRABUF(TWNBET)
	  OFF = I - 1
	  IF (I .NE. 1 .AND. TRABUF(TWSYST) .NE. NOSYS) THEN
	    WRITE(BETS(I),901) EXP_BETS(1,I),EXP_BETS(2,I)
	  ELSE
	    AMT = TRABUF(TWDBAMT+OFF*TWDBBLEN)
	    IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / TRABUF(TNFRAC)
	    WRITE(BETS(I),900) EXP_BETS(1,I),EXP_BETS(2,I),
     *		 CMONY(AMT,9,BETUNIT)
	  ENDIF
	END DO

	LINES = TRABUF(TWNBET) + 1

	RETURN


C--------------------------- Format statements. -------------------------

900	FORMAT(1X,'1st ',A2,2X,'2nd ',A2,' Amount ',A9)

901	FORMAT(1X,'1st ',A2,2X,'2nd ',A2)

	END

