C
C SUBROUTINE CPBET
C 
C V04 01-FEB-2000 UXN Fractions changed.
C V03 28-JAN-1996 HXK Do not show unplayed system bets
C V02 05-JAN-1996 PXB No need to expand system bats
C V01 18-DEC-1995 PXB Initial revision.
C
C
C
C Subroutine to build bet image for Todays Couple transaction.
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

	SUBROUTINE CPBET(TRABUF,BIMAGE)

	IMPLICIT NONE

C---- Include files used.

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'

C---- Local Variables.

        CHARACTER*56 BIMAGE(12)

	INTEGER*4   I				!Loop variable
	INTEGER*4   OFF				!Offset into trabuf
        INTEGER*4   BETS(2,TWCPBMAX)		!Mark Table
        INTEGER*4   COUNT		        !Counter
	INTEGER*4   EVENT1
	INTEGER*4   EVENT2
	INTEGER*4   AMT
C------------------------- Start of Code --------------------------------

	DO I = 1,TRABUF(TWNBET)
	  OFF = I - 1
	  EVENT1 = TRABUF(TWCPROW1+OFF*TWCPBLEN)
	  EVENT2 = TRABUF(TWCPROW2+OFF*TWCPBLEN)
	  CALL BINASC(BETS(1,I),1,2,EVENT1)
	  CALL BINASC(BETS(2,I),1,2,EVENT2)
	  IF (EVENT1 .EQ. 'FF'X) BETS(1,I) = '--'
	  IF (EVENT2 .EQ. 'FF'X) BETS(2,I) = '--'
	END DO

	DO I = 1,TRABUF(TWNBET)
	  OFF = I - 1
	  IF (I .NE. 1 .AND. TRABUF(TWSYST) .NE. NOSYS) THEN
	    WRITE(BIMAGE(I),901) BETS(1,I),BETS(2,I)
	  ELSE
	    AMT = TRABUF(TWCPAMT+OFF*TWCPBLEN)
	    IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / TRABUF(TNFRAC)
	    WRITE(BIMAGE(I),900) BETS(1,I),BETS(2,I),
     *		  CMONY(AMT,9,BETUNIT)
	  ENDIF
	END DO

	RETURN
C--------------------------- Format statements. -------------------------

900	FORMAT(1X,'E1 ',A2,2X,'E2 ',A2,' Amount ',A9)

901	FORMAT(1X,'E1 ',A2,2X,'E2 ',A2)

	END
