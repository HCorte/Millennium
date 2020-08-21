C
C SUBROUTINE DBBET
C 
C V04 01-FEB-2000 UXN TNFRAC added.
C V03 28-JAN-1996 HXK Do not show unplayed system bets
C V02 05-JAN-1006 PXB No need to expand system bets
C V01 18-DEC-1995 PXB Initial revision.
C
C Subroutine to build bet image for Super Double transaction.
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

	SUBROUTINE DBBET(TRABUF,BIMAGE)

	IMPLICIT NONE

C---- Include files used.

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'

C---- Local Variables.

        CHARACTER*56 BIMAGE(12)

	INTEGER*4   I				!Loop variable
	INTEGER*4   J				!Loop variable
	INTEGER*4   OFF				!Offset into trabuf
        INTEGER*4   BETS(2,TWDBBMAX)		!Mark Table
        INTEGER*4   COUNT		        !Counter
	INTEGER*4   FIRST
	INTEGER*4   SECOND
	INTEGER*4   AMT
C------------------------- Start of Code --------------------------------

	DO I = 1,TRABUF(TWNBET)
	   OFF = I - 1
	   FIRST  = TRABUF(TWDBROW1+OFF*TWDBBLEN)
	   SECOND = TRABUF(TWDBROW2+OFF*TWDBBLEN)
	   CALL BINASC(BETS(1,I),1,2,FIRST )
	   CALL BINASC(BETS(2,I),1,2,SECOND)
	   IF(FIRST  .EQ. 'FF'X) BETS(1,I) = '--'
	   IF(SECOND .EQ. 'FF'X) BETS(2,I) = '--'
	END DO

	DO I = 1,TRABUF(TWNBET)
	  OFF = I - 1
	  IF (I .NE. 1 .AND. TRABUF(TWSYST) .NE. NOSYS) THEN
	    WRITE(BIMAGE(I),901) BETS(1,I),BETS(2,I)
	  ELSE
	    AMT = TRABUF(TWDBAMT+OFF*TWDBBLEN)
	    IF(TRABUF(TFAMTFLG).EQ.1) AMT = AMT / TRABUF(TNFRAC)
	    WRITE(BIMAGE(I),900) BETS(1,I),BETS(2,I),
     *		 CMONY(AMT,9,BETUNIT)
	  ENDIF
	END DO

	RETURN
C--------------------------- Format statements. -------------------------

900	FORMAT(1X,'1st ',A2,2X,'2nd ',A2,' Amount ',A9)

901	FORMAT(1X,'1st ',A2,2X,'2nd ',A2)

	END
