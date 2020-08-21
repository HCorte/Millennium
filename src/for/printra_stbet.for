C
C SUBROUTINE PRINTRA_STBET
C
C V01 21-MAY-1999 UXN INITIAL RELEASE. 
C
C BUILD BET IMAGE FOR SUPER TRIPLE GAME TRANSACTIONS
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

C=======OPTIONS /CHECK=NOOVERFLOW /EXT

	SUBROUTINE PRINTRA_STBET(TRABUF,BETS,LINES)

	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'

        CHARACTER*80 BETS(14)

        INTEGER*4 EXP_BETS(MAXSTRRW,3)	!For Expanded System bet.
	INTEGER*4 LINES,I,OFF,MAXNO
	INTEGER*4 NOTHING/'----'/

C------------------------- Start of Code --------------------------------

	CALL FASTSET(NOTHING,EXP_BETS,3*MAXSTRRW)
        MAXNO=JMAX0(1,TRABUF(TWSTM1),TRABUF(TWSTM2),TRABUF(TWSTM3))

	OFF = -1
	DO I = 1, TRABUF(TWSTM1)
	   CALL BINASC(EXP_BETS(I,1),1,2,TRABUF(TWSTBET+OFF+I)) 
	ENDDO

	OFF = OFF + TRABUF(TWSTM1)
	DO I = 1, TRABUF(TWSTM2)
	   CALL BINASC(EXP_BETS(I,2),1,2,TRABUF(TWSTBET+OFF+I)) 
	ENDDO

	OFF = OFF + TRABUF(TWSTM2)
	DO I = 1, TRABUF(TWSTM3)
	   CALL BINASC(EXP_BETS(I,3),1,2,TRABUF(TWSTBET+OFF+I)) 
	ENDDO

        WRITE(BETS(1),901) '1st',(EXP_BETS(I,1),I=1,MAXNO)
        WRITE(BETS(2),901) '2nd',(EXP_BETS(I,2),I=1,MAXNO)
        WRITE(BETS(3),901) '3rd',(EXP_BETS(I,3),I=1,MAXNO)

	LINES = 4
	RETURN

C--------------------------- Format statements. -------------------------

901	FORMAT(1X,A3,': ',<MAXNO>(A2,2X))

	END


