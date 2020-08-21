C
C SUBROUTINE PRINTRA_TRBET
C
C BUILD BET IMAGE FOR TRIPLE GAME TRANSACTIONS
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW /EXT

	SUBROUTINE PRINTRA_TRBET(TRABUF,BETS,LINES)

	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'

        CHARACTER*80 BETS(14)

        INTEGER*4 EXP_BETS(MAXTRPRW,3)	!For Expanded System bet.
	INTEGER*4 LINES,I,OFF,MAXNO
	INTEGER*4 NOTHING/'----'/

C------------------------- Start of Code --------------------------------

	CALL FASTSET(NOTHING,EXP_BETS,3*MAXTRPRW)
        MAXNO=JMAX0(1,TRABUF(TWTTMA),TRABUF(TWTTMB),TRABUF(TWTTMC))

	OFF = -1
	DO I = 1, TRABUF(TWTTMA)
	   CALL BINASC(EXP_BETS(I,1),1,2,TRABUF(TWTTBET+OFF+I)) 
	ENDDO

	OFF = OFF + TRABUF(TWTTMA)
	DO I = 1, TRABUF(TWTTMB)
	   CALL BINASC(EXP_BETS(I,2),1,2,TRABUF(TWTTBET+OFF+I)) 
	ENDDO

	OFF = OFF + TRABUF(TWTTMB)
	DO I = 1, TRABUF(TWTTMC)
	   CALL BINASC(EXP_BETS(I,3),1,2,TRABUF(TWTTBET+OFF+I)) 
	ENDDO

        WRITE(BETS(1),901) 'A',(EXP_BETS(I,1),I=1,MAXNO)
        WRITE(BETS(2),901) 'B',(EXP_BETS(I,2),I=1,MAXNO)
        WRITE(BETS(3),901) 'C',(EXP_BETS(I,3),I=1,MAXNO)

	LINES = 4
	RETURN

C--------------------------- Format statements. -------------------------

901	FORMAT(1X,A1,': ',<MAXNO>(A2,2X))

	END


