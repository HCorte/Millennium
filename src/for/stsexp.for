C
C SUBROUTINE STSEXP
C
C V01 18-MAY-1999 UXN Initial release.
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
C  
C  
C SUBROUTINE TO EXPAND SUPER TRIPLE SYSTEM BETS
C

C=======OPTIONS /CHECK=NOOVERFLOW

	SUBROUTINE STSEXP(TRABUF,BETS,COUNT)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'

C---- Local variables.

	INTEGER*4 BETS(3,500)
	INTEGER*4 RW1,RW2,RW3
	INTEGER*4 OFF1, OFF2, OFF3, I1, I2, I3, COUNT

C------------------------- Start of Code -----------------------------

        CALL FASTSET(0,BETS,3*500)

	IF (TRABUF(TWSYST) .EQ. NOSYS) THEN
	  COUNT = 1
 	  BETS(1,COUNT)=TRABUF(TWSTBET)
 	  BETS(2,COUNT)=TRABUF(TWSTBET+1)
 	  BETS(3,COUNT)=TRABUF(TWSTBET+2)
	  RETURN
	ENDIF

C---- Expand system bets

	COUNT = 0
        OFF1=-1
        OFF2=OFF1+TRABUF(TWSTM1)
        OFF3=OFF2+TRABUF(TWSTM2)
        DO 30 I1=1,TRABUF(TWSTM1)
           DO 20 I2=1,TRABUF(TWSTM2)
              DO 10 I3=1,TRABUF(TWSTM3)
		    RW1 = TRABUF(TWSTBET+OFF1+I1)   
		    RW2 = TRABUF(TWSTBET+OFF2+I2)
		    RW3 = TRABUF(TWSTBET+OFF3+I3)
		    IF(RW1.EQ.RW2) GOTO 20
		    IF(RW1.EQ.RW3) GOTO 10
		    IF(RW2.EQ.RW3) GOTO 10
		    COUNT=COUNT+1
		    BETS(1,COUNT)=RW1
		    BETS(2,COUNT)=RW2
	 	    BETS(3,COUNT)=RW3
10            CONTINUE  
20         CONTINUE  
30      CONTINUE

	RETURN

	END
