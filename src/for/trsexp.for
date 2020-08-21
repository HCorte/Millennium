C
C SUBROUTINE TRSEXP
C
C V02 01-JUN-1999 UXN BETS resized.
C V01 XX-XXX-XXXX RXK INITIAL RELEASE.
C  
C SUBROUTINE TO EXPAND TODAY'S TRIPLE SYSTEM BETS
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

	SUBROUTINE TRSEXP(TRABUF,BETS,COUNT)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'

C---- Local variables.

	INTEGER*4 BETS(3,729) ! 9*9*9
	INTEGER*4 OFF1, OFF2, OFF3, I1, I2, I3, COUNT

C------------------------- Start of Code -----------------------------

        CALL FASTSET(0,BETS,3*729)

	IF (TRABUF(TWSYST) .EQ. NOSYS) THEN
	  COUNT = 1
 	  BETS(1,COUNT)=TRABUF(TWTTBET)
 	  BETS(2,COUNT)=TRABUF(TWTTBET+1)
 	  BETS(3,COUNT)=TRABUF(TWTTBET+2)
	  RETURN
	ENDIF

C---- Expand system bets

	COUNT = 0
        OFF1=-1
        OFF2=OFF1+TRABUF(TWTTMA)
        OFF3=OFF2+TRABUF(TWTTMB)

	IF(TRABUF(TWTTMB).GT.0 .AND. TRABUF(TWTTMC).GT.0) THEN
           DO 30 I1=1,TRABUF(TWTTMA)
              DO 20 I2=1,TRABUF(TWTTMB)
                 DO 10 I3=1,TRABUF(TWTTMC)
		    COUNT=COUNT+1
		    BETS(1,COUNT)=TRABUF(TWTTBET+OFF1+I1)
		    BETS(2,COUNT)=TRABUF(TWTTBET+OFF2+I2)
	 	    BETS(3,COUNT)=TRABUF(TWTTBET+OFF3+I3)
10               CONTINUE  
20            CONTINUE  
30         CONTINUE

	ELSEIF(TRABUF(TWTTMB).GT.0 .AND. TRABUF(TWTTMC).EQ.0) THEN
           DO 50 I1=1,TRABUF(TWTTMA)
              DO 40 I2=1,TRABUF(TWTTMB)
		 COUNT=COUNT+1
		 BETS(1,COUNT)=TRABUF(TWTTBET+OFF1+I1)
		 BETS(2,COUNT)=TRABUF(TWTTBET+OFF2+I2)
40            CONTINUE  
50         CONTINUE

	ELSEIF(TRABUF(TWTTMB).EQ.0 .AND. TRABUF(TWTTMC).EQ.0) THEN
           DO 60 I1=1,TRABUF(TWTTMA)
	      COUNT=COUNT+1
	      BETS(1,COUNT)=TRABUF(TWTTBET+OFF1+I1)
60         CONTINUE
  
        ENDIF

	RETURN

	END
