C KIKNUM.FOR
C
C V01 17-DEC-96 WXM RELEASED FOR VAX
C
C SUBROUTINE TO CONVERT BETWEEN KICKER SEED AND 7-DIGIT KICKER NUMBER
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM KIKNUM
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INTEGER*4 OCTDIG, LIMIT, PARAM, DRAW, I
	INTEGER*4 NUMBER, OPT, EXT
C
C RANDOMIZE KICKER NUMBER
C
	PARAM=1
	LIMIT=9999999
	OCTDIG=8
C
10	CONTINUE
	TYPE*,IAM(),'Available options:'
	TYPE*,IAM()
	TYPE*,IAM(),'     1 - Joker Seed to Joker Number'
	TYPE*,IAM(),'     2 - Joker Number to Joker Seed'
	TYPE*,IAM()
	CALL INPNUM('Select option (E-exit)',OPT,1,2,EXT)
	IF(EXT.LT.0) GOTO 1000
C
	CALL INPNUM('Enter draw number',DRAW,1,99999,EXT)
	IF(EXT.LT.0) GOTO 1000
	DRAW=MOD(DRAW,64)
c
	GOTO (100,200) OPT
C
100	CONTINUE
	TYPE*,IAM()
	CALL INPNUM('Enter joker seed (E-exit)',I,0,99999999,EXT)
	IF(EXT.LT.0) GOTO 10
	NUMBER=I
	CALL RND64(NUMBER,DRAW,PARAM,LIMIT,OCTDIG)
	WRITE(5,900) IAM(),I,NUMBER
	GOTO 100
C
200	CONTINUE
	TYPE*,IAM()
	CALL INPNUM('Enter joker number (E-exit)',I,0,99999999,EXT)
	IF(EXT.LT.0) GOTO 10
	NUMBER=I
	CALL INV64(NUMBER,DRAW,PARAM,LIMIT,OCTDIG)
	WRITE(5,901) IAM(),I,NUMBER
	GOTO 200
C
1000	CONTINUE
	CALL GSTOP(GEXIT_SUCCESS)
C
900	FORMAT(1X,A,'  Joker Seed:',I9,10X,'Joker Number:',I9)
901	FORMAT(1X,A,'Joker Number:',I9,10X,' Joker Seed :',I9)
C
	END
