C
C Subroutine COMBINATION calculates n!/(m!*(n-m)!)
C
C 04-SEP-97 UXN Initial release.
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
C
	SUBROUTINE COMBINATION(ARG1,ARG2,RESULT)
	IMPLICIT NONE
C
	INTEGER*4   ARG1,ARG2,RESULT,I,TMP
	EXTERNAL    FACTORIAL
	INTEGER*4   FACTORIAL
C
	RESULT = 0
	IF(ARG1.LT.ARG2) RETURN
	IF(ARG1.LT.0.OR.ARG2.LT.0) RETURN
	RESULT = 1
	TMP = MAX(ARG2,ARG1-ARG2)
	DO I=ARG1,TMP+1,-1
	   RESULT = RESULT * I
	ENDDO 
	RESULT = RESULT / FACTORIAL(MIN(ARG2,ARG1-ARG2))
	END
C
C FUNCTION TO CALCULATE FACTORIAL
C
        OPTIONS /RECURSIVE
	INTEGER*4   FUNCTION FACTORIAL(ARG)
	IMPLICIT NONE
	INTEGER*4   ARG
C
	IF(ARG.LT.0) THEN
	  FACTORIAL = 0
	  RETURN
	ENDIF
	IF(ARG.EQ.0.OR.ARG.EQ.1) THEN 
	   FACTORIAL = 1
	ELSE
	   FACTORIAL = ARG*FACTORIAL(ARG-1)
	ENDIF
	END		
