C
C SUBROUTINE YESNO
C
C V03 24-MAY-1999 UXN OUTPUT LUN CHANGED TO 6
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C SUBROUTINE TO ACCEPT YES/NO ANSWER FROM TERMINAL.
C CALLING SEQUENCE:
C     CALL YESNO(ANS)
C INPUT
C     NONE
C OUTPUT
C     ANS - ( 1-YES, 2-NO, 3-EXIT, -7=HELP)
C
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
	SUBROUTINE YESNO(ANS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INTEGER*4  ANS
	CHARACTER*1 A
	ANS=0
10	CONTINUE
	READ(5,900) A
	IF(A.EQ.'Y'.OR.A.EQ.'y') ANS=1
	IF(A.EQ.'N'.OR.A.EQ.'n') ANS=2
	IF(A.EQ.'E'.OR.A.EQ.'e') ANS=3
	IF(A.EQ.'H'.OR.A.EQ.'h') ANS=-7
	IF(ANS.NE.0) RETURN
C
C INVALID RESPONSE
C
	WRITE(6,901)
	GOTO 10
900	FORMAT(A1)
901	FORMAT('  *** Invalid input ***')
	END
