C
C FUNCTION IAM
C IAM.FOR
C
C V02 26-JUL-2000 UXN PROC_NAME removed from SYSPARAM.DEF
C V01 26-FEB-1991 LMF INITIAL RELEASE FOR DEC PLATFORM
C
C THIS FUNCTION WILL RETURN A CHARACTER*18 CONSISTING OF
C     'HH:MM:SS PROCNAME '
C
C THIS FUNCTION IS TO BE USED FOR ALL TYPE & WRITE STATEMENTS
C THAT WILL PRINT OUTPUT TO THE CONSOLE
C
C FOR EXAMPLE:
C	CODE:	    TYPE *,IAM(),'THIS IS A TEST'
C	OUTPUT:	    12:34:56 TASKNAME THIS IS A TEST
C
C	CODE:	    WRITE(5,901) IAM(),'TESTING',1,2,3
C	CODE:	901 FORMAT(1X,A,A8,I1.1,I2.2,I3.3)
C	OUTPUT:	    12:34:56 TASKNAME TESTING 1 02 003
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
	CHARACTER*18 FUNCTION IAM()
	IMPLICIT NONE
C
	CHARACTER*8 MYTIME
	CHARACTER*8 PROC_NAME
C
	CALL GETNAM(%REF(PROC_NAME))
	CALL TIME (MYTIME)
	IAM = MYTIME // ' ' // PROC_NAME // ' '
C
	RETURN
	END
