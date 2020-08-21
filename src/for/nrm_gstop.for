C
C SUBROUTINE GSTOP
C
C V04 25-JUN-2000 UXN OPSTXT() AND ISDETACHED() ADDED.
C V03 31-JAN-1996 HXK CHANGED LU FROM 5 TO 6
C V02 28-MAR-1991 KWP Changed format and use new GEXIT definitions
C V01 26-FEB-1991 LMF INITIAL RELEASE FOR DEC PLATFORM
C
C THIS SUBROUTINE WILL PRINT A MESSAGE TO THE CONSOLE DISPLAYING
C THE TASKNAME AND THE TIME THAT IT IS STOPPING
C
C THIS SUBROUTINE IS TO BE USED INSTEAD OF 'STOP' IN ALL PROGRAMS.
C
C FOR EXAMPLE:
C	CODE:	    CALL GSTOP(GEXIT_SUCCESS)
C
C	OUTPUT:	    12:34:56 TASKNAME  STOP0001 **** SUCCESS
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
	SUBROUTINE GSTOP(GEXIT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 GEXIT
C
	CHARACTER*30 GEXIT_TEXT(1:5)
	DATA	     GEXIT_TEXT/'SUCCESS                       ', ! 1
     *			        'OTHER                         ', ! 2
     *			        'OPERATOR ABORT                ', ! 3
     *			        'OTHER                         ', ! 4
     *			        'FATAL ERROR                   '/ ! 5
C
	CHARACTER*30 GEXIT_NOTEXT
	DATA	     GEXIT_NOTEXT/'OTHER                         '/
C
	CHARACTER*132 MSG
C
	IF(GEXIT.LT.MIN_GEXIT .OR. GEXIT.GT.MAX_GEXIT) THEN
	  WRITE(MSG,9000) GEXIT, GEXIT_NOTEXT
	ELSE
	  WRITE(MSG,9000) GEXIT, GEXIT_TEXT(GEXIT)
	ENDIF
C
	IF(ISDETACHED()) THEN
	   CALL OPSTXT(MSG)
	ELSE
           WRITE(6,9010) IAM(),MSG(1:60)
	ENDIF
C
	CALL EXIT (GEXIT)
C
9000	FORMAT('STOP',I4.4,'  ****  ',A30)
9010    FORMAT(1X,A,A)
C
	END
