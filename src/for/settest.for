C SETTEST.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 10-APR-89 TKO  INITIAL RELEASE
C
C This will set P(TSTMOD) on/off
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
	PROGRAM SETTEST
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 ANS, TSTVAL
C
	CALL COPYRITE
C
C
	TSTVAL=P(TSTMOD)
10	CONTINUE
	IF(TSTVAL.GT.0) THEN
	   TYPE*,'TEST MODE is set on'
	ELSE
	   TYPE*,'TEST MOSE is set off'
	ENDIF
	TYPE*,' '
	TYPE*,' '
	TYPE*,'Do you want to change this values? (Y/N) '
	CALL YESNO(ANS)
	IF(ANS.EQ.2) THEN
	   CALL GSTOP(GEXIT_SUCCESS)
	ELSE
	   IF(TSTVAL.LE.0) THEN
	      TYPE*,'Do you want to turn TEST MODE on? (Y/N)'
	      CALL YESNO(ANS)
	      IF(ANS.EQ.1) TSTVAL=1
	   ELSEIF(TSTVAL.GT.0) THEN
	      TYPE*,'Do you want to turn TEST MODE off? (Y/N)'
	      CALL YESNO(ANS)
	      IF(ANS.EQ.1) TSTVAL=0
	   ENDIF
	ENDIF
	P(TSTMOD)=TSTVAL
	END
