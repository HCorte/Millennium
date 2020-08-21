C
C SUBROUTINE GETPRFX
C
C V02 24-JUL-2000 UXN If GXPROJ not in LNM$JOB table, try LNM$GROUP instead.
C V01 XX-XXX-XXXX XXX Initial release.
C
C GETPRFX - FIND EQUIVALENCE NAME FOR APPCOM AND GET THE PREFIX
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE  GETPRFX(PROJ_PREFIX, PROJ_PREFIX_LEN)
	IMPLICIT    NONE
C
	INTEGER*4   PROJ_PREFIX, PROJ_PREFIX_LEN
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INTEGER*4	IANYL
	EXTERNAL	IANYL
C
	CHARACTER*7	PROJNAM/'GXPROJ '/
	CHARACTER*80	TRNAM
	INTEGER*4	STAT, I, J
	INTEGER*4	I4PREF
	EQUIVALENCE	(I4PREF, TRNAM)
C
	CALL LNMTRN(PROJNAM, TRNAM, 'LNM$JOB', STAT)
	IF (STAT .NE. 0) THEN
	    CALL LNMTRN(PROJNAM, TRNAM, 'LNM$GROUP', STAT)
	    IF(STAT.NE.0) THEN
	       TYPE *,'TERMINATING: NO TRANSLATION FOR THE NAME ',PROJNAM
	       STOP 'NRM_GETPRFX: FATAL ERROR'
	    ENDIF
	ENDIF
D	TYPE *,'THE TRANSLATION FOR "GXPROJ" IS ',TRNAM
C
C	SKIP DISK AND DIRECTORY NAMES
C
	J = 0
10	CONTINUE
	    I = IANYL (TRNAM, '[')
	    IF (I .LT. 1 .OR. I .GT.80) THEN
		IF(J .EQ. 0) THEN
		    TYPE *,'PROJECT NAME SHOULD CONTAIN DIRECTORY NAME'
		    CALL GSTOP(GEXIT_FATAL)
		ENDIF
		GO TO 20
	    ENDIF
C	    KEEP LOOKING
	    J = J + 1
	    TRNAM = TRNAM((I+1):80)
	GOTO 10
C
20	CONTINUE
C
C	USE TWO CHARACTERS WITH UNDESCORE...
C
	I = IANYL(TRNAM, '.')
	IF(I .LT. 1) THEN
	    I = IANYL(TRNAM, ']')
	ENDIF
	IF (I .LE. 1) THEN
	    TYPE *,'THE TRANSLATION FOR "GXPROJ" IS NOT CORRECT'
	    CALL GSTOP(GEXIT_FATAL)
	ENDIF
	IF (I .GT. 5) THEN
	    TYPE *,'WARNING: THE TRANSLATION FOR "GXPROJ" IS TOO LONG'
	ENDIF
	I = MIN(I,4)
C
	PROJ_PREFIX = I4PREF
	PROJ_PREFIX_LEN = I
	RETURN
	END
