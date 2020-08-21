C
C SUBROUTINE XWAIT
C
C V05 24-JAN-2011 RXK Change for LIB$WAIT
C V04 22-APR-91 TKO Name changed to XWAIT to avoid conflict with C library
C V03 16-APR-91 TKO Use LIB$WAIT instead
C V02 14-APR-91 MP  CHECK RETURN CODE FROM SYS$GET_EF SERVICE,
C		    CHANGED LIB$STOP TO LIB$SIGNAL,
C		    CHANGED THE APPROACH: USED TO CALCULATE REAL*8,
C		    NOW IT BUILDS TIME INTERVAL AS ASCII STRING AND 
C		    CALLS SYSTEM ROUTINE 'BINTIM' TO CINVERT
C		    THE TIME INTO SYSTEM FORMAT. THERE ARE PROS.
C		    AND CONS. OF EITHER WAY, BUT IN NEW WAY THE
C		    CODE IS MORE UNDERSTANDABLE.
C		    IMPROVED COMMENTS.
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C This routine is provided for compatability with concurrent.  It will
C perform a wait of a user supplied number of milliseconds, seconds, or
C minutes.
C
C CALLING SEQUENCE:
C
C 	CALL XWAIT(NUMUNIT, TYPUNIT, STATUS)
C
C INPUT:
C	NUMUNIT	    # of units to wait
C	TYPUNIT	    Unit type (1 = msec, 2=seconds, 3= minutes)
C
C OUTPUT:
C	STATUS	    0 = ok, -1 = error
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
C
C
C=======OPTIONS	/CHECK=NOOVERFLOW
	SUBROUTINE XWAIT (NUMUNIT, TYPUNIT, STATUS)
	IMPLICIT NONE
C
	INTEGER*4	NUMUNIT
	INTEGER*4	TYPUNIT
	INTEGER*4	STATUS
C
	INTEGER*4	XUNITS
	REAL*4		R4SECS
C
C
	IF(TYPUNIT.EQ.1)THEN
	  XUNITS = MAX(10,NUMUNIT)	    !MINIMUM = 10 MSECS
	  R4SECS = FLOAT(XUNITS)/1000.0
	ELSE IF(TYPUNIT.EQ.2)THEN
	  R4SECS = FLOAT(NUMUNIT)
	ELSE IF(TYPUNIT.EQ.3)THEN
	  R4SECS = FLOAT(NUMUNIT)*60.0
	ELSE
	  STATUS = -1
	  GOTO 9000
	ENDIF
C
	CALL LIB$WAIT(R4SECS,0,4)
	STATUS = 0
C
9000	CONTINUE
	RETURN
	END
