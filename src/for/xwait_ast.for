C
C *** SUBROUTINE XWAIT_AST ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]XWAIT_AST.FOV                                $
C  $Date::   17 Apr 1996 16:47:36                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - xwait_ast.for ***
C
C V05 24-JAN-2011 RXK Change for LIB$WAIT
C V04 22-APR-91 TKO NAME CHANGED TO XWAIT_AST TO AVOID CONFLICT WITH C LIBRARY
C V03 16-APR-91 TKO USE LIB$WAIT INSTEAD
C V02 14-APR-91 MP  CHECK RETURN CODE FROM SYS$GET_EF SERVICE,
C		    CHANGED LIB$STOP TO LIB$SIGNAL,
C		    CHANGED THE APPROACH: USED TO CALCULATE REAL*8,
C		    NOW IT BUILDS TIME INTERVAL AS ASCII STRING AND 
C		    CALLS SYSTEM ROUTINE 'BINTIM' TO CONVERT
C		    THE TIME INTO SYSTEM FORMAT. THERE ARE PROS
C		    AND CONS OF EITHER WAY, BUT IN NEW WAY THE
C		    CODE IS MORE UNDERSTANDABLE.
C		    IMPROVED COMMENTS.
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	THIS ROUTINE IS PROVIDED FOR COMPATABILITY WITH CONCURRENT.
C	IT WILL PERFORM A WAIT OF A USER SUPPLIED NUMBER OF MILLISECONDS,
C	SECONDS, OR MINUTES.
C
C Calling Sequence:
C	CALL XWAIT_AST(NUMUNIT, TYPUNIT, STATUS)
C
C Input:
C	NUMUNIT	- # OF UNITS TO WAIT
C	TYPUNIT	- UNIT TYPE (1=MSEC, 2=SECONDS, 3=MINUTES)
C
C Output:
C	STATUS	- 0=OK, -1=ERROR
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS	/CHECK=NOOVERFLOW
C
	SUBROUTINE XWAIT_AST(NUMUNIT, TYPUNIT, STATUS)
C
	IMPLICIT NONE
C
	REAL*4		R4SECS
C
	INTEGER*4	NUMUNIT,
     *			TYPUNIT,
     *			STATUS
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	IF (TYPUNIT .EQ. 1) THEN			! MILLISECONDS.
	  R4SECS = FLOAT(MAX(10, NUMUNIT)) / 1000.0	! MINIMUM = 10 MSECS.
C
	ELSE IF (TYPUNIT .EQ. 2) THEN			! SECONDS.
	  R4SECS = FLOAT(NUMUNIT)
C
	ELSE IF (TYPUNIT .EQ. 3) THEN			! MINUTES.
	  R4SECS = FLOAT(NUMUNIT) * 60.0
C
	ELSE						! UNKNOWN ???
	  STATUS = -1
	  GOTO 9999
	ENDIF
C
	CALL LIB$WAIT(R4SECS,0,4)
C
	STATUS = 0
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
