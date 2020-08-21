C
C SUBROUTINE GPAUSE
C
C V02 08-NOV-2000 UXN Don't use GPAUSE for detached processes (GPAUSE should not
C                     be called by any online process)
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 02-APR-1991 TKO INITIAL RELEASE
C
C This routine is called in place of the normal FORTRAN PAUSE statement.
C It will do the following:
C
C 1)	If the parent process is in DCL mode, it will create a mailbox and
C	wait for mail indicating whether it should continue or cancel
C
C 2)	If the parent process is not in DCL mode, it will simply prompt
C	whether or not to continue.
C
C
C COPYRITF.DEF+++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C COPYRIGHT 1991 GTECH CORPORATION.  ALL RIGHTS RESERVED.
C
C CONFIDENTIAL PROPRIETARY INFORMATION
C This item is the property of GTECH Corporation, W. Greenwich, Rhode
C Island, and contains confidential and trade secret information.  It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH.  Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published or disclosed, in whold or in part, directly
C or indirectly, except as expressly authorized by an officer of
C GTECH pursuant to written agreement.
C COPYRITF.DEF-------------------------------------------------------
C
	SUBROUTINE GPAUSE
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER   ANSWER*4
	INTEGER*4   ANSLEN
C
C
	IF( ISDETACHED() ) THEN
	    CALL OPSTXT('GPAUSE: programming error')
	    CALL OPSTXT('GPAUSE: dont use CALL GPAUSE() for detached processes')
            CALL OPSTXT('GPAUSE: continuing...')
	    RETURN
	ENDIF
C
2000	CONTINUE
	CALL PRMTEXT('Enter CONT or STOP',ANSWER,ANSLEN)
C
	IF(ANSWER.EQ.'CONT' .OR. ANSWER.EQ.'cont')THEN
	  TYPE *,IAM(),'CONTINUING'
	  GOTO 9000
	ELSE IF(ANSWER.EQ.'STOP' .OR. ANSWER.EQ.'stop')THEN
	  CALL GSTOP(GEXIT_OPABORT)
	ENDIF
	TYPE *,IAM(),'Invalid answer'
	GOTO 2000
C
C
9000	CONTINUE
	RETURN
	END
