C
C PROGRAM TELL
C $Log:   GXAFXT:[GOLS]TELL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:32:46   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:50:24   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - tell.for **
C
C TELL.FOR
C
C V01 02-APR-91 TKO  INITIAL RELEASE
C
C This routine will write to a specified mailbox the info required.
C
C Use as (for example):
C
C	 TELL WAGPRO CONTINUE
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
	PROGRAM TELL
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4   I4PREFIXLEN
	INTEGER*4   I4PREFIX
	CHARACTER   CXPREFIX*4
	EQUIVALENCE (I4PREFIX,CXPREFIX)
C
	CHARACTER   CMDLIN*80
	INTEGER*4   CMDLEN
C
	CHARACTER   ANSWER*80
	INTEGER*4   ANSLEN
C
	CHARACTER   MBXNAME*80
	INTEGER*4   NAMLEN
C
	INTEGER*4   LIB$GET_FOREIGN
	EXTERNAL    LIB$GET_FOREIGN
C
	INTEGER*4   MBXCHANNEL
	INTEGER*4   K,ST
C
	INTEGER*4   NOFTLSIG
	EXTERNAL    NOFTLSIG
C
	CALL LIB$ESTABLISH(NOFTLSIG)
C
C
C
C Get the command line with which this task was started
C
	ST = LIB$GET_FOREIGN(CMDLIN,,CMDLEN,)
	IF(.NOT.ST)THEN
	  TYPE *,IAM(),'TELL - UNABLE TO PROCESS COMMAND'
	  GOTO 9000
	ENDIF
	IF(CMDLEN.LT.3)THEN
	  TYPE *,IAM(),'TELL - BAD COMMAND'
	  GOTO 9000
	ENDIF
C
C Task name must be first thing entered followed by text.
C
	NAMLEN = INDEX(CMDLIN(1:CMDLEN), ' ')
	IF(NAMLEN.EQ.0)THEN
	  TYPE *,IAM(),'TELL - BAD COMMAND'
	  GOTO 9000
	ENDIF
	NAMLEN =  NAMLEN-1
	MBXNAME = CMDLIN(1:NAMLEN)
C
C Find first non-blank after task name
C
	DO 1100 K = NAMLEN+1, CMDLEN
	  IF(CMDLIN(K:K).NE.' ')GOTO 1200
1100	CONTINUE
	TYPE *,IAM(),'TELL - BAD COMMAND'
	GOTO 9000
C
1200	CONTINUE
	ANSWER = CMDLIN(K:CMDLEN)
	ANSLEN = CMDLEN-K+1
C
C Now assign the mailbox.  Try assigning the specified name.  If we can't,
C try it with the prefix.
C
	CALL GETPRFX( I4PREFIX, I4PREFIXLEN )
	CALL MBXASSIGN( MBXNAME(1:NAMLEN), MBXCHANNEL, ST)
	IF(.NOT.ST)THEN
	  CALL MBXASSIGN( CXPREFIX(1:I4PREFIXLEN)//
     *                    MBXNAME (1:NAMLEN), MBXCHANNEL, ST)
	  IF(.NOT.ST)THEN
	    TYPE *,IAM(),'TELL - NO SUCH MAILBOX ',MBXNAME(1:NAMLEN)
	    GOTO 9000
	  ENDIF
	ENDIF
C
C Send the answer
C
	CALL MBXWRITE(MBXCHANNEL, ANSWER(1:ANSLEN), ST)
	IF(.NOT.ST)THEN
	  TYPE *,IAM(),'TELL - CANNOT SEND ANSWER'
	ENDIF
C
	CALL MBXDASSGN(MBXCHANNEL, ST)
C
9000	CONTINUE
	END
