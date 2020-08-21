C
C PROGRAM SBRUN
C $Log:   GXAFXT:[GOLS]SBRUN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:51:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:34:10   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - sbrun.for **
C
C SBRUN.FOR
C
C V01 02-APR-91 TKO  INITIAL RELEASE
C
C This routine will start a subprocess (or a detached process if
C NRUNTSK routine iis chnged). This program is used for the SUBRUN.COM
C command procedure. It may be used stand alone also - rules of
C activating 'foreign' commands apply.
C
C Use as (for example, POOLPRO):
C
C	SBRUN :== $GXTSK:SBRUN
C	SBRUN POOLPRO
C
C The name of the progarm should be no more than 8 characters.
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
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM SBRUN
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER   CMDLIN*80
	INTEGER*4   CMDLEN
C
	INTEGER*4   LIB$GET_FOREIGN
	EXTERNAL    LIB$GET_FOREIGN
C
	INTEGER*4   ST
C
	INTEGER*4   NOFTLSIG
	EXTERNAL    NOFTLSIG
C
        CHARACTER*8 XNAM
        REAL*8  PROGNAM
        EQUIVALENCE (XNAM,PROGNAM)
C
	CALL LIB$ESTABLISH(NOFTLSIG)
C
C
C
C Get the command line with which this task was started
C
	ST = LIB$GET_FOREIGN(CMDLIN,,CMDLEN,)
	IF(.NOT.ST)THEN
	  TYPE *,IAM(),'SUBRUN - UNABLE TO PROCESS COMMAND'
	  GOTO 9000
	ENDIF
	IF(CMDLEN.LT.1)THEN
	  TYPE *,IAM(),'SUBRUN - PROGRAM NAME IS TOO SHORT'
	  GOTO 9000
	ENDIF
C
	IF(CMDLEN.GT.8)THEN
	  TYPE *,IAM(),'SUBRUN - PROGRAM NAME IS TOO LONG'
	  GOTO 9000
	ENDIF
C
	XNAM = CMDLIN(1:CMDLEN)
C
C Now run the program
C
        CALL NRUNTSK(PROGNAM)
C
9000	CONTINUE
C
C***        CALL GSTOP(1)
        END
