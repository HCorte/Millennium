C
C PROGRAM DETRUN
C
C V01 24-JUL-2000 UXN Create a detached process.
C
C This routine will start a detached process.
C This program is used for the DETRUN.COM
C command procedure. It may be used stand alone also - rules of
C activating 'foreign' commands apply.
C
C Use as (for example, POOLPRO):
C
C	DETRUN :== $GXTSK:DETRUN
C	DETRUN POOLPRO
C
C The name of the progarm should be no more than 8 characters.
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
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM DETRUN
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
	  TYPE *,IAM(),'DETRUN - UNABLE TO PROCESS COMMAND'
	  GOTO 9000
	ENDIF
	IF(CMDLEN.LT.1)THEN
	  TYPE *,IAM(),'DETRUN - PROGRAM NAME IS TOO SHORT'
	  GOTO 9000
	ENDIF
C
	IF(CMDLEN.GT.8)THEN
	  TYPE *,IAM(),'DETRUN - PROGRAM NAME IS TOO LONG'
	  GOTO 9000
	ENDIF
C
	XNAM = CMDLIN(1:CMDLEN)
C
C Now run the on-line program
C
        CALL START(PROGNAM)
C
9000	CONTINUE
        END
