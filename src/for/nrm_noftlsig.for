C
C FUNCTION NOFTLSIG
C $Log:   GXAFXT:[GOLS]NOFTLSIG.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:13:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:07:52   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_noftlsig.for **
C
C NOFTLSIG.FOR
C
C V02 27-NOV-91 TKO  Name changed from GSIGNAL to avoid conflict with VAXCRTL
C V01 09-JAN-91 TKO  INITIAL RELEASE
C
C This function is used to turn all errors into warnings.  To use this,
C you must declare this function as an external (i.e.,:
C
C	INTEGER*4   NOFTLSIG
C	EXTERNAL    NOFTLSIG
C
C Then you must establish this as the condition handler as follows:
C
C	CALL LIB$ESTABLISH( NOFTLSIG )
C
C Any time an exception occurs during this routine or any lower subroutine
C (i.e., one called by this routine or its subroutines), this program will
C intercept the condition, turn it into a non-fatal error, and resignal to the
C next highest routine in the calling chain (which will normally simply print
C the warning and traceback info).
C
C After the establishing routine returns to its caller, this signal handler
C is disabled.  (You could also revert back to the normal condition handler
C by calling LIB$REVERT).
C
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
	INTEGER*4 FUNCTION NOFTLSIG(SIGARGS, MECHARGS)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    '($LIBDEF)'
	INCLUDE	    '($STSDEF)'
	INCLUDE	    '($SSDEF)'
C
	INTEGER*4   SIGARGS(*)
	INTEGER*4   MECHARGS(*)
C
	INTEGER*4   ERRORCODE
C
C
C Get the error code.  If it is a fatal error, change severity to a non-fatal
C error, then resignal.
C
	ERRORCODE = 0
	CALL MVBITS( SIGARGS(2), 0, 3, ERRORCODE, 0)
C
	IF( ERRORCODE.EQ.STS$K_SEVERE ) THEN
	  CALL MVBITS( STS$K_ERROR, 0, 3, SIGARGS(2), 0)
	ENDIF
C
	NOFTLSIG = SS$_RESIGNAL
C
	RETURN
	END
