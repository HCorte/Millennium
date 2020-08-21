C
C FUNCTION ACCVIO_SIGNAL
C $Log:   GXAFXT:[GOLS]ACCVIO_SIGNAL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:07:54   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:34:30   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_snif.for **
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ACCVIO_SIGNAL.FOR
C
C THIS IS ACCESS VIOLATION EXCEPTION HANDLER. ON ACCESS VIOLATION IT
C UNWINDS THE STACK, THUS RETURNING TO THE ROUTINE THAT CALLED THE
C ESTABLISHER OF THE SIGNAL HANDLER. IT RETURNS THE ACCVIO ERROR CODE
C WITH FATAL SEVERITY MODIFIED TO NON-FATAL.
C
C-----------------------------------------------------------------------
C
C=======OPTIONS /CHECK=NOOVERFLOW
	INTEGER*4 FUNCTION ACCVIO_SIGNAL(SIGARGS, MECHARGS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE	    '($LIBDEF)'
	INCLUDE	    '($STSDEF)'
	INCLUDE	    '($SSDEF)'
C
	INTEGER*4   SIGARGS(*)
	INTEGER*4   MECHARGS(*)
C
	INTEGER*4  ERRORCODE
C
C Check if the error code is ACCVIO, change severity to a non-fatal
C error, then return to the caller of the establisher of the signal handler.
C If error code is not ACCVIO, just resignal.
C
	ERRORCODE = 0
	CALL MVBITS( SIGARGS(2), 0, 3, ERRORCODE, 0)
C
	IF( SIGARGS(2).EQ.SS$_ACCVIO ) THEN
	  CALL MVBITS( STS$K_ERROR, 0, 3, SIGARGS(2), 0)
	  CALL LIB$SIG_TO_RET(SIGARGS, MECHARGS)
	ENDIF
C
	ACCVIO_SIGNAL = SS$_RESIGNAL
C
	RETURN
	END
