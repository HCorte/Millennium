C
C SUBROUTINE TAPINT
C $Log:   GXAFXT:[GOLS]TAPINT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:27:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:48:30   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_tapeio.for **
C
C
C
C*********************************************************************
C
C *** TAPINT	  SET TAPE BLOCK SIZE
C
C*********************************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TAPINT (FDB, DUMMY, NUMBYTES)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
	INCLUDE	    'INCLIB:TAPEIO.DEF'
C
	INTEGER*4   FDB(TAPFDB_LENGTH)
	INTEGER*4   DUMMY
	INTEGER*4   NUMBYTES
C
C
C
C Set I/O length in FDB
C
	IF(NUMBYTES.LT.14 .OR. NUMBYTES.GT.65535)THEN
	  TYPE *,IAM(),'TAPINT - BAD BLOCK LENGTH FOR TAPE = ',NUMBYTES
	  CALL LIB$SIGNAL(%VAL(0))
	  CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
	FDB(TAPFDB_BYTSZ) = NUMBYTES
C
	RETURN
	END
