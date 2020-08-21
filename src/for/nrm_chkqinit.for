C
C SUBROUTINE CHKQINIT
C $Log:   GXAFXT:[GOLS]CHKQINIT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:33:14   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:50:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskqio.for **
C
C
C
C **** CHKINIT
C
C This will close a file for a given FDB
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CHKQINIT()
	IMPLICIT NONE
C
	INCLUDE	'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DISKQIO.DEF'
C
	INTEGER*4   I
	LOGICAL*4   INITIALIZED/.FALSE./
C
	IF(INITIALIZED) THEN
	    RETURN
	ENDIF
	DO 100, I=1,MAXLUNARRAY
	    LUNARRAY(I) = -1
100	CONTINUE
	INITIALIZED = .TRUE.
C
	RETURN
	END
