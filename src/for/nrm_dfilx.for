C
C SUBROUTINE DFILX
C $Log:   GXAFXT:[GOLS]DFILX.FOV  $
C
C V02 06-NOV-97	UXN DELETING ALL THE VERSIONS OF THE FILE ADDED.
C		    DELETING THE FILE IS DONE WITH LIB$DELETE_FILE FUNCTION.
C  
C     Rev 1.0   17 Apr 1996 12:52:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:04:32   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_dfilw.for **
C
C
C
C *** DFILX
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DFILX(FILENAME, DUM1, DUM2, ST)
	IMPLICIT NONE
	INCLUDE '(LIB$ROUTINES)'
C
	CHARACTER   FILENAME*(*)
	INTEGER*4   DUM1
	INTEGER*4   DUM2
	INTEGER*4   ST
C
	ST = LIB$DELETE_FILE(FILENAME,';*',,,,,,,)
	ST = 0
	END
