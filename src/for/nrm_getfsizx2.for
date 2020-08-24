C
C SUBROUTINE GETFSIZX2
C $Log:   GXAFXT:[GOLS]GETFSIZX2.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:20:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:25:56   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_getfinfo.for **
C
C
C
C *** GETFSIZX2
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETFSIZX2(FAB, SIZE)
	IMPLICIT NONE
C
	INCLUDE	    '($FABDEF)'
C
	RECORD	    /FABDEF/ FAB
	INTEGER*4   SIZE
C
	SIZE = FAB.FAB$L_ALQ
	RETURN
	END
