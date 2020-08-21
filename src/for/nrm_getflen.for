C
C SUBROUTINE GETFLEN
C $Log:   GXAFXT:[GOLS]GETFLEN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:19:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:25:24   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_getfinfo.for **
C
C
C
C *** GETFLEN
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETFLEN(LUN, RLEN)
	IMPLICIT NONE
C
	INTEGER*4   LUN
	INTEGER*4   RLEN
C
C
	INQUIRE(LUN, RECL=RLEN)
C
	RETURN
	END
