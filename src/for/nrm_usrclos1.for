C
C SUBROUTINE USRCLOS1
C $Log:   GXAFXT:[GOLS]USRCLOS1.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:44:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:59:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskio.for **
C
C
C
C **** USRCLOS1
C
C This will close a file for a given LUN without returning status
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE USRCLOS1(LUN)
	IMPLICIT NONE
C
	INTEGER*4   LUN
C
C
	CLOSE(LUN)
	RETURN
	END
