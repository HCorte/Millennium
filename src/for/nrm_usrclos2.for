C
C SUBROUTINE USRCLOS2
C $Log:   GXAFXT:[GOLS]USRCLOS2.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:44:48   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:59:20   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskio.for **
C
C
C
C
C **** USRCLOS2
C
C This will close a file for a given LUN AND return status
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE USRCLOS2(LUN, STAT)
	IMPLICIT NONE
C
	INTEGER*4   LUN
	INTEGER*4   STAT
C
C
	CLOSE(LUN, IOSTAT=STAT)
	RETURN
	END
