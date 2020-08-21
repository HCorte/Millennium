C
C SUBROUTINE CLOSEQFILE
C $Log:   GXAFXT:[GOLS]CLOSEQFILE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:36:26   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:53:56   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskqio.for **
C
C
C
C **** CLOSEQFILE
C
C This is just like closefil except it has a longer name
C (With concurrent fortran, only the 1st 8 chars are significant)
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CLOSEQFILE(FDB)
	IMPLICIT NONE
C
	INTEGER*4   FDB(*)
C
	CALL CLOSEQFIL(FDB)
	RETURN
	END
