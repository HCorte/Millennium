C
C SUBROUTINE CLOSEFIL
C $Log:   GXAFXT:[GOLS]CLOSEFIL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:36:16   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:53:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskio.for **
C
C
C
C **** CLOSEFIL
C
C This will close a file for a given FDB
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CLOSEFIL(FDB)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
	INCLUDE	    'INCLIB:DISKIO.DEF'
C
	INTEGER*4   FDB(FDB_LENGTH)
	INTEGER*4   LUN
C
	LUN = FDB(FDB_LUN)
	FDB(FDB_LUN) = 0
	CLOSE(LUN)
C
	RETURN
	END
