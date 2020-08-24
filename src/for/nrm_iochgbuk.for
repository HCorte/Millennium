C
C SUBROUTINE IOCHGBUK
C $Log:   GXAFXT:[GOLS]IOCHGBUK.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:39:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:41:20   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskio.for **
C
C
C
C
C **** IOCHGBUK
C
C This will change the number of sectors per bucket
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE IOCHGBUK(FDB,NUMSEC)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
	INCLUDE	    'INCLIB:DISKIO.DEF'
C
	INTEGER*4   FDB(FDB_LENGTH)
	INTEGER*4   NUMSEC
C
C
C
	CALL VAXIOCHGBUK(FDB, NUMSEC*256)
C
	RETURN
	END
