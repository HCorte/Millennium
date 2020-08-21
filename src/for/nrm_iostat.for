C
C SUBROUTINE IOSTAT
C $Log:   GXAFXT:[GOLS]IOSTAT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:40:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:42:00   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskio.for **
C
C
C
C *** IOSTAT       <<<wait for completion of previous writenw>>
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE IOSTAT(FDB, STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DISKIO.DEF'
C
	INTEGER*4   FDB(FDB_LENGTH)
	INTEGER*4   STATUS
C
	INTEGER*4   FOR$RAB
	INTEGER*4   LUN
C
C
C
	LUN   = FDB(FDB_LUN)
C
	CALL IOSTATXX(%VAL(FOR$RAB(LUN)), FDB, STATUS)
C
	RETURN
	END
