C
C SUBROUTINE VAXIOQCHGBUK
C $Log:   GXAFXT:[GOLS]VAXIOQCHGBUK.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:49:58   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 18:01:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskqio.for **
C
C
C
C
C **** VAXIOQCHGBUK
C
C This will change the number of sectors per bucket
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE VAXIOQCHGBUK(FDB,LUN,NUMBYTES)
	IMPLICIT NONE
C
	INCLUDE	'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
	INCLUDE	'INCLIB:DISKQIO.DEF'
C
	INTEGER*4   FDB(7)
	INTEGER*4   LUN
	INTEGER*4   NUMBYTES
C
C
C
	FDB(FDB_BYTES) = NUMBYTES	    !BYTES PER I/O
C
	RETURN
	END
