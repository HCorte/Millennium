C
C SUBROUTINE VAXIOCHGBUK
C $Log:   GXAFXT:[GOLS]VAXIOCHGBUK.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:49:54   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 18:01:08   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskio.for **
C
C
C
C
C **** VAXIOCHGBUK
C
C This will change the number of sectors per bucket
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE VAXIOCHGBUK(FDB,NUMBYTES)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
	INCLUDE	    'INCLIB:DISKIO.DEF'
C
	INTEGER*4   FDB(FDB_LENGTH)
	INTEGER*4   NUMBYTES
C
C
C
	FDB(FDB_BYTSZ) = NUMBYTES	    !BYTES  PER I/O
	FDB(FDB_BLKSZ) = (NUMBYTES+511)/512 !BLOCKS PER I/O
C
	RETURN
	END
