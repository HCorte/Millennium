C
C SUBROUTINE WRITEXX
C $Log:   GXAFXT:[GOLS]WRITEXX.FOV  $
C  
C     Rev 1.0   17 Apr 1996 16:04:40   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 18:09:00   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskio.for **
C
C
C
C *** WRITEXX	  <<< general purpose write with wait >>>
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE WRITEXX(RAB, FDB, BEGBLK, BUFFER, STATUS)
	IMPLICIT NONE
C
	INCLUDE	'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DISKIO.DEF'
	INCLUDE	'($SYSSRVNAM)'
	INCLUDE '($RABDEF)'
	INCLUDE '($RMSDEF)'
C
	RECORD	    /RABDEF/ RAB
	INTEGER*4   FDB(FDB_LENGTH)
	INTEGER*4   BEGBLK
	INTEGER*4   BUFFER(*)
	INTEGER*4   STATUS
C
C
C	set # of bytes to transfer, beginning block #, and
C	user buffer address
C
	RAB.RAB$W_RSZ = FDB(FDB_BYTSZ)
	RAB.RAB$L_BKT = BEGBLK
	RAB.RAB$L_RBF = %LOC(BUFFER)
C
C	be sure asynchronous bit is OFF
C
	RAB.RAB$L_ROP = RAB.RAB$L_ROP .AND. .NOT.RAB$M_ASY
C
C Now do the write
C
	STATUS = SYS$WRITE(RAB)
	IF(STATUS)THEN
	  STATUS = 0		    ! FOR COMPATIBILITY
	ENDIF
C
	IF(STATUS .EQ. RMS$_EOF)THEN
	  STATUS = 144		    ! FOR COMPATIBILITY
	ENDIF
C
	FDB(FDB_STAT) = STATUS
C
	RETURN
	END
