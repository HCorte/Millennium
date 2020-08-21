C
C SUBROUTINE FTP_WRITEW
C $Log:   GXAFXT:[GOLS]FTP_WRITEW.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:15:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   02 Sep 1994 18:04:32   HXK
C  Merge of May,June RFSS batch 
C  
C     Rev 1.0   26 Jun 1994 14:14:58   HXK
C  Initial revision.
C  
C     Rev 1.0   21 Jan 1993 18:08:54   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskio.for **
C
C
C
C
C *** FTP_WRITEW       <<<WRITE and wait for completion>>>
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE FTP_WRITEW(FDB, BLOCK, BUFFER, STATUS)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
	INCLUDE	    'INCLIB:DISKIO.DEF'
C
	INTEGER*4   FDB(FDB_LENGTH)
	INTEGER*4   BLOCK
	INTEGER*4   BUFFER(*)
	INTEGER*4   STATUS
C
	INTEGER*4   FOR$RAB
	INTEGER*4   LUN
	INTEGER*4   BEGBLK
C
C
C
	LUN   = FDB(FDB_LUN)
        IF(BLOCK.EQ.1) THEN
	   BEGBLK= (BLOCK-1)*FDB(FDB_BLKSZ)
        ELSE
           BEGBLK= (BLOCK-1)*FDB(FDB_BLKSZ) + 1
        ENDIF
C
	CALL WRITEXX(%VAL(FOR$RAB(LUN)), FDB, BEGBLK, BUFFER, STATUS)
C
	RETURN
	END
