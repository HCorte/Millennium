C
C SUBROUTINE TAPUNLD
C $Log:   GXAFXT:[GOLS]TAPUNLD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:28:08   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:48:58   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_tapeio.for **
C
C
C
C*********************************************************************
C
C *** TAPUNLD	  REWIND & UNLOAD TAPE
C
C*********************************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TAPUNLD(FDB,STATUS)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:TAPEIO.DEF'
C
	INCLUDE	    '($SYSSRVNAM)'
	INCLUDE	    '($IODEF)'
C
	INTEGER*4   FDB(TAPFDB_LENGTH)
	INTEGER*4   STATUS
C
	INTEGER*4   CHAN
C
C
	INTEGER*4	NOFTLSIG
	EXTERNAL	NOFTLSIG
C
C
	CALL LIB$ESTABLISH(NOFTLSIG)	    !No fatal errors
C
	CHAN  = FDB(TAPFDB_CHAN)
C
C	*** DO A QIO WITH WAIT
C
	STATUS=SYS$QIOW ( , %VAL(CHAN), %VAL(IO$_UNLOAD + IO$M_NOWAIT),
     1		        FDB(TAPFDB_IOSB),,,
     1	                , , ,,,)
C
	IF (.NOT. STATUS) THEN
	  CALL LIB$SIGNAL(%VAL(STATUS))
	ELSE
	  STATUS = 0
	ENDIF
C
	RETURN
	END
