C
C SUBROUTINE XREWIND
C $Log:   GXAFXT:[GOLS]XREWIND.FOV  $
C  
C     Rev 1.0   17 Apr 1996 16:47:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 18:38:38   DAB
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
C *** XREWIND	  REWIND TAPE
C
C*********************************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE XREWIND(FDB,STATUS)
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
	STATUS=SYS$QIOW ( , %VAL(CHAN), %VAL(IO$_REWIND + IO$M_NOWAIT),
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
