C
C SUBROUTINE WEOT
C $Log:   GXAFXT:[GOLS]WEOT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:59:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 18:04:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_tapeio.for **
C
C
C*********************************************************************
C
C *** WTAPEQ	  WRITE BLOCK WITHOUT WAIT
C
C*********************************************************************
C
C***	SUBROUTINE WTAPEQ(FDB,BUF,OWNBUF,STAT)
C***	IMPLICIT    NONE
C
C***	INTEGER*4 FDB(TAPFDB_LENGTH), BUF(*), OWNBUF(*), STAT
C
C***	TYPE *,'VAX_TAPEIO:WTAPEQ: NOT IMPLEMENTED YET...'
C***	RETURN
C***	END
C
C
C*********************************************************************
C
C *** WEOT	  WRITE END OF TAPE MARK
C
C*********************************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE WEOT  (FDB,STATUS)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:TAPEIO.DEF'
C
	INCLUDE	    '($SYSSRVNAM)'
	INCLUDE	    '($SSDEF)'
	INCLUDE	    '($IODEF)'
C
	INTEGER*4   FDB(TAPFDB_LENGTH)
	INTEGER*4   STATUS
C
	INTEGER*4   CHAN
C
	INTEGER*4   NUMTRY
C
	INTEGER*4   TAP_STATUS
	EXTERNAL    TAP_STATUS
	INTEGER*4   NOFTLSIG
	EXTERNAL    NOFTLSIG
C
C
	CALL LIB$ESTABLISH(NOFTLSIG)	    !No fatal errors
C
	CHAN  = FDB(TAPFDB_CHAN)
C
C	*** DO A QIO WITH WAIT
C
	NUMTRY = 0
1000	CONTINUE
	STATUS=SYS$QIOW ( , %VAL(CHAN), %VAL(IO$_WRITEOF),
     1		        FDB(TAPFDB_IOSB),,,
     1	                , , ,,,)
C
C
C **V06** Following code was rearranged...
C
	IF (STATUS) THEN
	    STATUS = TAP_STATUS(FDB)
	ELSE
	    IF ((NUMTRY.EQ.1.AND.STATUS.EQ.SS$_MEDOFL).OR.
     *	      STATUS.NE.SS$_MEDOFL)THEN
		CALL LIB$SIGNAL(%VAL(STATUS))
	    ENDIF
	ENDIF
C
	IF (STATUS.EQ.SS$_MEDOFL)THEN
	    IF(NUMTRY.LT.1)THEN
		NUMTRY = NUMTRY + 1
		GOTO 1000			!**V05** TRY ONCE MORE
	    ENDIF
	ENDIF
C
	RETURN
	END
