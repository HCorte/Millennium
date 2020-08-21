C
C SUBROUTINE TAPCLOS
C $Log:   GXAFXT:[GOLS]TAPCLOS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:27:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:48:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_tapeio.for **
C
C
C*********************************************************************
C
C *** TAPCLOS	  CLOSE A TAPE
C
C*********************************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TAPCLOS(FDB, STATUS)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
	INCLUDE	    'INCLIB:TAPEIO.DEF'
	INCLUDE	    '($SYSSRVNAM)'
C
	INTEGER*4   FDB(TAPFDB_LENGTH)
	INTEGER*4   STATUS
C
	INTEGER*4   CHAN, ST
C
	INTEGER*4	NOFTLSIG
	EXTERNAL	NOFTLSIG
C
C
	CALL LIB$ESTABLISH(NOFTLSIG)	    !No fatal errors
C
C
C Get channel # from FDB and realease it
C
	CHAN = FDB(TAPFDB_CHAN)
	IF(CHAN.EQ.0)THEN
	  STATUS = -1
	  GOTO 9000
	ENDIF
C
	ST = SYS$DASSGN(%VAL(CHAN))
	IF(.NOT.ST)THEN
	  TYPE *,IAM(),'ERROR WHILE CLOSING TAPE'
	  CALL LIB$SIGNAL(%VAL(ST))
	  STATUS = -1
	  GOTO 9000
	ENDIF
C
	STATUS = 0
C
9000	CONTINUE
	RETURN
	END
