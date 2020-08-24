C
C SUBROUTINE TAPOPEN
C $Log:   GXAFXT:[GOLS]TAPOPEN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:28:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:48:52   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_tapeio.for **
C
C
C
C
C*********************************************************************
C
C *** TAPOPEN	  OPEN TAPE AND INITIALIZE FDB
C
C*********************************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TAPOPEN (FDB, DEVNAM, STATUS)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
	INCLUDE	    'INCLIB:TAPEIO.DEF'
	INCLUDE	    '($SYSSRVNAM)'
C
	INTEGER*4   FDB(TAPFDB_LENGTH)
	CHARACTER   DEVNAM*(*)
	INTEGER*4   STATUS
C
	INTEGER*4	K,ST, CHAN
C
	INTEGER*4	NOFTLSIG
	EXTERNAL	NOFTLSIG
C
C
	CALL LIB$ESTABLISH(NOFTLSIG)	    !No fatal errors
C
C
C
C
C
C Clear FDB
C
	DO 110 K = 1, TAPFDB_LENGTH
	  FDB(K) = 0
110	CONTINUE
C 
C Check to be sure the tape is mounted
C
	CALL TAPCHECK ( DEVNAM, STATUS )
	IF(STATUS.NE.0)GOTO 9000
C
C
C Assign the channel
C
	ST = SYS$ASSIGN(DEVNAM, CHAN,,)
	IF(.NOT.ST) THEN
	  TYPE *,IAM(),'FILE ', DEVNAM, ' OPEN ERROR'
	  CALL LIB$SIGNAL(%VAL(ST))
	  STATUS = -1
	  GOTO 9000
	ENDIF
C
C Save channel number in FDB
C
	FDB(TAPFDB_CHAN) = CHAN
C
	STATUS = 0
C
9000	CONTINUE
	RETURN
	END
