C
C SUBROUTINE TAPDISMT
C $Log:   GXAFXT:[GOLS]TAPDISMT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:27:28   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:48:20   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_tapeio.for **
C
C
C*********************************************************************
C
C *** TAPDISMT	  DISMOUNT A TAPE
C
C*********************************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TAPDISMT (DEVNAM, STATUS)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:SYSEXTRN.DEF'
	INCLUDE	    '($SYSSRVNAM)'
	INCLUDE	    '($DMTDEF)'
C
	CHARACTER   DEVNAM*(*)
	INTEGER*4   STATUS
C
	INTEGER*4   ST
C
	INTEGER*4	NOFTLSIG
	EXTERNAL	NOFTLSIG
C
C
	CALL LIB$ESTABLISH(NOFTLSIG)	    !No fatal errors
C
	ST = SYS$DISMOU( DEVNAM, %VAL(DMT$M_NOUNLOAD) )
	IF(.NOT.ST)THEN
	  TYPE *,IAM(),'ERROR WHILE DISMOUNTING ',DEVNAM
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
