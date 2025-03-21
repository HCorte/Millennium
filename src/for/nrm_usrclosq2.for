C
C SUBROUTINE USRCLOSQ2
C $Log:   GXAFXT:[GOLS]USRCLOSQ2.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:44:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:59:34   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskqio.for **
C
C
C
C **** USRCLOS2
C
C This will close a file for a given LUN AND return status
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE USRCLOSQ2(LUN, STAT)
	IMPLICIT NONE
C
	INCLUDE	'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
	INCLUDE	'INCLIB:DISKQIO.DEF'
	INCLUDE	'($SYSSRVNAM)'
	INCLUDE '($RMSDEF)'
C
	INTEGER*4   STAT, LUN
C
	CALL CHKQINIT()
C
C Check if file was opened for QIO
C
	IF(LUN.LT.1 .OR. LUN.GT.MAXLUNARRAY)THEN
	  TYPE *,IAM(),'BAD LUN IN VAX_DISKQIO/USRCLOS2 = ',LUN
C***	  CALL LIB$SIGNAL(%VAL(RMS$_FNF))
	  GOTO 9000
	ENDIF
	IF(LUNARRAY(LUN).GT.0)THEN
C
C File was opened for QIO and should be closed with SYS$DASSGN
C
	  STAT = SYS$DASSGN (%VAL(LUNARRAY(LUN)))
	  LUNARRAY(LUN) = -1
	  IF(.NOT. STAT) THEN
	    TYPE *,IAM(),'SYS$DASSGN FAILED, CHAN= ',LUNARRAY(LUN)
	    CALL LIB$SIGNAL(%VAL(STAT))
	  ENDIF
	ELSE
	  CLOSE (LUN, IOSTAT=STAT)
	ENDIF
C
9000	CONTINUE
	RETURN
	END
