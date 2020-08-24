C
C SUBROUTINE DSKQFSZ
C $Log:   GXAFXT:[GOLS]DSKQFSZ.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:00:38   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:09:34   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskqio.for **
C
C
C *** DSKFSZ
C
C -----------------------------------------------------------------------------
C	This routine returns the size of the file at the time of OPEN
C	statement !!!!
C
C	Inputs:
C		LUN - longword passed to us by RMS
C
C	Outputs:
C		FSIZE - file size at time of OPEN statement
C
C -----------------------------------------------------------------------------
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DSKQFSZ (LUN, FSIZE)
	IMPLICIT NONE
C
	INCLUDE	'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DISKQIO.DEF'
	INCLUDE '($RMSDEF)'
C
	INTEGER*4 LUN, FSIZE
C
	CALL CHKQINIT ()
C
	IF(LUN.LT.1 .OR. LUN.GT.MAXLUNARRAY)THEN
	  TYPE *,IAM(),'BAD LUN IN VAX_DISKQIO/DSKFSZ = ',LUN
C***	  CALL LIB$SIGNAL(%VAL(RMS$_FNF))
	  GOTO 9000
	ENDIF
	IF(LUNARRAY(LUN).LE.0)THEN
	  TYPE *,IAM(),'VAX_DISKQIO:DSKFSZ: FILE SIZE NOT AVAILABLE '
C***	  CALL LIB$SIGNAL(%VAL(RMS$_FNF))
	  GOTO 9000
	ENDIF
C
C Get file size
C
	FSIZE = FABFILSZ(LUN)
C
9000	CONTINUE
	RETURN
	END
