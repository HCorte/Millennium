C
C SUBROUTINE LKPMEM
C $Log:   GXAFXT:[GOLS]LKPMEM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:50:16   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:51:28   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_wrkset.for **
C
C******************************************************************
C	LKPMEM(FSTPAG, LSTPAG)
C******************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE LKPMEM(FSTPAG, LSTPAG)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE '($SYSSRVNAM)'
	INCLUDE '($SSDEF)'
C
	INTEGER*4   FSTPAG(*), LSTPAG(*)
C
	INTEGER*4   ARGS(2), STAT
C
C V03	PREVENT THE ERROR TO BE FATAL
C
        INTEGER*4   NOFTLSIG		  ! V03
        EXTERNAL    NOFTLSIG		  ! V03
C
        CALL LIB$ESTABLISH(NOFTLSIG)	  ! V03
C
	ARGS(1)	= %LOC(FSTPAG(1))
	ARGS(2) = %LOC(LSTPAG(1))
C
	STAT = SYS$LCKPAG(ARGS,,)
	IF (.NOT. STAT) THEN
	    CALL LIB$SIGNAL(%VAL(STAT))
	ENDIF
	END
