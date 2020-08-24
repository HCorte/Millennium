C
C SUBROUTINE FILRENAM
C $Log:   GXAFXT:[GOLS]FILRENAM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:10:08   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:18:00   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskio.for **
C
C**********************************************************************
C	RENAME FILE
C**********************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE FILRENAM(LUN, NAME, STAT)
	IMPLICIT NONE
C
	INCLUDE	'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DISKIO.DEF'
	INCLUDE	'($SYSSRVNAM)'
	INCLUDE '($LIBDEF)'
C
	INTEGER*4   LUN
	CHARACTER*(*)	NAME
	INTEGER*4   STAT
C
	INTEGER*4   ST
	INTEGER*4   LIB$RENAME_FILE
C
	ST = LIB$RENAME_FILE(FNAMES(LUN), NAME)
	IF (.NOT. ST) THEN
	    STAT = -1
	    RETURN
	ENDIF
	STAT = 0
	RETURN
	END
