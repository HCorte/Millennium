C
C SUBROUTINE TOPQUE
C $Log:   GXAFXT:[GOLS]TOPQUE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:36:12   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:52:28   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_appque.for **
C
C
C************************************************************
C
C	TOPQUE - RETURNS TOP ELEMENT ON THE QUE OR 0 IF NONE
C
C***********************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TOPQUE (TASK, BUFNUM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:QUECOM.DEF'
	INCLUDE 'INCLIB:GLIST.DEF'
C
	INTEGER*4	TASK
	INTEGER*4	BUFNUM
C
	INTEGER*4	STAT
C
C
	CALL LISTTOP (BUFNUM, QUETAB(1, TASK), STAT)
C
	IF (STAT .EQ. GLIST_STAT_EMPTY) THEN
	  BUFNUM = 0 !there is no element in the list so returns 0
	ENDIF
C
	RETURN
	END
