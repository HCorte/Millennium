C
C SUBROUTINE GETBUF
C $Log:   GXAFXT:[GOLS]GETBUF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:18:58   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:24:20   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_quemgr.for **
C
C
C************************************************************
C
C	GETBUF - DEQUEUES BUFFER FROM FREE BUFFERS QUEUE
C
C***********************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETBUF (BUFNUM)
	IMPLICIT NONE
C
	INTEGER*4	BUFNUM
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:GLIST.DEF'
C
	INTEGER*4	STAT
C
	CALL RTL (BUFNUM, FREEQ, STAT)
C
	IF (STAT .EQ. GLIST_STAT_EMPTY) THEN
		BUFNUM = 0
	ELSE
	  CALL FASTSET(0, PRO(1,BUFNUM), INPTAB-1) !PARAMETER (INPTAB=33)     !START INPUT AREA in procom.def
	ENDIF
C
	END
