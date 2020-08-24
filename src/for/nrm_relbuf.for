C
C SUBROUTINE RELBUF
C $Log:   GXAFXT:[GOLS]RELBUF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:41:38   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:28:12   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_quemgr.for **
C
C
C************************************************************
C
C	RELBUF - RELEASE FREE BUFFER TO THE FREQUE
C
C***********************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE RELBUF (BUFNUM)
	IMPLICIT NONE
C
	INTEGER*4	BUFNUM
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:GLIST.DEF'
C
	INTEGER*4	STAT
C
C	PARAMETER  (NUMPRO=2048)  !NUMBER OF PROCESSING BUFFERS
!	INTEGER*4	 QHEDSZ
!   PARAMETER	(QHEDSZ = 16)
	CALL ABL (BUFNUM, FREEQ, STAT) !C     FREEQ(NUMPRO+QHEDSZ)  FREE BUFFER QUEUE <-----procom.def
C the total of bytes are NUMPRO+QHEDSZ=2048+16
	IF (STAT .NE. GLIST_STAT_GOOD) THEN
		TYPE *,IAM(),'RELBUF: ABL STATUS BAD'
	ENDIF
C
	END
