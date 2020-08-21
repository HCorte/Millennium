C
C SUBROUTINE NRUNTSKNAM
C $Log:   GXAFXT:[GOLS]NRUNTSKNAM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:14:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:09:34   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_runtsk.for **
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C NRUNTSKNAM -
C	ACTIVATE THE SUBPROCESS UNDER THE SPECIFIED NAME
C	WITH THE PROJECT PREFIX AND DO NOT WAIT FOR COMPLETEION
C--------------------------------------------------------------------
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE NRUNTSKNAM(PRGNAME,PRCNAME)
	IMPLICIT NONE
C
	BYTE	PRGNAME(8),PRCNAME(8)
	LOGICAL WFLG
C
	WFLG=.FALSE.
C
	CALL XRUNTSK(PRGNAME,PRCNAME,WFLG)
	RETURN
	END
