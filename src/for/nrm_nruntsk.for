C
C SUBROUTINE NRUNTSK
C $Log:   GXAFXT:[GOLS]NRUNTSK.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:14:40   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:09:28   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_runtsk.for **
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C NRUNTSK,NRUNTASK -
C	ACTIVATE THE SUBPROCESS UNDER THE NAME OF THE PROGRAM
C	WITH THE PROJECT PREFIX AND DO NOT WAIT FOR COMPLETEION
C--------------------------------------------------------------------
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE NRUNTSK(NAME)
	IMPLICIT NONE
C
	BYTE	NAME(8)
	LOGICAL WFLG
C
	ENTRY NRUNTASK(NAME)
	WFLG=.FALSE.
C
	CALL XRUNTSK(NAME,NAME,WFLG)
	RETURN
	END
