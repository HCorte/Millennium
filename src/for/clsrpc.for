C
C SUBROUTINE CLSRPC
C $Log:   GXAFXT:[GOLS]CLSRPC.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:37:10   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:54:54   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - repcan.for **
C
C
C SUBROUTINE TO CLOSE FILES FOR REPCAN
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CLSRPC(FDB)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INTEGER*4 FDB(7)
	CALL CLOSEFIL(FDB)
	RETURN
	END
