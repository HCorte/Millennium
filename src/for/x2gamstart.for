C
C PROGRAM X2GAMSTART
C $Log:   GXAFXT:[GOLS]X2GAMSTART.FOV  $
C  
C     Rev 1.0   17 Apr 1996 16:18:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 18:18:18   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2gamstart.for;1 **
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM X2GAMSTART
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	X2X_GAME_STATE=X2X_GAMES_REQUP
C
	CALL GSTOP(GEXIT_SUCCESS)
	END
