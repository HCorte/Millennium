C
C FUNCTION INANYL
C $Log:   GXAFXT:[GOLS]INANYL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:35:54   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:38:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_strings.for **
C
C
C
C **** INANYL
C
	INTEGER*4 FUNCTION INANYL(STRING,LOOK)
	IMPLICIT NONE
C
	CHARACTER	STRING*(*)
	CHARACTER	LOOK*(*)
C
	INTEGER*4	STR$FIND_FIRST_NOT_IN_SET
	EXTERNAL	STR$FIND_FIRST_NOT_IN_SET
C
	INANYL = STR$FIND_FIRST_NOT_IN_SET( STRING, LOOK)
	RETURN
	END
