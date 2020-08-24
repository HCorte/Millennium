C
C SUBROUTINE CONVERT_CASE
C $Log:   GXAFXT:[GOLS]CONVERT_CASE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:42:16   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:00:04   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lod1x2pol.for **
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CONVERT_CASE (IN, OUT)
C	CONVERTS CHARACTER 'IN' INTO OPPOSITE CASE AND PUT IT INTO 'OUT'
C-----------------------------------------------------------------------
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CONVERT_CASE(IN,OUT)
	IMPLICIT NONE
C
 
	CHARACTER*1 IN,OUT,A_UPPER/'A'/,A_LOWER/'a'/,C
	BYTE	    CB,A_BYT_UPPER,A_BYT_LOWER
	EQUIVALENCE (CB,C)
	EQUIVALENCE (A_BYT_UPPER,A_UPPER)
	EQUIVALENCE (A_BYT_LOWER,A_LOWER)
C
	OUT = ' '
C
	IF(IN .GE. 'A' .AND. IN .LE. 'Z') THEN
	    C = IN
	    CB = CB - A_BYT_UPPER + A_BYT_LOWER
	    OUT = C
	    RETURN
	ENDIF
C
	IF(IN .GE. 'a' .AND. IN .LE. 'z') THEN
	    C = IN
	    CB = CB - A_BYT_LOWER + A_BYT_UPPER
	    OUT = C
	    RETURN
	ENDIF
C
	END
