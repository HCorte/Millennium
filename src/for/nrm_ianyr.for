C
C FUNCTION IANYR
C $Log:   GXAFXT:[GOLS]IANYR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:34:28   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:36:58   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_strings.for **
C
C
C
C **** IANYR
C
	INTEGER*4 FUNCTION IANYR(STRING,LOOK)
	IMPLICIT NONE
C
	CHARACTER	STRING*(*)
	CHARACTER	LOOK*(*)
C
	INTEGER*4	LENSTRING, LENLOOK
	INTEGER*4	STRINGOFF, LOOKOFF
C
C
	LENSTRING = LEN(STRING)
	LENLOOK   = LEN(LOOK)
C
	DO 1900 STRINGOFF = LENSTRING, 1, -1
	  DO 1800 LOOKOFF = LENLOOK,   1, -1
	    IF(STRING(STRINGOFF:STRINGOFF).EQ.LOOK(LOOKOFF:LOOKOFF))THEN
	      GOTO 2000
	    ENDIF
1800	  CONTINUE
1900	CONTINUE
	STRINGOFF = 0
C
2000	CONTINUE
	IANYR = STRINGOFF
C
	RETURN
	END
