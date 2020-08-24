C
C SUBROUTINE GETFTYP
C $Log:   GXAFXT:[GOLS]GETFTYP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:20:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:26:02   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_getfinfo.for **
C
C
C
C *** GETFTYP
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETFTYP(LUN, TYP)
	IMPLICIT NONE
C
	INTEGER*4   LUN
	INTEGER*4   TYP
C
	LOGICAL	    ISTHERE
	CHARACTER*20 ORG
C
C
	INQUIRE(LUN, OPENED=ISTHERE, ORGANIZATION=ORG)
	IF(ISTHERE)THEN
	  IF(ORG.EQ.'RELATIVE')THEN
	    TYP = 0
	  ELSE IF(ORG.EQ.'SEQUENTIAL')THEN
	    TYP = 2
	  ELSE
	    TYP = 255
	  ENDIF
	ELSE
	  TYP = 255
	ENDIF
	RETURN
	END
