C
C SUBROUTINE RIMG
C $Log:   GXAFXT:[GOLS]RIMG.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:44:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:31:12   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_wimg.for **
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C	RIMG
C----------------------------------------------------------------------
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE RIMG(STRING, LENGTH)
	IMPLICIT NONE
C
	INTEGER*4   LENGTH
	CHARACTER   STRING*(*)
C
	INTEGER*4   I,L
C
	L = LEN(STRING)
C
	READ (5, 1001) STRING
1001	FORMAT(A,$)
C
	DO 20 I=L,1,-1
	    IF(STRING(I:I) .NE. ' ') GOTO 30
20	CONTINUE
30	CONTINUE
	LENGTH = I
	RETURN
	END
