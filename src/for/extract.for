C
C SUBROUTINE EXTRACT
C $Log:   GXAFXT:[GOLS]EXTRACT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:07:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:14:34   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lod1x2pol.for **
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C EXTRACT (CHAR_ARRAY, COL_SIZE, COUNT, CHAR_STR)
C	EXTRACTS 3 LEFT CHARACTERS OUT OF CHAR_ARRAY
C	STARTING FROM POSITION 'COUNT'*'COL_SIZE'
C	INTO CHARRACTER*3 ARRAY 'CHAR_STR'
C------------------------------------------------------------------------
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE EXTRACT(CHAR_ARRAY, COL_SIZE, COUNT, CHAR_STR)
	IMPLICIT NONE
C
	CHARACTER*1  CHAR_ARRAY(80)
	INTEGER*4   COL_SIZE, COUNT
	CHARACTER*3  CHAR_STR
C
	INTEGER*4   START, I, J
C
	J = JMIN0(3, COL_SIZE)
	START = COL_SIZE * COUNT - J
	CHAR_STR = '   '
C
	DO 10, I = 1, J
	    CHAR_STR(I:I) = CHAR_ARRAY(START+I)
10	CONTINUE
C
	RETURN
	END
