C
C SUBROUTINE INPRESP
C $Log:   GXAFXT:[GOLS]INPRESP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:37:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   23 Jul 1993 18:36:52   SXH
C  Initial revision.
C  
C
C ** Source - sptent.for **
C
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE INPRESP(STRING,NUM,EXT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

        ! arguments
	CHARACTER  STRING*(*)

        INTEGER*4  NUM
        INTEGER*4  EXT

        ! variables
	INTEGER*4  INBUF(5)
        INTEGER*4  BLANK
        INTEGER*4  ZERO

	CHARACTER  CBUF(20)

	EQUIVALENCE(INBUF,CBUF)

	DATA ZERO/Z0/
	DATA BLANK/'    '/
C
C
10	CONTINUE

	EXT = 0
	NUM = 0
	CALL WIMG(5,STRING)
	READ (5,900) INBUF
C
C CHECK FOR EXIT
C
	IF(CBUF(1).EQ.'E'.OR.CBUF(1).EQ.'e') THEN
	    EXT = -1
	    RETURN
	ENDIF
C
	IF(CBUF(1).EQ.'1'.AND.CBUF(2).EQ.' ') THEN
	    NUM = 1
	    RETURN
	ENDIF
C
	IF(CBUF(1).EQ.'2'.AND.CBUF(2).EQ.' ') THEN
	    NUM = 2
	    RETURN
	ENDIF
C
	IF(CBUF(1).EQ.'3'.AND.CBUF(2).EQ.' ') THEN
	    NUM = 4
	    RETURN
	ENDIF
C
	IF(CBUF(1).EQ.'4'.AND.CBUF(2).EQ.' ') THEN
	    NUM = 8
	    RETURN
	ENDIF
C
	WRITE(5,901)
	GOTO 10
C
C
900	FORMAT(5A4)
901	FORMAT(' Invalid input - Enter 1, 2, 3 or 4 only')

	END
