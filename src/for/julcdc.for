C
C SUBROUTINE JULCDC
C $Log:   GXAFXT:[GOLS]JULCDC.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:42:34   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   09 Feb 1993 13:24:10   EBD
C  Merge contents of dval julcdc routine and dcan julcdc routine to form
C  one julcdc routine to be used by both dval and dcan
C  
C     Rev 1.0   21 Jan 1993 16:44:10   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - dval.for **
C
C
C
C
C
C     JULCDC.FTN
C
C     CONVERT JULIAN DATE TO CDC
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE JULCDC(JUL,CDC,YEAR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INTEGER*2 DATE(12)
C
	INTEGER*4 JUL,CDC,YEAR
	INTEGER*4 JULIAN
C
	JULIAN=JUL
	IF (JUL.GE.500) JULIAN=JUL-500
	IF( (DAYJUL .GE. 500 .AND. JUL .LT. 500)
     *    .OR. (JULIAN.GT.DAYJUL) ) YEAR=YEAR-1
        IF(YEAR.LT.0) YEAR=99
	DATE(3)=YEAR
	DATE(4)=JULIAN
	DATE(5)=0
	CALL JDATE(DATE)
	CDC=DATE(5)
	RETURN
	END
