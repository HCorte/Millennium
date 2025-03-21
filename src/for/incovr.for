C
C SUBROUTINE INCOVR
C $Log:   GXAFXT:[GOLS]INCOVR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:36:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:38:48   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - poolupd.for **
C
C====================================================================
C    SUBROUTINE INCOVR-INCREMENT LOTTO POOL ON CONBINATION BET
C
C    CALL SEQUENCE-CALL INCOVR(OFFSET,GAM)
C    IN-OFFSET=OFFSET OF SEQUENCE SEQUENCE INTO LOTTO POOL TABLE
C       GAM  - GAM NR, OFFSET ADJUSTED FOR NO BASE
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE INCOVR(OFFSET,GAM)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
C
	INTEGER*4 OFFSET, OFF, GAM
C***  TYPE *,IAM(),'GAM ',GAM,OFFSET
10	CONTINUE
	DO 110, OFF=1,MAXOVR-1
	IF (LTOVR(2,OFF,GAM).EQ.0) GOTO 150   !SET NEW OFFSET
	IF (LTOVR(1,OFF,GAM).EQ.OFFSET) GOTO 200 !SAME OFFSET REPEATED
110	CONTINUE
	CALL FLUOVR(GAM)               !FLUSH ALL OVERFLOWS
	GOTO 10
C
150	CONTINUE
	LTLSTOVR(GAM)=OFF                       !SET LAST OVERFLOW
200	CONTINUE
	LTOVR(1,OFF,GAM)=OFFSET
	LTOVR(2,OFF,GAM)=LTOVR(2,OFF,GAM)+LTPOOL_GAMLIMIT(GAM)+1
	IF (LTOVR(2,OFF,GAM).NE.0) RETURN  !
	LTOVR(1,OFF,GAM)=LTOVR(1,LTLSTOVR(GAM),GAM)
	LTOVR(2,OFF,GAM)=LTOVR(2,LTLSTOVR(GAM),GAM)
	LTOVR(1,LTLSTOVR(GAM),GAM)=0
	LTOVR(2,LTLSTOVR(GAM),GAM)=0
	LTLSTOVR(GAM)=LTLSTOVR(GAM)-1
	RETURN
	END
