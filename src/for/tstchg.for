C
C SUBROUTINE TSTCHG
C $Log:   GXAFXT:[GOLS]TSTCHG.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:38:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:54:48   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - net_netsub2.for;1 **
C
C
C     SAME AS TSTCHG, USES LOKON, LOKOFF
C     LIMITS DATA TO [0,32 K]
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TSTCHG(DATA,INCREMENT,FINAL)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	LOGICAL DELAY
	INTEGER*2 DATA(2)
	INTEGER*4 FINAL, ST, INCREMENT
C
10	CONTINUE
	DELAY=LOKON(DATA(1))
	IF (DELAY) THEN
	  CALL XWAIT(4,1,ST)
	  GOTO 10
	ENDIF
C
	DATA(2)=DATA(2)+INCREMENT
	IF(DATA(2).LT.0)DATA(2)=0
	FINAL=DATA(2)
	CALL LOKOFF(DATA(1))
	RETURN
	END
