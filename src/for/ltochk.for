C
C SUBROUTINE LTOCHK
C $Log:   GXAFXT:[GOLS]LTOCHK.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:58:02   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   23 Jul 1993 18:31:46   SXH
C  Relesaed for Finland
C  
C     Rev 1.0   21 Jan 1993 16:57:14   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - ltoent.for **
C
C
C
C SUBROUTINE TO CHECK LOTTO WINNING NUMBERS
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE LTOCHK(BDROFF,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'

        ! arguments                
	INTEGER*4  ST              !
        INTEGER*4  BDROFF          !

        ! variables
	INTEGER*4  J               !
	INTEGER*4  TEMP            !
	INTEGER*4  I               !

	LOGICAL    SORT            !

C
C SORT THE NUMBERS INTO ASCENDING SEQUENCE
C
	ST=-1
10	CONTINUE
	SORT=.FALSE.

	DO I=1,DLTNUM-1
	    IF(DLTWIN(I,BDROFF).GT.DLTWIN(I+1,BDROFF)) THEN
	        TEMP = DLTWIN(I,BDROFF)
	        DLTWIN(I,BDROFF) = DLTWIN(I+1,BDROFF)
	        DLTWIN(I+1,BDROFF) = TEMP
	        SORT=.TRUE.
	    ENDIF
        END DO
	IF(SORT) GOTO 10
C
25	CONTINUE
	SORT=.FALSE.
	DO I=1,DLTBFL-1
	    IF(DLTBNM(I,BDROFF).GT.DLTBNM(I+1,BDROFF)) THEN
	        TEMP=DLTBNM(I,BDROFF)
	        DLTBNM(I,BDROFF)=DLTBNM(I+1,BDROFF)
	        DLTBNM(I+1,BDROFF)=TEMP
	        SORT=.TRUE.
	    ENDIF
        END DO
	IF(SORT) GOTO 25
C
C
C
	DO I=1,DLTNUM-1
	    IF(DLTWIN(I,BDROFF).EQ.DLTWIN(I+1,BDROFF)) RETURN
        END DO
C
C
	DO I = 1,DLTBFL
	    DO J = 1,DLTNUM
	        IF(DLTBNM(I,BDROFF).EQ.DLTWIN(J,BDROFF)) RETURN
            END DO
        END DO
C
C
	ST=0

	RETURN

	END
