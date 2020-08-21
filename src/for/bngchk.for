C
C SUBROUTINE BNGCHK
C $Log:   GXAFXT:[GOLS]BNGCHK.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:20:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.3   12 Nov 1994 19:15:40   HXK
C  FIXED BUG IN FULL HOUSE
C  
C     Rev 1.2   12 Nov 1994 18:33:00   HXK
C  Removed sort of Full House numbers
C  
C     Rev 1.1   08 Nov 1994 15:13:58   HXK
C  Ensure loops don ot exceed array bounds
C  
C     Rev 1.0   17 Oct 1994 14:24:12   HXK
C  Initial revision.
C  
C
C
C SUBROUTINE TO CHECK LOTTO WINNING NUMBERS
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE BNGCHK(SUBGAME,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'

        ! arguments                
	INTEGER*4  ST              !
        INTEGER*4  SUBGAME         !

        ! variables
	INTEGER*4  J               !
	INTEGER*4  TEMP            !
	INTEGER*4  I               !

        INTEGER*4  WRKWIN(BGONBR)

	LOGICAL    SORT            !

        ST = -1

        GOTO (100,200,300) SUBGAME

C
C BINGO A,B
C
100	CONTINUE
C
C SORT THE NUMBERS INTO ASCENDING SEQUENCE
C
110	CONTINUE
	SORT=.FALSE.
	DO I=1,BGONAB-1
	    IF(DBNWAB(I).GT.DBNWAB(I+1)) THEN
	        TEMP = DBNWAB(I)
	        DBNWAB(I) = DBNWAB(I+1)
	        DBNWAB(I+1) = TEMP
	        SORT=.TRUE.
	    ENDIF
        END DO
	IF(SORT) GOTO 110
C
C
	DO I=1,BGONAB-1
	    IF(DBNWAB(I).EQ.DBNWAB(I+1)) RETURN
        END DO
C
	ST=0
	RETURN
C
C FULL HOUSE
C
200     CONTINUE
C
C SORT THE NUMBERS INTO ASCENDING SEQUENCE
C
        DO I=1,BGONBR
           WRKWIN(I) = DBNWIN(I)
        ENDDO
210     CONTINUE
        SORT=.FALSE.
        DO I=1,BGONBR-1
            IF(WRKWIN(I).GT.WRKWIN(I+1)) THEN
                TEMP = WRKWIN(I)
                WRKWIN(I) = WRKWIN(I+1)
                WRKWIN(I+1) = TEMP
                SORT=.TRUE.
            ENDIF
        END DO
        IF(SORT) GOTO 210
C
C
        DO I=1,BGONBR-1
            IF(WRKWIN(I).EQ.WRKWIN(I+1)) RETURN
        END DO
C
        ST=0
        RETURN
C
C LUCKY NUMBER
C
300     CONTINUE
C
C SORT THE NUMBERS INTO ASCENDING SEQUENCE
C
310     CONTINUE
        SORT=.FALSE.
        DO I=1,BGONLN-1
            IF(DBNWLN(I).GT.DBNWLN(I+1)) THEN
                TEMP = DBNWLN(I)
                DBNWLN(I) = DBNWLN(I+1)
                DBNWLN(I+1) = TEMP
                SORT=.TRUE.
            ENDIF
        END DO
        IF(SORT) GOTO 310
C
C
        DO I=1,BGONLN-1
            IF(DBNWLN(I).EQ.DBNWLN(I+1)) RETURN
        END DO
C
        ST=0
        RETURN

	END
