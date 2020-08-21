C
C SUBROUTINE VER_BNGCHK
C $Log:   GXAFXT:[GOLS]VER_BNGCHK.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:53:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.3   12 Nov 1994 19:15:04   HXK
C  FIXED BUG IN FULL HOUSE
C  
C     Rev 1.2   12 Nov 1994 18:33:40   HXK
C  Removed sort of Full House numbers
C  
C     Rev 1.1   08 Nov 1994 15:14:28   HXK
C  Ensure loops don ot exceed array bounds 
C  
C     Rev 1.0   17 Oct 1994 14:30:52   HXK
C  Initial revision.
C  
C
C
C SUBROUTINE TO CHECK LOTTO WINNING NUMBERS
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE VER_BNGCHK(SUBGAME,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'

        ! arguments                
	INTEGER*4  ST              !
        INTEGER*4  SUBGAME         !

        ! variables
	INTEGER*4  TEMP            !
	INTEGER*4  I               !

        INTEGER*4 WRKHLD(BGONBR)

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
	    IF(DBNHAB(I).GT.DBNHAB(I+1)) THEN
	        TEMP = DBNHAB(I)
	        DBNHAB(I) = DBNHAB(I+1)
	        DBNHAB(I+1) = TEMP
	        SORT=.TRUE.
	    ENDIF
        ENDDO
	IF(SORT) GOTO 110
C
C
	DO I=1,BGONAB-1
	    IF(DBNHAB(I).EQ.DBNHAB(I+1)) RETURN
        ENDDO
C
	ST=0
	RETURN
C
C FULL HOUSE
C
200     CONTINUE
C
C CHECK THE NUMBERS
C
        DO I = 1,BGONBR
           WRKHLD(I) = DBNHLD(I)
        ENDDO
210     CONTINUE
        SORT=.FALSE.
        DO I=1,BGONBR-1
            IF(WRKHLD(I).GT.WRKHLD(I+1)) THEN
                TEMP = WRKHLD(I)
                WRKHLD(I) = WRKHLD(I+1)
                WRKHLD(I+1) = TEMP
                SORT=.TRUE.
            ENDIF
        ENDDO
        IF(SORT) GOTO 210
C
        DO I=1,BGONBR-1
            IF(WRKHLD(I).EQ.WRKHLD(I+1)) RETURN
        ENDDO
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
            IF(DBNHLN(I).GT.DBNHLN(I+1)) THEN
                TEMP = DBNHLN(I)
                DBNHLN(I) = DBNHLN(I+1)
                DBNHLN(I+1) = TEMP
                SORT=.TRUE.
            ENDIF
        ENDDO
        IF(SORT) GOTO 310
C
C
        DO I=1,BGONLN-1
            IF(DBNHLN(I).EQ.DBNHLN(I+1)) RETURN
        ENDDO
C
        ST=0
        RETURN

	END
