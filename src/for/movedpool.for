C MOVEDPOOL.FOR
C
C V03 08-JUN-2000 UXN IMPLICIT NONE added.
C V02 07-APR-2000 OXK %-split fixed
C V01 23-FEB-2000 OXK Initial release
C
C This subroutine moves money between winning divisions & rollover according to
C given rule. The rules are following:
C RULE  1st target  Explanation	    
C 1	ROLLOVER    Always to rollover
C 2	DIV1	    To DIV1
C 3	HIGHDIV	    To higest division in which there are winners
C 4	UP	    UPwards to closest division in which winners
C 5	DOWN	    DOWNwards to closest division in which winners
C 6	%-SPLIT	    Split according set percentages -"-
C
C In all the cases 'if all else fails' the money is moved to rollover.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXTEND
	SUBROUTINE MOVEDPOOL(DPOOL,ROLLOVER,RULE,CURDIV,SHR,PER,NUMDIV)
	IMPLICIT NONE

        INCLUDE 'INCLIB:SYSDEFINE.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'

C PARAMETERS
	INTEGER*4 NUMDIV
	REAL*8	  DPOOL(NUMDIV)	    ! POOLS FOR EACH DIV
	INTEGER*4 ROLLOVER	    ! ROLLOVER AMOUNT
	INTEGER*4 RULE		    ! RULE TO APPLY
	INTEGER*4 CURDIV	    ! CURRENT DIVISION
	INTEGER*4 SHR(NUMDIV)
	INTEGER*4 PER(NUMDIV)
C VARIABLES
	INTEGER*4 UPDIV(20)	    ! CLOSEST DIVISION UPWARDS
	INTEGER*4 DOWNDIV(20)       ! CLOSEST DIVISION DOWNWARDS
	INTEGER*4 HIGHDIV	    ! HIGHEST DIVISION WITH WINNERS

	INTEGER*4 CNT
	INTEGER*4 TOTPER
	INTEGER*4 I,J

C Collect info about the next winning division w/ winners in either direction,
C highest winning division w/ winners etc...

        CALL FASTSET(0,UPDIV  ,NUMDIV)
        CALL FASTSET(0,DOWNDIV,NUMDIV)
	HIGHDIV=0

        DO I=1,NUMDIV-1
            IF (SHR(I).GT.0) THEN
                DO J=I+1,NUMDIV
                    UPDIV(J)=I
                ENDDO
            ENDIF
        ENDDO
        DO I=NUMDIV,2,-1
            IF (SHR(I).GT.0) THEN
                DO J=1,I-1
                    DOWNDIV(J)=I
                ENDDO
            ENDIF
        ENDDO
        DO I=NUMDIV,1,-1
            IF (SHR(I).GT.0) HIGHDIV=I
        ENDDO

C
	GOTO (100, 200, 300, 400, 500, 600) RULE

	TYPE*,'Invalid rule requested:',RULE
	CALL GSTOP(GEXIT_FATAL)

C -> ROLLOVER
100	CONTINUE
	ROLLOVER = ROLLOVER + IDNINT(DPOOL(CURDIV))
	DPOOL(CURDIV)=0.D0
C	TYPE*,'FROM',CURDIV,' TO ROLLOVER'
	RETURN

C -> DIV1 (-> ROLLOVER)
200	CONTINUE
	IF (SHR(1).GT.0) THEN
	   DPOOL(1) = DPOOL(1) + DPOOL(CURDIV)
	   DPOOL(CURDIV) = 0.D0
C	   TYPE*,'FROM',CURDIV,' TO DIV1'
	   RETURN
	ELSE
	   GOTO 100
	ENDIF

C -> HIGHDIV (-> ROLLOVER)
300	CONTINUE
	IF (HIGHDIV.GT.0) THEN
	   DPOOL(HIGHDIV) = DPOOL(HIGHDIV) + DPOOL(CURDIV)
	   DPOOL(CURDIV) = 0.D0
C	   TYPE*,'FROM',CURDIV,' TO HIGHDIV',HIGHDIV
	   RETURN
	ELSE
	   GOTO 100
	ENDIF

C -> UP (-> ROLLOVER)
400	CONTINUE
	IF (UPDIV(CURDIV).GT.0) THEN
	   DPOOL(UPDIV(CURDIV)) = DPOOL(UPDIV(CURDIV)) + DPOOL(CURDIV)
	   DPOOL(CURDIV) = 0.D0
C	   TYPE*,'FROM',CURDIV,' TO UPDIV',UPDIV(CURDIV)
	   RETURN
	ELSE
	   GOTO 100
	ENDIF

C -> DOWN (-> ROLLOVER)
500	CONTINUE
	IF (DOWNDIV(CURDIV).GT.0) THEN
	   DPOOL(DOWNDIV(CURDIV)) = DPOOL(DOWNDIV(CURDIV)) + DPOOL(CURDIV)
	   DPOOL(CURDIV) = 0.D0
C	   TYPE*,'FROM',CURDIV,' TO DOWNDIV',DOWNDIV(CURDIV)
	   RETURN
	ELSE
	   GOTO 100
	ENDIF

C SPLIT ACCORDING TO SET PERCENTAGES (-> ROLLOVER)
600	CONTINUE
	CNT=0
	TOTPER=0
	DO I=1,NUMDIV
	   IF (SHR(I).GT.0 .AND. DPOOL(I).GT.0.D0 .AND. I.NE.CURDIV) THEN
	      CNT=CNT+1
	      TOTPER=TOTPER+PER(I)
	   ENDIF
	ENDDO
	IF (CNT.GT.0) THEN
C	   TYPE*,'FROM',CURDIV,' ACCORDING TO PER TO OTHERS, CNT=',CNT
	   DO I=1,NUMDIV
	      IF (SHR(I).GT.0 .AND. DPOOL(I).GT.0.D0 .AND. I.NE.CURDIV) 
     *	      DPOOL(I)=DPOOL(I) + DPOOL(CURDIV)*CALPER(PER(I))/CALPER(TOTPER)
C	      IF (SHR(I).GT.0 .AND. DPOOL(I).GT.0.D0 .AND. I.NE.CURDIV)) 
C     *		       TYPE*,I,' GOT ITS PART ~', DPOOL(CURDIV)*PER(I)/TOTPER
	   ENDDO
	   DPOOL(CURDIV) = 0.D0
	   RETURN
	ELSE
	   GOTO 100
	ENDIF


C
C THIS IS NEVER REACHED
C
	END
