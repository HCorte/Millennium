C SUBROUTINE STRDRAW
C
C V02 08-JUN-2000 UXN GOTO 10 replaced with GOTO 20
C V01 17-MAY-1999 UXN Initial revision.
C  
C STRDRAW.FOR
C
C SUBROUTINE TO ENTER RESULTS FOR SUPER TRIPLE.
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT

	SUBROUTINE STRDRAW(TIES1,TIES2,TIES3,NSTR)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'

C---- Local Variables used.

	INTEGER*4 I
	INTEGER*4 TIES1
	INTEGER*4 TIES2
	INTEGER*4 TIES3
	INTEGER*4 ROW
	INTEGER*4 EXT
	INTEGER*4 FLAG
	INTEGER*4 K,TOT_WIN
	INTEGER*4 J
	INTEGER*4 NSTR
	INTEGER*4 WINNERS(3,MAXSTRTI)

	CHARACTER*54 BUF

	CHARACTER*6 ENTRY(4)
	DATA ENTRY/'first ','second','third ','forth '/

	CHARACTER*10 ENTRY2(4)
	DATA ENTRY2/'first 2nd ','second 2nd ',
     *	            'third 2nd ','forth 2nd '/

	CHARACTER*10 ENTRY3(4)
	DATA ENTRY3/'first 3rd ','second 3rd ',
     *	            'third 3rd ','forth  3rd '/


C----------------------- Start of code -----------------------------
	
C---- Enter first place.
        CALL FASTSET(0,WINNERS,2*MAXSTRTI)

	DO 100 I = 1,TIES1
	  WRITE (5,9000) ENTRY(I)
10	  CONTINUE	  
	  CALL INPNUM(BUF,ROW,1,MAXSTRRW,EXT)
	  IF (EXT .NE. 0 .AND. EXT .NE. -5) GOTO 10
	  IF (EXT .EQ. -5) THEN
	    TYPE *,' Event will be cancelled '
	    CALL INPYESNO('Is this correct <Y/N> ',FLAG)
	    IF (FLAG .NE. 1) GOTO 10
	    GOTO 999
	  END IF
	  WRITE(5,9001) ROW,(DSTNMS(K,ROW),K=1,3)
	  CALL INPYESNO('Is this correct [Y/N] ',FLAG)
	  IF (FLAG .NE. 1) GOTO 10
	  IF (STROWSTS(ROW,NSTR) .EQ. GAMCAN .OR. 
     *	      STROWSTS(ROW,NSTR) .EQ. GAMREF) THEN
	    WRITE(5,9002) ROW,(DSTNMS(K,ROW),K=1,3)
	    GOTO 10
	  END IF
	  WINNERS(1,I) = ROW
100	CONTINUE

C---- Enter second place.

	IF (TIES2 .LE. 0) GOTO 210

	DO 200 I = 1,TIES2
20	  CONTINUE	  
	  WRITE (5,9003) ENTRY2(I)
	  CALL INPNUM(BUF,ROW,1,MAXSTRRW,EXT)
	  IF (EXT .NE. 0 .AND. EXT .NE. -5) GOTO 20
	  IF (EXT .EQ. -5) THEN
	    TYPE *,' Cannot Cancel results for 1st are in '
	    GOTO 20
	  END IF
	  WRITE(5,9001) ROW,(DSTNMS(K,ROW),K=1,3)
	  CALL INPYESNO('Is this correct [Y/N] ',FLAG)
	  IF (FLAG .NE. 1) GOTO 20
	  IF (STROWSTS(ROW,NSTR) .EQ. GAMCAN .OR. 
     *	      STROWSTS(ROW,NSTR) .EQ. GAMREF) THEN
	    WRITE(5,9002) ROW,(DSTNMS(K,ROW),K=1,3)
	    GOTO 20
	  END IF
	  DO J = 1,TIES1	!---- Check for duplicate 1st + 2nd.
	      IF (WINNERS(1,J) .EQ. ROW) THEN
	        TYPE *,' 2nd place cannot be same as 1st place.'
	        GOTO 20
	      END IF
	  END DO
	  WINNERS(2,I) = ROW
200	CONTINUE

210     CONTINUE

C---- Enter third place.

	IF (TIES3 .LE. 0) GOTO 900

	DO 300 I = 1,TIES3
30	  CONTINUE	  
	  WRITE (5,9003) ENTRY3(I)
	  CALL INPNUM(BUF,ROW,1,MAXSTRRW,EXT)
	  IF (EXT .NE. 0 .AND. EXT .NE. -5) GOTO 30
	  IF (EXT .EQ. -5) THEN
	    TYPE *,' Cannot Cancel results for 1st are in '
	    GOTO 30
	  END IF
	  WRITE(5,9001) ROW,(DSTNMS(K,ROW),K=1,3)
	  CALL INPYESNO('Is this correct [Y/N] ',FLAG)
	  IF (FLAG .NE. 1) GOTO 30
	  IF (STROWSTS(ROW,NSTR) .EQ. GAMCAN .OR. 
     *	      STROWSTS(ROW,NSTR) .EQ. GAMREF) THEN
	    WRITE(5,9002) ROW,(DSTNMS(K,ROW),K=1,3)
	    GOTO 30
	  END IF
	  DO J = 1,TIES1	!---- Check for duplicate 1st + 3rd.
	      IF (WINNERS(1,J) .EQ. ROW) THEN
	        TYPE *,' 3rd place cannot be same as 1st place.'
	        GOTO 30
	      END IF
	  END DO
	  DO J = 1,TIES2	!---- Check for duplicate 2nd + 3rd.
	      IF (WINNERS(1,J) .EQ. ROW) THEN
	        TYPE *,' 3rd place cannot be same as 2nd place.'
	        GOTO 30
	      END IF
	  END DO
	  WINNERS(3,I) = ROW
300	CONTINUE
C
C Set correct winning combinations.
C
900	CONTINUE
	TOT_WIN = 0
	IF(WINNERS(3,1).EQ.0) THEN
	   DO I=1,MAXSTRTI
	      WINNERS(3,I) = WINNERS(2,I)
	   ENDDO
	ENDIF
	IF(WINNERS(3,1).EQ.0) THEN
	   DO I=1,MAXSTRTI
	      WINNERS(3,I) = WINNERS(1,I)
	   ENDDO
	ENDIF
	IF(WINNERS(2,1).EQ.0) THEN
	   DO I=1,MAXSTRTI
	      WINNERS(2,I) = WINNERS(1,I)
	   ENDDO
	ENDIF
	
	DO 930 I=1,MAXSTRTI
	   IF(WINNERS(1,I).LE.0) GOTO 930
	   DO 920 J=1,MAXSTRTI
	      IF(WINNERS(2,J).LE.0) GOTO 920
	      DO 910 K=1,MAXSTRTI
	         IF(WINNERS(3,K).LE.0) GOTO 910
		 IF(WINNERS(1,I).EQ.WINNERS(2,J)) GOTO 920
		 IF(WINNERS(1,I).EQ.WINNERS(3,K)) GOTO 910
		 IF(WINNERS(2,J).EQ.WINNERS(3,K)) GOTO 910
		 TOT_WIN = TOT_WIN + 1
		 IF(NSTR.EQ.1) THEN
		    DSTWIN(1,TOT_WIN) = WINNERS(1,I)
		    DSTWIN(2,TOT_WIN) = WINNERS(2,J)
		    DSTWIN(3,TOT_WIN) = WINNERS(3,K)
	         ELSE
		    DSTHLD(1,TOT_WIN) = WINNERS(1,I)
		    DSTHLD(2,TOT_WIN) = WINNERS(2,J)
		    DSTHLD(3,TOT_WIN) = WINNERS(3,K)
		 ENDIF
910	      CONTINUE
920	   CONTINUE
930	CONTINUE
	IF(NSTR.EQ.1) DSTCMB = TOT_WIN
	RETURN
C 
C Event was cancelled !!!
C 
999	CONTINUE
	IF (NSTR .EQ. 1) THEN
	  DSTWIN(1,1) = -1
	  DSTCMB = 0
	ELSE
	  DSTHLD(1,1) = -1
	END IF
	RETURN
C------------------------ Format Statements ------------------------

9000     FORMAT (' Enter ',A6,' winning row number < C to Cancel Event > ')

9001	FORMAT (' Row ',I2,1X,3A4)

9002	FORMAT (' Sorry row ',I2,1X,3A4,' has been cancelled')

9003     FORMAT (' Enter ',A10,' winning row number < C to Cancel Event > ')

	END
