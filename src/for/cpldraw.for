C SUBROUTINE CPLDRAW
C
C V02 18-MAY-1999 UXN DCPWIN and DCPHLD changed.
C V01 23-NOV-1995 PXB Initial revision.
C  
C CPLDRAW.FOR
C
C SUBROUTINE TO ENTER RESULTS FOR TODAY'S COUPLE.
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT

	SUBROUTINE CPLDRAW(TIES1,TIES2,NCPL)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'

C---- Local Variables used.

	INTEGER*4 I,J,TOT_WIN
	INTEGER*4 TIES1
	INTEGER*4 TIES2
	INTEGER*4 ROW
	INTEGER*4 DISP_ROW
	INTEGER*4 EXT
	INTEGER*4 FLAG
	INTEGER*4 K
	INTEGER*4 NCPL

	CHARACTER*54 BUF

	CHARACTER*6 ENTRY(4)
	DATA ENTRY/'first ','second','third ','forth '/

	CHARACTER*10 ENTRY2(4)
	DATA ENTRY2/'first E2 ','second E2 ',
     *	            'third E2 ','forth E2 '/
	INTEGER*4   WINNERS(2,MAXCPLTI)

C----------------------- Start of code -----------------------------

C---- Enter winner of event 1.
	CALL FASTSET(0,WINNERS,2*MAXCPLTI)

	DO 100 I = 1,TIES1
	  WRITE (5,900) ENTRY(I)
10	  CONTINUE	  
	  CALL INPNUM(BUF,ROW,1,(MAXCPLRW/2),EXT)
	  IF (EXT .NE. 0 .AND. EXT .NE. -5) GOTO 10
	  IF (EXT .EQ. -5) THEN
	    TYPE *,' Event will be cancelled '
	    CALL INPYESNO('Is this correct <Y/N>',FLAG)
	    IF (FLAG .NE. 1) GOTO 10
	    GOTO 999
	  END IF
	  WRITE(5,901) ROW,(DCPNMS(K,ROW),K=1,3)
	  CALL INPYESNO('Is this correct [Y/N]',FLAG)
	  IF (FLAG .NE. 1) GOTO 10
	  IF (CPROWSTS(ROW,NCPL) .EQ. GAMCAN .OR. 
     *	      CPROWSTS(ROW,NCPL) .EQ. GAMREF) THEN
	    WRITE(5,902) ROW,(DCPNMS(K,ROW),K=1,3)
	    GOTO 10
	  END IF
	  WINNERS(1,I) = ROW
100	CONTINUE

C---- Enter winner of event 2.

	DO 200 I = 1,TIES2
	  WRITE (5,9001) ENTRY2(I)
20	  CONTINUE	  
	  CALL INPNUM(BUF,ROW,1,(MAXCPLRW/2),EXT)
	  IF (EXT .NE. 0 .AND. EXT .NE. -5) GOTO 20
	  IF (EXT .EQ. -5) THEN
	    TYPE *,' Event will be cancelled '
	    CALL INPYESNO('Is this correct <Y/N>',FLAG)
	    IF (FLAG .NE. 1) GOTO 20
	    GOTO 999
	  END IF
	  DISP_ROW = ROW
	  ROW = ROW + MAXCPLRW/2
	  WRITE(5,901) DISP_ROW,
     *		       (DCPNMS(K,ROW),K=1,3)
	  CALL INPYESNO('Is this correct [Y/N]',FLAG)
	  IF (FLAG .NE. 1) GOTO 20
	  IF (CPROWSTS(ROW,NCPL) .EQ. GAMCAN .OR. 
     *	      CPROWSTS(ROW,NCPL) .EQ. GAMREF) THEN
	    WRITE(5,902) DISP_ROW,(DCPNMS(K,ROW),K=1,3)
	    GOTO 20
	  END IF
	  WINNERS(2,I) = ROW
200	CONTINUE
C
C Set correct winning combinations.
C
	TOT_WIN = 0
	DO 920 I=1,MAXCPLTI
	   IF(WINNERS(1,I).LE.0) GOTO 920
	   DO 910 J=1,MAXCPLTI
	      IF(WINNERS(2,J).LE.0) GOTO 910
	      TOT_WIN = TOT_WIN + 1
	      IF(NCPL.EQ.1) THEN
		 DCPWIN(1,TOT_WIN) = WINNERS(1,I)
		 DCPWIN(2,TOT_WIN) = WINNERS(2,J)
	      ELSE
		 DCPHLD(1,TOT_WIN) = WINNERS(1,I)
		 DCPHLD(2,TOT_WIN) = WINNERS(2,J)
	      ENDIF
910	   CONTINUE
920	CONTINUE
	IF(NCPL.EQ.1) DCPCMB = TOT_WIN
	RETURN
C
C Event cancelled !
C
999	CONTINUE
	IF (NCPL .EQ. 1) THEN
	      DCPWIN(1,1) = -1
	      DCPCMB = 0
	 ELSE
	      DCPHLD(1,1) = -1
	 END IF
	 RETURN

C------------------------ Format Statements ------------------------

900     FORMAT (' Enter ',A6,' winning row number E1 < C to Cancel Event > ')
9001    FORMAT (' Enter ',A6,' winning row number E2 < C to Cancel Event > ')

901	FORMAT (' Row ',I2,1X,3A4)

902	FORMAT (' Sorry row ',I2,1X,3A4,' has been cancelled')
	END
