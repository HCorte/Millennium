C PROGRAM SWINS
C  
C V11 30-MAR-2015 MTK Modified Super 14 game
C V10 10-NOV-2003 FRP Modify for Batch2 Totobola Changes.
C V05 31-JAN-2000 OXK Removed hardcoded GIND=1 etc (Vakio changes)
C V04 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			Instant Pass Thru Phase 1
C V03 06 Sep 1993 GXA Removed hardcoded Maxrows.
C V02 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C V01 01-AUG-1990 XXX RELEASED FOR VAX
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
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM SWINS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
C
	INTEGER*2 ROWS(SPGNBR),RROWS(2,TGGNBR)
	INTEGER*4 MATCH(0:SPGNBR),WINNUM(SPGNBR),RMATCH(SPGDIV)
	INTEGER*4 BET(16),BOARD(4)
	INTEGER*4 NUMROW, NUMRROW, I, J, K, EXT, SYS, BYTES, IND
	INTEGER*4 TEMP, DUMMY, HISHR
	INTEGER*4 GIND
        INTEGER*4 CANCEL_EVENT
	INTEGER*4 SCORE(2)
	BYTE BOARD1(4)
	EQUIVALENCE (DUMMY,BOARD1)
	CHARACTER*22 STRING(SPGNBR)
	CHARACTER*22 RSTRING2
	CHARACTER*50 RSTRING(2)
	LOGICAL MULTIPLE
	DATA STRING/'Enter event  1:','Enter event  2:',
     *	            'Enter event  3:','Enter event  4:',
     *	            'Enter event  5:','Enter event  6:',
     *	            'Enter event  7:','Enter event  8:',
     *	            'Enter event  9:','Enter event 10:',
     *	            'Enter event 11:','Enter event 12:',
     *	            'Enter event 13:','Enter event 14:',
     *	            'Enter event 15:','Enter event 16:',
     *	            'Enter event 17:' /
        DATA RSTRING/'Enter SUPER 14 Home result:  ',
     *               'Enter SUPER 14 Away result:  '/
	DATA RSTRING2/'Enter SUPER 14 result:'/
C
C
C
C ENTER WINNING RESULTS
C
	CALL PRMNUM('Give game index:',GIND,1,NUMSPT,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
        CANCEL_EVENT = 0  ! SET ZERO DUE IS NOT USED IN THE INPRES/INPRRES BECAUSE THE CHECK FLAG IS SET TO .FALSE.
C
	TYPE*,'SPTMAX(GIND) = ',SPTMAX(GIND)
	TYPE*,'SPTFRG(GIND) = ',SPTFRG(GIND)
        NUMROW=SPTMAX(GIND)
        NUMRROW = 0
	IF(SPTFRG(GIND).NE.0) NUMRROW = 1
20	CONTINUE
	TYPE*,'Enter winning sports results [1,X,2,C] (C: cancel events)'
	DO 30 I=1,NUMROW-NUMRROW
	CALL INPRES(STRING(I), WINNUM(I), CANCEL_EVENT, .FALSE., EXT)
	IF(EXT.LT.0) GOTO 20
30	CONTINUE

	IF(SPTFRG(GIND).EQ.1) THEN
	  TYPE*,'Enter winning SUPER14 results [0,1,M,C] (C: cancel events)'
21        CONTINUE
          DO J=1,2
            CALL INPRRES(RSTRING(J), SCORE(J), CANCEL_EVENT, .FALSE., EXT)
            IF(EXT.LT.0) GOTO 21
          ENDDO
          WINNUM(NUMROW)=ISHFT(SCORE(1),4)+IAND(SCORE(2),'0F'X)
	ENDIF

        IF(SPTFRG(GIND).EQ.2) THEN
          TYPE*,'Enter winning SUPER14 results [1 X 2,C] (C: cancel events)'
22        CONTINUE
          CALL INPRES(RSTRING2, WINNUM(NUMROW), CANCEL_EVENT, .FALSE., EXT)
          IF(EXT.LT.0) GOTO 22
        ENDIF
C
C ENTER BET
C
41	CONTINUE
	IF(SPTFRG(GIND).EQ.1) THEN
     	  TYPE*,'Enter SUPER14 bet data [1 for 0, 2 for 1, 3 for M]'
	  DO 52 J=1,2
	    CALL GETENT(BET(J),NUMROW,EXT)
	    IF(EXT.LT.0) STOP
	    RROWS(J,I)=BET(J)
52	  CONTINUE
	ENDIF

        IF(SPTFRG(GIND).EQ.2) THEN
          TYPE*,'Enter SUPER14 bet data [1 for 1, 2 for X, 3 for 2]'
          CALL GETENT(BET(1),NUMROW,EXT)
	  BET(2) = 0
	  RROWS(1,1) = BET(1)
	  RROWS(1,2) = BET(2)
          IF(EXT.LT.0) STOP
	ENDIF
C
40	CONTINUE
	MULTIPLE=.FALSE.
	TYPE*,'Enter system bet'
	CALL INPNUM('Enter system number [O-NOSYS,1-FULSYS]',SYS,0,1,EXT)
	IF(EXT.LT.0) STOP
	IF(SYS.EQ.0) GOTO 42
	IF(SPSATR(SYS).EQ.0) THEN
	  TYPE*,'Sorry, system ',SYS,' not defined'
	  GOTO 40
	ENDIF
	MULTIPLE=.TRUE.
	IF(SPSATR(SYS).EQ.FULSYS) TYPE*,'This is a full system '
	IF(SPSATR(SYS).EQ.REDSYS) TYPE*,'This is a reduced system'
C	TYPE*,SPSNUM(2,SYS)-SPSNUM(1,SYS),' triples'
C	TYPE*,SPSNUM(3,SYS)-SPSNUM(2,SYS),' doubles'
C
42	CONTINUE
	TYPE*,'Enter sports bet data [1 for 1, 2 for X, 3 for 2]'
	DO 50 I=3,NUMROW+NUMRROW
	CALL GETENT(BET(I),I-2*NUMRROW,EXT)
	IF(EXT.LT.0) GOTO 40
	ROWS(I-2*NUMRROW)=BET(I)
50	CONTINUE
C
C SAVE BOARD
C
        BYTES=(NUMROW+NUMRROW+1)/2
        IND=1
        DO 60 I=1,BYTES
        TEMP=ISHFT(BET(IND),4)+BET(IND+1)
        CALL ISBYTE(TEMP,BOARD,I-1)
C	TYPE*,IND,BET(IND),BET(IND+1),BOARD
        IND=IND+2
60      CONTINUE
C
	DO I=1,4
	  DUMMY=BOARD(I)
          WRITE(6,900) I,(BOARD1(J),J=1,4)
	ENDDO
900     FORMAT(' Board ',I1,' entered ',4Z2.2)
C
C CHECK WINSEL
C
	DUMMY=0
	CALL FASTSET(0,MATCH,SPGNBR+1)
	CALL FASTSET(0,RMATCH,SPGDIV)
	IF(MULTIPLE) THEN
	  CALL SPTCHK(BOARD,DUMMY,SYS,WINNUM,HISHR,MATCH,NUMROW,NUMRROW)
	ELSE
	  CALL FULLCHK(ROWS,WINNUM,NUMROW-NUMRROW,HISHR,MATCH)
	ENDIF
	IF(NUMRROW.GT.0 .AND. MATCH(NUMROW-NUMRROW).GT.0)
     *    CALL RFULLCHK(RROWS,WINNUM,NUMROW,SPTFRG(GIND),HISHR,RMATCH)
C
	DO 70 I=NUMROW-NUMRROW,1,-1
	TYPE*,'Sports : you match ',MATCH(I),' times ',I,' rows'
70	CONTINUE
C
	IF(NUMRROW.NE.0) 
     *	  TYPE*,'Results: you match ',RMATCH(1),' times ',1,' rows'
C
	TYPE*
	GOTO 41
	END
