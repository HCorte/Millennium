C
C SUBROUTINE LSYSDSP
C $Log:   GXAFXT:[GOLS]LSYSDSP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:57:12   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:56:56   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lsyschk.for **
C
C
C+++++++++++++++++++++++++++++++++++++++++++++
C+
C+    LSYSDSP(SYSNR,STATUS)     ;DISPLAY SYSTEM BET
C+    IN:
C+    SYSNR     - SYSTEM NUMBER
C+    OUT:
C+    STATUS    - .NON. 0 IF INVALID BET
C+
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE LSYSDSP(SYSNR,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LSYSCOM.DEF'
C
	INTEGER*4 BOARD(LMXMARK), PARTIAL, OFFSET, COUNT, MARKS
	INTEGER*4 TOTAL_BOARDS, II, I, OFF, POINTER, BONUSBET
	INTEGER*4 FROM, CHOSE, GAME, STATUS, SYSNR
C
	CHARACTER*1 SHARE(0:LMXSHR),BON_SHARE(0:LMXSHR)
	STATUS=0
	WRITE(6,*) ' '
	WRITE(6,*) 'Displaying system bet number ',SYSNR,' game ',
     *	            LSYS_GAME(SYSNR)
	IF (LSYS_ATR(SYSNR).EQ.0.OR.
     *	    LSYS_NUMBET(SYSNR).LE.0.OR.
     *	    LSYS_GAME(SYSNR).LE.0.OR.
     *	    LSYS_NUMMRK(SYSNR).LE.0.OR.
     *	    LSYS_PTR(SYSNR).LE.0) THEN
	   WRITE(6,*) 'Invalid definition:'
	   WRITE(6,*) 'Atribute ',LSYS_ATR(SYSNR)
	   WRITE(6,*) '# of bets ',LSYS_NUMBET(SYSNR)
	   WRITE(6,*) 'Game nr    ',LSYS_GAME(SYSNR)
	   WRITE(6,*) 'Number of marks ',LSYS_NUMMRK(SYSNR)
	   IF (LSYS_PTR(SYSNR).LT.0) WRITE (6,*) 'Invalid pointer'
	   STATUS=-1
	ENDIF
C
	GAME=LSYS_GAME(SYSNR)
C
	CHOSE=LSYS_GAMCHOSE(GAME)
	FROM=LSYS_GAMFROM(GAME)
	BONUSBET=LSYS_BONUSBET(GAME)
      WRITE(6,*) 'Game ',CHOSE,'/',FROM,' ** mark ',LSYS_NUMMRK(SYSNR)
     *	           ,' ** ',BONUSBET,' bonus number(s) drawn'
	IF (CHOSE.LE.0.OR.FROM.LE.0.OR.CHOSE.GE.FROM.OR.
     *	    BONUSBET.LT.0.OR.BONUSBET.GT.LMXBONUS) THEN
	   WRITE(6,*) 'Invalid game'
	   STATUS=-1
	ENDIF
C
	IF (STATUS.NE.0) RETURN
C
	POINTER=LSYS_PTR(SYSNR)
	IF (LSYS_ATR(SYSNR).EQ.LSYS_FULL) THEN
	   WRITE (6,*) 'Full system bet'
	   IF (LSYS_NUMBET(SYSNR).NE.1) THEN
	      WRITE(6,*) 'Invalid number of bets ',LSYS_NUMBET(SYSNR)
	      STATUS=-1
	      RETURN
	   ENDIF
	   IF (LSYS_NUMMRK(SYSNR).NE.LSYS_TAB(POINTER)) THEN
	      WRITE(6,*) 'Invalid number of marks ',LSYS_NUMMRK(SYSNR)
     *	                 ,LSYS_TAB(POINTER)
	      STATUS=-1
	      RETURN
	   ENDIF
	ELSE
	   WRITE(6,*) 'reduced system bet, '
     *	          ,'bet corresponds to ',LSYS_NUMBET(SYSNR),' bet(s)'
	ENDIF
C
	DO 10, OFF=0,CHOSE
	   SHARE(OFF)=CHAR(LSYS_GAMDIV(OFF,GAME)+48)
	   BON_SHARE(OFF)=CHAR(LSYS_GAMBON(OFF,GAME)+48)
10	CONTINUE
C
	WRITE(6,*) 'Game divisions ',(SHARE(I),I=0,CHOSE),' bonus '
     *	      ,'divisions ',(BON_SHARE(II),II=0,CHOSE)
	WRITE(6,*) ' '
	WRITE(6,*) 'Bet definition'
C
	TOTAL_BOARDS=0
	DO 60, OFF=1,LSYS_NUMBET(SYSNR)
	   MARKS=LSYS_TAB(POINTER)
	   CALL BITCNT(LSYS_TAB(POINTER+1),L_SYSBYTES,COUNT)
	   IF (COUNT.NE.MARKS.OR.MARKS.EQ.0) THEN
	      WRITE(6,*) 'Invalid bet ',OFF,' counts ',MARKS,COUNT
	      STATUS=-1
	      RETURN
	   ENDIF
	   CALL FASTSET(0,BOARD,LMXMARK)
	   COUNT=0
	   DO 50, OFFSET=0,LMXMARK-1
	      IF (TSBIT(LSYS_TAB(POINTER+1),OFFSET)) THEN
	         COUNT=COUNT+1
	         BOARD(COUNT)=OFFSET+1
	      ENDIF
50	   CONTINUE
C
	   WRITE(6,900) COUNT,(BOARD(I),I=1,COUNT)
900	   FORMAT(1H ,I3,' marks ',', board: ',32I3)
	   CALL SYSPRICE(CHOSE,FROM,MARKS,PARTIAL)
	   TOTAL_BOARDS=TOTAL_BOARDS+PARTIAL
	   POINTER=POINTER+2
60	CONTINUE
	IF (LSYS_BOARD(SYSNR).NE.TOTAL_BOARDS) THEN
	 WRITE (6,*) 'boards do not correspond to bet definition '
	 WRITE (6,*)LSYS_BOARD(SYSNR),' defined ',TOTAL_BOARDS,' found'
	 STATUS=-1
	 RETURN
	ENDIF
C
	WRITE(6,*) 'This bet corresponds to ',TOTAL_BOARDS,' boards'
	WRITE(6,*) '++++++++++++++++++++++++++++++++++++++++++++'
	RETURN
	END
