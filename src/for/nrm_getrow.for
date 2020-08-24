C
C SUBROUTINE GETROW
C $Log:   GXAFXT:[GOLS]GETROW.FOV  $
C  
C V02 30-MAR-2015 MTK Modified Super 14 game
C V01 27-OCT-2003 FRP Modify for Batch2 Totobola Changes.
C
C     Rev 1.0   17 Apr 1996 13:22:32   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:28:04   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_logtra.for **
C
C
C
C SUBROUTINE TO EXPAND SPORTS BOARDS
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETROW(TRABUF,ROWS,RROWS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
	INTEGER*4 ROWS(SPGNBR,12),RROWS(2,TGGNBR,12)
	INTEGER*4 ROW, NUM, I, RCNT, J, LIMIT, IND
C
C
C if TRABUF(TWSPFRG)=0, position IND=0 corresponds to start of sports bet data;
C if TRABUF(TWSPFRG)=1, position IND=0 corresponds to SUPER14 (results) bet data
C                     and position IND=1 corresponds to start of sports bet data
	IND = 0
	IF(TRABUF(TWSPFRG).NE.0) IND = 1
	LIMIT = (TRABUF(TWSRW)+1)/2
	CALL FASTSET(0,ROWS,SPGNBR*12)
	DO 20 J = 1,TRABUF(TWNBET)
	  RCNT = 0
	  DO 10 I = 1,LIMIT
	    CALL ILBYTE(NUM,TRABUF(TWBORD),IND)
	    IND = IND+1
	    ROW = ISHFT(NUM,-4)
	    RCNT = RCNT+1
	    ROWS(RCNT,J) = ROW
	    IF(RCNT.EQ.TRABUF(TWSRW)-IND) GOTO 20
	    ROW = IAND(NUM,'0F'X)
	    RCNT = RCNT+1
	    ROWS(RCNT,J) = ROW
10	  CONTINUE
20	CONTINUE
C
C if TRABUF(TWSPFRG)>0, a bonus game goes together with the sport game

	IND = 0
        CALL FASTSET(0,RROWS,TGGNBR*2*12)

	IF(TRABUF(TWSPFRG).EQ.1) THEN
          CALL ILBYTE(NUM,TRABUF(TWBORD),IND)
          ROW = ISHFT(NUM,-4)
          RROWS(1,1,1) = ROW
          ROW = IAND(NUM,'0F'X)
          RROWS(2,1,1) = ROW
	ENDIF

        IF(TRABUF(TWSPFRG).EQ.2) THEN
          CALL ILBYTE(NUM,TRABUF(TWBORD),IND)
          ROW = IAND(NUM,'0F'X)
          RROWS(1,1,1) = ROW
        ENDIF
C
	RETURN
	END
