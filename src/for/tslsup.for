C TSLSUP.FCC
C
C V06 06-JUL-1994 UXN ALLOW TO CHANGE DATA ONLY ON PRIMARY SYSTEM.
C V05 04-MAR-1994 HXK TIDIED UP DISPLAY OF COMBINATION CHANGE FOR 4,5,6.
C V04 03-MAR-1994 JXP Use open / close instead of suppress / unsuppress 
C                     terminology
C V03 01-MAR-1994 JXP Correction for last mark in combination suppression
C V02 01-MAR-1994 HXK CHANGED CHECK FOR 'X' (DRAW) ON MARKING ROW MARK.
C V01 01-MAR-1994 NOW SENDS STATE CHANGES THROUGH CMDPRO IF GAME LIVE.
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
C
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM TSLSUP
	IMPLICIT NONE
C
c
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:TSLCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:HASHMEM.DEF'

	INTEGER*4  GNUM, GIND, DRAW, ST, FLAG, EFLG, I, K
	INTEGER*4  GTYP                    
	INTEGER*4  EXT, REPLY, OFFSET, CMD
	INTEGER*4  ROW(6)
	INTEGER*4  TAB_1X2(6)
	INTEGER*4  X, Y, RW,J,TMP
	INTEGER*4  SUP, MAX_LINES, LUN, PAGE, LINCNT, COPY
        INTEGER*4  FDB(7)                  
        INTEGER*4  NEXT_REC, NEXT_BLOCK, PSTAT, NUM_ROWS
        INTEGER*4  CBUF(CDLEN)
C
	CHARACTER*35	TXT(3)
	CHARACTER*35	UTXT(3)
	CHARACTER*29	BETLIN
	PARAMETER (MAX_LINES=62)
        LOGICAL LIVE_GAM /.FALSE./
	DATA LINCNT/70/,PAGE/0/
C
c	CALL COPYRITE
C
	TXT(1)='***** Mark 1 is CLOSED, on row '
	TXT(2)='***** Mark 2 is CLOSED, on row '
	TXT(3)='***** Mark X is CLOSED, on row '
	UTXT(1)='*** Mark 1 is OPEN, on row '
	UTXT(2)='*** Mark 2 is OPEN, on row '
	UTXT(3)='*** Mark X is OPEN, on row '

	GTYP=TTSL
	GIND=1
C	DRAW=NUM
C        IF(GIND.LT.1.OR.GIND.GT.MAXIND) THEN
C            WRITE(5,3000) GTNAMES(TTSL)
C            CALL GSTOP(GEXIT_FATAL)
C        ENDIF
C
C

        IF(DAYSTS.EQ.DSOPEN) LIVE_GAM=.TRUE.

        GNUM=GTNTAB(TTSL,GIND)
        IF(GNUM.LT.1) THEN
            WRITE(5,3010) GTNAMES(TTSL),GIND
            CALL GSTOP(GEXIT_FATAL)
        ENDIF
        IF(DRAW.LT.1) DRAW=DAYDRW(GNUM)
        IF(DRAW.EQ.0) DRAW=DAYHDR(GNUM)
C
C GET DATA FROM COMMON OR DISK
C
        IF(DRAW.EQ.DAYDRW(GNUM)) THEN
            CALL GAMLOG(TTSL,GIND,DTSREC,TSLSTS)
            GOTO 50
        ENDIF
C
C
        CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
        CALL IOINIT(FDB,1,DTSSEC*256)
        IF(ST.NE.0) THEN
            WRITE(5,3020) (GFNAMES(K,GNUM),K=1,5),ST
            CALL USRCLOS1(     1)
            CALL GSTOP(GEXIT_FATAL)
        ENDIF
        CALL READW(FDB,DRAW,DTSREC,ST)
        IF(ST.NE.0) THEN
            WRITE(5,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
            CALL USRCLOS1(     1)
            CALL GSTOP(GEXIT_FATAL)
        ENDIF
        IF(DTSSTS.EQ.0) THEN
            WRITE(5,3040) GTNAMES(TTSL),GIND,DRAW
            CALL USRCLOS1(     1)
            CALL GSTOP(GEXIT_FATAL)
        ENDIF
        CALL USRCLOS1(     1)
C
C SCAN THROUGH THE DATE TABLE FOR TOTO SELECT AND SEE WHAT
C ROWS ARE TO BE MODIFIED
C
        EFLG=0
C
C PRINT MENU AND GET FUNCTION
C      
50	CONTINUE
C	CALL CLRSCR(5)
	WRITE(5,902) 
	WRITE(5,902) 
	WRITE(5,900) 

        CALL INPNUM('ENTER OPTION: ',CMD,1,4,EXT)
	IF(EXT.EQ.-1) GOTO 5000
        IF(EXT.LT.0) GOTO 50
	IF(CMD.NE.4.AND.P(SYSTYP).NE.LIVSYS) THEN
	    TYPE*,IAM(),'Combinations can be closed/opened only on ',
     *                  'primary system!'
	    GOTO 50
	ENDIF
        GOTO (100,200,300,400) CMD

100	CONTINUE
	WRITE(5,902) 
        CALL INPNUM('Enter row number to be modified (E-Exit)',
     *        ROW(1),1,40,EXT)
C     *        ROW(1),1,DTSRWS,EXT)
	IF(EXT.EQ.-1) THEN              ! Exit
            EFLG=0
            GOTO 50
        ENDIF
        IF(EXT.LT.0) GOTO 100
	    WRITE(5,901)	ROW(1),
     *	    	  (DTSNMS(K,1,ROW(1)),K=1,3),
     *		  (DTSNMS(K,2,ROW(1)),K=1,3)
        CALL WIMG(5,'Is this correct [Y/N] ')
        CALL YESNO(FLAG)
        IF(FLAG.NE.1) GOTO 100
110	CONTINUE
	CALL INPNUM('Suppress  X=draw, 1=home, 2=away, E=Exit',SUP,1,2,EXT)
	IF(EXT.EQ.-1) THEN              ! Exit
            EFLG=0
            GOTO 50
        ENDIF
 	IF(EXT.EQ.-6) THEN
           EXT=3
           SUP=EXT
        ENDIF
        IF(EXT.LT.0) GOTO 50
        
        IF(LIVE_GAM) THEN
           CALL FASTSET(0,CBUF,CDLEN)
           CBUF(1)=6                                               
           CBUF(3)=TCTSL
           CBUF(6)='SYS '                                             
           CBUF(8)=GIND                                           
           CBUF(9)=ROW(1)
           CBUF(10)=SUP
111        CONTINUE
           CALL QUECMD(CBUF,ST)
           IF(ST.NE.0) THEN
              TYPE*,IAM(),' Queue command error > ',ST,' continue to retry'
	      WRITE(5,902) 
              CALL GPAUSE
              GOTO 111
           ENDIF
	   IF(HASH_ROW_SUP(ROW(1),SUP).EQ.HASH_CLOSE) THEN
	      WRITE(5,903)  UTXT(SUP),ROW(1)
	   ELSE
	      WRITE(5,903)  TXT(SUP),ROW(1)
	   ENDIF
        ELSE
	   IF(HASH_ROW_SUP(ROW(1),SUP).EQ.HASH_CLOSE) THEN
	      HASH_ROW_SUP(ROW(1),SUP)=HASH_OK
	      WRITE(5,903)  UTXT(SUP),ROW(1)
	      WRITE(5,902) 
	   ELSE
	      HASH_ROW_SUP(ROW(1),SUP)=HASH_CLOSE
	      WRITE(5,903)  TXT(SUP),ROW(1)
	      WRITE(5,902) 
	   ENDIF
        ENDIF

	GOTO 100


200	CONTINUE
	WRITE(5,902) 
        CALL INPNUM('Enter FIRST row number (E-Exit)',ROW(1),1,40,EXT)
	IF(EXT.EQ.-1) THEN              ! Exit
            EFLG=0
            GOTO 50
        ENDIF
        IF(EXT.LT.0) GOTO 200
	    WRITE(5,901)  ROW(1),
     *	    	  (DTSNMS(K,1,ROW(1)),K=1,3),
     *		  (DTSNMS(K,2,ROW(1)),K=1,3)
        CALL WIMG(5,'Is this correct [Y/N] ')
        CALL YESNO(FLAG)
        IF(FLAG.NE.1) GOTO 200
210	CONTINUE
        CALL INPNUM('Enter SECOND row number (E-Exit)',
     *        ROW(2),1,40,EXT)
	IF(EXT.EQ.-1) THEN              ! Exit
            EFLG=0
            GOTO 50
        ENDIF
        IF(EXT.LT.0) GOTO 210
	    WRITE(5,901)  ROW(2),
     *	    	  (DTSNMS(K,1,ROW(2)),K=1,3),
     *		  (DTSNMS(K,2,ROW(2)),K=1,3)
        CALL WIMG(5,'Is this correct [Y/N] ')
        CALL YESNO(FLAG)
        IF(FLAG.NE.1) GOTO 210

        IF(LIVE_GAM) THEN
           CALL FASTSET(0,CBUF,CDLEN)
           CBUF(1)=7
           CBUF(3)=TCTSL
           CBUF(6)='SYS '
           CBUF(8)=GIND
           CBUF(9)=ROW(1)
           CBUF(10)=ROW(2)
211        CONTINUE
           CALL QUECMD(CBUF,ST)
           IF(ST.NE.0) THEN
              TYPE*,IAM(),' Queue command error > ',ST,' continue to retry'
              CALL GPAUSE
              GOTO 211
           ENDIF
	   IF(HASH_COMB_SUP(ROW(1),ROW(2)).EQ.HASH_CLOSE) THEN
	      WRITE(5,907)  ROW(1),ROW(2)
	  ELSE
	      WRITE(5,904)  ROW(1),ROW(2)
	  ENDIF
        ELSE
	   IF(HASH_COMB_SUP(ROW(1),ROW(2)).EQ.HASH_CLOSE) THEN
	      HASH_COMB_SUP(ROW(1),ROW(2))=HASH_OK
	      HASH_COMB_SUP(ROW(2),ROW(1))=HASH_OK
	      WRITE(5,907)  ROW(1),ROW(2)
	  ELSE
	      HASH_COMB_SUP(ROW(1),ROW(2))=HASH_CLOSE
	      HASH_COMB_SUP(ROW(2),ROW(1))=HASH_CLOSE
	      WRITE(5,904)  ROW(1),ROW(2)
	  ENDIF
        ENDIF

	GOTO 200

300	CONTINUE
        WRITE(5,902) 
	CALL INPNUM('Enter number of rows in combination (E-Exit)',
     *        NUM_ROWS,1,6,EXT)
	IF(EXT.EQ.-1) THEN              ! Exit
            EFLG=0
            GOTO 50
        ENDIF
	IF(EXT.LT.0) GOTO 300
	DO X=1,NUM_ROWS
305	    CONTINUE
            CALL INPNUM('Enter row number to be modified (E-Exit)',
     *        ROW(X),1,40,EXT)
	    IF(EXT.EQ.-1) THEN              ! Exit
                EFLG=0
                GOTO 50
            ENDIF
            IF(EXT.LT.0) GOTO 305
	    DO Y=1,X-1
		IF(ROW(X).EQ.ROW(Y)) GOTO 305
	    END DO
	    WRITE(5,901)  ROW(X),
     *	    	  (DTSNMS(K,1,ROW(X)),K=1,3),
     *		  (DTSNMS(K,2,ROW(X)),K=1,3)
            CALL WIMG(5,'Is this correct [Y/N] ')
            CALL YESNO(FLAG)
            IF(FLAG.NE.1) GOTO 305
C
C	    CHECK IF ROW IS MODIFIABLE
C
C            IF(DTSSTA(ROW(X)).NE.GAMINF.AND.DTSSTA(ROW(X)).NE.0) THEN
C		TYPE*,  'Row ',ROW,' is not open, so you do not want ',
C     *           'to modify it. '
C		GOTO 305
C	    ENDIF
C
C	    Get mark for this row
C
310	    CONTINUE
	    WRITE(5,902)
	    CALL INPNUM('Enter mark, X=draw, 1=home, 2=away, E=Exit',SUP,
     *			 1,2,EXT)
	    IF(EXT.EQ.-1) THEN              ! Exit
                EFLG=0
                GOTO 50
	    ENDIF
 	    IF(EXT.EQ.-6) THEN
               EXT=4
               SUP=EXT
            ENDIF
            IF(EXT.LT.0) GOTO 50
	    TAB_1X2(X)=SUP
	END DO
	DO I=1,NUM_ROWS-1
	 DO J=I+1,NUM_ROWS
	    IF(ROW(I).GT.ROW(J)) THEN
		TMP    = ROW(I)
		ROW(I) = ROW(J)
		ROW(J) = TMP
		TMP    = TAB_1X2(I)
		TAB_1X2(I) = TAB_1X2(J)
		TAB_1X2(J) = TMP
	    ENDIF
	  ENDDO
	ENDDO		
	CALL ODDSOFF(ROW,TAB_1X2,NUM_ROWS,OFFSET)
        IF(LIVE_GAM) THEN
           CALL FASTSET(0,CBUF,CDLEN)
           CBUF(1)=8
           CBUF(3)=TCTSL
           CBUF(6)='SYS '
           CBUF(8)=GIND
           CBUF(9)=OFFSET
           CBUF(10)=NUM_ROWS
311        CONTINUE
           CALL QUECMD(CBUF,ST)
           IF(ST.NE.0) THEN
              TYPE*,IAM(),' Queue command error > ',ST,' continue to retry'
              CALL GPAUSE
              GOTO 311
           ENDIF
           IF(NUM_ROWS.LE.3) THEN
 	      IF(HASH_DIR_SUP(1,OFFSET).EQ.HASH_CLOSE) THEN
     		 WRITE(5,906)  BETLIN(OFFSET,NUM_ROWS)
                 WRITE(5,902)
	      ELSE
     		 WRITE(5,905)  BETLIN(OFFSET,NUM_ROWS)
                 WRITE(5,902)
              ENDIF
	   ELSE
              CALL HASHGET(OFFSET,NUM_ROWS,X,NEXT_REC,NEXT_BLOCK,PSTAT)
              IF(PSTAT.EQ.HASH_RETURN_OK) THEN
                 IF(HASH_TAB_SUP(1,NEXT_REC,NEXT_BLOCK).EQ.HASH_CLOSE)THEN
                    WRITE(5,906)  BETLIN(OFFSET,NUM_ROWS)
                    WRITE(5,902)
                 ELSE
                    WRITE(5,905)  BETLIN(OFFSET,NUM_ROWS)
                    WRITE(5,902)
                 ENDIF
              ELSE
                 WRITE(5,908)  BETLIN(OFFSET,NUM_ROWS)
                 WRITE(5,902)
              ENDIF
           ENDIF
        ELSE
	   IF(NUM_ROWS.LE.3) THEN
	      IF(HASH_DIR_SUP(1,OFFSET).EQ.HASH_CLOSE) THEN
		 HASH_DIR_SUP(1,OFFSET)=HASH_OK
     		 WRITE(5,906)  BETLIN(OFFSET,NUM_ROWS)
                 WRITE(5,902)
	      ELSE
		 HASH_DIR_SUP(1,OFFSET)=HASH_CLOSE
     		 WRITE(5,905)  BETLIN(OFFSET,NUM_ROWS)
                 WRITE(5,902)
	      ENDIF
	   ELSE
	      CALL HASHGET(OFFSET,NUM_ROWS,X,NEXT_REC,NEXT_BLOCK,PSTAT)
              IF(PSTAT.EQ.HASH_RETURN_OK) THEN
                 IF(HASH_TAB_SUP(1,NEXT_REC,NEXT_BLOCK).EQ.HASH_CLOSE)THEN
                    HASH_TAB_SUP(1,NEXT_REC,NEXT_BLOCK)=HASH_OK
		    WRITE(5,906)  BETLIN(OFFSET,NUM_ROWS)
		    WRITE(5,902)
	         ELSE
                    HASH_TAB_SUP(1,NEXT_REC,NEXT_BLOCK)=HASH_CLOSE
     		    WRITE(5,905)  BETLIN(OFFSET,NUM_ROWS)
		    WRITE(5,902)
	         ENDIF
              ELSE
     		 WRITE(5,908)  BETLIN(OFFSET,NUM_ROWS)
                 WRITE(5,902)
	      ENDIF
	   ENDIF
        ENDIF
        WRITE(5,902)

	GOTO 300
C
400	CONTINUE
	LUN=5
	WRITE(5,902) 
	CALL WIMG(5,'Print Report [Y/N] ')
        CALL YESNO(REPLY)
 	IF(REPLY.EQ.1) THEN		! PRINT
            LUN=6
	    CALL INPNUM('Enter number of report copies: ',COPY,0,20,EXT)
	    CALL ROPEN('TSLSUP.REP',6,ST)
        ENDIF
        IF(EXT.LT.0) GOTO 50
        DO RW=1,40
C	    IF(DTSDAT(RW).LT.DAYCDC) GOTO 40
C	    IF(DTSSTA(RW).NE.GAMBFD) GOTO 40
            EFLG=1
            WRITE(LUN,901)  RW,(DTSNMS(K,1,RW),K=1,3),(DTSNMS(K,2,RW),K=1,3)
            LINCNT=LINCNT+1
	    IF(LINCNT.GT.MAX_LINES.AND.LUN.EQ.6) THEN
		CALL TITLE('Pitka Restriction Listing','TSLSUP  ',
     *			1,6,PAGE,DAYCDC)
		WRITE(6,902)
		LINCNT=4
	    ENDIF

           DO X=1,3		! Check if any marK is closed
	        IF(HASH_ROW_SUP(RW,X).NE.HASH_OK) THEN
			WRITE(LUN,903) TXT(X),RW
			LINCNT=LINCNT+1
			IF(LINCNT.GT.MAX_LINES.AND.LUN.EQ.6) THEN
			    CALL TITLE('Pitka Restriction Listing','TSLSUP  ',
     *				1,6,PAGE,DAYCDC)
			    WRITE(6,902)
			    LINCNT=4
			ENDIF
	        ENDIF
           END DO

           DO X=1,40		! Check if any pair of rows are closed
		IF(HASH_COMB_SUP(RW,X).NE.HASH_OK) THEN
		    WRITE(LUN,904) RW,X
	            LINCNT=LINCNT+1
	            IF(LINCNT.GT.MAX_LINES.AND.LUN.EQ.6) THEN
		        CALL TITLE('Pitka Restriction Listing','TSLSUP  ',
     *			    1,6,PAGE,DAYCDC)
	                 WRITE(6,902)
		        LINCNT=4
	            ENDIF
	        ENDIF
           END DO
        END DO
C
C	Check if any 3 event combination is closed
C
        DO X=1,HASH_NUM_COMB3
	    IF(HASH_DIR_SUP(1,X).EQ.HASH_CLOSE) THEN
	        WRITE(LUN,905) BETLIN(X,3)
                LINCNT=LINCNT+1
	        IF(LINCNT.GT.MAX_LINES.AND.LUN.EQ.6) THEN
	    	    CALL TITLE('Pitka Restriction Listing','TSLSUP  ',
     *			1,6,PAGE,DAYCDC)
	    	    WRITE(6,902)
		    LINCNT=4
	        ENDIF
	    ENDIF
	END DO
C
C	Check if any 4,5,6 event combination is closed
C
        DO X=1,HASH_RECORDS
	    DO Y=1,HASH_NUM_BLOCKS
	       IF(HASH_TAB_SUP(1,X,Y).EQ.HASH_CLOSE) THEN
		   WRITE(LUN,905) BETLIN(HASH_TAB(1,X,Y),
     *                                       IAND(HASH_TAB(2,X,Y),15))
	           LINCNT=LINCNT+1
	           IF(LINCNT.GT.MAX_LINES.AND.LUN.EQ.6) THEN
		       CALL TITLE('Pitka Restriction Listing','TSLSUP  ',
     *				1,6,PAGE,DAYCDC)
		       WRITE(6,902)
		       LINCNT=4
	           ENDIF
	        ENDIF
            END DO
	END DO
	IF(LUN.EQ.6) THEN
	    WRITE(6,952)
	    CALL USRCLOS1(     6)
	    CALL SPOOL('TSLSUP.REP',COPY,ST)
c	    CALL XWAIT(2,2,ST)
	    LUN=5
	ENDIF
	GOTO 50
C
C
5000    CONTINUE
        CALL USRCLOS1(1)
        CALL CLRSCR(5)
        CALL GSTOP(GEXIT_SUCCESS)
C
C
900	FORMAT(25X,' Pitka Restriction Maintenance ',//,
     *	       '  ',/,
     *	       15X,' 1 >> Open / Close a mark on a row ',/,
     *	       15X,' 2 >> Open / Close a row in combination with another row',/,
     *	       15X,' 3 >> Open / Close a combination ',/,
     *	       15X,' 4 >> List restrictions',/,
     *	       15X,' E >> Program exit ',//)

901     FORMAT(1X,'Row ',I4,1X,3A4,' vs ',3A4)
902	FORMAT(1X,A)
903	FORMAT(1X,A35,I4)
904	FORMAT(1X,'Row ',I4,' with Row ',I4,' is closed')
905	FORMAT(1X,'Combination ',A29,' is closed')
906     FORMAT(1X,'Combination ',A29,' is open')
907	FORMAT(1X,'Row ',I4,' with Row ',I4,' is open')
908     FORMAT(1X,'Combination ',A29,' has not been played YET.')
952     FORMAT('------END OF REPORT------')
3010    FORMAT(A8,I1,' game not active')
3020    FORMAT(1X,5A4,' open error ',I4)
3030    FORMAT(1X,5A4,' read error ',I4,' record > ',I4)
3040    FORMAT(1X,A8,1X,I1,' game not initialized event > ',I4)
	END
