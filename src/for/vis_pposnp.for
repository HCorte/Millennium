C SUBROUTINE PPOSNP
C
C V15 31-MAY-2000 PXO Subroutine name PAROSNP -> PPOSNP
C V14 23-SEP-1999 UXN Format statement changed.
C V13 18-MAY-1999 UXN DCPWIN CHANGED.
C V12 27-AUG-1996 RXK Bug fixed (cases when results are not in) 
C V11 19-MAY-1996 HXK Rita's fix for PAROSNP
C V10 23-APR-1996 RXK RFSS 275. Odds display changed for cancelled rows
C V09 21-FEB-1996 RXK Rfss 252.Round number and description lines added
C V08 25-JAN-1996 HXK Various fixes 
C V07 24-JAN-1996 HXK Allow all combinations to be viewd
C V06 24-JAN-1996 HXK Chnages for odds sorting
C V05 12-JAN-1996 PXB Bug fix in calculation of row for first event
C V04 07-JAN-1996 HXK Event B participants now numbered 1 to 18 instead 
C                     of 19 to 36
C V03 05-JAN-1996 PXB Bug fixed in rank selection
C V02 03-JAN-1996 PXB Now displays odds
C V01 21-DEC-1995 PXB Initial revision.
C  
C VIS_PPOSNP.FOR
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

	SUBROUTINE PPOSNP(DRAW,ROWNUM,GIND,CLINE)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'   
	INCLUDE 'INCLIB:CPLCOM.DEF'
	INCLUDE 'INCLIB:DCPREC.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

C---- Local Variables used.


	INTEGER*2 DBUF(LDATE_LEN)	    !data buffer
	INTEGER*2 DBUF2(LDATE_LEN)	    !data buffer
	INTEGER*2 BEGSAL(LDATE_LEN)	    !beginning sales date
	INTEGER*2 ENDSAL(LDATE_LEN)	    !ending sales date

	INTEGER*4 ROWNUM	    !
	INTEGER*4 ROW		    !
	INTEGER*4 GIND		    !game index
	INTEGER*4 CLINE(20)	    !command line
	INTEGER*4 BUF(CDLEN)	    !command buffer
	INTEGER*4 FDB(7)	    !file discriptor block
	INTEGER*4 DRAW		    !selected draw/event
	INTEGER*4 GNUM		    !game number
	INTEGER*4 POS		    !
	INTEGER*4 VALUE		    !
	INTEGER*4 TAX		    !not used
	INTEGER*4 I		    !misc counter
	INTEGER*4 K		    !,,      ,,
	INTEGER*4 X		    !,,      ,,
	INTEGER*4 Y		    !,,      ,,
	INTEGER*4 ST		    !state indicator (after call)
	INTEGER*4 LIN		    !lin counter
	INTEGER*4 DISP_AMOUNT
	INTEGER*4 DISP_ODDS
	INTEGER*4 NUMWIN            !number of winners

        CHARACTER DESCRNAM1(CPLDES_LEN)
        CHARACTER DESCRNAM2(CPLDES_LEN)
        EQUIVALENCE (DCPDES(1,1),DESCRNAM1)
        EQUIVALENCE (DCPDES(1,2),DESCRNAM2)

	CHARACTER*17 POLSTS(11)	    !pool status
	INTEGER*4    WEEK,YEAR

C---- Data statements.

	DATA POLSTS/'Not initialized  ','No drawing       ',
     *	            'Info entered     ','Game open        ',
     *	            'End of game      ','Results entered  ',
     *	            'Results verified ','Drawing completed',
     *	            'Results are final','Cancelled/Ref dis',
     *	            'Cancelled/Ref ena'/


        INTEGER*4   FRST_WIN,LAST_WIN,PAGE

C----------------------------- Start of Code -----------------------------

	TAX = 0
        NUMWIN = 0

	IF (GIND .LT. 1 .OR. GIND .GT. MAXIND) THEN
	  WRITE(CLIN23,3000) GTNAMES(TCPL)
	  RETURN
	END IF

C---- Check command line for info.

	CALL FASTSET(0,BUF,CDLEN)

        POS = 1
        CALL NUMB(CLINE,POS,VALUE)
        IF (VALUE .GE. 1) ROWNUM = VALUE

C---- Check input parameters for boundry errors

        IF (ROWNUM .LT. 1) ROWNUM = 1
        IF (ROWNUM .GT. ((MAXCPLRW/2)*(MAXCPLRW/2))-15) THEN
            ROWNUM = ((MAXCPLRW/2)*(MAXCPLRW/2))-15
	ENDIF
        POS=1

	GNUM = GTNTAB(TCPL,GIND)

	IF (GNUM .LT. 1) THEN
	  WRITE(CLIN23,3010) GTNAMES(TCPL),GIND
	  RETURN
	END IF

	IF (DRAW .LT. 1) DRAW = DAYDRW(GNUM)
	IF (DRAW .EQ. 0) DRAW = DAYHDR(GNUM)
C
	IF (DRAW .LT. 1) THEN
          DO I=1,22
             WRITE(XNEW(I),3050)
          ENDDO
	  WRITE(CLIN23,3010) GTNAMES(TCPL),GIND
	  RETURN
	END IF

C---- Set up screen

100	CONTINUE

C---- Get data from memory.

	IF (DRAW .EQ. DAYDRW(GNUM)) THEN
	  CALL GAMLOG(TCPL,GIND,DCPREC,CPLSTS)
          GOTO 120
	END IF
	
C---- Get game data from file.

	SMODE = .TRUE.
	CALL OPENW(1,
     *	           GFNAMES(1,GNUM),
     *		   0,
     *             0,
     *             0,
     *             ST)

	CALL IOINIT(FDB,
     *              1,
     *              DCPSEC*256)

	IF (ST .NE. 0) THEN
	  WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
	  CALL USRCLOS1(1)
	  RETURN
	END IF

	CALL READW(FDB,
     *             DRAW,
     *             DCPREC,
     *             ST)

	IF (ST .NE. 0) THEN
	  WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
	  CALL USRCLOS1(1)
	  RETURN
	END IF

	CALL USRCLOS1(1)

120     CONTINUE

	IF (DCPSTS .EQ. 0) THEN
	  DO I = 3,22
             WRITE(XNEW,3050)
          ENDDO
	  WRITE(CLIN23,3040) GTNAMES(TCPL),GIND,DRAW
	  RETURN
	END IF

C---- Now build screen.

	!---- Header Info first.

	DBUF(5) = DAYCDC
	BEGSAL(5) = DCPBSD
	ENDSAL(5) = DCPESD
	DBUF2(5) = DCPDAT
	CALL LCDATE(DBUF)
	CALL LCDATE(BEGSAL)
	CALL LCDATE(ENDSAL)
	CALL LCDATE(DBUF2)
	WRITE(CLIN1,901) GTNAMES(TCPL),GIND,
     *			 (BEGSAL(I),I=7,13),(ENDSAL(I),I=7,13)
	WRITE(CLIN2,902) DRAW,POLSTS(DCPSTS+1),(DBUF2(I),I=7,13)
	CALL FIGWEK(DCPESD - WEEK_OFFSET, WEEK, YEAR)
	WRITE(CLIN3,9021) WEEK,YEAR,
     *           (DESCRNAM1(I),I=1,CPLDES_LEN/4),
     *           (DESCRNAM2(I),I=1,CPLDES_LEN/4)
        WRITE(CLIN4,9022)
     *           (DESCRNAM1(I),I=CPLDES_LEN/4+1,2*CPLDES_LEN/4),
     *         	 (DESCRNAM2(I),I=CPLDES_LEN/4+1,2*CPLDES_LEN/4)
        WRITE(CLIN5,9022)
     *           (DESCRNAM1(I),I=2*CPLDES_LEN/4+1,3*CPLDES_LEN/4),
     *           (DESCRNAM2(I),I=2*CPLDES_LEN/4+1,3*CPLDES_LEN/4)

	!---- If draw is open display odds

        IF (DRAW .EQ. DAYDRW(GNUM)) THEN
150         CONTINUE

  	    LIN = 6

	    WRITE(XNEW(  LIN),903) (DCPENM(I,1),I=1,4),
     *	  		           (DCPENM(I,2),I=1,4)

	    LIN = LIN + 1
            DO I=LIN,22
              WRITE(XNEW(I),3050)
            ENDDO

	    DO ROW = ROWNUM,ROWNUM+15
	    IF (CPPSORT(2,ROW,GIND) .EQ. 0) GOTO 200

	     X = (CPPSORT(2,ROW,GIND)-1)/(MAXCPLRW/2) + 1
	     Y = MOD(CPPSORT(2,ROW,GIND)-1,(MAXCPLRW/2))+(MAXCPLRW/2)+1

	     DISP_ODDS = 
     *            CPPOOL(CPPSORT(2,ROW,GIND),CPLPODDS,CPLPSTAT,GIND)
	     DISP_AMOUNT = 
     *            CPPOOL(CPPSORT(2,ROW,GIND),CPLPAMNT,CPLPSTAT,GIND)

             IF(((DCPSTA(X).EQ.GAMCAN.OR.DCPSTA(X).EQ.GAMREF).OR.
     *           (DCPSTA(Y).EQ.GAMCAN.OR.DCPSTA(Y).EQ.GAMREF)).AND.
     *           DISP_AMOUNT.NE.0) DISP_ODDS = 100

	     WRITE (XNEW(  LIN),904) ROW,
     *                            X,
     *                            (DCPNMS(I,X),I=1,4),
     *	                          Y - MAXCPLRW/2,
     *                            (DCPNMS(I,Y),I=1,4),
     *                            DISP_ODDS/100,
     *                            MOD(DISP_ODDS,100),
     *			          CMONY(DISP_AMOUNT,10,BETUNIT)
 
	     LIN = LIN + 1

200	     CONTINUE

	   END DO
C
        ELSE
           IF (DCPSTS.LE.GAMINF) THEN
              DO LIN=6,22
                 WRITE (XNEW(  LIN),3050)
              ENDDO
              RETURN
           ENDIF
           IF (DCPSTS.LT.GAMDON .OR.
     *         DCPSTS.EQ.GAMCAN .OR. DCPSTS.EQ.GAMREF) THEN
              DO LIN=6,15
                 WRITE (XNEW(  LIN),3050)
              ENDDO
              GOTO 210
           ENDIF
	!---- If results are in display winners and some totals
           LIN=6
           WRITE (XNEW(  LIN),3050)
           LIN=LIN+1
           WRITE (XNEW(  LIN),905)
           LIN=LIN+1
           WRITE (XNEW(  LIN),3050)
           LIN=LIN+1
           WRITE (XNEW(  LIN),906) (DCPENM(I,1),I=1,4),
     *                             (DCPENM(I,2),I=1,4)
           LIN=LIN+1
           WRITE (XNEW(  LIN),3050)
           LIN=LIN+1
C
C Prepare printout for winners
C
	  PAGE = ROWNUM
	  IF(PAGE.LE.0) PAGE = 1
          IF((PAGE-1)*6.GE.DCPCMB) THEN
               FRST_WIN = 1
	       PAGE = 1
          ELSE
               FRST_WIN = (PAGE-1)*6+1
          ENDIF
          LAST_WIN = MIN(FRST_WIN+5,DCPCMB)

	  DO 300 I=1,DCPCMB
              NUMWIN = NUMWIN + DCPWBT(TRACNT,I)
	      IF(I.LT.FRST_WIN.OR.I.GT.LAST_WIN) GOTO 300	  
              WRITE (XNEW(  LIN),907)  DCPWIN(1,I),
     *                                 (DCPNMS(K,DCPWIN(1,I)),K=1,4),
     *                                 DCPWIN(2,I)-MAXCPLRW/2,
     *                                 (DCPNMS(K,DCPWIN(2,I)),K=1,4),
     *                                 DCPWBT(TRACNT,I),
     *                                 CMONY(DCPWBT(DOLAMT,I),12,BETUNIT),
     *                                 DCPODS(I)/100,
     *                                 MOD(DCPODS(I),100)
              LIN = LIN + 1
300        ENDDO

210        CONTINUE

	   LIN = 18
           WRITE (XNEW(  LIN),908) CSMONY(DCPSAL(DOLAMT),12,BETUNIT)
           LIN=LIN+1
           WRITE (XNEW(  LIN),909) CSMONY(DCPPOL(1),10,BETUNIT)
           LIN=LIN+1
           WRITE (XNEW(  LIN),910) CSMONY(DCPWON-DCPREF,12,BETUNIT)
           LIN=LIN+1
           WRITE (XNEW(  LIN),911) NUMWIN
C
C If there are more than 6 winners, then other winners are in next page
C
	   IF(DCPCMB.GT.6) THEN
	      WRITE(XNEW(6),912) PAGE,DCPCMB/7+1
	      WRITE(XNEW(23),913)
	   ELSE
	      WRITE(XNEW(23),914)
	   ENDIF
	    
        ENDIF

	RETURN

C------------------- Formating statements ------------------------------

C---- Error format statements.

800	FORMAT ('Input error')

810	FORMAT ('Value error')

820	FORMAT ('Sorry, that row not activated, no change allowed')

C---- Screen format statements.

	!---- Header statements.

900	FORMAT ('* ',A8,1X,I1,1X,6A2,' *',30(' '))

901	FORMAT (A8,1X,I1,7A2,' -',7A2)

902	FORMAT (2(' '),'Event code -',2X,
     *	        I4,15(' '),'* ',A17,' *',2X,'Draw ',7A2)
9021    FORMAT (2(' '),'Week ',I2.2,'/',I4,
     *                 '(',<CPLDES_LEN/4>A,') (',<CPLDES_LEN/4>A,') ')

9022    FORMAT (14(' '),'(',<CPLDES_LEN/4>A,') (',<CPLDES_LEN/4>A,') ')

903	FORMAT (1X,'Rank',3X,4A4,6X,4A4,3X,'Odds',
     *          4X,'Amount Bet')

904	FORMAT (1X,I3,2X,I2,2X,4A4,2X,I2,2X,4A4,2X,I3,'.',I2.2,2X,A10)

C---- Error format Statements.

3000	FORMAT ('Enter !',A8,' game index ')

3010	FORMAT (A8,1X,I1,' game not active')

3020	FORMAT (5A4,' open error ',I4)

3030	FORMAT (5A4,' read error ',I4,' record > ',I4)

3040	FORMAT (A8,1X,I1,' game not initialized event > ',I4)

3050    FORMAT (80(' '))

905     FORMAT (1(' '),'Winner(s):')
906     FORMAT ('A. ',4A4,T24,'B. ',4A4,T52,'Count',
     *         T62,'Amount',T77,'Odds')
907     FORMAT(I2.2,1X,4A4,T24,I2.2,1X,4A4,T49,
     *         I6,1X,A12,1X,I9,'.',I2.2)
908     FORMAT (10(' '),'Total Sales          ',A12,' mk.')
909     FORMAT (10(' '),'Extra Amount           ',A10,' mk.')
910     FORMAT (10(' '),'Winning Amount       ',A12,' mk.')
911     FORMAT (10(' '),'Number of winners       ',I7)
912     FORMAT(T65,'Page',1X,I3,' of ',I3)
913     FORMAT('Enter !game index or event number or /page number')
914     FORMAT('Enter !game index or event number')
	END
