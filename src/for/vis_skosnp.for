C SUBROUTINE SKOSNP
C
C V15 31-MAY-2000 PXO Subroutine name SUKOSNP -> SKOSNP
C V14 23-SEP-1999 UXN Format statement changed.
C V13 27-MAY-1999 UXN DDBCMB added.
C V12 25-APR-1997 UXN Fix for RFSS #301. DDBBSD replaced with DDBDAT to 
C                     display correct draw date.
C V11 27-AUG-1996 RXK Bug fixed (the case when game status stays less than 
C                     gamdon after end of sales) 
C V10 15-AUG-1996 RXK Fix for cases of cancelled and info entered events 
C V09 19-MAY-1996 HXK Rita's update from Finland
C V08 23-APR-1996 RXK RFSS 275. Odds display changed for cancelled rows
C V07 12-MAR-1996 RXK Fix for the odds displayed
C V06 21-FEB-1996 RXK Rfss 252.Round number and description lines added
C V05 25-JAN-1996 HXK Various fixes 
C V04 24-JAN-1996 HXK Chnages for odds sorting
C V03 05-JAN-1996 PXB Bug fixed in rank selection
C V02 03-JAN-1996 PXB Now displays odds
C V01 21-DEC-1995 PXB Initial revision.
C  
C VIS_SKOSNP.FOR
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

	SUBROUTINE SKOSNP(DRAW,ROWNUM,GIND,CLINE)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'   
	INCLUDE 'INCLIB:DBLCOM.DEF'
	INCLUDE 'INCLIB:DDBREC.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

C---- Local Variables used.


	INTEGER*2 DBUF(LDATE_LEN)   !data buffer
	INTEGER*2 DBUF2(LDATE_LEN)  !data buffer
	INTEGER*2 BEGSAL(LDATE_LEN) !beginning sales date
	INTEGER*2 ENDSAL(LDATE_LEN) !ending sales date

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

        CHARACTER   DESCRNAME(DBLDES_LEN)
        EQUIVALENCE (DDBDES(1),DESCRNAME)

	CHARACTER*17 POLSTS(11)	    !pool status
	INTEGER*4    WEEK,YEAR

C---- Data statements.

	DATA POLSTS/'Not initialized  ','No drawing       ',
     *	            'Info entered     ','Game open        ',
     *	            'End of game      ','Results entered  ',
     *	            'Results verified ','Drawing completed',
     *	            'Results are final','Cancelled/Ref dis',
     *	            'Cancelled/Ref ena'/

C----------------------------- Start of Code -----------------------------

	TAX = 0
        NUMWIN = 0

	IF (GIND .LT. 1 .OR. GIND .GT. MAXIND) THEN
	  WRITE(CLIN23,3000) GTNAMES(TDBL)
	  RETURN
	END IF

C---- Check command line for info.

	CALL FASTSET(0,BUF,CDLEN)

        POS = 1
        CALL NUMB(CLINE,POS,VALUE)
        IF (VALUE .GE. 1) ROWNUM = VALUE

C---- Check input parameters for boundry errors
        IF (ROWNUM .LT. 1 .OR. ROWNUM .GT. (MAXDBLRW*MAXDBLRW))
     *     ROWNUM = 1
        IF (ROWNUM .GT. 306-15) ROWNUM = 306-15
        POS=1


	GNUM = GTNTAB(TDBL,GIND)

	IF (GNUM .LT. 1) THEN
	  WRITE(CLIN23,3010) GTNAMES(TDBL),GIND
	  RETURN
	END IF

	IF (DRAW .LT. 1) DRAW = DAYDRW(GNUM)
	IF (DRAW .EQ. 0) DRAW = DAYHDR(GNUM)

	IF (DRAW .LT. 1) THEN
          DO I=1,22
	     WRITE(XNEW(I),3050)
          ENDDO   
	  WRITE(CLIN23,3010) GTNAMES(TDBL),GIND
	  RETURN
	END IF

C---- Set up screen

100	CONTINUE

C---- Get data from memory.

	IF (DRAW .EQ. DAYDRW(GNUM)) THEN
	  CALL GAMLOG(TDBL,GIND,DDBREC,DBLSTS)
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
     *              DDBSEC*256)

	IF (ST .NE. 0) THEN
	  WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
	  CALL USRCLOS1(1)
	  RETURN
	END IF

	CALL READW(FDB,
     *             DRAW,
     *             DDBREC,
     *             ST)

	IF (ST .NE. 0) THEN
	  WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
	  CALL USRCLOS1(1)
	  RETURN
	END IF

	CALL USRCLOS1(1)

120     CONTINUE

	IF (DDBSTS .EQ. 0) THEN
	  DO I=3,22
	     WRITE(XNEW(I),3050)
          ENDDO
	  WRITE(CLIN23,3040) GTNAMES(TDBL),GIND,DRAW
	  RETURN
	END IF

C---- Now build screen.

	!---- Header Info first.

	DBUF(5) = DDBBSD
	DBUF2(5) = DDBDAT
	BEGSAL(5) = DDBBSD
	ENDSAL(5) = DDBESD
	CALL LCDATE(DBUF)
	CALL LCDATE(DBUF2)
	CALL LCDATE(BEGSAL)
	CALL LCDATE(ENDSAL)
	WRITE(CLIN1,901) GTNAMES(TDBL),GIND,
     *			 (BEGSAL(I),I=7,13),(ENDSAL(I),I=7,13)
	WRITE(CLIN2,902) (DDBENM(I),I=1,4),DRAW,POLSTS(DDBSTS+1),
     *			 (DBUF2(I),I=7,13)
	CALL FIGWEK(DDBESD - WEEK_OFFSET, WEEK, YEAR)
        WRITE(CLIN3,9021) WEEK,YEAR,
     *                    (DESCRNAME(I),I=1,DBLDES_LEN/4)
        WRITE(CLIN4,9022) (DESCRNAME(I),I=DBLDES_LEN/4+1,2*DBLDES_LEN/4)
        WRITE(CLIN5,9022) (DESCRNAME(I),I=2*DBLDES_LEN/4+1,3*DBLDES_LEN/4)

        !---- If draw is open display odds

	IF (DRAW .EQ. DAYDRW(GNUM)) THEN
150        CONTINUE
	   LIN = 6

	   WRITE(XNEW(  LIN),903)

	   LIN = LIN + 1
	   DO I=LIN,22
              WRITE(XNEW(I),3050)
           ENDDO

	   DO ROW = ROWNUM,ROWNUM+15
	      IF (DBPSORT(2,ROW,GIND) .EQ. 0) GOTO 200
	      X = ((DBPSORT(2,ROW,GIND)-1)/MAXDBLRW)+1
	      Y = (MOD(DBPSORT(2,ROW,GIND)-1,MAXDBLRW))+1
	      IF (Y .EQ. 0) Y = MAXDBLRW
	      IF (X .GT. MAXDBLRW) X = MAXDBLRW

	      IF(DDBNMS(1,X).EQ.'    ' .OR. DDBNMS(1,X).EQ.0 .OR.
     *           DDBNMS(1,Y).EQ.'    ' .OR. DDBNMS(1,Y).EQ.0) THEN
                 ROWNUM = ROWNUM - 1
	         GOTO 150
              ENDIF

	      DISP_ODDS =   
     *             DBPOOL(DBPSORT(2,ROW,GIND),DBLPODDS,DBLPSTAT,GIND)
	      DISP_AMOUNT = 
     *             DBPOOL(DBPSORT(2,ROW,GIND),DBLPAMNT,DBLPSTAT,GIND)

              IF(((DDBSTA(X).EQ.GAMCAN.OR.DDBSTA(X).EQ.GAMREF).OR.
     *            (DDBSTA(Y).EQ.GAMCAN.OR.DDBSTA(Y).EQ.GAMREF)).AND.
     *            DISP_AMOUNT.NE.0) DISP_ODDS = 100

	      WRITE (XNEW(  LIN),904) ROW,
     *                            X,
     *                            (DDBNMS(I,X),I=1,4),
     *	                          Y,
     *                            (DDBNMS(I,Y),I=1,4),
     *                            DISP_ODDS/100,
     *                            MOD(DISP_ODDS,100),
     *			          CMONY(DISP_AMOUNT,10,BETUNIT)
 
	      LIN = LIN + 1

200	      CONTINUE

	   END DO

        ELSE
           IF (DDBSTS.LE.GAMINF) THEN
              DO LIN=6,22
                 WRITE (XNEW(  LIN),3050) 
              ENDDO
              RETURN 
           ENDIF 
           IF (DDBSTS.LT.GAMDON .OR.
     *         DDBSTS.EQ.GAMCAN .OR. DDBSTS.EQ.GAMREF) THEN
              DO LIN=6,12
                 WRITE (XNEW(  LIN),3050) 
              ENDDO
        !---- If draw is closed display winners and some totals
           ELSE
              LIN=6
              WRITE (XNEW(  LIN),3050) 
              LIN=LIN+1
              WRITE (XNEW(  LIN),905) 
              LIN=LIN+1
              WRITE (XNEW(  LIN),3050) 
              LIN=LIN+1
              WRITE (XNEW(  LIN),906) 
              LIN=LIN+1
              WRITE (XNEW(  LIN),3050) 
              LIN=LIN+1
	      DO I=1,DDBCMB
                 WRITE (XNEW(  LIN),907) DDBWIN(1,I),
     *                               (DDBNMS(K,DDBWIN(1,I)),K=1,4),
     *                               DDBWIN(2,I),
     *                               (DDBNMS(K,DDBWIN(2,I)),K=1,4),
     *                               DDBWBT(TRACNT,I),
     *                               CMONY(DDBWBT(DOLAMT,I),12,BETUNIT),
     *                               DDBODS(I)/100,
     *                               MOD(DDBODS(I),100)
                 LIN = LIN + 1
                 NUMWIN = NUMWIN + DDBWBT(TRACNT,I)
              ENDDO
              WRITE(XNEW(23),914)
           ENDIF

           LIN=18
           WRITE (XNEW(  LIN),908) CSMONY(DDBSAL(DOLAMT),12,BETUNIT)
           LIN=LIN+1
           WRITE (XNEW(  LIN),909) CSMONY(DDBPOL(1),10,BETUNIT)
           LIN=LIN+1
           WRITE (XNEW(  LIN),910) CSMONY(DDBWON-DDBREF,12,BETUNIT) 
           LIN=LIN+1
           WRITE (XNEW(  LIN),911) NUMWIN
        
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

901	FORMAT (A8,I1,7A2,' -',7A2)

902	FORMAT (1(' '),4A4,1(' '),'Event code -',
     *	        I4,1(' '),'* ',A17,' *',2X,'Draw ',7A2)

9021    FORMAT (1(' '),'Week  ',I2.2,'/',I4.4,9(' '),
     *                  '(',<DBLDES_LEN/4>A,')',24(' '))

9022    FORMAT (23(' '),'(',<DBLDES_LEN/4>A,')',24(' '))

903	FORMAT (1X,'Rank',5X,'First',17X,'Second',13X,'Odds',
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
906     FORMAT (T4,'First',T28,'Second',T52,'Count',
     *         T62,'Amount',T77,'Odds')
907     FORMAT(I2.2,1X,4A4,T24,I2.2,1X,4A4,T49,
     *         I6,1X,A12,1X,I9,'.',I2.2)
908     FORMAT (10(' '),'Total Sales          ',A12,' mk.')
909     FORMAT (10(' '),'Extra Amount           ',A10,' mk.') 
910     FORMAT (10(' '),'Winning Amount       ',A12,' mk.') 
911     FORMAT (10(' '),'Number of winners      ',I7)
914     FORMAT('Enter !game index or event number')     
	END
