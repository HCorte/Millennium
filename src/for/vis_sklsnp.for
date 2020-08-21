C SUBROUTINE SKLSNP
C
C V21 31-MAY-2000 PXO Subroutine name SUKSNP -> SKLSNP
C V20 23-SEP-1999 UXN Format statement changed.
C V19 06-JUL-1999 UXN Minimum stake added.
C V18 02-JUN-1999 UXN Take data from memory only if DAYSTS is DSOPEN
C V17 25-MAY-1999 UXN DDBWIN changed.
C V16 04-FEB-1999 UXN Fix for big odds.
C V15 23-APR-1996 RXK RFSS 275.Row and total pool amounts fixed in the case 
C                     when some of rows has been cancelled
C V14 18-MAR-1996 WPW RFSS #265, change for displaying odds from I3 to I5, 
C                     format 1004.
C V13 22-FEB-1996 RXK Layout fixed
C V12 21-FEB-1996 RXK Layout fixed.
C V11 13-FEB-1996 RXK Cleared from old output 
C V10 06-FEB-1996 RXK Rfss 248. Cancelled rows.
C V09 25-JAN-1996 HXK Various fixes 
C V08 25-JAN-1996 HXK Show WBT dolamt
C V07 23-JAN-1996 HXK Clear screen when draw has no data
C V06 12-JAN-1996 HXK Fix for negatives
C V05 05-JAN-1996 PXB Now works when games closed
C V04 22-DEC-1995 HXK Use dynamic part of DBPOOL
C V03 19-DEC-1995 HXK Fix for amount display
C V02 18-DEC-1995 HXK Fix for indexing into DBPOOL
C V01 12-DEC-1995 PXB Initial revision.
C  
C SKLSNP.FOR
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

	SUBROUTINE SKLSNP(NUM,GIND,PAGE,CLINE)

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

	INTEGER*4 INDEX             !index into DBPOOL
	INTEGER*4 NUM		    !draw/event number
	INTEGER*4 GIND		    !game index
	INTEGER*4 CLINE(20)	    !command line

	INTEGER*2 DBUF(LDATE_LEN)   !data buffer
	INTEGER*2 DBUF2(LDATE_LEN)  !data buffer
	INTEGER*2 BEGSAL(LDATE_LEN) !beginning sales date
	INTEGER*2 ENDSAL(LDATE_LEN) !ending sales date
	INTEGER*4 AMOUNT(MAXDBLRW)  !amount bet on row
	INTEGER*4 BUF(CDLEN)	    !command buffer
	INTEGER*4 FDB(7)	    !file discriptor block
	INTEGER*4 DRAW		    !selected draw/event
	INTEGER*4 GNUM		    !game number
	INTEGER*4 POS		    !
	INTEGER*4 KEYNUM	    !
	INTEGER*4 VALUE		    !
	INTEGER*4 TOTAMT	    !total amount bet
	INTEGER*4 TAX		    !not used
	INTEGER*4 NETPOL	    !net pool
	INTEGER*4 TOTPOL	    !tot pool, includes roll over
	INTEGER*4 RNDPOL	    !rounded pool
	INTEGER*4 NETSAL	    !net sales
	INTEGER*4 WINSHR	    !winning share
	INTEGER*4 I		    !misc counter
	INTEGER*4 K		    !,,      ,,
	INTEGER*4 R		    !,,      ,,
	INTEGER*4 ST		    !state indicator (after call)
	INTEGER*4 LIN		    !lin counter
        INTEGER*4 PRG_AMT           !
	REAL*8	  CMDCHG(3)	    !command
	CHARACTER*17 POLSTS(11)	    !pool status
	CHARACTER*3 RWSTS(11)	    !row status
        INTEGER*4 CANROWS(18)       !cancelled rows
        INTEGER*4 LCAN              !# of cancelled rows

C---- Data statements.

	DATA POLSTS/'Not initialized  ','No drawing       ',
     *	            'Info entered     ','Game open        ',
     *	            'End of game      ','Results entered  ',
     *	            'Results verified ','Drawing completed',
     *	            'Results are final','Cancelled/Ref dis',
     *	            'Cancelled/Ref ena'/

	DATA RWSTS/'   ','cls','ent','opn','cls','rin','ver',
     *	           'drw','fin','can','ref'/

	DATA CMDCHG/'OPN     ','CLS     ','CAN     '/
        CHARACTER*80 TEMPLINE
        INTEGER*4   FRST_WIN,LAST_WIN,PAGE

C----------------------------- Start of Code -----------------------------

	TAX = 0
	DRAW = NUM

	IF(GIND.LE.0) GIND = 1
	IF(PAGE.LE.0) PAGE = 1

	IF (GIND .LT. 1 .OR. GIND .GT. MAXIND) THEN
	  WRITE(CLIN23,3000) GTNAMES(TDBL)
	  RETURN
	END IF

	GNUM = GTNTAB(TDBL,GIND)

	IF (GNUM .LT. 1) THEN
	  WRITE(CLIN23,3010) GTNAMES(TDBL),GIND
	  RETURN
	END IF

	IF (DRAW .LT. 1) DRAW = DAYDRW(GNUM)
	IF (DRAW .EQ. 0) DRAW = DAYHDR(GNUM)
	IF (DRAW .LE. 0) RETURN

C---- Check for command input.

	POS = 1

	CALL KEY(CLINE,CMDCHG,3,POS,KEYNUM)

	IF (POS .GT. 40) GOTO 100                    !-- NO INPUT

	IF (KEYNUM .EQ. 0) GOTO 70                   !-- INPUT ERROR

	CALL NUMB(CLINE,POS,VALUE)                   !-- GET VALUE

	IF (VALUE .LT. 1 .OR. VALUE .GT. MAXDBLRW) GOTO 80

	CALL FASTSET(0,BUF,CDLEN)

	GOTO(10,20,30) KEYNUM

	GOTO 70

C---- Open row command

10	CONTINUE
	IF (DBLSTA(VALUE,GIND) .EQ. GAMNUL) GOTO 90
	IF (DBLSTA(VALUE,GIND) .EQ. GAMOPN) GOTO 80
	BUF(1)=2
	BUF(2)=GAMOPN
	BUF(3)=TCDBL
	BUF(6)=IDNUM
	BUF(8)=GIND
	BUF(9)=VALUE
	GOTO 60

C---- Close row command

20	CONTINUE
	IF (DBLSTA(VALUE,GIND) .EQ. GAMNUL) GOTO 90
	IF (DBLSTA(VALUE,GIND) .EQ. GAMBFD) GOTO 80
	BUF(1)=2
	BUF(2)=GAMBFD
	BUF(3)=TCDBL
	BUF(6)=IDNUM
	BUF(8)=GIND
	BUF(9)=VALUE
	GOTO 60

C---- Cancell row command

30	CONTINUE
	IF (DBLSTA(VALUE,GIND) .EQ. GAMNUL) GOTO 90
	IF (DBLSTA(VALUE,GIND) .EQ. GAMCAN) GOTO 80
	IF (DBLSTA(VALUE,GIND) .EQ. GAMREF) GOTO 80
	BUF(1)=2
	BUF(2)=GAMCAN
	BUF(3)=TCDBL
	BUF(6)=IDNUM
	BUF(8)=GIND
	BUF(9)=VALUE
	GOTO 60

C---- Queue command to system input queue

60	CONTINUE
	CALL VISCMD(BUF,ST)
	CALL XWAIT(2,2,ST)
	GOTO 100

C---- Input errors

70	CONTINUE
	WRITE(CLIN23,800)
	RETURN

80	CONTINUE
	WRITE(CLIN23,810)
	RETURN

90	CONTINUE
	WRITE(CLIN23,820)
	RETURN

C---- Set up screen

100	CONTINUE

C---- Get data from memory.

	IF (DRAW .EQ. DAYDRW(GNUM).AND.DAYSTS.EQ.DSOPEN) THEN
	   CALL GAMLOG(TDBL,GIND,DDBREC,DBLSTS)
	   DO I=1,MAXDBLRW*MAXDBLRW
                DDBODT(I) = DBPOOL(I,DBLPAMNT,DBLPDYNM,GIND)
           ENDDO
	   GOTO 300
	ENDIF
	
C---- Get game data from file.

	SMODE = .TRUE.

	CALL OPENW(1,GFNAMES(1,GNUM),0,0,0,ST)
	CALL IOINIT(FDB,1,DDBSEC*256)

	IF (ST .NE. 0) THEN
	  WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
	  CALL USRCLOS1(1)
	  RETURN
	END IF

	CALL READW(FDB,DRAW,DDBREC,ST)

	IF (ST .NE. 0) THEN
	  WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
	  CALL USRCLOS1(1)
	  RETURN
	END IF

	CALL USRCLOS1(1)

	IF (DDBSTS .EQ. 0) THEN
            DO I = 2,22
               WRITE (XNEW(  I),3050)
            END DO
	    WRITE(CLIN23,3040) GTNAMES(TDBL),GIND,DRAW
	    RETURN
	END IF
C
300	CONTINUE
C
	IF(DDBSTS.NE.GAMREF.AND.DDBSTS.NE.GFINAL) THEN
	   TOTAMT = 0
           DO I = 1,MAXDBLRW
              AMOUNT(I) = 0
           ENDDO
           DO I = 1,MAXDBLRW
              DO K = 1,MAXDBLRW
                 INDEX = (I-1)*MAXDBLRW + K
                 IF(DDBSTA(I).NE.GAMCAN.AND.
     *              DDBSTA(K).NE.GAMCAN) THEN
                    AMOUNT(K) = AMOUNT(K) + DDBODT(INDEX)/2
                    AMOUNT(I) = AMOUNT(I) + DDBODT(INDEX)/2
                    TOTAMT = TOTAMT + DDBODT(INDEX)
                 ENDIF
                 IF(DDBSTA(I).EQ.GAMCAN) THEN
                    AMOUNT(I) = AMOUNT(I) + DDBODT(INDEX)/2
                 ENDIF
                 IF(DDBSTA(K).EQ.GAMCAN) THEN
                    AMOUNT(K) = AMOUNT(K) + DDBODT(INDEX)/2
                 ENDIF
              ENDDO
           ENDDO
        ELSE

	   TOTAMT = 0

	   DO I = 1,MAXDBLRW
	      AMOUNT(I) = DDBSBR(I)
	      IF(DDBSTA(I) .NE. GAMCAN .AND. DDBSTA(I) .NE. GAMREF) THEN
     	         TOTAMT = TOTAMT + DDBSBR(I)
	      END IF
	   ENDDO

	ENDIF
C
C---- Calculate taxes and net pool.
C
	TAX = 0 
	RNDPOL = DDBBRK(1)
	NETPOL = (TOTAMT * CALPER(DDBSPR) - TAX) + RNDPOL
	TOTPOL = (TOTAMT * CALPER(DDBSPR) - TAX) + RNDPOL + DDBPOL(1)	

C---- Now build screen.

	!---- Header Info first.

	IF (DDBSTS .GE. GFINAL) THEN
	  DBUF(5) = DAYCDC
	  CALL LCDATE(DBUF)
	  WRITE(CLIN1,900) GTNAMES(TDBL),GIND,(DBUF(I),I=7,13)
	  IF (DDBWIN(1,1) .LT. 0) THEN
	    WRITE(CLIN2,9221) (DDBENM(I),I=1,4),DRAW,POLSTS(11)
	  ELSE
	    WRITE(CLIN2,9221) (DDBENM(I),I=1,4),DRAW,POLSTS(DDBSTS+1)
	  END IF
        ELSE IF (DDBSTS .EQ. 0) THEN
          WRITE(CLIN1,900) GTNAMES(TCPL),GIND,(DBUF(I),I=7,13)
	  WRITE(CLIN2,9221) (DDBENM(I),I=1,4),DRAW,POLSTS(DDBSTS+1)
          WRITE(CLIN23,3040) GTNAMES(TDBL),GIND,DRAW
          RETURN
 	ELSE
	  DBUF(5) = DAYCDC
	  BEGSAL(5) = DDBBSD
	  ENDSAL(5) = DDBESD
	  DBUF2(5) = DDBDAT
	  CALL LCDATE(DBUF)
	  CALL LCDATE(BEGSAL)
	  CALL LCDATE(ENDSAL)
          CALL LCDATE(DBUF2)
	  WRITE(CLIN1,901) GTNAMES(TDBL),GIND,
     *			   (BEGSAL(I),I=7,13),(ENDSAL(I),I=7,13)
	  WRITE(CLIN2,902) (DDBENM(I),I=1,4),DRAW,POLSTS(DDBSTS+1),
     *		           (DBUF2(I),I=7,13)
	END IF
        LCAN=0
        DO R=1,18
           IF(DDBSTA(R).EQ.GAMCAN) THEN
              LCAN=LCAN+1
              CANROWS(LCAN)=R
           ENDIF
        ENDDO
        IF(LCAN.GT.0) THEN
           IF(LCAN.EQ.1) THEN
              WRITE(CLIN3,92221) CANROWS(1)
           ELSE
              WRITE(CLIN3,9222) (CANROWS(R),R=1,LCAN)
           ENDIF
        ELSE
       	   WRITE(CLIN3,3050)
        ENDIF
       
	WRITE(CLIN4,3050)
	WRITE(CLIN5,3050)

	LIN = 4

C---- Game Info.

	IF (DDBSTS .LT. GFINAL) THEN
	  WRITE(CLIN3,9031) CMONY(DDBPRC,5,BETUNIT)
	  DO R = 1,18
	    WRITE (XNEW(LIN),9041) R,
     *		                   (DDBNMS(I,R),I=1,4),
     *	                           CSMONY(AMOUNT(R),10,BETUNIT),
     *	                           RWSTS(DDBSTA(R)+1)
	    LIN = LIN + 1
	  END DO
	  WRITE(CLIN22,922) CSMONY(TOTAMT,12,BETUNIT),
     *	                    CSMONY(NETPOL,11,BETUNIT),
     *			    CSMONY(DDBPOL(1),10,BETUNIT),
     *			    CSMONY(TOTPOL,11,BETUNIT)
	  LIN = 22
	ELSE
	  BEGSAL(5) = DDBBSD
	  ENDSAL(5) = DDBESD
          DBUF2(5)=DDBDAT
	  CALL LCDATE(BEGSAL)
	  CALL LCDATE(ENDSAL)
          CALL LCDATE(DBUF2)
	  NETSAL = DDBSAL(DOLAMT) - DDBREF
	  TAX = 0
	  WINSHR = NETSAL * CALPER(DDBSPR)
	  WRITE (CLIN5,1003)
	  WRITE (CLIN6,10031)
	  LIN = 7
	  WRITE (CLIN7,1441)  (BEGSAL(I),I=7,13)
	  WRITE (CLIN8,1442)  (ENDSAL(I),I=7,13)
	  WRITE (CLIN9,10042) (DBUF2(I),I=7,13)
	  WRITE (CLIN10,1000)
	  WRITE (CLIN11,10441) CSMONY(DDBSAL(DOLAMT),12,BETUNIT)
	  WRITE (CLIN12,10045) CSMONY(DDBREF,12,BETUNIT)
	  WRITE (CLIN13,10461) CSMONY(NETSAL,12,BETUNIT)
	  WRITE (CLIN14,10471) CSMONY(WINSHR,12,BETUNIT)
	  WRITE (CLIN15,10481) CSMONY(DDBPOL(1),10,BETUNIT)
	  WRITE (CLIN16,10491) CSMONY(DDBBRK(2),12,BETUNIT)
	  WRITE (CLIN17,10048) CSMONY(DDBWON-DDBREF,12,BETUNIT)
	  WRITE (CLIN18,10501) CSMONY(DDBPOL(2),10,BETUNIT)
	  WRITE (CLIN19,10051) CSMONY(DDBPAD-DDBPRF,12,VALUNIT)
	  WRITE (CLIN20,10052) CSMONY(DDBPRF,12,BETUNIT)
          IF(DDBPUP.NE.0) THEN
             PRG_AMT = ((DDBWON-DDBREF) - (DDBPAD-DDBPRF) 
     *                   + DDBREF - DDBPRF)
          ELSE
             PRG_AMT = 0
          ENDIF
	  WRITE (CLIN21,10053) CSMONY(PRG_AMT,12,BETUNIT)
C
	  WRITE (CLIN22,3050)
C
C Prepare printout for winners
C
          IF((PAGE-1)*4.GE.DDBCMB) THEN
               FRST_WIN = 1
          ELSE
               FRST_WIN = (PAGE-1)*4+1
          ENDIF
          LAST_WIN = MIN(FRST_WIN+3,DDBCMB)
          LIN = 7
          DO I=FRST_WIN,LAST_WIN
             IF(LIN.EQ.7.OR.LIN.EQ.10.OR.LIN.EQ.13.OR.LIN.EQ.16) THEN
                  TEMPLINE = XNEW(LIN)
                  WRITE(TEMPLINE(1:10),3051) I
                  XNEW(LIN) = TEMPLINE
                  LIN = LIN + 1
             ENDIF
             TEMPLINE = XNEW(LIN)
             WRITE(TEMPLINE(1:50),3055) DDBWIN(1,I),
     *               (DDBNMS(K,DDBWIN(1,I)),K=1,4),
     *                CSMONY(DDBWBT(DOLAMT,I),12,BETUNIT),
     *                DDBODS(I)/100,MOD(DDBODS(I),100)
             XNEW(LIN)= TEMPLINE
             LIN = LIN + 1
             TEMPLINE = XNEW(LIN)
             WRITE(TEMPLINE(1:50),3056)  DDBWIN(2,I),
     *               (DDBNMS(K,DDBWIN(2,I)),K=1,4),
     *                DDBWBT(TRACNT,I)
             XNEW(LIN)= TEMPLINE
             LIN = LIN + 1
          ENDDO
C
	ENDIF
C
	RETURN

C------------------- Formating statements ------------------------------

C---- Error format statements.

800	FORMAT ('Input error')

810	FORMAT ('Value error')

820	FORMAT ('Sorry, that row not activated, no change allowed')

C---- Screen format statements.

	!---- Header statements.

900	FORMAT ('* ',A8,1X,I1,1X,7A2,' *',28(' '))

901	FORMAT (A8,1X,I1,7A2,' -',7A2)

902	FORMAT (1(' '),4A4,1(' '),'Event code -',
     *	        I4,1(' '),'* ',A17,' *',4X,'Draw ',7A2)

9221	FORMAT (1(' '),4A4,1(' '),'Event code -',
     *	        I4,1(' '),'* ',A17,'  *')

9222    FORMAT (1X,'Cancelled: ',I2.2,<LCAN-1>(',',I2.2))
92221   FORMAT (1X,'Cancelled: ',I2.2)

	!---- Game open statements.

9031	FORMAT ('Row',1X,'Name',32X,'Amount',2X,'Sts',T61,'Minimum stake ',A5)

9041	FORMAT (I2.2,2X,4A4,16X,A10,2X,A3)

922	FORMAT ('Sales ',A12,1(' '),'Net pool ',A11,1(' '),
     *	        'Extra ',A10,1(' '),'Tot pool ',A11)

	!---- Game results in statements.

1000	FORMAT (80(' '))

1003	FORMAT (4X,'Row Results ',10X,'Amount bet',6X,'Odds')
10031   FORMAT (39X,'#winners')

1004	FORMAT (1X,'1. ',I2.2,2X,4A4,2X,A10,I8,'.',I2.2,
     *          3X,'Beginning sales ',7A2)

1441    FORMAT (50X,'Beginning sales ',7A2)

10041	FORMAT (1X,'2. ',I2.2,2X,4A4,15X,I8,
     *          3X,'Ending    sales ',7A2)

1442    FORMAT (50X,'Ending    sales ',7A2)

10042	FORMAT (50X,'Draw date       ',7A2)

10043	FORMAT (1X,'1. ',I2.2,2X,4A4,2X,A10,I8,'.',I2.2)

10044	FORMAT (1X,'2. ',I2.2,2X,4A4,15X,I8,
     *          3X,'Total Sales     ',A12)

10441	FORMAT (50X,'Total Sales     ',A12)

10045	FORMAT (50X,'Total Refunds   ',A12)

10046	FORMAT (1X,'1. ',I2.2,2X,4A4,2X,A10,I8,'.',I2.2,
     *          3X,'Net Sales       ',A12)

10461	FORMAT (50X,'Net Sales       ',A12)

10047	FORMAT (1X,'2. ',I2.2,2X,4A4,15X,I8,
     *          3X,'Winners Share   ',A12)

10471	FORMAT (50X,'Winners Share   ',A12)

10481	FORMAT (50X,'Extra Amount    ',2X,A10)

10049	FORMAT (1X,'1. ',I2.2,2X,4A4,2X,A10,I8,'.',I2.2,
     *          3X,'Rounding Pot    ',A12)

10491	FORMAT (50X,'Rounding Pot    ',A12)

10050	FORMAT (1X,'2. ',I2.2,2X,4A4,15X,I8,
     *          3X,'Winning Amount  ',A12)

10048	FORMAT (50X,'Winning Amount  ',A12)

10501	FORMAT (50X,'Roll Pot        ',2X,A10) 

10051	FORMAT (50X,'Winners Paid    ',A12)

10052	FORMAT (50X,'Refunds Paid    ',A12)

10053	FORMAT (50X,'Amount Purged   ',A12)

1006    FORMAT ('Cancelled  rows---> ',19(I2,' '))                       

1007    FORMAT (20X,19(I2,' '))

C---- Error format Statements.

3000	FORMAT ('Enter !',A8,' game index ')

3010	FORMAT (A8,1X,I1,' game not active')

3020	FORMAT (5A4,' open error ',I4)

3030	FORMAT (5A4,' read error ',I4,' record > ',I4)

3040	FORMAT (A8,1X,I1,' game not initialized event > ',I4)

3050	FORMAT (80(' '))
3051    FORMAT(1X,'(',I2.2,')')
3055    FORMAT(1X,'1. ',I2.2,1X,4A4,A12,I8,'.',I2.2)
3056    FORMAT(1X,'2. ',I2.2,1X,4A4,12X,I11)

	END
