C SUBROUTINE PPLSNP
C
C  V18 31-MAY-2000 PXO Subroutine name PARSNP -> PPLSNP
C  V17 23-SEP-1999 UXN Format statement changed.
C  V16 02-JUN-1999 UXN Take data from memory only if DAYSTS is DSOPEN
C  V15 25-MAY-1999 UXN DCPWIN changed.
C  V14 04-FEB-1999 UXN Fix for big odds.
C  V13 23-APR-1996 RXK RFSS 275.Row and total pool amounts fixed in the case 
C                      when some of rows is cancelled 
C  V12 04-MAR-1996 RXK Format for odds increased 
C  V11 13-FEB-1996 RXK Rfss 247, change of the value of row number
C  V10 07-FEB-1996 RXK Rfss 248. Cancelled rows.
C  V09 29-JAN-1996 HXK Fix for game status set to refund
C  V08 28-JAN-1996 HXK Fix for FORMAT statements
C  V07 26-JAN-1996 HXK Show row numbers
C  V06 25-JAN-1996 HXK Various fixes 
C  V05 23-JAN-1996 HXK Clear screen when draw has no data
C  V04 12-JAN-1996 HXK Fix for neagtives
C  V03 05-JAN-1996 PXB Fixed bugs in screen when games are closed
C  V02 19-DEC-1995 HXK Fix for amount display
C  V01 12-DEC-1995 PXB Initial revision.
C  
C VIS_PPLSNP.FOR
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

	SUBROUTINE PPLSNP(NUM,GIND,PAGE,CLINE)

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

	INTEGER*4 NUM		    !draw/event number
	INTEGER*4 GIND		    !game index
	INTEGER*4 CLINE(20)	    !command line

	INTEGER*2 DBUF(LDATE_LEN)   !data buffer
	INTEGER*2 DBUF2(LDATE_LEN)  !data buffer
	INTEGER*2 BEGSAL(LDATE_LEN) !beginning sales date
	INTEGER*2 ENDSAL(LDATE_LEN) !ending sales date
	INTEGER*4 AMOUNT(MAXCPLRW)    !amount bet on row
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
        INTEGER*4 TOTPOL            !tot pool, includes roll over
	INTEGER*4 RNDPOL	    !rounded pool
	INTEGER*4 NETSAL	    !net sales
	INTEGER*4 WINSHR	    !winning share
	INTEGER*4 I		    !misc counter
	INTEGER*4 J		    !,,      ,,
	INTEGER*4 K		    !,,      ,,
	INTEGER*4 R		    !,,      ,,
	INTEGER*4 ST		    !state indicator (after call)
	INTEGER*4 LIN		    !lin counter
        INTEGER*4 PRG_AMT           !
	INTEGER*4 GET_E2
	INTEGER*4 NUMWIN_E1	    !# winners in E1
	INTEGER*4 NUMWIN_E2	    !# winners in E2
	INTEGER*4 INDEX             !
	REAL*8	  CMDCHG(3)	    !command

	CHARACTER*17 POLSTS(11)	    !pool status
	CHARACTER*3 RWSTS(11)	    !row status
	CHARACTER*10 A10_SPACE
	CHARACTER*10 A10_CANC

        INTEGER*4 A_CAN(18),B_CAN(18),LA,LB

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
	INTEGER*4   WINNERS(MAXCPLRW,2)
C----------------------------- Start of Code -----------------------------

	TAX = 0
	DRAW = NUM
	GET_E2 = MAXCPLRW / 2
	NUMWIN_E1 = 0
	NUMWIN_E2 = 0
	A10_SPACE = ' '
	A10_CANC = 'Cancelled '

	IF(GIND.LE.0) GIND = 1
	IF(PAGE.LE.0) PAGE = 1

	IF (GIND .LT. 1 .OR. GIND .GT. MAXIND) THEN
	  WRITE(CLIN23,3000) GTNAMES(TCPL)
	  RETURN
	END IF

	GNUM = GTNTAB(TCPL,GIND)

	IF (GNUM .LT. 1) THEN
	  WRITE(CLIN23,3010) GTNAMES(TCPL),GIND
	  RETURN
	END IF

	IF (DRAW .LT. 1) DRAW = DAYDRW(GNUM)
	IF (DRAW .EQ. 0) DRAW = DAYHDR(GNUM)

	IF(DRAW.LE.0) RETURN

C---- Check for command input.

	POS = 1

	CALL KEY(CLINE,CMDCHG,3,POS,KEYNUM)

	IF (POS .GT. 40) GOTO 100                    !-- NO INPUT

	IF (KEYNUM .EQ. 0) GOTO 70                   !-- INPUT ERROR

	CALL NUMB(CLINE,POS,VALUE)                   !-- GET VALUE

C***	IF (VALUE .LT. 1 .OR. VALUE .GT. MAXCPLRW) GOTO 80
        IF (VALUE .GE. 101 .AND. VALUE .LE.118) THEN
           VALUE=VALUE-100
        ELSEIF (VALUE .GE. 201 .AND. VALUE .LE.218) THEN
           VALUE=VALUE-200+18
        ELSE 
           GOTO 80
        ENDIF

	CALL FASTSET(0,BUF,CDLEN)

	GOTO(10,20,30) KEYNUM

	GOTO 70

C---- Open row command

10	CONTINUE
	IF (CPLSTA(VALUE,GIND) .EQ. GAMNUL) GOTO 90
	IF (CPLSTA(VALUE,GIND) .EQ. GAMOPN) GOTO 80
	BUF(1)=2
	BUF(2)=GAMOPN
	BUF(3)=TCCPL
	BUF(6)=IDNUM
	BUF(8)=GIND
	BUF(9)=VALUE
	GOTO 60

C---- Close row command

20	CONTINUE
	IF (CPLSTA(VALUE,GIND) .EQ. GAMNUL) GOTO 90
	IF (CPLSTA(VALUE,GIND) .EQ. GAMBFD) GOTO 80
	BUF(1)=2
	BUF(2)=GAMBFD
	BUF(3)=TCCPL
	BUF(6)=IDNUM
	BUF(8)=GIND
	BUF(9)=VALUE
	GOTO 60

C---- Cancell row command

30	CONTINUE
	IF (CPLSTA(VALUE,GIND) .EQ. GAMNUL) GOTO 90
	IF (CPLSTA(VALUE,GIND) .EQ. GAMCAN) GOTO 80
	IF (CPLSTA(VALUE,GIND) .EQ. GAMREF) GOTO 80
	BUF(1)=2
	BUF(2)=GAMCAN
	BUF(3)=TCCPL
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

C---- Set up Screen.

100	CONTINUE

C---- Get data from memory.

	DO I = 3,22
	   WRITE(XNEW(I),3050)
        ENDDO

	IF(DRAW.EQ.DAYDRW(GNUM).AND.DAYSTS.EQ.DSOPEN) THEN
	   CALL GAMLOG(TCPL,GIND,DCPREC,CPLSTS)
	   DO I =1, (MAXCPLRW/2)*(MAXCPLRW/2)
              DCPODT(I) = CPPOOL(I,CPLPAMNT,CPLPDYNM,GIND)
	   ENDDO
	   GOTO 300
	ENDIF

C---- Get game data from file.

	SMODE = .TRUE.

	CALL OPENW(1,GFNAMES(1,GNUM),0,0,0,ST)
	CALL IOINIT(FDB,1,DCPSEC*256)

	IF (ST .NE. 0) THEN
	  WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
	  CALL USRCLOS1(1)
	  RETURN
	END IF

	CALL READW(FDB,DRAW,DCPREC,ST)

	IF (ST .NE. 0) THEN
	  WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
	  CALL USRCLOS1(1)
	  RETURN
	END IF

	CALL USRCLOS1(1)

	IF (DCPSTS .EQ. 0) THEN
	    WRITE(CLIN23,3040) GTNAMES(TCPL),GIND,DRAW
	    RETURN
	END IF
C
300	CONTINUE
C
	IF(DCPSTS.NE.GAMREF.AND.DCPSTS.NE.GFINAL) THEN
	   TOTAMT = 0
	   DO I = 1,MAXCPLRW
              AMOUNT(I) = 0
           ENDDO 
           DO I = 1,MAXCPLRW/2
              DO K = 1,MAXCPLRW/2
                 INDEX = (I-1)*MAXCPLRW/2 + K
                 J = MAXCPLRW/2 + K
                 IF(.NOT.(DCPSTA(I).EQ.GAMCAN.OR.DCPSTA(I).EQ.GAMREF) .AND.
     *              .NOT.(DCPSTA(J).EQ.GAMCAN.OR.DCPSTA(J).EQ.GAMREF)) THEN
                    AMOUNT(J) = AMOUNT(J) + DCPODT(INDEX)/2
                    AMOUNT(I) = AMOUNT(I) + DCPODT(INDEX)/2
                    TOTAMT = TOTAMT + DCPODT(INDEX)
                 ENDIF
                 IF(.NOT.(DCPSTA(I).EQ.GAMCAN.OR.DCPSTA(I).EQ.GAMREF) .AND.
     *              (DCPSTA(J).EQ.GAMCAN.OR.DCPSTA(J).EQ.GAMREF)) THEN
                    AMOUNT(J) = AMOUNT(J) + DCPODT(INDEX)/2
                 ENDIF
                 IF((DCPSTA(I).EQ.GAMCAN.OR.DCPSTA(I).EQ.GAMREF) .AND.
     *              .NOT.(DCPSTA(J).EQ.GAMCAN.OR.DCPSTA(J).EQ.GAMREF)) THEN
                    AMOUNT(I) = AMOUNT(I) + DCPODT(INDEX)/2
                 ENDIF
                 IF((DCPSTA(I).EQ.GAMCAN.OR.DCPSTA(I).EQ.GAMREF) .AND.
     *              (DCPSTA(J).EQ.GAMCAN.OR.DCPSTA(J).EQ.GAMREF)) THEN
                    AMOUNT(J) = AMOUNT(J) + DCPODT(INDEX)/2
                    AMOUNT(I) = AMOUNT(I) + DCPODT(INDEX)/2
                 ENDIF
              ENDDO
           ENDDO
	ELSE
	   TOTAMT = 0

	   DO I = 1,MAXCPLRW
	      AMOUNT(I) = DCPSBR(I)
	      IF(DCPSTA(I) .NE. GAMCAN .AND. DCPSTA(I) .NE. GAMREF) THEN
     	         TOTAMT = TOTAMT + DCPSBR(I)
	      ENDIF
	   ENDDO
	ENDIF
C---- Work out number of winners.

	CALL FASTSET(0,WINNERS,2*MAXCPLRW)
  	DO I = 1,MAXCPLTI
	  IF (DCPWIN(1,I) .GT. 0) WINNERS(DCPWIN(1,I),1) = 1 
	  IF (DCPWIN(2,I) .GT. 0) WINNERS(DCPWIN(2,I),2) = 1 
	ENDDO
	DO I=1, MAXCPLRW
	   IF(WINNERS(I,1).GT.0) NUMWIN_E1 = NUMWIN_E1 + 1
	   IF(WINNERS(I,2).GT.0) NUMWIN_E2 = NUMWIN_E2 + 1
	ENDDO

C---- Calculate taxes and net pool.
	TAX = 0 
	RNDPOL = DCPBRK(1)
	NETPOL = (TOTAMT * CALPER(DCPSPR) - TAX) + RNDPOL
	TOTPOL = (TOTAMT * CALPER(DCPSPR) - TAX) + RNDPOL + DCPPOL(1)

C---- Now build screen.

	!---- Header Info first.

	IF (DCPSTS .EQ. GFINAL) THEN
	  DBUF(5) = DAYCDC
	  CALL LCDATE(DBUF)
	  WRITE(CLIN1,900) GTNAMES(TCPL),GIND,(DBUF(I),I=7,13)
	  WRITE(CLIN2,9221) DRAW,POLSTS(DCPSTS+1)
	ELSE IF (DCPSTS .EQ. 0) THEN
	  WRITE(CLIN1,900) GTNAMES(TCPL),GIND,(DBUF(I),I=7,13)
	  WRITE(CLIN2,9221) DRAW,POLSTS(DCPSTS+1)
	  WRITE(CLIN23,3040) GTNAMES(TCPL),GIND,DRAW
	  RETURN
	ELSE
	  DBUF(5) = DAYCDC
	  BEGSAL(5) = DCPBSD
	  ENDSAL(5) = DCPESD
          DBUF2(5)=DCPDAT
	  CALL LCDATE(DBUF)
	  CALL LCDATE(BEGSAL)
	  CALL LCDATE(ENDSAL)
          CALL LCDATE(DBUF2)
	  WRITE(CLIN1,901) GTNAMES(TCPL),GIND,
     *			   (BEGSAL(I),I=7,13),(ENDSAL(I),I=7,13)
	  WRITE(CLIN2,902) DRAW,POLSTS(DCPSTS+1),(DBUF2(I),I=7,13)
	END IF
	IF (DCPSTS .LT. GFINAL) THEN
	  DO I = 3,22
	     WRITE(XNEW(I),3050)
          ENDDO
	  WRITE(CLIN3,9031) (DCPENM(I,1),I=1,3),
     *			    (DCPENM(I,2),I=1,3)
	  LIN = 4
	ELSE
	  DO I = 3,22
	     WRITE(XNEW(I),3050)
          ENDDO
	  IF (DCPEST(1) .NE. GAMCAN) THEN  
	    WRITE(CLIN3,9022) (DCPENM(I,1),I=1,4),NUMWIN_E1,A10_SPACE
	  ELSE
	    WRITE(CLIN3,9022) (DCPENM(I,1),I=1,4),NUMWIN_E1,A10_CANC
	  END IF
	  IF (DCPEST(2) .NE. GAMCAN) THEN
	    WRITE(CLIN4,9222) (DCPENM(I,2),I=1,4),NUMWIN_E2,A10_SPACE
	  ELSE
	    WRITE(CLIN4,9222) (DCPENM(I,2),I=1,4),NUMWIN_E2,A10_CANC
	  END IF
          LA=0
          LB=0 
          DO R=1,18
             IF(DCPSTA(R).EQ.GAMCAN) THEN
                LA=LA+1  
                A_CAN(LA)=R
             ENDIF
             IF(DCPSTA(R+18).EQ.GAMCAN) THEN
                LB=LB+1  
                B_CAN(LB)=R
             ENDIF
          ENDDO
          LIN=5
	  IF(LA.GT.0) THEN
             IF(LA.EQ.1) THEN
                WRITE(CLIN5,90241) A_CAN(1)
             ELSE
                WRITE(CLIN5,9024) (A_CAN(R),R=1,LA)
             ENDIF 
             LIN=LIN+1
          ENDIF
          IF(LB.GT.0) THEN
             IF(LB.EQ.1) THEN
                WRITE(CLIN6,90251) B_CAN(1)
             ELSE
                WRITE(CLIN6,9025) (B_CAN(R),R=1,LB)
             ENDIF 
             LIN=LIN+1
          ENDIF 
	END IF
C
C Game Info.
C
	IF (DCPSTS .LT. GFINAL) THEN
	  DO R = 1,18
	    WRITE (XNEW(LIN),9041) R,
     *		                   (DCPNMS(I,R),I=1,4),
     *	                           CSMONY(AMOUNT(R),10,BETUNIT),
     *			           RWSTS(DCPSTA(R)+1),
     *				   R,
     *			           (DCPNMS(I,R+GET_E2),I=1,4),
     *				   CSMONY(AMOUNT(R+GET_E2),10,BETUNIT),
     *                             RWSTS(DCPSTA(R+GET_E2)+1)
	    LIN=LIN+1
	  END DO
	  WRITE(CLIN22,922) CSMONY(TOTAMT,12,BETUNIT),
     *                      CSMONY(NETPOL,11,BETUNIT),
     *			    CSMONY(DCPPOL(1),10,BETUNIT),
     *	                    CSMONY(TOTPOL,11,BETUNIT)
	  LIN = 22
	ELSE
	  BEGSAL(5) = DCPBSD
	  ENDSAL(5) = DCPESD
          DBUF2(5)=DCPDAT
	  CALL LCDATE(BEGSAL)
	  CALL LCDATE(ENDSAL)
          CALL LCDATE(DBUF2)
	  NETSAL = DCPSAL(DOLAMT) - DCPREF
	  TAX = 0
	  WINSHR = NETSAL * CALPER(DCPSPR)
	  WRITE (CLIN7,3050)
	  WRITE (CLIN8,1003) (BEGSAL(I),I=7,13)
	  WRITE (CLIN9,10031) (ENDSAL(I),I=7,13)
	  LIN = 11
	  WRITE (CLIN11,1441) (DBUF2(I),I=7,13)
	  WRITE (CLIN12,10441) CSMONY(DCPSAL(DOLAMT),12,BETUNIT)
	  WRITE (CLIN13,10045) CSMONY(DCPREF,12,BETUNIT)
	  WRITE (CLIN14,10461) CSMONY(NETSAL,12,BETUNIT)
	  WRITE (CLIN15,10471) CSMONY(WINSHR,12,BETUNIT)
          WRITE (CLIN16,10502) CSMONY(DCPPOL(1),10,BETUNIT)
	  WRITE (CLIN17,10491) CSMONY(DCPBRK(2),12,BETUNIT)
	  WRITE (CLIN18,10481) CSMONY(DCPWON-DCPREF,12,BETUNIT)
          WRITE (CLIN19,10501) CSMONY(DCPPOL(2),10,BETUNIT)
	  WRITE (CLIN20,10051) CSMONY(DCPPAD-DCPPRF,12,VALUNIT)
	  WRITE (CLIN21,10052) CSMONY(DCPPRF,12,BETUNIT)
          IF(DCPPUP.NE.0) THEN
             PRG_AMT = ((DCPWON-DCPREF) - (DCPPAD-DCPPRF) 
     *                   + DCPREF - DCPPRF)
          ELSE
             PRG_AMT = 0
          ENDIF
	  WRITE (CLIN22,10053) CSMONY(PRG_AMT,12,BETUNIT)
C
C Prepare printout for winners
C
          IF((PAGE-1)*4.GE.DCPCMB) THEN
               FRST_WIN = 1
          ELSE
               FRST_WIN = (PAGE-1)*4+1
          ENDIF
          LAST_WIN = MIN(FRST_WIN+3,DCPCMB)
          LIN = 10
          DO I=FRST_WIN,LAST_WIN
             IF(LIN.EQ.10.OR.LIN.EQ.13.OR.LIN.EQ.16.OR.LIN.EQ.19) THEN
                  TEMPLINE = XNEW(LIN)
                  WRITE(TEMPLINE(1:10),3051) I
                  XNEW(LIN) = TEMPLINE
                  LIN = LIN + 1
             ENDIF
             TEMPLINE = XNEW(LIN)
             WRITE(TEMPLINE(1:49),3055) DCPWIN(1,I),
     *               (DCPNMS(K,DCPWIN(1,I)),K=1,4),
     *                CSMONY(DCPWBT(DOLAMT,I),12,BETUNIT),
     *                DCPODS(I)/100,MOD(DCPODS(I),100)
             XNEW(LIN)= TEMPLINE
             LIN = LIN + 1
             TEMPLINE = XNEW(LIN)
             WRITE(TEMPLINE(1:49),3056)  DCPWIN(2,I)-MAXCPLRW/2,
     *               (DCPNMS(K,DCPWIN(2,I)),K=1,4),
     *                DCPWBT(TRACNT,I)
             XNEW(LIN)= TEMPLINE
             LIN = LIN + 1
          ENDDO
C

	ENDIF
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

902	FORMAT (' ','Event code -',
     *	        I4,18(' '),'* ',A17,' *',2X,'Draw ',7A2)

9221	FORMAT (' ','Event code -',
     *	        I4,20(' '),'* ',A17,' *')

9021	FORMAT (1X,'A. ',4A4,20X,'B. ',4A4)

9022	FORMAT (1X,'A. ',4A4,10X,I4,' winner(s)',' ',A10)
9222    FORMAT (1X,'B. ',4A4,10X,I4,' winner(s)',' ',A10)

9024    FORMAT (1X,'A. Cancelled: ',I2.2,<LA-1>(',',I2.2))
90241   FORMAT (1X,'A. Cancelled: ',I2.2)
9025    FORMAT (1X,'B. Cancelled: ',I2.2,<LB-1>(',',I2.2))
90251   FORMAT (1X,'B. Cancelled: ',I2.2)

	!---- Game open statements.

9031	FORMAT ('Row',1X,'Name  ',3A4,'    Amount',2X,'Sts',3X,
     *	        'Row',1X,'Name  ',3A4,'    Amount',2X,'Sts')

9041	FORMAT (I2.2,2X,4A4,2X,A10,2X,A3,3X,
     *          I2.2,2X,4A4,2X,A10,2X,A3)

922     FORMAT ('Sales ',A12,1(' '),'Net pool ',A11,1(' '),
     *          'Extra ',A10,1(' '),'Tot pool ',A11)

	!---- Game results in statements.

1000	FORMAT (80(' '))
1003    FORMAT (3X,'Row Winner',12X,'Amount bet',7X,'Odds',T50,
     *          'Beginning sales ',7A2)

10031	FORMAT (38X,'#winners',5X,T50,'Ending    sales ',7A2)


1441    FORMAT (49X,'Draw date       ',7A2)


10441	FORMAT (49X,'Total Sales       ',A12)
10045	FORMAT (49X,'Total Refunds     ',A12)
10461	FORMAT (49X,'Net Sales         ',A12)
10471	FORMAT (49X,'Winners Share     ',A12)
10502   FORMAT (49X,'Extra Amount      ',2X,A10)
10491	FORMAT (49X,'Rounding Pot      ',A12)
10481	FORMAT (49X,'Winning Amount    ',A12)
10501	FORMAT (49X,'Roll Pot          ',2X,A10)

10051	FORMAT (49X,'Winners Paid      ',A12)

10052	FORMAT (49X,'Refunds Paid      ',A12)

10053	FORMAT (49X,'Amount Purged     ',A12)

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
3055    FORMAT(1X,'A. ',I2.2,1X,4A4,A12,I8,'.',I2.2)
3056    FORMAT(1X,'B. ',I2.2,1X,4A4,12X,I11)

	END
