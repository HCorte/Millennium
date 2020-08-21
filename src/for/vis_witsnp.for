C SUBROUTINE VOISNP
C
C V22 31-MAY-2000 PXO  Subroutine name from WITSNP -> VOISNP
C V21 23-SEP-1999 UXN  Format statement changed.
C V20 17-JUN-1999 UXN  Header line corrected.
C V19 12-FEB-1999 UXN  Fix for big odds.
C V18 19-MAY-1996 HXK  Wojtek's security stuff added
C V17 23-NOV-1995 HXK  Merge of post 65 stuff; changes for Double/Couple
C V16 30-OCT-1995 RXK  Draw date and purge date displayed now 
C V15 23-MAR-1994 HXK  Fix for Purge amount
C V14 02-SEP-1994 HXK  Merge of May,June RFSS batch 
C V13 06-JUN-1994 HXK  UPDATE FOR FIXES.
C V12 17-MAR-1994 HXK  FURTHER FIX FOR CANCELLED ROWS.
C V11 17-MAR-1994 HXK  FIX FOR MORE THAN 15 CANCELLED ROWS.
C V10 14-FEB-1994 HXK  FIXED OVERFLOW.
C V09 03-FEB-1994 HXK  bug fixes
C V08 02-FEB-1994 HXK  CHANGES FOR FINLAND ODDSET INSTALLATION.
C V07 13-JUN-1993 HXK  added AGTINF.DEF, PRMAGT.DEF
C V06 21-JAN-1993 DAB  Initial Release
C                      Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                      DEC Baseline
C V05 29-JUL-1992 HDB  CORRECTED ODDS CALCULATION
C V04 01-JUL-1992 HDB  CORRECTED UPDATING, STARTED ADDING COMMENTS
C V03 11-MAY-1992 GCAN CHANGED TAX AND POOL ROULES.
C V02 18-OCT-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE WITSNP(NUM,GIND,CLINE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'   
	INCLUDE 'INCLIB:WITCOM.DEF'
	INCLUDE 'INCLIB:DWIREC.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*4 NUM		    !draw/event number
	INTEGER*4 GIND		    !game index
	INTEGER*4 CLINE(20)	    !command line
C
	REAL*8	  RODDS		    !row odds
	INTEGER*2 DBUF(LDATE_LEN)   !data buffer
	INTEGER*2 DBUF2(LDATE_LEN)  !data buffer
	INTEGER*2 BEGSAL(LDATE_LEN) !beginning sales date
	INTEGER*2 ENDSAL(LDATE_LEN) !ending sales date
	INTEGER*4 AMOUNT(MAXWRW)    !amount bet on row
	INTEGER*4 ODDS(MAXWRW)	    !odds for row
	INTEGER*4 CANRWS(MAXWRW)    !is the row canceled ?
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
	INTEGER*4 J		    !,,      ,,
	INTEGER*4 K		    !,,      ,,
	INTEGER*4 R		    !,,      ,,
	INTEGER*4 W		    !,,      ,,
	INTEGER*4 ST		    !state indicator (after call)
	INTEGER*4 LIN		    !lin counter
        INTEGER*4 PRG_AMT           !
	REAL*8	  CMDCHG(3)	    !command
	CHARACTER*17 POLSTS(11)	    !pool status
	CHARACTER*3 RWSTS(11)	    !row status
	DATA POLSTS/'Not initialized  ','No drawing       ',
     *	            'Info entered     ','Game open        ',
     *	            'End of game      ','Results entered  ',
     *	            'Results verified ','Drawing completed',
     *	            'Results are final','Cancelled/Ref dis',
     *	            'Cancelled/Ref ena'/
	DATA RWSTS/'   ','cls','ent','opn','cls','rin','ver',
     *	           'drw','fin','can','ref'/
C
	DATA CMDCHG/'OPN     ','CLS     ','CAN     '/
C
C
	TAX = 0
	DRAW=NUM
	IF(GIND.LT.1.OR.GIND.GT.MAXIND) THEN
	  WRITE(CLIN23,3000) GTNAMES(TWIT)
	  RETURN
	ENDIF
C
C
	GNUM=GTNTAB(TWIT,GIND)
	IF(GNUM.LT.1) THEN
	  WRITE(CLIN23,3010) GTNAMES(TWIT),GIND
	  RETURN
	ENDIF
	IF(DRAW.LT.1) DRAW=DAYDRW(GNUM)
	IF(DRAW.EQ.0) DRAW=DAYHDR(GNUM)
C
C CHECK FOR COMMAND INPUT
C
	POS=1
	CALL KEY(CLINE,CMDCHG,3,POS,KEYNUM)
	IF(POS.GT.40) GOTO 100                    !NO INPUT
	IF(KEYNUM.EQ.0)GOTO 70                    !INPUT ERROR
	CALL NUMB(CLINE,POS,VALUE)                !GET VALUE
	IF(VALUE.LT.1.OR.VALUE.GT.MAXWRW) GOTO 80
	CALL FASTSET(0,BUF,CDLEN)
	GOTO(10,20,30) KEYNUM
	GOTO 70
C
C OPEN ROW COMMAND
C
10	CONTINUE
	IF(WITSTA(VALUE,GIND).EQ.GAMNUL) GOTO 90
	IF(WITSTA(VALUE,GIND).EQ.GAMOPN) GOTO 80
	BUF(1)=2
	BUF(2)=GAMOPN
	BUF(3)=TCWIT
	BUF(6)=IDNUM
	BUF(8)=GIND
	BUF(9)=VALUE
	GOTO 60
C
C CLOSE ROW COMMAND
C
20	CONTINUE
	IF(WITSTA(VALUE,GIND).EQ.GAMNUL)GOTO 90
	IF(WITSTA(VALUE,GIND).EQ.GAMBFD) GOTO 80
	BUF(1)=2
	BUF(2)=GAMBFD
	BUF(3)=TCWIT
	BUF(6)=IDNUM
	BUF(8)=GIND
	BUF(9)=VALUE
	GOTO 60
C
C CANCELL ROW COMMAND
C
30	CONTINUE
	IF(WITSTA(VALUE,GIND).EQ.GAMNUL)GOTO 90
	IF(WITSTA(VALUE,GIND).EQ.GAMCAN) GOTO 80
	IF(WITSTA(VALUE,GIND).EQ.GAMREF) GOTO 80
	BUF(1)=2
	BUF(2)=GAMCAN
	BUF(3)=TCWIT
	BUF(6)=IDNUM
	BUF(8)=GIND
	BUF(9)=VALUE
	GOTO 60
C
C QUEUE COMMAND TO SYSTEM INPUT QUEUE
C
60	CONTINUE
	CALL VISCMD(BUF,ST)
C***	CALL QUECMD(BUF,ST)
	CALL XWAIT(2,2,ST)
	GOTO 100
C
C INPUT ERRORS
C
70	CONTINUE
	WRITE(CLIN23,800)
	RETURN
80	CONTINUE
	WRITE(CLIN23,810)
	RETURN
90	CONTINUE
	WRITE(CLIN23,820)
	RETURN
C
C
100	CONTINUE
	IF(DRAW.EQ.DAYDRW(GNUM)) THEN
	  CALL GAMLOG(TWIT,GIND,DWIREC,WITSTS)
	  TOTAMT=0
	  DO 110 I=1,MAXWRW
	  ODDS(I)=WTPOOL(I,1,2,GIND)
	  AMOUNT(I)=WTPOOL(I,2,2,GIND)
	  IF(DWISTA(I).EQ.GAMCAN.OR.DWISTA(I).EQ.GAMREF
     *	     .OR.DWISTA(I).EQ.GAMBFD.OR.AMOUNT(I).EQ.0) THEN
	    ODDS(I)=99999
	  ELSE
	    TOTAMT=TOTAMT+AMOUNT(I)
	  ENDIF
110	  CONTINUE
	  GOTO 300
	ENDIF
C
C GET GAME DATA FROM FILE
C
	SMODE=.TRUE.
	CALL OPENW(1,GFNAMES(1,GNUM),0,0,0,ST)
	CALL IOINIT(FDB,1,DWISEC*256)
	IF(ST.NE.0) THEN
	  WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
	  CALL USRCLOS1(1)
	  RETURN
	ENDIF
	CALL READW(FDB,DRAW,DWIREC,ST)
	IF(ST.NE.0) THEN
	  WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
	  CALL USRCLOS1(1)
	  RETURN
	ENDIF
	CALL USRCLOS1(1)
	IF(DWISTS.EQ.0) THEN
	  WRITE(CLIN23,3040) GTNAMES(TWIT),GIND,DRAW
	  RETURN
	ENDIF
C
C
	TOTAMT=0
	DO 210 I=1,MAXWRW
	   AMOUNT(I)=0
	   AMOUNT(I)=DWISBR(I)
	   IF(DWISTA(I).NE.GAMCAN.AND.DWISTA(I).NE.GAMREF) THEN
     	       TOTAMT=TOTAMT+DWISBR(I)
	   ELSE
	       ODDS(I)=99999
	   ENDIF
210	CONTINUE
C
	TAX = 0
	NETPOL = TOTAMT * CALPER(DWISPR)
	DO 220 I=1,MAXWRW
	  IF(AMOUNT(I).EQ.0) THEN
	    RODDS = 99999
	  ELSE
	    RODDS = NETPOL / REAL(AMOUNT(I))*100
	  ENDIF
	  ODDS(I) = NINT(RODDS)
	  IF(ODDS(I).LT.100)                      ODDS(I)=100
	  IF(ODDS(I).GT.100 .AND.ODDS(I).LT.150)  ODDS(I)=(ODDS(I)/5)   *  5
	  IF(ODDS(I).GT.150 .AND.ODDS(I).LT.1000) ODDS(I)=(ODDS(I)/10)  * 10
	  IF(ODDS(I).GT.1000.AND.ODDS(I).LT.99999)ODDS(I)=(ODDS(I)/100) *100
	  IF(ODDS(I).GT.99999)                    ODDS(I)=99999
	  IF(DWISTA(I).EQ.GAMCAN.OR.DWISTA(I).EQ.GAMREF) ODDS(I)=99999
C	ENDIF
220	CONTINUE
C
C CALCULATE TAXES AND NET POOL
C
300	CONTINUE
	TAX = 0 
	RNDPOL = DWIBRK(1)
	NETPOL = (TOTAMT * CALPER(DWISPR) - TAX) + RNDPOL
	TOTPOL = (TOTAMT * CALPER(DWISPR) - TAX) + RNDPOL + DWIPOL(1)
C
C
	IF(DWISTS.EQ.GFINAL) THEN
	  DBUF(5)=DAYCDC
	  CALL LCDATE(DBUF)
	  WRITE(CLIN1,900) GTNAMES(TWIT),GIND
	  WRITE(CLIN2,902)(DWIENM(I),I=1,3),DRAW,POLSTS(DWISTS+1)
	ELSE
	  DBUF(5)=DAYCDC
	  BEGSAL(5)=DWIBSD
	  ENDSAL(5)=DWIESD
	  DBUF2(5) = DWIDAT
	  CALL LCDATE(DBUF)
	  CALL LCDATE(BEGSAL)
	  CALL LCDATE(ENDSAL)
	  CALL LCDATE(DBUF2)
	  WRITE(CLIN1,901) GTNAMES(TWIT),GIND,
     *			   (BEGSAL(I),I=7,13),(ENDSAL(I),I=7,13)
	  WRITE(CLIN2,9021)(DWIENM(I),I=1,3),DRAW,POLSTS(DWISTS+1),
     *                     (DBUF2(I),I=7,13)        
	ENDIF
	LIN=4
	IF(DWISTS.LT.GFINAL) THEN
	  WRITE(CLIN3,903)
	  DO 310 R=1,18
	  WRITE(XNEW(LIN),904) R,(DWINMS(I,R),I=1,3),ODDS(R)/100,
     *	   MOD(ODDS(R),100),CMONY(AMOUNT(R),9,BETUNIT),
     *	   RWSTS(DWISTA(R)+1),R+18,
     *	   (DWINMS(I,(R+18)),I=1,3),ODDS(R+18)/100,
     *	   MOD(ODDS(R+18),100),CMONY(AMOUNT(R+18),9,BETUNIT),
     *	   RWSTS(DWISTA(R+18)+1)
	 LIN=LIN+1
310	 CONTINUE
	 WRITE(CLIN22,922) CMONY(TOTAMT,12,BETUNIT),
     *                     CMONY(NETPOL,11,BETUNIT),
     *                     CMONY(DWIPOL(1),10,BETUNIT),
     *	                   CMONY(TOTPOL,11,BETUNIT)
	ELSE
	  BEGSAL(5)=DWIBSD
	  ENDSAL(5)=DWIESD
	  CALL LCDATE(BEGSAL)
	  CALL LCDATE(ENDSAL)
	  NETSAL=DWISAL-DWIREF
	  TAX = 0
	  WINSHR = NETSAL * CALPER(DWISPR)
	  WRITE(CLIN3,1003)
	  DO 320 W=1,4
	  IF(DWIWIN(W).NE.0) THEN
	    WRITE(XNEW(  LIN),1004)W,DWIWIN(W),
     *	      (DWINMS(I,DWIWIN(W)),I=1,4),
     *	      CMONY(DWISBR(DWIWIN(W)),10,BETUNIT),DWIODS(W)/100,
     *	      MOD(DWIODS(W),100)
	  ENDIF
	  IF(DWIWIN(W).EQ.0) WRITE(XNEW(LIN),1000)
	  LIN=LIN+1
320	  CONTINUE
	  J=1
	  DO 330 I=1,MAXWRW
	  IF(DWISTA(I).EQ.GAMCAN.OR.DWISTA(I).EQ.GAMREF) THEN
	    CANRWS(J)=I
	    J=J+1
	  ENDIF
330	  CONTINUE
          WRITE(CLIN8,1000)
          WRITE(CLIN9,1000)
	  IF(J.GE.2) THEN
             IF(J.LE.20) THEN
                WRITE(CLIN8,1006) (CANRWS(I),I=1,J-1)
             ELSE
                WRITE(CLIN8,1006) (CANRWS(I),I=1,19)
                WRITE(CLIN9,1007) (CANRWS(I),I=20,J-1)
             ENDIF
          ENDIF
          DBUF2(5)=DWIDAT
          CALL LCDATE(DBUF2)
          WRITE(CLIN10,1010) (BEGSAL(I),I=7,13),(DBUF2(I),I=7,13)
          IF(DWIPUP.GT.0) THEN
             DBUF2(5)=DWIPUP
             CALL LCDATE(DBUF2)
             WRITE(CLIN11,10111) (ENDSAL(I),I=7,13),(DBUF2(I),I=7,13)
          ELSE
             WRITE(CLIN11,1011) (ENDSAL(I),I=7,13)
          ENDIF
C	  WRITE(CLIN12,1000)
	  WRITE(CLIN12,1012) CMONY(DWISAL,11,BETUNIT)
	  WRITE(CLIN13,1013) CMONY(DWIREF,11,BETUNIT)
	  WRITE(CLIN14,1014) CMONY(NETSAL,11,BETUNIT)
	  WRITE(CLIN15,1015) CMONY(WINSHR,11,VALUNIT)
	  WRITE(CLIN16,1016) CSMONY(DWIPOL(1),10,BETUNIT)
	  WRITE(CLIN17,1017) CSMONY(DWIBRK(2),11,BETUNIT)
	  WRITE(CLIN18,1018) CMONY(DWIWON-DWIREF,11,VALUNIT)
	  WRITE(CLIN19,1019) CSMONY(DWIPOL(2),10,BETUNIT)
	  WRITE(CLIN20,1020) CMONY(DWIPAD-DWIPRF,11,VALUNIT)
	  WRITE(CLIN21,1021) CMONY(DWIPRF,11,BETUNIT)
          IF(DWIPUP.NE.0) THEN
             PRG_AMT = ((DWIWON-DWIREF) - (DWIPAD-DWIPRF) 
     *                   + DWIREF - DWIPRF)
          ELSE
             PRG_AMT = 0
          ENDIF
	  WRITE(CLIN22,1022) CMONY(PRG_AMT,11,BETUNIT)
	ENDIF
	RETURN
C
C FORMATING STATEMENTS
C
800	FORMAT('Input error')
810	FORMAT('Value error')
820	FORMAT('Sorry, that row not activated, no change allowed')
900	FORMAT('* ',A8,1X,I1,1X,' *',30(' '))
901	FORMAT(A8,1X,I1,7A2,' -',7A2)
902	FORMAT(3(' '),3A4,5(' '),'Event code -',
     *	   I4,5(' '),'* ',A17,' *')
9021	FORMAT(3(' '),3A4,5(' '),'Event code -',
     *	   I4,3(' '),'* ',A17,' *',1X,'Draw ',7A2)
903	FORMAT('Row',1(' '),'Name',10(' '),'Odds',
     *	  3(' '),'Amount',1(' '),'Sts',2(' '),
     *	       'Row',1(' '),'Name',10(' '),'Odds',
     *	       3(' '),'Amount',1(' '),'Sts')
904	FORMAT(I2.2,1(' '),3A4,T15,' ',I3,'.',I2.2,
     *	 1(' '),A9,1(' '),A3,2(' '),
     *	 I2.2,1(' '),3A4,T53,' ',I3,'.',I2.2,1(' '),A9,1(' '),A3)
922	FORMAT('Sales ',A12,1(' '),'Net pool ',A11,1(' '),
     *	       'Extra ',A10,1(' '),'Tot pool ',A11)
1000	FORMAT(80(' '))
1003	FORMAT(15X,'Row',2X,'Winner',12X,'Amount bet',8X,'Odds',T80)
1004    FORMAT('Results  ',I1,6X,I2.2,2X,4A4,2X,A10,1X,I8,'.',I2.2)
1006    FORMAT('Cancelled  rows---> ',19(I2,' '))                       
1007    FORMAT(20X,19(I2,' '))
1010  FORMAT(10(' '),'Beginning sales ',7A2,6(' '),'Draw  ',7A2,12(' '))
1011  FORMAT(10(' '),'Ending    sales ',7A2,40(' '))
10111 FORMAT(10(' '),'Ending    sales ',7A2,6(' '),'Purge ',7A2,12(' '))
1012	FORMAT(15(' '),'Total Sales',9(' '),A11,34(' '))
1013	FORMAT(15(' '),'Total Refunds ',6(' '),A11,34(' '))
1014	FORMAT(15(' '),'Net Sales',11(' '),A11,34(' '))
1015	FORMAT(15(' '),'Winners Share',7(' '),A11,34(' '))
1016	FORMAT(15(' '),'Extra Amount  ',7(' '),A10,34(' '))
1017	FORMAT(15(' '),'Rounding Pot',8(' '),A11,34(' '))
1018	FORMAT(15(' '),'Winning Amount',6(' '),A11,34(' '))
1019	FORMAT(15(' '),'Roll Pot   ',10(' '),A10,34(' '))
1020	FORMAT(15(' '),'Winners Paid',8(' '),A11,34(' '))
1021	FORMAT(15(' '),'Refunds Paid',8(' '),A11,34(' '))
1022	FORMAT(15(' '),'Amount Purged',7(' '),A11,34(' '))
3000	FORMAT('Enter !',A8,' game index ')
3010	FORMAT(A8,1X,I1,' game not active')
3020	FORMAT(5A4,' open error ',I4)
3030	FORMAT(5A4,' read error ',I4,' record > ',I4)
3040	FORMAT(A8,1X,I1,' game not initialized event > ',I4)
	END
