C
C SUBROUTINE DATSUB
C  
C V12 06-APR-2017 MTK Fixed V11 change, added BSD check after file update
C V11 29-NOV-2000 JHR Added Results Game Type.
C V10 23-NOV-2000 UXN Logical units fixed.
C V09 13-OCT-1999 RXK Notused setting of advance dates removed (not correct
C                     in the case of World Tour). 
C V08 29-MAY-1997 RXK More fixes
C V07 14-SEP-1995 HXK Make changes to allow for Ravi game BSD, ESD
C V06 10-AUG-1995 HXK Fix for setting up draw days / sales days 
C V05 19-FEB-1995 HXK Change for V5/V65
C V04 18-FEB-1995 HXK Changes for V5 game
C V03 15-OCT-1994 HXK Adding /developing Bingo (15.Oct.94)
C V02 07-JUL-1993 HXK ADDED RAVI, SPEDE GAMES
C V01 21-JAN-1993 DAB Initial Release Based on Netherlands Bible
C                     DEC Baseline
C
C
C SUBROUTINE TO SET DRAW DATES FOR LOTTO/SPORTS GAMES
C
C
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
        SUBROUTINE DATSUB(DFILE,GFILE,GNUM,GTYP)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECDAF.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
        INCLUDE 'INCLIB:DTGREC.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:DNBREC.DEF'
        INCLUDE 'INCLIB:DBNREC.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'

        INTEGER*4 DFDB(7),GFDB(7),DFILE(5),GFILE(5),GBUF(1100)
        INTEGER*4 GAMDAT(DATLEN),ADAYS(7)
C       INTEGER*4 BDAYS(7)
        INTEGER*4 DOW, LCDC, ECDC, FCDC, INDSAV, ANS, K, TOP
        INTEGER*4 J, I, EXT, FDRAW, LDRAW,NEXT, MAX, IND, CDC, ST
        INTEGER*4 GAMESD, GAMBSD, GAMDRW, GAMSTS, LAST_DRAW
        INTEGER*4 GTYP, GNUM, CDC1, CDC2, REMDRW, REMCDC, REMFDRAW
        INTEGER*4 BEGIND, CNT, REMSTS, DRWDAYOFF, CHKDRW, BSD, ESD
        INTEGER*4 LASTDRAW
        EQUIVALENCE (DLTREC,DSPREC,DTGREC,DKKREC,DNBREC,DBNREC,GBUF)
        EQUIVALENCE (GBUF(1),GAMSTS)
        EQUIVALENCE (GBUF(4),GAMDRW)
        EQUIVALENCE (GBUF(5),GAMBSD)
        EQUIVALENCE (GBUF(6),GAMESD)
        EQUIVALENCE (GBUF(9),GAMDAT)
        INTEGER*2 DATE(LDATE_LEN)
        INTEGER*2 DRAWS(7,5000)        !1-cdc,2-day of week,3-day sts,4-draw #,
                         !5-draw day(if not zero),6-beg.of sales,7-end of sales
        CHARACTER*3 FUN
        CHARACTER*80 LINE
        LOGICAL CONLY, SEEFLG
C
C OPEN ALL FILES
C
        CONLY=.FALSE.
        SEEFLG=.FALSE.
        CALL OPENW(2,DFILE,4,0,0,ST)
        CALL IOINIT(DFDB,2,DAFSEC*256)
        IF(ST.NE.0) CALL FILERR(DFILE,1,ST,0)
C
        CALL OPENW(3,GFILE,4,0,0,ST)
        IF(GTYP.EQ.TLTO) CALL IOINIT(GFDB,3,DLTSEC*256)
        IF(GTYP.EQ.TSPT) CALL IOINIT(GFDB,3,DSPSEC*256)
        IF(GTYP.EQ.TTGL) CALL IOINIT(GFDB,3,DTGSEC*256)
        IF(GTYP.EQ.TKIK) CALL IOINIT(GFDB,3,DKKSEC*256)
        IF(GTYP.EQ.TNBR) CALL IOINIT(GFDB,3,DNBSEC*256)
        IF(GTYP.EQ.TBNG) CALL IOINIT(GFDB,3,DBNSEC*256)
        IF(ST.NE.0) CALL FILERR(GFILE,1,ST,0)
C
C POSITION FILE
C
50      CONTINUE
        CDC=0
        IND=0
        MAX=0
        NEXT=0
        LDRAW=0
        FDRAW=0
        LAST_DRAW = 0
        WRITE(6,901)
        CALL INPDAT(CDC,EXT)
        CDC=CDC+1
        REMCDC=CDC
        IF(EXT.LT.0) CDC=1
C
C CLEAR MEMORY DATE TABLE
C
        DO 110 I=1,5000
           DO 110 J=1,7
              DRAWS(J,I)=0
110     CONTINUE
        IND=1
        TOP=CDC
C
C BUILD MEMORY DATE TABLE FROM DAF FILE
C
        WRITE(6,902)
120     CONTINUE
        CALL READW(DFDB,CDC,DAFREC,ST)
        IF(ST.NE.0.AND.ST.NE.144) CALL FILERR(DFILE,2,ST,CDC)
        IF(DAFSTS.EQ.DUNUSD.OR.ST.EQ.144) GOTO 130
        IF(DAFSTS.GT.DSOPEN) THEN
          IF(DAFSTS.LE.DSKILL) THEN
            TYPE*,'Last stopsys date not entered '
            GOTO 50
	  ELSE
            GOTO 130  !any other value like treated like end of file
	  ENDIF
        ENDIF
        REMSTS = DAFSTS
        DATE(5)=CDC
        CALL LCDATE(DATE)
        DRAWS(1,IND)=DATE(5)
        DRAWS(2,IND)=DATE(6)
        DRAWS(3,IND)=DAFSTS
        DRAWS(4,IND)=DAFDRW(GNUM)
        IF(DAFDRW(GNUM).GT.0.AND.FDRAW.EQ.0) THEN
          FDRAW=DAFDRW(GNUM)
        ELSEIF(DAFHDR(GNUM).GT.0.AND.FDRAW.EQ.0) THEN
          FDRAW=DAFHDR(GNUM) + 1
        ENDIF
        IF(FDRAW.EQ.0) THEN
          CALL INPNUM('Enter first draw number ',FDRAW,1,999999,EXT)
          LDRAW=FDRAW
        ENDIF
        IF(DAFDRW(GNUM).LT.1.AND.NEXT.EQ.0) NEXT=IND-1
        IF(DAFDRW(GNUM).GE.1) NEXT=0
        IF(IND.GE.2) THEN
          IF(DRAWS(4,IND).NE.DRAWS(4,IND-1)) THEN
C*** *       .AND.DRAWS(4,IND).GT.0) THEN
             DRAWS(5,IND-1)=DRAWS(4,IND-1)
             LDRAW=DRAWS(4,IND-1)
          ENDIF
        ENDIF
        CDC=CDC+1
        MAX=IND
        IND=IND+1
        IF(IND.GT.5000) THEN
          TYPE*,'Memory date table full '
          CALL XWAIT(2,2,ST)
          CALL CLOSEFIL(DFDB)
          RETURN
        ENDIF
        GOTO 120
C
130     CONTINUE
        IF(CDC.EQ.TOP) THEN
          TYPE*,'Set sales dates first '
          CALL XWAIT(2,2,ST)
          CALL CLOSEFIL(DFDB)
          RETURN
        ELSEIF(DAFSTS.EQ.DUNUSD) THEN
           TYPE*,'Last set CDC is ',CDC-1,'  with sales status',REMSTS
        ELSEIF(ST.EQ.144) THEN
           TYPE*,'Last CDC in DAF is',CDC-1 
        ENDIF
        CALL XWAIT(2,2,ST)
        
CCC     IF(DRAWS(4,MAX).GT.0) CONLY=.TRUE.    
        IF(NEXT.GE.2) THEN
          IF(DRAWS(4,NEXT-1).EQ.DRAWS(5,NEXT-1)) THEN 
C*** *       .AND. DRAWS(4,NEXT-1).GT.0) LDRAW=LDRAW+1
          ENDIF
        ENDIF
        IF(FDRAW.EQ.0) FDRAW=1
C
C DISPLAY MENU AND GET FUNCTION
C
140     CONTINUE
        CALL CLRSCR(6)
        WRITE(6,903)
        CALL WIMG(6,'Enter function ')
        READ(5,904) FUN
C
        IF((FUN.EQ.'UPD'.OR.FUN.EQ.'SET').AND.CONLY) THEN
          TYPE*,'You must set more sales dates'
          TYPE*,'for UPD or SET functions '
          CALL XWAIT(2,2,ST)
          GOTO 140
        ENDIF
C
C
        IF(FUN.EQ.'UPD') GOTO 1000
        IF(FUN.EQ.'CHA') GOTO 2000
        IF(FUN.EQ.'SET') GOTO 4000
        IF(FUN.EQ.'EXT') GOTO 6000
        IF(FUN.EQ.'TES') GOTO 500
C
C INVALID FUNCTION
C
        CALL CLRSCR(6)
        TYPE*,'Invalid function '
        CALL XWAIT(2,2,ST)
        GOTO 140
C
C TEST FUNCTION
C
500     CONTINUE

        CALL INPNUM('Enter beginning CDC to be displayed (E=all)',
     *              CDC1,1,99999,EXT)
        IF(EXT.LT.0) CDC1=DRAWS(1,1)
        CALL INPNUM('Enter ending CDC to be displayed (E=all)',
     *                  CDC2,1,99999,EXT)
        IF(EXT.LT.0) CDC2=99999
        IF(CDC1.GT.CDC2) THEN
          TYPE *,IAM()
          TYPE *,IAM(), ' Bad beginning/ending CDC dates'
          CALL XWAIT(2,2,ST)
          GOTO 140
        ENDIF
C
        WRITE(6,910)

        REMDRW=0
        DO 550 I=1,MAX  
          IF(DRAWS(1,I).LT.CDC1) GOTO 550
          IF(DRAWS(1,I).GT.CDC2) GOTO 560
          IF(REMDRW.NE.DRAWS(4,I).AND.DRAWS(1,I).GT.REMCDC.AND.
     *       DRAWS(6,I).EQ.0.AND.DRAWS(7,I).EQ.0) THEN
             IF(DRAWS(4,I).GT.0) THEN
                CALL READW(GFDB,DRAWS(4,I),GBUF,ST)
                IF(ST.NE.0) THEN
                   CALL FILERR(GFILE,2,ST,DRAWS(4,I))
                ENDIF
                WRITE(6,909)(DRAWS(J,I),J=1,5),GAMBSD,GAMESD, 
     *             '  bsd,esd from game file'
             ELSE
                WRITE(6,909)(DRAWS(J,I),J=1,7), ' '
             ENDIF
             REMDRW=DRAWS(4,I)
          ELSE
             WRITE(6,909)(DRAWS(J,I),J=1,7), ' '
          ENDIF
          IF((I/40)*40.EQ.I) WRITE(6,910)
550     CONTINUE
C
560     CONTINUE
        CALL XWAIT(2,2,ST)
C
        IF(SEEFLG) GOTO 1500
        GOTO 140
C
C *** DRAW UPDATE
C
1000    CONTINUE
        NEXT=1
        IND=1
        CNT=0
        CDC=DRAWS(1,IND)
1005    CONTINUE 
        LDRAW=DRAWS(4,IND)
        TYPE*,IAM(),'UPDATE FOR DRAW',LDRAW
        BEGIND=IND
1010    CONTINUE
        DATE(5)=CDC
        CALL LCDATE(DATE)
        WRITE(LINE,905) (DATE(K),K=7,13)
        CALL WIMG(6,LINE)
        CALL YESNO(ANS)
        IF(ANS.EQ.3) GOTO 1020
        DRAWS(4,IND)=LDRAW
        DRAWS(5,IND)=0
        IF(ANS.EQ.1) THEN
          DRAWS(5,IND)=LDRAW
      type*,'upd ind,ldraw',IND,LDRAW
          LDRAW=LDRAW+1
        ENDIF
        CDC=CDC+1
        IND=IND+1
        CNT=CNT+1
        GOTO 1010

1020    CONTINUE
C
C UPDATE MEMORY TABLE FOR UPDATED DRAW
C
        DO 1030 I=BEGIND,BEGIND+CNT
           IF(DRAWS(5,I).GT.0) THEN
              DRAWS(7,I)=DRAWS(1,I)
              DRAWS(6,I+1)=DRAWS(1,I+1)
           ENDIF
1030    CONTINUE

C
C APPLY DATE UPDATE TO FILES
C
1500    CONTINUE
        IF(.NOT.SEEFLG) THEN
           CALL WIMG(6,'Do you want to see changes to files [Y/N] ')
           CALL YESNO(ANS)
           IF(ANS.NE.2) THEN
              SEEFLG=.TRUE.
              GOTO 500
           ENDIF 
        ENDIF
        SEEFLG=.FALSE.
        CALL WIMG(6,'Do you want to apply changes to files [Y/N] ')
        CALL YESNO(ANS)
        IF(ANS.NE.1) THEN
          TYPE*,'Files not updated '
          CALL XWAIT(3,2,ST)
          GOTO 50
        ENDIF
C
C READ DAF AND GAME FILE RECORDS
C
        IND = 1
        INDSAV = 1
        LAST_DRAW = 0
1505    CONTINUE
        IF(IND.GT.MAX.OR.DRAWS(4,IND).LT.0) THEN
	  GOTO 5000
	ENDIF

C***    !IF(DRAWS(4,IND).EQ.0) THEN
C***    !   IND=IND+1
C***    !   GOTO 1505
C***    !ENDIF

        IF(IND.GE.2) THEN
           IF((DRAWS(4,IND).NE.0 .AND. DRAWS(4,IND-1).EQ.0) .OR. !new condn.
     *        (DRAWS(4,IND).EQ.LAST_DRAW+1) ) THEN
              LAST_DRAW = LAST_DRAW + 1
              INDSAV = IND
           ENDIF
        ENDIF
        CDC=DRAWS(1,IND)
        CALL READW(DFDB,CDC,DAFREC,ST)
        IF(ST.NE.0) CALL FILERR(DFILE,2,ST,CDC)
        DAFDRW(GNUM)=DRAWS(4,IND)
        IF(DRAWS(5,IND).GT.0) THEN
           CALL READW(GFDB,DAFDRW(GNUM),GBUF,ST)
           IF(ST.NE.0) CALL FILERR(GFILE,2,ST,DAFDRW(GNUM))
           IF(DRAWS(6,IND).EQ.0) THEN
C
C JHR - REMOVED FOR PORTUGAL -
C
C              IF(GAMBSD.GE.DRAWS(1,1).OR.GAMBSD.EQ.0) THEN
C
                IF(GAMBSD.GE.DRAWS(1,1).OR.GAMBSD.EQ.0) GAMBSD=DRAWS(1,INDSAV)
                INDSAV = INDSAV + 1
C              ENDIF
           ELSE
              IF(GAMBSD.GE.DRAWS(1,1).OR.GAMBSD.EQ.0) GAMBSD=DRAWS(6,IND)
              GAMESD=DRAWS(7,IND)
           ENDIF 
        ENDIF
        GAMSTS=GAMOPN
        GAMDRW=DRAWS(4,IND)
        CALL FASTSET(0,GAMDAT,DATLEN)
C
C INITIALIZE DRAW DATES
C
        I=IND-1
        DO 1620 J=CURDRW,NDRW19
1610    CONTINUE
        I=I+1
        IF(I.GT.MAX) GOTO 1630
        IF(DRAWS(5,I).EQ.0) GOTO 1610
        GAMDAT(J) = DRAWS(1, I) + DRWDAYOFF
1620    CONTINUE
C
C WRITE RECORDS BACK TO FILES
C
1630    CONTINUE
        GAMESD = GAMDAT(CURDRW) - DRWDAYOFF
        CALL WRITEW(DFDB,CDC,DAFREC,ST)
        IF(ST.NE.0) CALL FILERR(DFILE,3,ST,CDC)
        IF(DRAWS(5,IND).GT.0) THEN
          CALL WRITEW(GFDB,DAFDRW(GNUM),GBUF,ST)
          IF(ST.NE.0) CALL FILERR(GFILE,3,ST,DAFDRW(GNUM))
          LASTDRAW = DAFDRW(GNUM)
        ENDIF
        LDRAW=DRAWS(4,IND)
        IF(DRAWS(4,IND).EQ.DRAWS(5,IND)) LDRAW=LDRAW+1
        NEXT=IND+1
        IND=IND+1
        GOTO 1505
C
C *** CHANGE EXISTING DATES
C
2000    CONTINUE
        WRITE(6,906)
        CALL INPDAT(CDC,EXT)
        IF(EXT.LT.0) GOTO 2100
        IND=CDC-TOP+1
        IF(IND.LT.1) IND=1
        IF(CDC.LT.TOP.OR.DRAWS(4,IND).LE.0) THEN
          WRITE(6,907) CDC
          GOTO 2000
        ENDIF
        DATE(5)=CDC
        CALL LCDATE(DATE)
        IF(DRAWS(1,IND).NE.CDC) THEN
          WRITE(6,908) DRAWS(1,IND),CDC
          CALL GPAUSE
        ENDIF
C
C
        DATE(5)=CDC
        CALL LCDATE(DATE)
        CALL CLRSCR(6)
        WRITE(LINE,905) (DATE(K),K=7,13)
        CALL WIMG(6,LINE)
        CALL YESNO(ANS)
        IF(ANS.EQ.3) GOTO 2100
        DRAWS(5,IND)=0
        IF(ANS.EQ.1) DRAWS(5,IND)=1
        GOTO 2000
C
C UPDATE MEMORY TABLE
C
2100    CONTINUE
        LDRAW=FDRAW
        DO 2200 I=1,MAX
           IF(DRAWS(4,I).GT.0) THEN
              DRAWS(4,I)=LDRAW     !!     <---
           ENDIF
           IF(DRAWS(5,I).NE.0) THEN
              DO K=1,9
                 IF((I-K).GE.1) THEN
                    IF(DRAWS(4,I-K).EQ.LDRAW) THEN
                       DRAWS(6,I)=DRAWS(1,I-K)
                    ENDIF 
                 ENDIF
              ENDDO
              DRAWS(5,I)=LDRAW
              LDRAW=LDRAW+1
              DRAWS(7,I)=DRAWS(1,I)
           ENDIF
2200    CONTINUE
        GOTO 1500
C
C *** AUTO UPDATE
C
4000    CONTINUE
        CALL CLRSCR(6)
        FCDC=DRAWS(1,1)
        ECDC=DRAWS(1,MAX)
C
C GET LAST DATE TO INITIALIZE
C
4010    CONTINUE
        TYPE*,'Enter last day to be initialized'
        CALL INPDAT(LCDC,EXT)
        IF(EXT.LT.0) GOTO 140
C        IF(LCDC.GT.ECDC.OR.LCDC.LT.FCDC) THEN
C        IF(LCDC.LT.FCDC) THEN
C          TYPE*,'Sorry, date out for range '
C          GOTO 4010
C        ENDIF
C
C
        CALL INPYESNO('Is Monday    A End Sales Day [Y/N]', ADAYS(1))
        CALL INPYESNO('Is Tueday    A End Sales Day [Y/N]', ADAYS(2))
        CALL INPYESNO('Is Wednesday A End Sales Day [Y/N]', ADAYS(3))
        CALL INPYESNO('Is Thursday  A End Sales Day [Y/N]', ADAYS(4))
        CALL INPYESNO('Is Friday    A End Sales Day [Y/N]', ADAYS(5))
        CALL INPYESNO('Is Saturday  A End Sales Day [Y/N]', ADAYS(6))
        CALL INPYESNO('Is Sunday    A End Sales Day [Y/N]', ADAYS(7))
C
C ENTER HOW MAY DAYS FOR DRAW DATE AFTHER END SALES DAY
C
	TYPE *, IAM()
	TYPE *, IAM(), 'Attention ...'
	TYPE *, IAM()
	TYPE *, IAM(), 'Be Carefully With Number Of Days For Draw Date Question'
	TYPE *, IAM()
	TYPE *, IAM(), 'Draw Day Must Be Before Than Next End Sales Day'
	TYPE *, IAM()
        CALL INPNUM('Enter Number Of Days For Draw Date Afther End Sales Day', 
     *               DRWDAYOFF, 0, 6, EXT)
        IF(EXT .LT. 0) DRWDAYOFF = 0
C
C ASK IF USER WANTS RESTRIC GAME SALES DAY
C
C        RESTRICT = 0
C        CALL INPYESNO('Do you want to restrict game sale days [Y/N]',RESTRICT)
C        IF(RESTRICT.EQ.1) THEN
C           CALL INPYESNO('Is Monday    A Sales Day [Y/N]', BDAYS(1))
C           CALL INPYESNO('Is Tuesday   A Sales Day [Y/N]', BDAYS(2))
C           CALL INPYESNO('Is Wednesday A Sales Day [Y/N]', BDAYS(3))
C           CALL INPYESNO('Is Thrusday  A Sales Day [Y/N]', BDAYS(4))
C           CALL INPYESNO('Is Friday    A Sales Day [Y/N]', BDAYS(5))
C           CALL INPYESNO('Is Saturday  A Sales Day [Y/N]', BDAYS(6))
C           CALL INPYESNO('Is Sunday    A Sales Day [Y/N]', BDAYS(7))
C        ENDIF
C
C GET NEXT DATE TO INITIALIZE
C
        NEXT=1
        IND=1
        CDC=DRAWS(1,1)
        IF(LDRAW.EQ.0) LDRAW=1
        REMFDRAW = FDRAW 
4020    CONTINUE
        DATE(5)=CDC
        CALL LCDATE(DATE)
        DOW=DATE(6)
        DRAWS(4,IND)=FDRAW
C        IF(RESTRICT.EQ.1) THEN
C           DRAWS(4,IND)=0
C           IF(BDAYS(DOW).EQ.1) DRAWS(4,IND)=FDRAW
C        ENDIF
        DRAWS(5,IND)=0
        IF(ADAYS(DOW).EQ.1) THEN
          DRAWS(5,IND)=FDRAW
          FDRAW=FDRAW+1
        ENDIF
        CDC=CDC+1
        IND=IND+1
        NEXT=IND
        IF(CDC.GT.LCDC) THEN
           FDRAW = REMFDRAW
           GOTO 2100
        ENDIF
        GOTO 4020
C
C VERIFY BEGINNING / ENDING SALES DATES
C
5000	CONTINUE
	TYPE*,'Verifying start / end sales dates by draw'
	CHKDRW = DRAWS(4,1)-1
	IF(CHKDRW.LT.1) CHKDRW = 1
	CALL READW(GFDB,CHKDRW,GBUF,ST)
	IF(ST.NE.0) CALL FILERR(GFILE,2,ST,CHKDRW)
	ESD = GAMESD
5001	CONTINUE
	CHKDRW = CHKDRW + 1
	IF(CHKDRW.GT.LASTDRAW) GOTO 140
	CALL READW(GFDB,CHKDRW,GBUF,ST)
	IF(ST.NE.0) CALL FILERR(GFILE,2,ST,CHKDRW)
	IF(GAMBSD.NE.ESD+1) THEN
	  TYPE*,'Adjusting beginning sales date for draw ',CHKDRW
	  GAMBSD = ESD + 1
	  CALL WRITEW(GFDB,CHKDRW,GBUF,ST)
          IF(ST.NE.0) CALL FILERR(GFILE,3,ST,CHKDRW)
	ENDIF
	ESD = GAMESD
	GOTO 5001

C
C CLOSE FILES AND EXIT
C
6000    CONTINUE
        CALL CLOSEFIL(DFDB)
        CALL CLOSEFIL(GFDB)
        RETURN
C
C
C
901     FORMAT(' Enter last stopsys date [E-First run]: ')
902     FORMAT(' Building memory table ')
903     FORMAT(' Date update functions: ',//,
     *         ' UPD - Update sales and draw date (1 draw at time) ',/,
     *         ' CHA - Change existing dates (all draws) ',/,
     *         ' PRT - Print dates from files ',/,
     *         ' SET - Preset drawing dates ',/,
     *         ' EXT - Exit program ',//)
904     FORMAT(A3)
905     FORMAT('Is ',7A2,' a draw date [Y/N] ')
906     FORMAT(' Enter date to change [E-Exit] ')
907     FORMAT('  CDC ',I4,' date not changeable')
908     FORMAT(' Table ',I4,' CDC ',I4,' internal error  ')
909     FORMAT(7(3X,I5),A)
910     FORMAT(/,1X,'  ================== MEMORY TABLE =============='
     *     '========',/         
     *              '     CDC     DOW     STS    DRW#    DRWD     BSD',
     *     '     ESD',/)
        END
