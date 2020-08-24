C SUBROUTINE SPTSET
C
C V13 30-MAR-2015 MTK Modified Super 14 game
C V12 06-JAN-2009 CPH NEW QUESTION  
C V11 23-OCT-2003 FRP Modify for Batch2 Totobola Changes.
C V10 04-may-2000 OXK Winning div params from VAKDIV, not manual input,
C			Event name & Row price asked
C V09 01-MAR-2000 OXK Winning div parameters added
C V08 05 Jun 1997 UXN Calculation of REV3 changed.
C V07 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			Instant Pass Thru Phase 1
C V06 06 Nov 1993 GXA Touched it inorder to update commonolb!
C V05 05 Nov 1993 GXA Restored Previous Version 
C			(Revisions do work this way after all).
C V04 30 Jul 1993 CXK CHANGED FORMAT(907) TO 4A4 (SMH)
C V03 07 Jul 1993 GXA  Released for Finland Dec Conversion / Oddset.
C V02 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
	SUBROUTINE SPTSET(GIND)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:DESPAR.DEF'
	COMMON SCFREC
	INTEGER*4 DFDB(7),SFDB(7),BYTTAB(500),NAME(SPNMS_LEN/4)
	INTEGER*4 REV1,REV2,REV3,REV4
	INTEGER*4 GNUM,GIND,DRW,LSTAT
	INTEGER*4 I,WEK,J,K,BROW,EROW,ST,EXT,ANS,ROW,YEAR
	INTEGER*4 BUFIND,PREV3,VERR,CDC, CLEARED,MTX_IDX
	INTEGER*2 BDATE(LDATE_LEN),EDATE(LDATE_LEN),
     *		   DATE(LDATE_LEN),DDATE(LDATE_LEN)
        CHARACTER*1 BELL /Z07/
	CHARACTER*6 PASS,GODPAS
	CHARACTER*20 PWORD
	EQUIVALENCE (PASS,PWORD)
	CHARACTER*2 FUN
	CHARACTER CNAME(SPNMS_LEN)
        CHARACTER * 16 TEAM_NAME
	EQUIVALENCE (NAME,CNAME,TEAM_NAME)
	CHARACTER*12 GSTAT(11)
	DATA GSTAT/'Not set     ','Game closed ',
     *	           'Info entered','Game open   ',
     *	           3*'End of game ',
     *	           'Drawing done','Game final  ',
     *	           'Cancelled   ','Refund      '/
	CHARACTER*14 CDAY
	INTEGER*2 DAY(7)
	LOGICAL TSTFLG/.FALSE./
	DATA CDAY/'****notset****'/
	DATA GODPAS/'PERKIN'/
	EQUIVALENCE(CDAY,DAY)

        INTEGER     PRC
	INTEGER*4   BCNT
C	INTEGER	    SHR_ID,NROWS
C
C CHECK IF SPORTS GAME IS ACTIVE
C
        VERR = 0
	GNUM=SCFGTN(TSPT,GIND)
	IF(GNUM.LT.1) THEN
	  TYPE*,'Sorry, Sports ',GIND,' game not active '
	  CALL XWAIT(2,2,ST)
	  RETURN
	ENDIF
C
C
	CALL OPENW(1,SCFSFN(1,DAF),4,0,0,ST)
	CALL IOINIT(DFDB,1,DAFSEC*256)
	IF(ST.NE.0) THEN
	  CALL FILERR(SCFSFN(1,DAF),1,ST,0)
	  RETURN
	ENDIF
C
C
	CALL OPENW(2,SCFGFN(1,GNUM),4,0,0,ST)
	CALL IOINIT(SFDB,2,DSPSEC*256)
	IF(ST.NE.0)THEN
	  CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
	  CALL CLOSEFIL(DFDB)
	  RETURN
	ENDIF
C
C
10	CONTINUE
	CALL CLRSCR(6)
	WRITE(6,900)
	CALL WIMG(6,'Enter function: ')
	READ(6,901) FUN
C
C
	IF(FUN.EQ.'EX') THEN
          IF(VERR .NE. 0) THEN
            TYPE *, IAM()
            TYPE *, IAM(), 'Solve File Configuration Error For Draw: ', DRW
            TYPE *, IAM()
            CALL XWAIT(2, 2, ST)
            GOTO 10
          ENDIF
	  CALL CLOSEFIL(DFDB)
	  CALL CLOSEFIL(SFDB)
	  RETURN
	ENDIF
C
C
	IF(FUN.NE.'LI'.AND.FUN.NE.'MO') THEN
	  CALL CLRSCR(6)
	  TYPE*,'Invalid input '
	  CALL XWAIT(1,2,ST)
	  GOTO 10
	ENDIF
C
C
20	CONTINUE
	TSTFLG = .FALSE.
        TYPE *, ' '
	CALL INPNUM('Enter event number(E-exit) ',DRW,-9999,9999,EXT)
	IF(EXT.LT.0)GOTO 10
	IF(DRW.EQ.0)GOTO 10
	IF(DRW.LT.0) THEN
	   DRW = ABS(DRW)
	   TSTFLG = .TRUE.
	ENDIF
C
C	Get previous text checksum
C
	PREV3 = 0
	IF(DRW.GT.1) THEN
	  CALL READW(SFDB,DRW-1,DSPREC,ST)
	  IF(ST.NE.0) THEN
	    CALL CLRSCR(6)
	    CALL FILERR(SCFGFN(1,GNUM),2,ST,DRW-1)
	    GOTO 20
	  ENDIF
	  CALL ILBYTE(PREV3,DSPREV,2)          !GET PREVIOUS TEXT REV #
	ENDIF
C	
C
	CALL CLRSCR(6)
	CALL READW(SFDB,DRW,DSPREC,ST)
	IF(ST.NE.0) THEN
	  CALL CLRSCR(6)
	  CALL FILERR(SCFGFN(1,GNUM),2,ST,DRW)
	  GOTO 20
	ENDIF
C
C
        VERR = 0
	IF(FUN.EQ.'MO') GOTO 120
C
C
	LSTAT=DSPSTS+1
	DO 30 I=1,7
	BDATE(I+6)=DAY(I)
	EDATE(I+6)=DAY(I)
	DDATE(I+6)=DAY(I)
30	CONTINUE
	BDATE(5)=DSPBSD
	IF(BDATE(5).NE.0)CALL LCDATE(BDATE)
	EDATE(5)=DSPESD
	IF(EDATE(5).NE.0)CALL LCDATE(EDATE)
	DDATE(5)=DSPDAT(CURDRW)
	IF(DDATE(5).NE.0)CALL LCDATE(DDATE)
	IF(DSPBSD .NE. 0) THEN
          CALL GETWEK(DRW, GNUM, WEK, YEAR, ST)
          IF(ST .NE. 0) THEN
            TYPE *, IAM()
            TYPE *, IAM(), 'Error Getting Santa Casa Week Number'
            TYPE *, IAM()
            CALL GSTOP(GEXIT_FATAL)
          ENDIF
        ENDIF
C
C START
C
	DO 70 I=1,SPNMS_LEN/4
	DO 70 J=1,2
	DO 70 K=1,SPGNBR
	IF(DSPNMS(I,J,K).EQ.0)DSPNMS(I,J,K)='    '
70	CONTINUE
C
C DISPLAY GAME DATA
C
	BROW=1
	EROW=DSPMAX
105	CONTINUE
	CALL CLRSCR(6)
	WRITE(6,903) GIND,DRW,WEK,GSTAT(LSTAT),
     *	             DISTIM(DSPTIM)
	WRITE(6,904)(BDATE(I),I=7,13),BDATE(5),(EDATE(I),I=7,13),
     *	            EDATE(5)
	WRITE(6,9041)(DDATE(I),I=7,13),DDATE(5)
	WRITE(6,9042)(DSPEVN(J),J=1,SPEVN_LEN/4)
	WRITE(6,9060)DSPMAX, DFLOAT(DSPPRC)/(SCFPAR(PRFACTOR)*DOLL_BASE)
C	WRITE(6,905)
	BCNT = 0
	IF(DSPFRG.NE.0) BCNT = 1
	DO 110 I=BROW,EROW
	  IF(I.LE.DSPMAX-BCNT) THEN
	    WRITE(6,906) I,(DSPNMS(J,1,I),J=1,SPNMS_LEN/4),
     *                     (DSPNMS(J,2,I),J=1,SPNMS_LEN/4)
	  ELSE
	    WRITE(6,9061) I,(DSPNMS(J,1,I),J=1,SPNMS_LEN/4),
     *                      (DSPNMS(J,2,I),J=1,SPNMS_LEN/4)
	  ENDIF
110	CONTINUE
	WRITE(6,910)DSPDIV,DISPER(DSPSPR),DSPREV
	IF(TSTFLG) THEN
          TYPE*,'***** TEST DATA *****'
           TYPE*
           TYPE*,'DSPMAX: ',DSPMAX
	   TYPE*,'DSPREV: ',DSPREV
           TYPE*
	ENDIF
	GOTO 20
C
C
C MODIFY GAME DATE
C
C
120	CONTINUE
C
C READ DAF AT BEGSAL TO SEE IF DRAW HAS SALES
C
	IF(DSPSTS.GE.GAMOPN) THEN
	   CALL READW(DFDB,DSPBSD,DAFREC,ST)
	   IF(ST.NE.0) THEN
	      CALL FILERR(SCFSFN(1,DAF),2,ST,DSPBSD)
	      GOTO 10
	   ENDIF
C
	   IF(DAFTYP(DOLAMT,TWAG,GNUM).GT.0) THEN
	      CALL CLRSCR(6)
	      CALL BELLS(1)
	      TYPE*,'There has been sales on this event  '
	      CALL WIMG(6,'Are you sure you want to change it? ')
	      CALL YESNO(ANS)
	      IF(ANS.NE.1) GOTO 10
	      CALL CLRSCR(6)
	      CALL PASSWORD(6,PWORD)
	      IF(PASS.NE.GODPAS) THEN
	         TYPE*,'Invalid password entered',BELL
	         CALL XWAIT(2,2,ST)
	         GOTO 10
	      ENDIF
	      TYPE*,'Proceed with care!',BELL
	   ENDIF
	ENDIF
C
C GET EVENT NAME
C
	CALL INPYESNO('Modify event name [Y/N]?', EXT)
	IF (EXT.NE.1) GOTO 150
	    CALL WIMG(6,'Enter Event Name ')
	    READ(6,907)NAME
      	    CLEARED = 0
      	    DO I = 1,SPEVN_LEN/4
      	       IF(NAME(I).EQ.'    ') CLEARED = CLEARED + 1
      	    END DO
      	    IF(CLEARED.EQ.SPEVN_LEN/4) GOTO 150
      	    IF(CNAME(1).EQ.'+') THEN
      	       DO I = 1,SPEVN_LEN/4
      		  NAME(I) = '    '
      	       END DO
      	    ENDIF
      	    CALL FASTMOV(NAME,DSPEVN(1),SPEVN_LEN/4)
150	CONTINUE
C
C V12 CPH - ASK IF USER WANT TO CONFIG BSD,ESD, DRAW DATE AND BET PRICE
C SE RESPONDE NAO TEM DE GRAVAR COM AS DATAS ACTUAIS
C
	   CALL INPYESNO('DESEJA CONFIGURAR BSD, ESD, DRAW DATE, CLOSE TIME E TICKET BET PRICE [Y/N]?', ANS)
        IF(ANS.NE.1) THEN
C GRAVA BSD		 
		BDATE(5)=DSPBSD
		CALL LCDATE(BDATE)
C GRAVA ESD			
		EDATE(5)=DSPESD
		CALL LCDATE(EDATE)
C GRAVA DRAW DATE
		DDATE(5)=DSPDAT(CURDRW)
		CALL LCDATE(DDATE)
	ELSE
C
C GET BEGINNING AND ENDING DRAW DATES
C
	TYPE*,'Enter begining sales date (E-no change) '
	CALL INPDAT(CDC,EXT)
	BDATE(5)=CDC
	IF(EXT.LT.0) BDATE(5)=DSPBSD
	CALL LCDATE(BDATE)
C
	TYPE*,'Enter ending sales date (E-no change) '
	CALL INPDAT(CDC,EXT)
	EDATE(5)=CDC
	IF(EXT.LT.0) EDATE(5)=DSPESD
	CALL LCDATE(EDATE)
C
	TYPE*,'Enter draw date (E-no change) '
	CALL INPDAT(CDC,EXT)
	DDATE(5)=CDC
	IF(EXT.LT.0) DDATE(5)=DSPDAT(CURDRW)
	CALL LCDATE(DDATE)
C
	TYPE*,'Enter close time for Sports ',GIND,' (E-no change)'
	CALL INPTIM('Enter time HH:MM:SS ',DSPTIM,EXT)
C
        CALL INPNUM('Enter ticket price/bet for single row without decimal point (E-no change) ',
     *		      PRC,1,9999999,EXT)
	IF(EXT.GE.0) DSPPRC=PRC
C
	ENDIF

C 
C GET WINNING DIVISIONS & ROLLING PARAMETERS
C
C       IF(GIND.EQ.1) GOTO 300
C	CALL INPYESNO('Set winning divisions or rolling params? [Y/N]', EXT)
C	IF (EXT.NE.1) GOTO 300
C
C	CALL INPNUM('Enter # of rows (E-no change) ',NROWS,1,SPGNBR,EXT)
C	IF(EXT.GE.0) DSPMAX=NROWS
C
C	SHR_ID=0
C	CALL SPTDIV(DSPREC,SHR_ID)
C
C GET TEAM NAMES
C
C ASK TO REQUEST DEFAULT TEAM NAMES
C
	CALL PRMYESNO('Do you want to use default team names setup [Y/N]? ', ANS)
C
        IF(ANS .EQ. 1) THEN
          DO ROW     = 1, MIN(DSPMAX, SPGNBR)
          DO MTX_IDX = 1, 2
             IF(MTX_IDX .EQ. 1) WRITE(TEAM_NAME, 200) ROW 
             IF(MTX_IDX .EQ. 2) WRITE(TEAM_NAME, 201) ROW 
             CALL FASTMOV(NAME, DSPNMS(1, MTX_IDX, ROW), SPNMS_LEN / 4)
          ENDDO
          ENDDO
          GOTO 500
        ENDIF
C
300	CONTINUE
	CALL INPNUM('Enter row number to be modified (E-Exit) ',
     *	      ROW,1,DSPMAX,EXT)
	IF(EXT.LT.0) GOTO 500
C
C
	CALL WIMG(6,'Enter Team 1 Name ')
	READ(6,907)NAME
        CLEARED = 0
        DO I = 1,SPNMS_LEN/4
           IF(NAME(I).EQ.'    ') CLEARED = CLEARED + 1
        END DO
        IF(CLEARED.EQ.SPNMS_LEN/4) GOTO 210
        IF(CNAME(1).EQ.'+') THEN
           DO I = 1,SPNMS_LEN/4
              NAME(I) = '    '
           END DO
        ENDIF
        CALL FASTMOV(NAME,DSPNMS(1,1,ROW),SPNMS_LEN/4)
210     CONTINUE
C
	CALL WIMG(6,'Enter Team 2 Name ')
	READ(6,907)NAME
        CLEARED = 0
        DO I = 1,SPNMS_LEN/4
           IF(NAME(I).EQ.'    ') CLEARED = CLEARED + 1
        END DO
        IF(CLEARED.EQ.SPNMS_LEN/4) GOTO 57
        IF(CNAME(1).EQ.'+') THEN
           DO I = 1,SPNMS_LEN/4
              NAME(I) = '    '
           END DO
        ENDIF
        CALL FASTMOV(NAME,DSPNMS(1,2,ROW),TNMS_LEN/4)
57      CONTINUE
C
 	GOTO 300
C
C
500	CONTINUE
	CALL INPNUM('Enter Maximum Events Cancelled To Cancel The Draw', DSPMCE, 0, DSPMAX, EXT)
	IF(EXT .LT. 0)GOTO 10
C
C	IF(SCFSTP(GIND).NE.1) THEN
	   DSPSTS=GAMOPN
	   DSPDAT(CURDRW)=DDATE(5)
C	ENDIF
C
	DSPDRW=DRW
	DSPESD=EDATE(5)
	DSPBSD=BDATE(5)
C
C CREATE TABLE OF TEXT MESSAGE TO CHECKSUM FOR REV
C
	BUFIND=1
C
C LOAD UP TEAM NAMES
C
	DO 530 I=1,SPGNBR                       !FOR ALL ROWS
	  DO 520 J=1,2                          !FOR ALL TEAMS
	    CALL MOVBYT(DSPNMS(1,J,I),1,BYTTAB,BUFIND,SPNMS_LEN-2)
	    BUFIND=BUFIND+SPNMS_LEN-2
520	  CONTINUE
530	CONTINUE
	BUFIND=BUFIND-1
	CALL CHECKSUM(BYTTAB,1,BUFIND,REV4)
	CALL ILBYTE(REV1,DSPREV,0)          !GET PREVIOUS TCONTROL REV #
	REV1 = REV1 + 1
	REV2 = MOD(DSPDRW,255)
	CALL ILBYTE(REV3,DSPREV,2)          !GET PREVIOUS TEXT REV #
	REV3 = MOD(PREV3 + REV3,255) + 1
	CALL ISBYTE(REV1,DSPREV,0)          !CONTROL REV BYTE   (SEQUENCE #)
	CALL ISBYTE(REV2,DSPREV,1)          !DRAW REV BYTE
	CALL ISBYTE(REV3,DSPREV,2)          !TEXT REV # BYTE    (SEQUENCE #)
	CALL ISBYTE(REV4,DSPREV,3)          !TEXT CHECKSUM BYTE
	CALL WRITEW(SFDB,DRW,DSPREC,ST)
	IF(ST.NE.0) THEN
	  CALL CLRSCR(6)
	  CALL FILERR(SCFGFN(1,GNUM),3,ST,DRW)
	  GOTO 10
	ENDIF
C
C UPDATE DAF WITH NEW DRAW NUMBERS
C
	DO 1020 I=DSPBSD,DSPESD
	DATE(5)=I
	CALL LCDATE(DATE)
	CALL READW(DFDB,I,DAFREC,ST)
	IF(ST.NE.0) THEN
	  CALL CLRSCR(6)
	  CALL FILERR(SCFSFN(1,DAF),2,ST,I)
	  GOTO 10
	ENDIF
C
C
	IF(DAFSTS.GT.DSOPEN) THEN
	  WRITE(6,908) IAM(), (DATE(K),K=7,13)
	  VERR=VERR+1
	ENDIF
C
C CHECK IF END SALES DATE IT'S GREATER THAN BEGIN SALES DATES
C
        IF(DSPBSD .GT. DSPESD) THEN
          TYPE *, IAM()
          TYPE *, IAM(), 'Error Begin Sales Date Greater Than End Sales Date'
          TYPE *, IAM()
          VERR = VERR + 1
        ENDIF
C
C
	IF(DAFDRW(GNUM).NE.0.AND.DAFDRW(GNUM).NE.DRW) THEN
          TYPE *, IAM()
          TYPE *, IAM(), ' Warning ....'
          TYPE *, IAM()
	  WRITE(6, 909) IAM(), (DATE(K), K = 7, 13), DRW
          TYPE *, IAM()
          CALL WIMG(6,'Do Yo Want Omit This Warning [Y/N] ?') 
	  CALL YESNO(ANS)
	  IF(ANS .NE. 1) VERR = VERR + 1
	ENDIF
1020	CONTINUE
C
C
	IF(VERR .NE. 0) THEN
          TYPE *, IAM()
          WRITE(6, 800) IAM(), VERR, DRW
          TYPE *, IAM()
	  CALL WIMG(6,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	ENDIF
C
C
	DO 1040 I=DSPBSD,DSPESD
	CALL READW(DFDB,I,DAFREC,ST)
	IF(ST.NE.0) THEN
	  CALL FILERR(SCFSFN(1,DAF),2,ST,I)
	  GOTO 10
	ENDIF
	DAFDRW(GNUM)=DRW
	CALL WRITEW(DFDB,I,DAFREC,ST)
	IF(ST.NE.0) THEN
	  CALL FILERR(SCFSFN(1,DAF),3,ST,I)
	  GOTO 10
	ENDIF
1040	CONTINUE
C
C
	 CALL WRITEW(SFDB,DRW,DSPREC,ST)
	 IF(ST.NE.0) THEN
	   CALL FILERR(SCFGFN(1,GNUM),3,ST,DRW)
	   GOTO 10
	 ENDIF
	 GOTO 10
C
C
C
C
C
200     FORMAT('Home', X, I2.2, 9X)
201     FORMAT('Away', X, I2.2, 9X)
C
800     FORMAT(X, A, I2.2, ' game date errors found for Sports event ', I)
900	FORMAT(' LI - List event data',/,
     *	       ' MO - Modify event data',/,
     *	       ' EX - Return to main menu',/)
901	FORMAT(A2)
903	FORMAT(' Sports ',I1,2X,'DRAW ',I4.4,2X,'WEEK ',I2.2,
     *	       2X,'STATUS - ',A12,2X,'CLOSING TIME - ',A8)
904	FORMAT(/,' SALES DATES  ',7A2,'  CDC - ',I4,
     *	               '  < TO >  ',7A2,'  CDC - ',I4)
9041	FORMAT(' DRAW DATE    ',7A2,'  CDC - ',I4)
9042	FORMAT(2X,<SPEVN_LEN/4>A4)
C905	FORMAT(' Row',1X,'Home name',7X,'/Away name')
9051	FORMAT(1X,' # of rows : ',I2,5X,' Price : ',A5)
906	FORMAT(2X,I2.2,1X,<SPNMS_LEN/4>A4,'/',<SPNMS_LEN/4>A4,
     *         1X,'(Sports  row)')
9061	FORMAT(1X,'S',I2.2,1X,<SPNMS_LEN/4>A4,'/',<SPNMS_LEN/4>A4,
     *         1X,'(Super14 row)')
907	FORMAT(4A4)
908	FORMAT(1X,A,7A2,' has an invalid day status ')
909	FORMAT(1X,A, 7A2,' is already active for Sports event # ',I4)
910	FORMAT(/,1X,' # Of divisions :',I2,'  Pool % of sales : ',F6.2,
     *           3X,'Game Revision: ', Z8.8)
930	FORMAT(1X,I2,4X,I2,3X,F6.2,2X,I2,2X,A30)

8997    FORMAT('Fixed prize for div ',I2,' 0 = not fixed')
8998    FORMAT('Rollover from div ',I2,' to div1? [Y/N]')
8999    FORMAT('Enter rolling rule for div ',I2)
9000    FORMAT('Enter division ',I2,' % of pool')
9001    FORMAT('Enter # rows to match for div ',I2)
9060	FORMAT(' Row', 4X, 'Home name', 4X, '/   Away name', 4X
     *         ' # of rows : ',I2,5X,' Price : ',F8.4)

	END
