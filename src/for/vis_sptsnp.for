C SUBROUTINE SPOSNP
C
C V22 30-MAR-2015 MTK Modified Super 14 game
C V21 10-OCT-2012 FRP GETWEEK: AAAACCC FORMAT INSTEAD OF YYYYWW FORMAT
C V20 28-JUL-2004 FRP Display DSPPRN.
C V19 04-NOV-2003 FRP Modify for Batch2 Totobola Changes.
C V18 15-DEC-2000 ANG Enable week input
C V17 31-MAY-2000 OXK Display dates from DSPESD, not from DSPDAT(CURDRW)
C V16 24-MAY-2000 OXK New rolling rules for GIND 1
C V15 10-MAR-2000 OXK Added rollovers for div2..div4
C V14 29-FEB-2000 OXK GIND.eq.1 -checking
C V13 22-FEB-2000 OXK Layout fixes (Vakio changes)
C V12 10-JAN-2000 PXO Changed DSPWRF(6) to DSPAPL
C V11 07-JAN-2000 UXN WIN(4) changed to WIN(0:4)
C V10 18-OCT-1999 UXN OFFLINE sales field removed. RFSS #92
C V09 19-MAY-1996 HXK Wojtek added round to snapshot
C V08 14-SEP-1995 HXK Added Rollover to lower part of Snapshot, do not show 
C                     Rollover in grand total
C V07 16-AUG-1995 HXK Added Rollover field
C V06 12-OCT-1993 HXK Changed TIER to DIV.
C V05 06-JUL-1993 SXH Released for Finland
C V04 13-JUN-1993 HXK added AGTINF.DEF, PRMAGT.DEF
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 25-OCT-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE SPTSNP(NUM,GIND,CLINE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C
        ! arguments
        INTEGER*4  NUM                      !
        INTEGER*4  GIND                     !

        ! variables
	INTEGER*4  SPAD(SPGDIV)             !
	INTEGER*4  SVAL(SPGDIV)             !
	INTEGER*4  SWIN(SPGDIV)             !
	INTEGER*4  FDB(7)                   !
	INTEGER*4  TOTWON(2)                !
	INTEGER*4  TOTPAD(2)                !
	INTEGER*4  SSAL(12)                 !
	INTEGER*4  GNUM                     !
	INTEGER*4  DRAW                     !
	INTEGER*4  LIN                      !
	INTEGER*4  ST                       !
	INTEGER*4  I                        !
	INTEGER*4  K                        !
	INTEGER*4  VAL                      !
	INTEGER*4  SPTOT1
	INTEGER*4  WEEK,DAT,CLINE(20)
	INTEGER*4  GETDRW                   !FUNCTION
	INTEGER*4  SCORE(2)
	INTEGER*4  ROLL
	INTEGER*4  BCNT
        INTEGER*2  WEK
        INTEGER*4  YEAR

	INTEGER*2  SDAT(LDATE_LEN,12)       ! DATES FOR SALES
	INTEGER*2  DRDAT(LDATE_LEN)         ! DRAWING DATE

	REAL*8     SPTOT                    !
        REAL*8     POOL                     !

        CHARACTER * 32 EVENT_CANCEL_MESSAGE
	CHARACTER*3 DNAME(SPGDIV)           !
	CHARACTER WIN(0:4)                  !
	CHARACTER RWIN(0:4)                 !
	CHARACTER*28 CSDAT(12)              !
	EQUIVALENCE  (CSDAT(1),SDAT(1,1))
C
	DATA WIN/'-','1','X','C','2'/
	DATA RWIN/'-','0','1','C','M'/
	CHARACTER*32 STATUS(0:10)           !

        ! function
        INTEGER*4  FRAMT                    !

	DATA STATUS/'** Game not initialized **      ',
     *	            '** Not a drawing day **         ',
     *	            '** Game info. entered **        ',
     *	            '** Game is open **              ',
     *	            '** End of game/before drawing **',
     *	            '** Winning numbers entered **   ',
     *	            '** Winning numbers verified **  ',
     *	            '** Drawing completed **         ',
     *	            '** Results are final **         ',
     *	            '** Game or row cancelled **     ',
     *	            '** Game or row refund enabled **'/
C
C
	SMODE = .FALSE.
	DRAW = NUM
	IF(GIND.LT.1.OR.GIND.GT.MAXIND) THEN
	    WRITE(CLIN23,3000) GTNAMES(TSPT)
	    RETURN
	ENDIF
C
C
	GNUM = GTNTAB(TSPT,GIND)
	IF(GNUM.LT.1) THEN
	    WRITE(CLIN23,3010) GTNAMES(TSPT),GIND
	    RETURN
	ENDIF

	CALL GETWEEK(CLINE,DAT)
        IF (DAT.GT.0) THEN
            WEEK = MOD(DAT,1000)
            YEAR = INT(DAT/1000)
            DRAW = GETDRW(YEAR,WEEK,GNUM)
            IF (DRAW.LE.0) THEN
                WRITE(CLIN23,3040)
                RETURN
             ENDIF
        ENDIF

	IF(DRAW.LT.1) DRAW = DAYDRW(GNUM)
	IF(DRAW.EQ.0) DRAW = DAYHDR(GNUM)
	IF(DRAW.EQ.0) DRAW = 1
C
C GET DATA FROM COMMON OR DISK
C
	IF(DRAW .EQ. DAYDRW(GNUM) .AND. DAYSTS .NE. DSCLOS) THEN
	    CALL GAMLOG(TSPT,GIND,DSPREC,SPTSTS)
	    GOTO 100
	ENDIF
C
C
C SET FOR NOT ACTIVE GAMES
C
        IF(DRAW .EQ. DAYHDR(GNUM) .AND. DAYDRW(GNUM) .LE. 0 .AND. DAYSTS .NE. DSCLOS) THEN
            CALL GAMLOG(TSPT,GIND,DSPREC,SPTSTS)
            GOTO 100
        ENDIF
C
C
	SMODE=.TRUE.
	CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,1,DSPSEC*256)
	IF(ST.NE.0) THEN
	    WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
	    CALL USRCLOS1(1)
	    RETURN
	ENDIF

	CALL READW(FDB,DRAW,DSPREC,ST)
	IF(ST.NE.0) THEN
	    WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
	    CALL USRCLOS1(1)
	    RETURN
	ENDIF
	CALL USRCLOS1(1)
C
C
100	CONTINUE
	CALL FASTSET(0,SVAL,SPGDIV)
	CALL FASTSET(0,SPAD,SPGDIV)
	CALL FASTSET(0,SWIN,SPGDIV)
	CALL FASTSET(0,SSAL,12)
	SPTOT=0.0D0
	SPTOT1=0
	DO I=1,2
	    TOTWON(I)=0
	    TOTPAD(I)=0
        END DO

	DO I=1,SPGENT
	    IF(I.EQ.1) WRITE (CSDAT(1)(17:26),801)
	    IF(I.EQ.2) WRITE (CSDAT(2)(17:26),802)
            IF(I.EQ.12) WRITE (CSDAT(12)(17:26),803)
	    IF(I.GT.2.AND.I.LT.12) THEN
	        SDAT(VCDC,I)=DSPESD-(I-3)
	        CALL LCDATE(SDAT(1,I))
	    ENDIF

	    K = I + (DSPDAT(CURDRW)-DSPESD) ! SINCE DSPSAL INDEXED W/ DSPDAT -
					    ! BETTER WOULD BE W/ DSPESD, BUT...
	    IF (I.LE.2) THEN
		SSAL(I) = DSPSAL(I)
	    ELSEIF (I.LE.11 .AND. K.LE.SPGENT) THEN
		SSAL(I) = DSPSAL(K)
	    ELSEIF (K.LT.SPGENT) THEN
		SSAL(12) = SSAL(12)+DSPSAL(K)
	    ELSEIF(K .EQ. SPGENT) THEN
                SSAL(2) = SSAL(2) + DSPSAL(K)
            ELSE
		! FALLTHROUGH
	    ENDIF
        END DO
C
C	SSAL(2)=SSAL(12)
C
C WEEK No.
C
        CALL FIGWEK(DSPBSD,WEK,YEAR)
C
C POOL DATA
C
	DO I=1,SPGENT
	   SPTOT=SPTOT+DFLOAT(DSPSAL(I))
	   SPTOT1=SPTOT1+DSPSAL(I)
        END DO
C
	POOL=SPTOT*CALPER(DSPSPR)
	DO I=1,DSPDIV
	    SVAL(I)=IDNINT(POOL*CALPER(DSPPER(I))) + DSPPOL(I) + DSPASH(I)
            IF(I.EQ.1 .AND. DSPPOL(I).NE.0 .AND. GIND.EQ.1)
     *		  SVAL(I) = SVAL(I) + DSPAPL
	    SWIN(I)=0
	    SPAD(I)=0
        END DO
C 
C
C
	IF(DSPSTS.GE.GAMDON) THEN
	    DO I=1,DSPDIV
	        SVAL(I) = DSPSHV(I)
	        SPAD(I) = FRAMT(MAXFRC(GNUM),DSPPAD(I),SVAL(I))
	        SWIN(I) = DSPSHR(I)*SVAL(I)
            END DO
	ENDIF
C
C GET DIVISION NAMES
C
	DO I = 1, SPGDIV
	    DNAME(I)=' - '
	    IF(DSPMAT(I).NE.0) WRITE (DNAME(I),800) DSPMAT(I)
	    IF(I.EQ.1 .AND. DSPFRG.GT.0) WRITE(DNAME(I),8001) DSPMAT(I) !SUPER14
            IF(I.EQ.DSPRWD .AND. DSPRWD. NE. 0)  WRITE(DNAME(I),8002)   ! REFUND
        END DO
C
C
	DO I=1,DSPDIV
	    TOTWON(1) = TOTWON(1) + DSPSHR(I)
	    TOTWON(2) = TOTWON(2) + SWIN(I)
	    TOTPAD(1) = TOTPAD(1) + DSPPAD(I)
	    TOTPAD(2) = TOTPAD(2) + SPAD(I)
        END DO
C
C ENCODE SPORTS GAME SNAPSHOT
C
	DRDAT(VCDC)=DSPDAT(CURDRW)
	CALL LCDATE(DRDAT(1))
	

	DO VAL=0,10
	    IF(DSPSTS.EQ.VAL) THEN
	        WRITE(CLIN1,901) GTNAMES(TSPT),GIND,DRAW
	        WRITE(CLIN2,902) (DSPEVN(K),K=1,4),(DRDAT(K),K=7,13),
     *				      DISTIM(DSPTIM)
C***	        WRITE(CLIN3,9010) STATUS(VAL),DSPSTS
	        WRITE(CLIN3,9010) WEK,YEAR,STATUS(VAL),DSPSTS
	    ENDIF
        END DO

	IF(DSPSTS.LT.GAMDON) THEN
	    WRITE(CLIN4,903)
	ELSE
	    WRITE(CLIN4,9031)
	ENDIF

	LIN=5

	DO I=1,SPGDIV
	    LIN=LIN+1
	    IF(I.GT.DSPDIV) WRITE(XNEW(  LIN),908)
	    IF(DNAME(I).EQ.' - ') THEN
		WRITE(XNEW(LIN),911) (SDAT(K,I),K=9,13),
     *		                      CMONY(SSAL(I),11,BETUNIT)
	    ELSE
	        WRITE(XNEW(LIN),904) I,DNAME(I),CMONY(SVAL(I),11,VALUNIT),
     *			             DSPSHR(I),CMONY(SWIN(I),11,VALUNIT),
     *	                             DSPPAD(I),CMONY(SPAD(I),11,VALUNIT),
     *	                           (SDAT(K,I),K=9,13),
     *				    CMONY(SSAL(I),11,BETUNIT)
	   ENDIF
        END DO

	LIN=LIN+1
	WRITE(XNEW(  LIN),905) TOTWON(1),CMONY(TOTWON(2),11,VALUNIT),
     *	                       TOTPAD(1),CMONY(TOTPAD(2),11,VALUNIT),
     *	                       (SDAT(K,9),K=9,13),
     *			       CMONY(SSAL(9),11,BETUNIT)

	DO I=10,12
	    LIN=LIN+1
	    IF(I.EQ.12 .AND. DSPFRG.GT.0) THEN  !SUPER14
	      WRITE(XNEW(  LIN),9061) DSPPRN,(SDAT(K,I),K=9,13),
     *			              CMONY(SSAL(I),11,BETUNIT)
	    ELSE
	      WRITE(XNEW(  LIN),906) (SDAT(K,I),K=9,13),
     *			              CMONY(SSAL(I),11,BETUNIT)
	    ENDIF
        END DO

	LIN=LIN+1
	WRITE(XNEW(  LIN),907) CMONY(SPTOT1,11,BETUNIT)
	LIN=LIN+1
	WRITE(XNEW(LIN),908)
	WRITE(XNEW(LIN+1),908)
	ROLL=2
	IF(DSPSTS.LT.GAMENV) THEN
	   WRITE(XNEW(  LIN),9042) CMONY(DSPPOL(1)+
     *                                   DSPASH(1)+
     *                                   DSPAPL,
     *                                   11,BETUNIT)
        ELSE
	   BCNT = 0
	   IF(DSPFRG.NE.0) BCNT = 1
           IF(DSPDCD .EQ. 0) THEN
	      WRITE(XNEW(  LIN), 909) (WIN(DSPWIN(K)),K=1,DSPMAX-BCNT),
     *                                CMONY(DSPPOL(1) + DSPASH(1) + DSPAPL, 11, BETUNIT)
           ELSE
              CALL FRMT_CANCEL_EVENT_DATE_TIME(DSPDCD, DSPDAT(CURDRW), EVENT_CANCEL_MESSAGE)
              WRITE(XNEW(LIN), 910) EVENT_CANCEL_MESSAGE, CMONY(DSPPOL(1) + DSPASH(1) + DSPAPL, 11, BETUNIT)
           ENDIF
C              
	   IF(DSPFRG .EQ. 1 .AND. DSPDCD .EQ. 0) THEN  ! SUPER14 (RESULTS) WINNING NUMBERS IF THE DRAW IS NOT CANCELLED
             SCORE(1)=ISHFT(DSPWIN(DSPMAX),-4)         !SUPER14 Home result
             SCORE(2)=IAND(DSPWIN(DSPMAX),'0F'X)       !SUPER14 Away result
	     LIN=LIN+1
	     WRITE(XNEW(  LIN),9044) (RWIN(SCORE(K)),K=1,2),
     *                                CMONY(DSPPOL(2),11,BETUNIT)
	     ROLL=ROLL+1
	   ENDIF
C
           IF(DSPFRG .EQ. 2 .AND. DSPDCD .EQ. 0) THEN  ! SUPER14 (1X2) WINNING NUMBERS IF THE DRAW IS NOT CANCELLED
             LIN=LIN+1
             WRITE(XNEW(  LIN),9045) WIN(DSPWIN(DSPMAX)),
     *                               CMONY(DSPPOL(2),11,BETUNIT)
             ROLL=ROLL+1
           ENDIF
C
        ENDIF
	DO I=ROLL,MIN(4,DSPDIV)		! 4 fits to the page; not more...
	   LIN=LIN+1
	   WRITE(XNEW(  LIN),9043) I,CMONY(DSPPOL(I),11,BETUNIT)
	ENDDO

C
	RETURN

C
800	FORMAT(I2,1X)
8001	FORMAT('S',I2)
8002	FORMAT('REF')
801	FORMAT('Advance   ')
802     FORMAT('Offline   ')
803	FORMAT('Previous  ')
901	FORMAT(A8,1X,I1,' game data for draw ',I4)
902	FORMAT(4A4,T37,'Draw',7A2,2X,A8)
C***9010 FORMAT(36X,A32,2X,I2)
9010    FORMAT('Round 'I2,'/',I4,13X,A32,2X,I2)
903	FORMAT('---DIV-- ---POOL--- ',4('-'),
     *	    'SHARES WON',4('-'),
     *	    1X,3('-'),'FRACTIONS PAID',1('-'),1X,9('-'),
     *	  'SALES',8('-'))
9031	FORMAT('---DIV-- ---PRIZE-- ',4('-'),
     *	    'SHARES WON',4('-'),
     *	    1X,3('-'),'FRACTIONS PAID-',1X,9('-'),
     *	  'SALES',8('-'))
904	FORMAT(I1,'(',A3,')',1X,A11,1X,I6,1X,A11,1X,I7,1X,A11,1X,
     *	       5A2,1X,A11)
9041	FORMAT(I1,'(',A3,')',1X,A11,1X,I6,1X,A11,1X,I7,1X,A11)
9042    FORMAT(58X,'Rollover 1 ',A11)
9043    FORMAT(58X,'Rollover',I2,' ',A11)
9044	FORMAT('Winning result  : ',A1,":",A1,T59,'Rollover 2 ',A11)
9045    FORMAT('Winning S14     : ',A1,T59,'Rollover 2 ',A11)
905	FORMAT('Total ',13X,I6,1X,A11,1X,I7,1X,A11,1X,5A2,1X,A11)
906	FORMAT(58X,5A2,1X,A11)
9061	FORMAT('Week counter    : ',I5,35X,5A2,1X,A11)
907	FORMAT(58X,'Total',6X,A11)
908	FORMAT(80(' '))
909 	FORMAT('Winning row     : ',<DSPMAX-BCNT>(A1,1X),T59,'Rollover 1 ',A11)
910     FORMAT(X, 'The Full Draw Has Been', X, A, T59, 'Rollover 1 ', A11)
911	FORMAT(58X,5A2,1X,A11)
C***912	FORMAT(22X,'COUNT',5X,'AMOUNT',4X,'COUNT',5X,'AMOUNT')
3000	FORMAT('Enter !',A8,' game index ')
3010	FORMAT(A8,1X,I1,' game not active')
3020	FORMAT(5A4,' open error ',I4)
3030	FORMAT(5A4,' read error ',I4,' record > ',I4)
3040    FORMAT('Input error ')
	END
