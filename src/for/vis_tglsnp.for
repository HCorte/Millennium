C SUBROUTINE TGLSNP
C
C V01 02-DEC-2000 UXN Initial release.
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
	SUBROUTINE TGLSNP(NUM,GIND)
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
	INCLUDE 'INCLIB:TGLCOM.DEF'
	INCLUDE 'INCLIB:DTGREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C
        ! arguments
        INTEGER*4  NUM                      !
        INTEGER*4  GIND                     !

        ! variables
	INTEGER*4  SPAD(TGGDIV)             !
	INTEGER*4  SVAL(TGGDIV)             !
	INTEGER*4  SWIN(TGGDIV)             !
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
	INTEGER*4  TGLOT1

        INTEGER*2  WEK
        INTEGER*4  YEAR

	INTEGER*2  SDAT(LDATE_LEN,12)       ! DATES FOR SALES
	INTEGER*2  DRDAT(LDATE_LEN)         ! DRAWING DATE

	REAL*8     TGLOT                    !
        REAL*8     POOL                     !

	CHARACTER*3 DNAME(TGGDIV)           !
	CHARACTER WIN(0:3)
	DATA WIN/'0','1','2','M'/
	
	CHARACTER*28 CSDAT(12)              !
	EQUIVALENCE  (CSDAT(1),SDAT(1,1))
C
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

	IF(GIND.LT.1.OR.GIND.GT.MAXIND) GIND = 1
C
	GNUM = GTNTAB(TTGL,GIND)
	IF(GNUM.LT.1) THEN
	    WRITE(CLIN23,3010) GTNAMES(TTGL),GIND
	    RETURN
	ENDIF

	IF(DRAW.LT.1) DRAW = DAYDRW(GNUM)
	IF(DRAW.EQ.0) DRAW = DAYHDR(GNUM)
	IF(DRAW.EQ.0) DRAW = 1
C
C GET DATA FROM COMMON OR DISK
C
	IF(DRAW.EQ.DAYDRW(GNUM)) THEN
	    CALL GAMLOG(TTGL,GIND,DTGREC,TGLSTS)
	    GOTO 100
	ENDIF
C
C SET FOR NOT ACTIVE GAMES
C
        IF(DRAW .EQ. DAYHDR(GNUM) .AND. DAYDRW(GNUM) .LE. 0) THEN
            CALL GAMLOG(TTGL,GIND,DTGREC,TGLSTS)
            GOTO 100
        ENDIF
C
C
	SMODE=.TRUE.
	CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,1,DTGSEC*256)
	IF(ST.NE.0) THEN
	    WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
	    CALL USRCLOS1(1)
	    RETURN
	ENDIF

	CALL READW(FDB,DRAW,DTGREC,ST)
	IF(ST.NE.0) THEN
	    WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
	    CALL USRCLOS1(1)
	    RETURN
	ENDIF
	CALL USRCLOS1(1)
C
C
100	CONTINUE
	CALL FASTSET(0,SVAL,TGGDIV)
	CALL FASTSET(0,SPAD,TGGDIV)
	CALL FASTSET(0,SWIN,TGGDIV)
	CALL FASTSET(0,SSAL,12)
	TGLOT=0.0D0
	TGLOT1=0
	DO I=1,2
	    TOTWON(I)=0
	    TOTPAD(I)=0
        END DO

	DO I=1,TGGENT
	    IF(I.EQ.1) WRITE (CSDAT(1)(17:26),801)
	    IF(I.EQ.2) WRITE (CSDAT(2)(17:26),802)
	    IF(I.EQ.12) WRITE (CSDAT(12)(17:26),803)
	    IF(I.GT.2.AND.I.LT.12) THEN
	        SDAT(VCDC,I)=DTGESD-(I-3)
	        CALL LCDATE(SDAT(1,I))
	    ENDIF

	    K = I + (DTGDAT(CURDRW)-DTGESD) ! SINCE DTGSAL INDEXED W/ DTGDAT -
					    ! BETTER WOULD BE W/ DTGESD, BUT...
	    IF (I.LE.2) THEN
		SSAL(I) = DTGSAL(I)
	    ELSEIF (I.LE.11 .AND. K.LE.TGGENT) THEN
		SSAL(I) = DTGSAL(K)
	    ELSEIF (K.LT.TGGENT) THEN
		SSAL(12) = SSAL(12)+DTGSAL(K)
            ELSEIF (K.EQ.TGGENT) THEN
                SSAL(2) = SSAL(2) + DTGSAL(K)
	    ELSE
		! FALLTHROUGH
	    ENDIF
        END DO
C
C	SSAL(2)=SSAL(12)
C
C WEEK No.
C
        CALL FIGWEK(DTGESD,WEK,YEAR)
C
C POOL DATA
C
	DO I=1,TGGENT
	   TGLOT=TGLOT+DFLOAT(DTGSAL(I))
	   TGLOT1=TGLOT1+DTGSAL(I)
        END DO
C
	POOL=TGLOT*CALPER(DTGSPR)
	DO I=1,DTGDIV
	    SVAL(I)=IDNINT(POOL*CALPER(DTGPER(I))) + DTGPOL(I) + DTGASH(I)
            IF(I.EQ.1 .AND. DTGPOL(I).NE.0 .AND. GIND.EQ.1)
     *		  SVAL(I) = SVAL(I) + DTGAPL
	    SWIN(I)=0
	    SPAD(I)=0
        END DO
C 
C
C
	IF(DTGSTS.GE.GAMDON) THEN
	    DO I=1,DTGDIV
	        SVAL(I) = DTGSHV(I)
	        SPAD(I) = FRAMT(MAXFRC(GNUM),DTGPAD(I),SVAL(I))
	        SWIN(I) = DTGSHR(I)*SVAL(I)
            END DO
	ENDIF
C
C GET DIVISION NAMES
C
	DO I = 1, TGGDIV
	    DNAME(I)=' - '
	    IF(DTGMAT(I).NE.0) WRITE (DNAME(I),800) DTGMAT(I)
        END DO
C
C
	DO I=1,DTGDIV
	    TOTWON(1) = TOTWON(1) + DTGSHR(I)
	    TOTWON(2) = TOTWON(2) + SWIN(I)
	    TOTPAD(1) = TOTPAD(1) + DTGPAD(I)
	    TOTPAD(2) = TOTPAD(2) + SPAD(I)
        END DO
C
C ENCODE SPORTS GAME SNAPSHOT
C
	DRDAT(VCDC)=DTGDAT(CURDRW)
	CALL LCDATE(DRDAT(1))
	

	DO VAL=0,10
	    IF(DTGSTS.EQ.VAL) THEN
	        WRITE(CLIN1,901) GTNAMES(TTGL),GIND,DRAW
	        WRITE(CLIN2,902) (DTGEVN(K),K=1,4),(DRDAT(K),K=7,13),
     *				      DISTIM(DTGTIM)
C***	        WRITE(CLIN3,9010) STATUS(VAL),DTGSTS
	        WRITE(CLIN3,9010) WEK,YEAR,STATUS(VAL),DTGSTS
	    ENDIF
        END DO

	IF(DTGSTS.LT.GAMDON) THEN
	    WRITE(CLIN4,903)
	ELSE
	    WRITE(CLIN4,9031)
	ENDIF

	LIN=5

	DO I=1,8
	    LIN=LIN+1
	    IF(I.GT.DTGDIV) WRITE(XNEW(  LIN),908)
            IF(I.GT.TGGDIV) THEN
		WRITE(XNEW(LIN),911) (SDAT(K,I),K=9,13),
     *		                      CMONY(SSAL(I),11,BETUNIT)
	    ELSEIF(DNAME(I).EQ.' - ') THEN
		WRITE(XNEW(LIN),911) (SDAT(K,I),K=9,13),
     *		                      CMONY(SSAL(I),11,BETUNIT)
	    ELSE
	        WRITE(XNEW(LIN),904) I,DNAME(I),CMONY(SVAL(I),11,VALUNIT),
     *			             DTGSHR(I),CMONY(SWIN(I),11,VALUNIT),
     *	                             DTGPAD(I),CMONY(SPAD(I),11,VALUNIT),
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
	    WRITE(XNEW(  LIN),906) (SDAT(K,I),K=9,13),
     *			            CMONY(SSAL(I),11,BETUNIT)
        END DO

	LIN=LIN+1
	WRITE(XNEW(  LIN),907) CMONY(TGLOT1,11,BETUNIT)

	LIN=LIN+2
	WRITE(XNEW(LIN),908)
	WRITE(XNEW(LIN+1),908)
	IF(DTGSTS.LT.GAMENV) THEN
	   WRITE(XNEW(  LIN),9042) CMONY(DTGPOL(1)+
     *                                   DTGASH(1)+
     *                                   DTGAPL,
     *                                   11,BETUNIT)
        ELSE
	   WRITE(XNEW(  LIN),9092) ((WIN(DTGWIN(I,K)),I=1,2),K=1,DTGMAX),
     *                             CMONY(DTGPOL(1)+
     *                                   DTGASH(1)+
     *                                   DTGAPL,
     *                                   11,BETUNIT)
        ENDIF
	DO I=2,MIN(4,DTGDIV)		! 4 fits to the page; not more...
	   LIN=LIN+1
	   WRITE(XNEW(  LIN),9043) I,CMONY(DTGPOL(I),11,BETUNIT)
	ENDDO

C
	RETURN

C
800	FORMAT(I2,1X)
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
9031	FORMAT('--_DIV-- ---PRIZE-- ',4('-'),
     *	    'SHARES WON',4('-'),
     *	    1X,3('-'),'FRACTIONS PAID-',1X,9('-'),
     *	  'SALES',8('-'))
904	FORMAT(I1,'(',A3,')',1X,A11,1X,I6,1X,A11,1X,I7,1X,A11,1X,
     *	       5A2,1X,A11)
9041	FORMAT(I1,'(',A3,')',1X,A11,1X,I6,1X,A11,1X,I7,1X,A11)
9042    FORMAT(58X,'Rollover 1 ',A11)
9043    FORMAT(58X,'Rollover',I2,' ',A11)
905	FORMAT('Total ',13X,I6,1X,A11,1X,I7,1X,A11,1X,5A2,1X,A11)
906	FORMAT(58X,5A2,1X,A11)
907	FORMAT(58X,'Total',6X,A11)
908	FORMAT(80(' '))
9092	FORMAT('Winning result: ',<DTGMAX>(A1,':',A1,1X),T59,
     *         'Rollover 1 ',A11)
911	FORMAT(58X,5A2,1X,A11)
C***912	FORMAT(22X,'COUNT',5X,'AMOUNT',4X,'COUNT',5X,'AMOUNT')
3000	FORMAT('Enter !',A8,' game index ')
3010	FORMAT(A8,1X,I1,' game not active')
3020	FORMAT(5A4,' open error ',I4)
3030	FORMAT(5A4,' read error ',I4,' record > ',I4)
	END
