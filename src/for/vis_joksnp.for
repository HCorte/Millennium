C SUBROUTINE JOKSNP
C
C V13 10-OCT-2012 FRP GETWEEK: AAAACCC FORMAT INSTEAD OF YYYYWW FORMAT
C V12 05-FEB-2001 ANG Enabled year-week input
C V11 06-DEC-2000 ANG Alterated for mixed Fixed/Shared divisions
C V10 31-MAY-2000 PXO Subroutine name from KIKSNP -> JOKSNP
C V09 25-APR-2000 UXN DKKPOL stored in I8
C V08 23-MAR-2000 UXN Fix for ROLLOVER money
C V07 18-OCT-1999 UXN OFFLINE sales field removed. RFSS #92
C V06 05-AUG-1998 RXK Changed to dispaly 2 kickers
C V05 19-MAY-1996 HXK Wojtek added round to snapshot
C V04 06-JUL-1993 SXH Released for Finland
C V03 13-JUN-1993 HXK added AGTINF.DEF, PRMAGT.DEF.
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 01-AUG-1990 XXX RELEASED FOR VAX
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE JOKSNP(NUM,GIND,CLINE)
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
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:DKKREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
        ! arguments
	INTEGER*4  NUM                      !
	INTEGER*4  GIND                     !

        ! variables
	INTEGER*4  KPAD(KIGDIV)             !
	INTEGER*4  KVAL(KIGDIV)             !
	INTEGER*4  KWIN(KIGDIV)             !
	INTEGER*4  FDB(7)                   !
	INTEGER*4  TOTWON(2)                !
	INTEGER*4  TOTPAD(2)                !
	INTEGER*4  KSAL(12)                 !
	INTEGER*4  KSHARE(KIGDIV)           !
        INTEGER*4  KPDTMP(KIGDIV)           !
C
	INTEGER*4  SIND                     !
	INTEGER*4  LIN                      !
	INTEGER*4  FRAMT                    !
	INTEGER*4  J                        !
	INTEGER*4  I                        !
	INTEGER*4  K                        !
	INTEGER*4  ST                       !
	INTEGER*4  GNUM                     !
	INTEGER*4  DRAW                     !
        INTEGER*4  DAT                      !
        INTEGER*4  WEEK,CLINE(20)           !
        INTEGER*4  GETDRW                   ! FUNCTION
C
        INTEGER*2  WEK
        INTEGER*4  YEAR			    !Year in 4 digits
        INTEGER*4  BALOFF
	PARAMETER(BALOFF=17)
C
	INTEGER*2 SDAT(LDATE_LEN,12)        !

        CHARACTER*28 CSDAT(12)
        EQUIVALENCE  (CSDAT(1),SDAT(1,1))
C
	REAL*8    KIKT                      !
        REAL*8    POOL                      !
	REAL*8    ROLLOVER(KIGDIV)


	CHARACTER DNAME(7,3,KIGDIV)         !
	CHARACTER*7 CDNAME(3,KIGDIV)        !
	EQUIVALENCE (CDNAME,DNAME)          !
C
	CHARACTER*8 STATUS
	CHARACTER*1 DTYP(KIGDIV)
C
C
	DRAW=NUM

	IF(GIND.LT.1.OR.GIND.GT.MAXIND) THEN
	    WRITE(CLIN23,3000)
	    RETURN
	ENDIF
C
	GNUM=GTNTAB(TKIK,GIND)
	IF(GNUM.LT.1) THEN
	    WRITE(CLIN23,3010) GIND
	    RETURN
	ENDIF
C
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

	IF(DRAW.LT.1) DRAW=DAYDRW(GNUM)
	IF(DRAW.EQ.0) DRAW=DAYHDR(GNUM)
C
C GET DATA FROM COMMON OR DISK
C
	IF(DRAW.EQ.DAYDRW(GNUM)) THEN
	    CALL GAMLOG(TKIK,GIND,DKKREC,KIKSTS)
	    GOTO 100
	ENDIF
C
	SMODE=.TRUE.
	CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,1,DKKSEC*256)
	IF(ST.NE.0) THEN
	    WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
	    CALL USRCLOS1(     1)
	    RETURN
	ENDIF
C
	CALL READW(FDB,DRAW,DKKREC,ST)
	IF(ST.NE.0) THEN
	    WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
	    CALL USRCLOS1(     1)
	    RETURN
	ENDIF
	CALL USRCLOS1(     1)
C
C
100	CONTINUE
	KIKT=0.0D0
C
        IF(DKKSTS .LE. GAMNUL) THEN
          WRITE(CLIN10, 3050) DRAW
          RETURN
        ENDIF
C
	DO I=1,KIGDIV
	    IF (DKKPER(I).EQ.0) THEN
                DTYP(I) = 'F'  
	    ELSE
		DTYP(I) = ' ' 
	    ENDIF
	ENDDO

	DO I = 1, 2
	    TOTWON(I)=0
	    TOTPAD(I)=0
        END DO

	CALL FASTSET(0,KSHARE,KIGDIV)
	CALL FASTSET(0,KSAL,12)

	DO 120 I = 1, KIGENT
	    IF(I.EQ.1) WRITE (CSDAT(1)(17:26),801)
	    IF(I.EQ.2) WRITE (CSDAT(2)(17:26),802)
	    IF(I.EQ.12) WRITE (CSDAT(12)(17:26),803)
	    IF(I.GT.2.AND.I.LT.12) THEN
	        SDAT(VCDC,I)=DKKDAT(CURDRW)-(I-3)
	        CALL LCDATE(SDAT(1,I))
	    ENDIF

	    DO 120 J=1,MAXGAM
                    IF(I.LT.12) THEN
                      KSAL(I)=KSAL(I)+DKKSAL(I,J)
                    ELSE
                      KSAL(12)=KSAL(12)+DKKSAL(I,J)
                    ENDIF
120	CONTINUE
C
C WEEK No.
C
        CALL FIGWEK(DKKESD,WEK,YEAR)
C
C POOL DATA
C
	  DO I = 1, KIGENT
	    DO J = 1, MAXGAM
	      KIKT = KIKT+DFLOAT(DKKSAL(I,J))
            END DO
          END DO
C
	POOL=KIKT*CALPER(DKKSPR)
	DO I = 1, DKKDIV
	    IF (DKKPER(I).NE.0) THEN
	        ROLLOVER(I) = DFLOAT(DKKPOL(1,I))*DFLOAT(DYN_BETUNIT) + 
     *                        DFLOAT(DKKPOL(2,I))
	        KVAL(I)=IDNINT(POOL*CALPER(DKKPER(I))) + 
     *                  IDINT(ROLLOVER(I)/DFLOAT(DYN_BETUNIT))
	    ELSE
		KVAL(I) = DKKSHV(I)
	    ENDIF 
	    KWIN(I) = 0
	    KPAD(I) = 0
            KSHARE(I) = KSHARE(I)+DKKSHR(I)
        END DO
C
C
C
C
	IF(DKKSTS.GE.GAMDON.OR.DKKSPR.EQ.0) THEN
	    DO I=1,DKKDIV
                   KVAL(I) = DKKSHV(I)
                   KPAD(I) = FRAMT(MAXFRC(GNUM),DKKPAD(I),KVAL(I))
	           KWIN(I) = KSHARE(I)*KVAL(I)
            END DO
	ENDIF
C
C GET DIVISION NAMES
C
	DO I = 1, KIGDIV
	    WRITE (CDNAME(1,I),800)
	    WRITE (CDNAME(2,I),800)
	    WRITE (CDNAME(3,I),800)
        END DO

	DO 170 I = 1, DKKDIV
	    DO 170 J = 1, 3
                WRITE (CDNAME(J,I),804) DKKMAT(J,I)
	        DO 170 K=1,7
	            IF(DNAME(K,J,I).EQ.'0'.OR.DNAME(K,J,I).EQ.'1') 
     *                DNAME(K,J,I)=' '
170	CONTINUE
C
C
	DO I = 1, DKKDIV
	    TOTWON(1) = TOTWON(1) + KSHARE(I)
	    TOTWON(2) = TOTWON(2) + KWIN(I)
            TOTPAD(1) = TOTPAD(1) + DKKPAD(I)
	    TOTPAD(2) = TOTPAD(2) + KPAD(I)
        END DO
C
C ENCODE KICKER GAME SNAPSHOT
C
	STATUS='*closed*'
	IF(DKKSTS.EQ.GAMOPN) STATUS='**open**'
	IF(DKKSTS.EQ.GAMDON) STATUS='**done**'
	IF(DKKSTS.EQ.GFINAL) STATUS='*final* '

	WRITE(CLIN1,901) GIND,DRAW
	WRITE(CLIN2,902) (GLNAMES(K,GNUM),K=1,4)
C***	WRITE(CLIN3,9010) STATUS
	WRITE(CLIN3,9010) WEK,YEAR,STATUS
	IF(DKKSTS.LT.GAMDON.AND.DKKSPR.NE.0) THEN
	    WRITE(CLIN4,903)
	ELSE
	    WRITE(CLIN4,9031)
	ENDIF

	LIN=5

C
	DO I = 1, DKKDIV
            KPDTMP(I) = DKKPAD(I)
	    LIN=LIN+1
            IF(DKKSTS.GT.GAMBFD.OR.DKKSPR.EQ.0) THEN
	      IF(I.EQ.2) THEN ! DON'T PRINT OFFLINE STUFF
                WRITE(XNEW(  LIN),90411) I,DTYP(I),CMONY(KVAL(I),11,BETUNIT),
     *                                  KSHARE(I),CMONY(KWIN(I),11,BETUNIT),
     *                                  KPDTMP(I),CMONY(KPAD(I),11,BETUNIT)
              ELSE
                WRITE(XNEW(  LIN),9041) I,DTYP(I),CMONY(KVAL(I),11,BETUNIT),
     *                                  KSHARE(I),CMONY(KWIN(I),11,BETUNIT),
     *                                  KPDTMP(I),CMONY(KPAD(I),11,BETUNIT),
     *                                  (SDAT(K,I),K=9,13),
     *                                  CMONY(KSAL(I),13,BETUNIT)
	      ENDIF
	    ELSE
	        WRITE(XNEW(  LIN),904) I,DTYP(I),CMONY(KVAL(I),11,BETUNIT),
     *                                 KSHARE(I),CMONY(KWIN(I),11,BETUNIT),
     *	                               KPDTMP(I),CMONY(KPAD(I),11,BETUNIT),
     *	                               (SDAT(K,I),K=9,13),
     *                                 CMONY(KSAL(I),13,BETUNIT)
	    ENDIF
        END DO

	LIN=LIN+1
	SIND=DKKDIV+1
	WRITE(XNEW(  LIN),905) TOTWON(1),CMONY(TOTWON(2),11,BETUNIT),
     *	                       TOTPAD(1),CMONY(TOTPAD(2),11,BETUNIT),
     *	                       (SDAT(K,SIND),K=9,13),
     *                         CMONY(KSAL(SIND),13,BETUNIT)

	LIN=LIN+1
	SIND=SIND+1
	WRITE(XNEW(  LIN),9051) (SDAT(K,SIND),K=9,13),
     *                          CMONY(KSAL(SIND),13,BETUNIT)

	DO I=1,DKKDIV
	    SIND=SIND+1
	    LIN=LIN+1
	    IF(SIND.LT.13) THEN
	        WRITE(XNEW(  LIN),906) I,((DNAME(K,J,I),K=1,7),J=1,3),
     *	                               (SDAT(K,SIND),K=9,13),
     *                                 CMONY(KSAL(SIND),13,BETUNIT)
	    ENDIF

	    IF(SIND.EQ.13) THEN
	        WRITE(XNEW(  LIN),907) I,((DNAME(K,J,I),K=1,7),J=1,3),
     *                                 CMONY(IDINT(KIKT),13,BETUNIT)
	    ENDIF

	    IF(SIND.GT.13) THEN
	        WRITE(XNEW(  LIN),9071) I,((DNAME(K,J,I),K=1,7),J=1,3)
	    ENDIF
        END DO

	LIN=LIN+1
	WRITE(XNEW(  LIN),909) ROLLOVER(1)/100.0           !/100.0
	LIN=LIN+1
	IF(DKKSTS.LT.GAMENV) THEN 
	      WRITE(XNEW(  LIN),9091) ROLLOVER(2)/100.0    !/100.0
        ELSE
	      WRITE(XNEW(  LIN),9081) DKKWIN,
     *                                ROLLOVER(2)/100.0    !/100.0
        ENDIF
	LIN=LIN+1
	WRITE(XNEW(  LIN),9092) ROLLOVER(3)/100.0          !/100.0
	RETURN

800	FORMAT('       ')
801	FORMAT('Advance   ')
802	FORMAT('Offline   ')
803	FORMAT('Previous  ')
804	FORMAT(I7.7)
901	FORMAT('Joker ',I1,8X,' game data for draw ',I4)
902	FORMAT(4A4)
C***9010 FORMAT(25X,' pools are ',A8)
9010	FORMAT('Round 'I2,'/',I4,13X,' pools are ',A8)
903	FORMAT('DIV ---POOL---- ',4('-'),'SHARES WON',4('-'),
     *	    1X,3('-'),'FRACTIONS PAID',2('-'),1X,7('-'),'SALES',9('-'))
9031	FORMAT('DIV ---PRIZE--- ',4('-'),'SHARES WON',4('-'),
     *	    1X,3('-'),'FRACTIONS PAID',2('-'),1X,7('-'),'SALES',9('-'))
9032    FORMAT(55(' '),'Old advcd',2x,A10)
90321    FORMAT(55(' '),'Old advcd',3x,'*Not set*')
904	FORMAT(I2,1X,A1,A11,1X,I6,1X,A11,1X,I7,1X,A11,1X,
     *	       5A2,1X,A13)
9041	FORMAT(I2,1X,A1,A11,1X,I6,1X,A11,1X,I7,1X,A11,1X,
     *	       5A2,1X,A13)
90411	FORMAT(I2,1X,A1,A11,1X,I6,1X,A11,1X,I7,1X,A11)
905	FORMAT('         Total  ',I6,1X,A11,1X,I7,1X,A11,1X,5A2,1X,A13)
9051	FORMAT('                ',39X,5A2,1X,A13)
906	FORMAT(I2,'  (',7A1,'/',7A1,'/',7A1,')',26X,5A2,1X,A13)
907	FORMAT(I2,'  (',7A1,'/',7A1,'/',7A1,')',26X,'Total',6X,A13)
9071	FORMAT(I2,'  (',7A1,'/',7A1,'/',7A1,')')
908	FORMAT(80(' '))
9081	FORMAT('Winning number : ',I7.7,28(' '),3X,'Rollover 2',2x,F12.2)
909     FORMAT(52(' '),3X,'Rollover 1',2x,F12.2)
9091    FORMAT(52(' '),3X,'Rollover 2',2x,F12.2)
9092    FORMAT(52(' '),3X,'Rollover 3',2x,F12.2)
9093    FORMAT(' *** Cannot display page 2 ***')
3000	FORMAT('Enter !joker game index ')
3010	FORMAT('Joker ',I1,' game not active')
3020	FORMAT(5A4,' open error ',I4)
3030	FORMAT(5A4,' read error ',I4,' record > ',I4)
3040    FORMAT('Input error ')
3050    FORMAT(10X, 'The Draw Is Not Active In The System, Draw:', X, I)
	END
