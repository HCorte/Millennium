C SUBROUTINE CPSHRRPT
C
C  V10 18-MAY-1999 UXN DCPWIN changed.
C  V09 04-FEB-1999 UXN Fix for big odds.
C  V08 18-DEC-1996 HXK Update from TEBE project (MXP,WXW,PXN,MJF)
C  V07 28-NOV-1996 WXW Telebetting startup, changes MP/PXN/WXW.
C                      Fix for canceled draws.
C  V07 21-FEB-1996 RXK Fix for last layout change
C  V06 16-FEB-1996 RXK Rfss 96251 Report layout changed 
C  V05 26-JAN-1996 HXK Fix for Row number for Row in second event
C  V04 26-JAN-1996 HXK Fix for scandinavian letters
C  V03 25-JAN-1996 HXK Fixed handling of tied events 
C  V02 23-JAN-1996 HXK Fix for rivia count
C  V01 21-DEC-1995 PXB Initial revision.
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
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT

        SUBROUTINE CPSHRRPT(GNUM,GIND,DRAW,COPY)

        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DCPREC.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

	REAL*4    WINPER		! Winning Amount Percentage.
        REAL*4    WONPER                ! Won Percentage.
        REAL*4    ROLPER                ! Roll Over Percentage.
        INTEGER*4 GNUM			! game number
        INTEGER*4 GIND			! game index
        INTEGER*4 DRAW			! draw number
        INTEGER*4 COPY			! number of report copies
        INTEGER*4 I,K			! Loop variables
        INTEGER*4 ST			! status
        INTEGER*4 PAGE	                ! Page Number
        INTEGER*4 FDB(7)		! file description block
	INTEGER*4 CDC			! Results valid for CDC.
	INTEGER*4 WEKNO			! Current Week Number.
	INTEGER*4 TOTWON		! Total # of winners.
        INTEGER*2 DATE(LDATE_LEN)	! 
	INTEGER*4 YEAR2			!

	CHARACTER*47 HEAD		! Report Header
	CHARACTER*12 REPNAM		! Report Name
C
C GET SYSTEM CONFIGURATION INFO.
C
	CALL GETSCONF(SCFREC,ST)

	IF (ST .NE. 0) THEN
	   TYPE*,IAM(),'Unable to get system configuration info. '
	   CALL GSTOP(GEXIT_FATAL)
	END IF
C
        PAGE = 0
	TOTWON = 0
        CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
        CALL IOINIT(FDB,3,DCPSEC*256)
        IF (ST .NE. 0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
C
        CALL READW(FDB,DRAW,DCPREC,ST)
        IF (ST .NE. 0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
	CDC = DCPDAT
	CALL FIGWEK(CDC-WEEK_OFFSET,WEKNO,YEAR2)
        DATE(5) = CDC
        CALL LCDATE(DATE)
C

        WRITE(REPNAM,800) GIND
        WRITE(HEAD,801) (SCFLGN(I,GNUM),I=1,3)
        CALL ROPEN(REPNAM,6,ST)
        IF (ST .NE. 0) THEN
            TYPE*,IAM(),REPNAM,' Open error  st - ',ST
            CALL USRCLOS1(6)
            RETURN
        END IF
C
        CALL TITLE(HEAD,'PPWINRP ',GIND,6,PAGE,CDC)
C
	WRITE(6,802) (SCFLGN(I,GNUM),I=1,4),DRAW,WEKNO,YEAR2,
     *               (DATE(K),K=7,13)
C
	WRITE(6,811) DCPWPA(1,PRWON)
C
        WRITE(6,921)  (DCPENM(I,1),I=1,CPLENM_LEN/4)
        WRITE(6,9211) (DCPENM(I,2),I=1,CPLENM_LEN/4)
C
        IF ((DCPSTS.EQ.GAMCAN).OR.(DCPSTS.EQ.GAMREF)) THEN
          WRITE(6,9301)
          GOTO 9998
        END IF
C
	IF (DCPCMB .EQ. 0) THEN
	  WRITE(6,9300)
	  GOTO 9999 
	END IF
C
	WRITE(6,9212)
	DO I=1,DCPCMB
           WRITE(6,9122) DCPWIN(1,I),
     *                (DCPNMS(K,DCPWIN(1,I)),K=1,CPLNMS_LEN/4)
           WRITE(6,9123) DCPWIN(2,I)-MAXCPLRW/2,
     *                (DCPNMS(K,DCPWIN(2,I)),K=1,CPLNMS_LEN/4),
     *                DCPODS(I)/100,MOD(DCPODS(I),100)
	ENDDO
C
C CALCULATE PERCENTAGES AND DISPLAY TOTALS
C
	IF (DCPSAL(DOLAMT) .GT. 0) THEN
           IF(DCPSAL(DOLAMT) .EQ. DCPREF) THEN
              WINPER = 0.0
	      WONPER = 0.0
           ELSE
 	      WINPER = (FLOAT(DCPWON-DCPREF)/
     *                 (DCPSAL(DOLAMT)-DCPREF))*100
              WONPER = (FLOAT((DCPWON-DCPREF)-DCPPOL(1))
     *                       /(DCPSAL(DOLAMT)-DCPREF))*100
           ENDIF
           IF(DCPWON.EQ.0) THEN
              ROLPER = (FLOAT(DCPTPL-DCPREF)/(DCPSAL(DOLAMT)-DCPREF))*100
           ELSE
              ROLPER = 0.0
           ENDIF
	ELSE
	   WINPER = 0.0
	   WONPER = 0.0
           ROLPER = 0.0
	END IF
C
9998 	CONTINUE

	WRITE(6,901) CMONY(DCPSAL(DOLAMT),12,BETUNIT),
     *               CMONY(DCPREF,12,BETUNIT),
     *               CMONY((DCPSAL(DOLAMT)-DCPREF),12,BETUNIT),
     *               CMONY((DCPWON-DCPREF)-DCPPOL(1),12,BETUNIT),WONPER,
     *               CMONY(DCPPOL(1),12,BETUNIT),
     *               DCPWPR(1,PRWON),CMONY(DCPWON-DCPREF,12,BETUNIT),
     *               CMONY(DCPPOL(2),12,BETUNIT),ROLPER
C
9999 	CONTINUE
C
        CALL USRCLOS1(6)
C
        CALL SPOOL(REPNAM,COPY,ST)
C
        CALL CLOSEFIL(FDB)
C
800     FORMAT('PP',I1,'WINRP.REP') 
801     FORMAT(3A4,' VOITTOLASKELMA, PÄÄTTYNEET KOHTEET')
802	FORMAT(//,1X,4A4,2X,'Draw:',I4,4X,'Kierros:',I2.2,'/',I4,4X,
     *         7A2,//)
803	FORMAT(1X,4('Kohde',4X,'Kerroin',10X))
804	FORMAT(132(' '))
810	FORMAT('VALVONTARAPORTTI')
811	FORMAT(1X,'Osallistuneet pelit ',I10,' riviä',///)
812	FORMAT(1X,'            ',10X,'Voittoja kpl',/)
813	FORMAT(1X,A10,13X,I10)
814	FORMAT(1X,23X,'            ',/,
     *         1X,'Yhteensä -->',11X,I10,////)
815	FORMAT(10X,'Tuloksen vahvisti               Vantaa    ',
     *         '_____/____  ______  _______________________________')
901     FORMAT(//,1X,50X,'Vaihto      ',A12,/,
     *            1X,50X,'Palautuksia ',A12,/,
     *            1X,50X,'Nettovaihto ',A12,5X,'100.00%',//,
     *            1X,50X,'Voittoihin  ',A12,3X,F8.2,'%'//,
     *            1X,50X,'Lisätty     ',A12,5X,//,
     *            1X,10X,'Voittoja yhteensä ',I10,' kpl',8X,
     *                   'Jaetaan     ',A12,5X,//,
     *            1X,50X,'Siirtyvä    ',A12,3X,F8.2,'%')
902	FORMAT('   ',I2,21(' '))
903	FORMAT(132A)
921     FORMAT(19X,'Kohde A:    ',<CPLENM_LEN/4>A4,/)
9211    FORMAT(19X,'Kohde B:    ',<CPLENM_LEN/4>A4)
9212	FORMAT(/,19X,'Voittajat:',/)
9122    FORMAT(19X,'A:  ',I2,'. ',<CPLNMS_LEN/4>A4)
9123    FORMAT(19X,'B:  ',I2,'. ',<CPLNMS_LEN/4>A4,
     *         5X,'Kerroin:  ',I8,'.',I2.2,/)
9300	FORMAT(///,19X,'Results Not IN')
9301    FORMAT(//,19X,'---> Kohde peruttu <---')
C
	RETURN
	END
