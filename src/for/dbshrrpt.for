C SUBROUTINE DBSHRRPT
C
C V08 05-APR-2000 OXK Layout fix
C V07 18-MAY-1999 UXN DDBWIN changed.
C V06 04-FEB-1999 UXN Fix for big odds.
C V05 18-DEC-1996 HXK Update from TEBE project (MXP,WXW,PXN,MJF)
C V04 28-NOV-1996 WXW Telebetting startup, changes MP/PXN/WXW.
C                     Fix for canceled draws.
C V03 26-JAN-1996 HXK Fix for scandinavian letters
C V02 23-JAN-1996 HXK Fix for rivia count
C V01 21-DEC-1995 PXB Initial revision.
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
C
        SUBROUTINE DBSHRRPT(GNUM,GIND,DRAW,COPY)
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DDBREC.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	REAL*4    WINPER		! Winning Amount Percentage.
	REAL*4    WONPER		! Won Percentage.
	REAL*4    ROLPER		! Roll Over Percentage.
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
	INTEGER*4 YEAR2                 ! Current year number.
	INTEGER*4 TOTWON		! Total # of winners.
        INTEGER*2 DATE(LDATE_LEN)	! 
C
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
        CALL IOINIT(FDB,3,DDBSEC*256)
        IF (ST .NE. 0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
C
        CALL READW(FDB,DRAW,DDBREC,ST)
        IF (ST .NE. 0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
	CDC = DDBDAT
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
        CALL TITLE(HEAD,'SKWINRP ',GIND,6,PAGE,CDC)
C
	WRITE(6,802) (SCFLGN(I,GNUM),I=1,4),DRAW,WEKNO,YEAR2,
     *               (DATE(K),K=7,13)
C
	WRITE(6,811) DDBWPA(1,PRWON)
C
        WRITE(6,921) (DDBENM(I),I=1,DBLENM_LEN/4)
C
	IF ((DDBSTS.EQ.GAMCAN).OR.(DDBSTS.EQ.GAMREF)) THEN
	  WRITE (6,9301)
	  GOTO 9998
	END IF
C
	IF (DDBCMB .EQ. 0) THEN
	  WRITE (6,9300)
	  GOTO 9999
	END IF
C
	DO I=1,DDBCMB
          WRITE(6,9221) DDBWIN(1,I),(DDBNMS(K,DDBWIN(1,I)),K=1,4)
          WRITE(6,9222) DDBWIN(2,I),(DDBNMS(K,DDBWIN(2,I)),K=1,4)
          WRITE(6,9223) DDBODS(I)/100,MOD(DDBODS(I),100)
	ENDDO
C
C CALCULATE PERCENTAGES AND DISPLAY TOTALS
C
	IF (DDBSAL(DOLAMT) .GT. 0) THEN
           IF(DDBSAL(DOLAMT) .EQ. DDBREF) THEN
              WINPER = 0.0
              WONPER = 0.0
           ELSE
 	      WINPER = (FLOAT(DDBWON-DDBREF)
     *                      /(DDBSAL(DOLAMT)-DDBREF))*100
 	      WONPER = (FLOAT((DDBWON-DDBREF)-DDBPOL(1))
     *                       /(DDBSAL(DOLAMT)-DDBREF))*100
           ENDIF
           IF(DDBWON.EQ.0) THEN
              ROLPER = (FLOAT(DDBTPL-DDBREF)/(DDBSAL(DOLAMT)-DDBREF))*100
           ELSE
              ROLPER = 0.0
           ENDIF
	ELSE
	   WINPER = 0.0
	   WONPER = 0.0
           ROLPER = 0.0
	END IF
C
9998	CONTINUE

	WRITE(6,901) CMONY(DDBSAL(DOLAMT),12,BETUNIT),
     *               CMONY(DDBREF,12,BETUNIT),
     *               CMONY((DDBSAL(DOLAMT)-DDBREF),12,BETUNIT),
     *               CMONY((DDBWON-DDBREF)-DDBPOL(1),12,BETUNIT),WONPER,
     *               CMONY(DDBPOL(1),12,BETUNIT),
     *               DDBWPR(1,PRWON),CMONY(DDBWON-DDBREF,12,BETUNIT),
     *               CMONY(DDBPOL(2),12,BETUNIT),ROLPER
C
9999	CONTINUE
C
        CALL USRCLOS1(6)
C
        CALL SPOOL(REPNAM,COPY,ST)
C
        CALL CLOSEFIL(FDB)
C
800     FORMAT('SK',I1,'WINRP.REP') 
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
     *         '_____/_____ ______  _______________________________')
901	FORMAT(//,1X,50X,'Vaihto      ',A12,/,
     *            1X,50X,'Palautuksia ',A12,/,
     *            1X,50X,'Nettovaihto ',A12,5X,'100.00%',//,
     *            1X,50X,'Voittoihin  ',A12,3X,F8.2,'%'//,
     *            1X,50X,'Lisätty     ',A12,5X,//,
     *            1X,10X,'Voittoja yhteensä ',I10,' kpl',8X,
     *                   'Jaetaan     ',A12,5X,//,
     *            1X,50X,'Siirtyvä    ',A12,3X,F8.2,'%')
902	FORMAT('   ',I2,21(' '))
903	FORMAT(132A)
921     FORMAT(19X,'Kohde:      ',<DBLENM_LEN/4>A4,/)
9221    FORMAT(19X,'Voittaja:   ',I2,'. ',4A4)
9222    FORMAT(19X,'Toinen:     ',I2,'. ',4A4)
9223    FORMAT(19X,'Kerroin:    ',I8,'.',I2.2,/)
9224    FORMAT(19X,'Voittaja:   ',I2,'. ',4A4,
     *	       ' ',I2,'. ',4A4)
9225    FORMAT(19X,'Toinen:     ',I2,'. ',4A4,
     *	       ' ',I2,'. ',4A4,/)
9226    FORMAT(19X,'Kerroin:    ',I8,'.',I2.2,
     *         14X,I8,'.',I2.2,//)
9227    FORMAT(19X,'Voittaja:   ',I2,'. ',4A4,
     *	       ' ',I2,'. ',4A4,
     *	       ' ',I2,'. ',4A4)
9228    FORMAT(19X,'Toinen:     ',I2,'. ',4A4,
     *	       ' ',I2,'. ',4A4,
     *	       ' ',I2,'. ',4A4,/)
9229    FORMAT(19X,'Kerroin:    ',I8,'.',I2.2,
     *         14X,I8,'.',I2.2,14X,I8,'.',I2.2,//)
9300	FORMAT(///,19X,'Results Not IN')
9301	FORMAT(//,19X,'---> Kohde peruttu <---')
C
	RETURN
	END
