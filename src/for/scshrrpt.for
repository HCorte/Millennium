C
C SUBROUTINE SCSHRRPT
C $Log:   GXAFXT:[GOLS]SCSHRRPT.FOV  
C  
C     Rev 1.0   17 Apr 1996 14:53:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.3   03 Feb 1994 13:20:06   HXK
C  ADDED EXTRA DETAILS TO WINRPT.
C  
C     Rev 1.2   30 Jan 1994 16:30:58   HXK
C  ALLOW FOR ENTIRE EVENT TO BE CANCELLED.
C  
C     Rev 1.1   30 Jan 1994 11:34:30   HXK
C  USE WPA INSTEAD OF WPO.
C  
C     Rev 1.0   03 Jan 1994 19:21:08   HXK
C  Initial revision.
C  
C  
C     Rev 1.0   17 Oct 1993 11:05:52   GXA
C  Initial revision.
C
C
C ** Source - SCSHRRPT.for **
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE SCSHRRPT(GNUM,GIND,DRAW,COPY)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DSCREC.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*4 NUMROW
	PARAMETER(NUMROW = 10)
C
	REAL*4    WONPER		! Net Sales Percentage
	REAL*4    WINPER		! Winning Amount Percentage.
 	REAL*4    ROLPER		! Rolover Percentage.
        INTEGER*4 GNUM			! game number
        INTEGER*4 GIND			! game index
        INTEGER*4 DRAW			! draw number
        INTEGER*4 COPY			! number of report copies
C
        INTEGER*4 I,K			! Loop variables
        INTEGER*4 ST			! status
        INTEGER*4 PAGE	                ! Page Number
        INTEGER*4 FDB(7)		! file description block
	INTEGER*4 CDC			! Results valid for CDC.
	INTEGER*4 WEKNO			! Current Week Number.
	INTEGER*4 YEAR		        ! Year number in 4 digits.
	INTEGER*4 TOTWON		! Total # of winners.
C
        INTEGER*2 DATE(LDATE_LEN)	! 

	CHARACTER*47 HEAD		! Report Header
	CHARACTER*40 HEAD2		! Report Header2
	CHARACTER*11 REPNAM		! Report Name
C
C GET SYSTEM CONFIGURATION INFO.
C
	CALL GETSCONF(SCFREC,ST)
	IF(ST.NE.0) THEN
	   TYPE*,IAM(),'Unable to get system configuration info. '
	   CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
        PAGE = 0
	TOTWON = 0
        CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
        CALL IOINIT(FDB,3,DSCSEC*256)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)

        CALL READW(FDB,DRAW,DSCREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
	CDC = DSCDAT
	CALL FIGWEK(CDC-WEEK_OFFSET,WEKNO,YEAR)   ! week starts on Tuesday
        DATE(5) = CDC
        CALL LCDATE(DATE)
C
C
        WRITE(REPNAM,800) GIND
        WRITE(HEAD,801) (SCFLGN(I,GNUM),I=1,3)
        CALL ROPEN(REPNAM,6,ST)
        IF(ST.NE.0) THEN
            TYPE*,IAM(),REPNAM,' Open error  st - ',ST
            CALL USRCLOS1(6)
            RETURN
        ENDIF
C
        CALL TITLE(HEAD,'SCWINRP ',GIND,6,PAGE,CDC)
C
	WRITE(6,802) (SCFLGN(I,GNUM),I=1,4),DRAW,WEKNO,YEAR,
     *               (DATE(K),K=7,13)
C

        WRITE(6,902) (DSCNM1(K),K=1,SNMS_LEN/4),
     *               (DSCNM2(K),K=1,SNMS_LEN/4)
        WRITE(6,903)  DSCWIN(1),DSCWIN(2)
C
	IF(DSCWON.NE.0) THEN
      	  WRITE(6,904)  DSCODS/100,MOD(DSCODS,100)
	ELSE
      	  WRITE(6,904)  0,0
	ENDIF
C
C CALCULATE PERCENTAGES AND DISPLAY TOTALS
C
	IF(DSCSAL.GT.0) THEN
           IF(DSCSAL.EQ.DSCREF) THEN
              WINPER = 0.0
	      WONPER = 0.0
           ELSE
	      WINPER = (FLOAT(DSCWON-DSCREF)/(DSCSAL-DSCREF)) * 100
	      WONPER = (FLOAT((DSCWON-DSCREF)-DSCPOL(1))/(DSCSAL-DSCREF)) * 100
           ENDIF
	   IF(DSCWON.EQ.0) THEN
	      ROLPER = (FLOAT(DSCTPL-DSCREF)/(DSCSAL-DSCREF))*100
	   ELSE
	      ROLPER = 0.0
	   ENDIF
	ELSE
	   WINPER = 0.0
	   WONPER = 0.0
	   ROLPER = 0.0
	ENDIF
C
	WRITE(6,901) CMONY(DSCSAL,12,BETUNIT),
     *               CMONY(DSCREF,12,BETUNIT),
     *               CMONY(DSCSAL-DSCREF,12,BETUNIT),
     *               CMONY((DSCWON-DSCREF)-DSCPOL(1),12,BETUNIT),WONPER,
     *               CMONY(DSCPOL(1),12,BETUNIT),
     *               DSCWPR(1,PRWON),CMONY(DSCWON-DSCREF,12,BETUNIT),
     *               CMONY(DSCPOL(2),12,BETUNIT),ROLPER
C
C============================================================================
C
C SECOND PART OF REPORT (FOR STATE CONTROLLER)
C
	WRITE(HEAD2,810)
	CALL TITLE(HEAD2,'SCWINRP ',GIND,6,PAGE,CDC)
C
	WRITE(6,802) (SCFLGN(I,GNUM),I=1,4),DRAW,WEKNO,YEAR,
     *               (DATE(K),K=7,13)
C
C
	WRITE(6,811) DSCWPA(1,PRWON) 
	WRITE(6,812) 
C
C
	WRITE(6,814) DSCWPR(1,PRWON)
	WRITE(6,815)
C
        CALL USRCLOS1(6)
C
        CALL SPOOL(REPNAM,COPY,ST)
C
        CALL CLOSEFIL(FDB)

C
800     FORMAT('TU',I1,'WINR.REP') 
801     FORMAT(3A4,' VOITTOLASKELMA, PÄÄTTYNEET KOHTEET')
802	FORMAT(//,1X,4A4,2X,'Draw: ',I4,4X,'Kierros ',I2.2,'/',I4,4X,
     *         7A2,//)
804	FORMAT(132(' '))
810	FORMAT('VALVONTARAPORTTI')
811	FORMAT(1X,'Osallistuneet pelit ',I10,' riviä',/)
C812	FORMAT(1X,'Voittoluokka',10X,'Voittoja kpl',/)
812     FORMAT(1X,'            ',10X,'Voittoja kpl',/)
813	FORMAT(1X,A10,13X,I10)
814	FORMAT(1X,23X,'            ',/,
     *         1X,'Yhteensä -->',11X,I10,////)
815	FORMAT(10X,'Tuloksen vahvisti               Vantaa    ',
     *         '_____/_____  _____  _______________________________')
901	FORMAT(//,1X,50X,'Vaihto      ',A12,/,
     *            1X,50X,'Palautuksia ',A12,/,
     *            1X,50X,'Nettovaihto ',A12,5X,'100.00%',//,
     *            1X,50X,'Voittoihin  ',A12,5X,F6.2,'%'//,
     *            1X,50X,'Lisätty     ',A12,5X,//,
     *            1X,10X,'Voittoja yhteensä ',I10,' kpl',8X,
     *                   'Jaetaan     ',A12,5X,//,
     *            1X,50X,'Siirtyvä    ',A12,5X,F6.2,'%')
902     FORMAT(/,19X,'Kohde:    ',3X,<SNMS_LEN/4>A4,' - ',<SNMS_LEN/4>A4)
903     FORMAT(/,19X,'Tulos:    ',3X,I2,' - ',I2)
904     FORMAT(/,19X,'Kerroin:  ',I7,'.',I2.2)
	RETURN
	END  
