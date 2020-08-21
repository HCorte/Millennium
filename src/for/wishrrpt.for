C
C SUBROUTINE WISHRRPT
C
C V07 10-FEB-1999 UXN Fix for big odds.
C V06 15-FEB-1994 HXK ADDED BLANK LINES, PUT ODDS ON THEIR OWN LINE.
C V05 03-FEB-1994 HXK ADDED EXTRA DETAILS TO WINRPT.
C V04 30-JAN-1994 HXK ALLOW FOR ENTIRE EVENT TO BE CANCELLED.
C V03 30-JAN-1994 HXK TEMPORARY CHANGE.
C V02 03-JAN-1994 HXK Initial revision.
C V01 17-OCT-1993 GXA Initial revision.
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

        SUBROUTINE WISHRRPT(GNUM,GIND,DRAW,COPY)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DWIREC.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
C
	REAL*4    WINPER		! Winning Amount Percentage.
	REAL*4    WONPER		! Won Percentage.
	REAL*4    ROLPER		! Roll Over Percentage.
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
	INTEGER*4 YEAR			! Year number in 4 digits
	INTEGER*4 TOTWON		! Total # of winners.
C
        INTEGER*2 DATE(LDATE_LEN)	! 

	CHARACTER*47 HEAD		! Report Header
	CHARACTER*40 HEAD2		! Report Header2
	CHARACTER*12 REPNAM		! Report Name
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
        CALL IOINIT(FDB,3,DWISEC*256)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)

        CALL READW(FDB,DRAW,DWIREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
	CDC = DWIDAT
	CALL FIGWEK(CDC-WEEK_OFFSET,WEKNO,YEAR)
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
        CALL TITLE(HEAD,'WIWINRP ',GIND,6,PAGE,CDC)
C
	WRITE(6,802) (SCFLGN(I,GNUM),I=1,4),DRAW,WEKNO,YEAR,
     *               (DATE(K),K=7,13)


        WRITE(6,921) (DWIENM(I),I=1,WENM_LEN/4)
        DO I=1,4
           IF(DWIWIN(I).NE.0) THEN
              WRITE(6,922) DWIWIN(I),(DWINMS(K,DWIWIN(I)),K=1,WNMS_LEN/4)
              WRITE(6,923) DWIODS(I)/100,MOD(DWIODS(I),100)
           ENDIF
        ENDDO
C
C CALCULATE PERCENTAGES AND DISPLAY TOTALS
C
	IF(DWISAL.GT.0) THEN
           IF(DWISAL.EQ.DWIREF) THEN
              WINPER = 0.0
              WONPER = 0.0
           ELSE
 	      WINPER = (FLOAT(DWIWON-DWIREF)/(DWISAL-DWIREF)) * 100
	      WONPER = (FLOAT((DWIWON-DWIREF)-DWIPOL(1))/(DWISAL-DWIREF)) * 100
           ENDIF
           IF(DWIWON.EQ.0) THEN
              ROLPER = (FLOAT(DWITPL-DWIREF)/(DWISAL-DWIREF))*100
           ELSE
              ROLPER = 0.0
           ENDIF
	ELSE
	   WINPER = 0.0
	   WONPER = 0.0
           ROLPER = 0.0
	ENDIF
C
	WRITE(6,901) CMONY(DWISAL,12,BETUNIT),
     *               CMONY(DWIREF,12,BETUNIT),
     *               CMONY(DWISAL-DWIREF,12,BETUNIT),
     *               CMONY((DWIWON-DWIREF)-DWIPOL(1),12,BETUNIT),WONPER,
     *               CMONY(DWIPOL(1),12,BETUNIT),
     *               DWIWPR(1,PRWON),CMONY(DWIWON-DWIREF,12,BETUNIT),
     *               CMONY(DWIPOL(2),12,BETUNIT),ROLPER
C
C============================================================================
C
C SECOND PART OF REPORT (FOR STATE CONTROLLER)
C
	WRITE(HEAD2,810)
	CALL TITLE(HEAD2,'WIWINRP ',GIND,6,PAGE,CDC)
C
	WRITE(6,802) (SCFLGN(I,GNUM),I=1,4),DRAW,WEKNO,YEAR,
     *               (DATE(K),K=7,13)
C
C
	WRITE(6,811) DWIWPA(1,PRWON) 
	WRITE(6,812) 
C
C
	WRITE(6,814) DWIWPR(1,PRWON)
	WRITE(6,815)
C
        CALL USRCLOS1(6)
C
        CALL SPOOL(REPNAM,COPY,ST)
C
        CALL CLOSEFIL(FDB)

C
800     FORMAT('VO',I1,'WINRP.REP') 
801     FORMAT(3A4,' VOITTOLASKELMA, PääTTYNEET KOHTEET')
802	FORMAT(//,1X,4A4,2X,'Draw:',I4,4X,'Kierros:',I2.2,'/',I4.4,4X,
     *         7A2,//)
803	FORMAT(1X,4('Kohde',4X,'Kerroin',10X))
804	FORMAT(132(' '))
810	FORMAT('VALVONTARAPORTTI')
811	FORMAT(1X,'Osallistuneet pelit ',I10,' riviä',/)
C812	FORMAT(1X,'Voittoluokka',10X,'Voittoja kpl',/)
812	FORMAT(1X,'            ',10X,'Voittoja kpl',/)
813	FORMAT(1X,A10,13X,I10)
814	FORMAT(1X,23X,'            ',/,
     *         1X,'Yhteensä -->',11X,I10,////)
815	FORMAT(10X,'Tuloksen vahvisti               Vantaa    ',
     *         '_____/_____  _____  _______________________________')
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
921     FORMAT(19X,'Kohde:      ',<WENM_LEN/4>A4,/)
922     FORMAT(19X,'Voittaja:   ',I2,'. ',<WNMS_LEN>A4,/)
923     FORMAT(19X,'Kerroin:    ',I8,'.',I2.2,//)
	RETURN
	END
