C SUBROUTINE TRSHRRPT
C  
C V02 28-MAY-1999 UXN DTRWIN changed.
C V01 XX-XXX-XXXX XXX INITIAL RELEASE.
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT

        SUBROUTINE TRSHRRPT(GNUM,GIND,DRAW,COPY)

        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DTRREC.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

	REAL*4    WINPER		! Winning Amount Percentage.
        REAL*4    WONPER                ! Won Percentage.
        REAL*4    ROLPER                ! Roll Over Percentage.
        INTEGER*4 GNUM			! game number
        INTEGER*4 GIND			! game index
        INTEGER*4 DRAW			! draw number
        INTEGER*4 COPY			! number of report copies
        INTEGER*4 I,J,K		  	! Loop variables
        INTEGER*4 ST			! status
        INTEGER*4 PAGE	                ! Page Number
        INTEGER*4 FDB(7)		! file description block
	INTEGER*4 CDC			! Results valid for CDC.
	INTEGER*4 WEKNO			! Current Week Number.
	INTEGER*4 YEAR		        ! Year number in 4 digits.
        INTEGER*2 DATE(LDATE_LEN)	! 

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
        CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
        CALL IOINIT(FDB,3,DTRSEC*256)
        IF (ST .NE. 0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
C
        CALL READW(FDB,DRAW,DTRREC,ST)
        IF (ST .NE. 0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
	CDC = DTRDAT
	CALL FIGWEK(CDC-WEEK_OFFSET,WEKNO,YEAR)
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
        CALL TITLE(HEAD,'TRWINRP ',GIND,6,PAGE,CDC)
C
	WRITE(6,802) (SCFLGN(I,GNUM),I=1,4),DRAW,WEKNO,YEAR,
     *               (DATE(K),K=7,13)
C
        IF ((DTRSTS.EQ.GAMCAN).OR.(DTRSTS.EQ.GAMREF)) THEN
          WRITE(6,9201) (DTRMNM(K),K=1,TRPENM_LEN/4)
	  WRITE(6,811)  DTRWPA(1,PRWON)
	  DO I=1,3
	    IF(DTREST(I).NE.GAMNUL) THEN
	       WRITE(6,91221) I,(DTRENM(K,I),K=1,TRPENM_LEN/4)
	    ENDIF
	  ENDDO
	  WRITE(6,9123) 1,0
          GOTO 9998
        END IF
C
        WRITE(6,920)  (DTRMNM(I),I=1,TRPENM_LEN/4)
	WRITE(6,811) DTRWPA(1,PRWON)
C
	IF (DTRSTS.LE.GAMBFD) THEN
	  DO I=1,3
	    IF(DTREST(I).NE.GAMNUL) THEN
	       WRITE(6,91221) I,(DTRENM(K,I),K=1,TRPENM_LEN/4)
	    ENDIF
	  ENDDO
	  WRITE(6,9300)
	  GOTO 9999 
	END IF
C
	DO I=1,DTRCMB
	   DO J=1,3
	     IF(DTREST(J).NE.GAMNUL) THEN
	       WRITE(6,9122) J,(DTRENM(K,J),K=1,TRPENM_LEN/4),
     *                       DTRWIN(J,I),
     *                     (DTRNMS(K,DTRWIN(J,I),J),K=1,TRPNMS_LEN/4)
	     ENDIF
           ENDDO
	   WRITE(6,9123) DTRODS(I)/100,MOD(DTRODS(I),100)
	ENDDO
C
C CALCULATE PERCENTAGES AND DISPLAY TOTALS
C
	IF (DTRSAL(DOLAMT) .GT. 0) THEN
           IF(DTRSAL(DOLAMT) .EQ. DTRREF) THEN
              WINPER = 0.0
	      WONPER = 0.0
           ELSE
 	      WINPER = (FLOAT(DTRWON-DTRREF)/
     *                 (DTRSAL(DOLAMT)-DTRREF))*100
              WONPER = (FLOAT((DTRWON-DTRREF)-DTRPOL(1))
     *                       /(DTRSAL(DOLAMT)-DTRREF))*100
           ENDIF
           IF(DTRWON.EQ.0) THEN
              ROLPER = (FLOAT(DTRTPL-DTRREF)/(DTRSAL(DOLAMT)-DTRREF))*100
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
	WRITE(6,901) DTRWPO(1,PRWON),DTRWRO(1,PRWON),DTRWPR(1,PRWON),
     *               CMONY(DTRSAL(DOLAMT),12,BETUNIT),
     *               CMONY(DTRREF,12,BETUNIT),
     *               CMONY((DTRSAL(DOLAMT)-DTRREF),12,BETUNIT),
     *               CMONY((DTRWON-DTRREF)-DTRPOL(1),12,BETUNIT),WONPER,
     *               CMONY(DTRPOL(1),12,BETUNIT),
     *               CMONY(DTRWON-DTRREF,12,BETUNIT),
     *               CMONY(DTRPOL(2),12,BETUNIT),ROLPER
C
9999 	CONTINUE
C
        CALL USRCLOS1(6)
C
        CALL SPOOL(REPNAM,COPY,ST)
C
        CALL CLOSEFIL(FDB)
C
800     FORMAT('TR',I1,'WINRP.REP') 
801     FORMAT(3A4,' VOITTOLASKELMA, PÄÄTTYNEET KOHTEET')
802	FORMAT(//,1X,4A4,2X,'Draw:',I4,4X,'Kierros:',I2.2,'/',I4.4,4X,
     *         7A2,//)
804	FORMAT(132(' '))
810	FORMAT('VALVONTARAPORTTI')
813	FORMAT(1X,A10,13X,I10)
814	FORMAT(1X,23X,'            ',/,
     *         1X,'Yhteensä -->',11X,I10,////)
815	FORMAT(10X,'Tuloksen vahvisti               Vantaa    ',
     *         '_____/_____  _____  _______________________________')
900	FORMAT('   ',I2,'     ',I3,'.',I2.2,10(' '))
901     FORMAT(//,1X,50X,'Voittorivejä kpl',I12,/,
     *            1X,50X,'Pal. rivejä  kpl',I12,//,
     *            1X,50X,'Voittoja yht.   ',I12,//,
     *            1X,50X,'Vaihto          ',A12,/,
     *            1X,50X,'Palautuksia     ',A12,/,
     *            1X,50X,'Nettovaihto     ',A12,5X,'100.00%',//,
     *            1X,50X,'Voittoihin      ',A12,5X,F6.2,'%'//,
     *            1X,50X,'Lisätty         ',A12,5X,//,
     *            1X,50X,'Jaetaan         ',A12,5X,//,
     *            1X,50X,'Siirtyvä        ',A12,5X,F6.2,'%')
902	FORMAT('   ',I2,21(' '))
903	FORMAT(132A)
920     FORMAT(19X,<TRPENM_LEN/4>A4,//)
9201    FORMAT(19X,<TRPENM_LEN/4>A4,4X,'*** KOHDE PERUTTU ***',//)
811	FORMAT(19X,'Osallistuneet pelit / riviä:    ',18x,I10,//)
9122    FORMAT(19X,I1,'.',<TRPENM_LEN/4>A4,2X,I2.2,2X,<TRPNMS_LEN/4>A4)
91221   FORMAT(19X,I1,'.',<TRPENM_LEN/4>A4)
9123    FORMAT(/,51X,'Kerroin:         ',I8,'.',I2.2,/)
9300	FORMAT(///,19X,'Results Not IN')
9301    FORMAT(//,19X,'---> Kohde peruttu <---')
C
	RETURN
	END

