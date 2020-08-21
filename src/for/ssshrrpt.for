C
C SUBROUTINE SSSHRRPT
C $Log:   GXAFXT:[GOLS]SSSHRRPT.FOV  
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
        SUBROUTINE SSSHRRPT(GNUM,GIND,DRAW,COPY)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DSSREC.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4 NUMROW
        PARAMETER(NUMROW = 10)
C
        REAL*4    WONPER                ! Net Sales Percentage
        REAL*4    WINPER                ! Winning Amount Percentage.
        REAL*4    ROLPER                ! Rolover Percentage.
        INTEGER*4 GNUM                  ! game number
        INTEGER*4 GIND                  ! game index
        INTEGER*4 DRAW                  ! draw number
        INTEGER*4 COPY                  ! number of report copies
C
        INTEGER*4 I,K                 ! Loop variables
        INTEGER*4 ST                    ! status
        INTEGER*4 PAGE                  ! Page Number
        INTEGER*4 FDB(7)                ! file description block
        INTEGER*4 CDC                   ! Results valid for CDC.
        INTEGER*4 WEKNO                 ! Current Week Number.
	INTEGER*4 YEAR			! Year number in 4 digits.
        INTEGER*4 TOTWON                ! Total # of winners.
C
        INTEGER*2 DATE(LDATE_LEN)       ! 

        CHARACTER*47 HEAD               ! Report Header
        CHARACTER*11 REPNAM             ! Report Name
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
        CALL IOINIT(FDB,3,DSSSEC*256)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)

        CALL READW(FDB,DRAW,DSSREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
        CDC = DSSDAT
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
        CALL TITLE(HEAD,'SSWINRP ',GIND,6,PAGE,CDC)
C
        WRITE(6,802) (SCFLGN(I,GNUM),I=1,4),DRAW,WEKNO,YEAR,
     *               (DATE(K),K=7,13)
C
        IF(DSSSTS.LE.GAMBFD) THEN
            WRITE(6,902) (DSSMNM(K),K=1,SSNMS_LEN/4)
            DO I=1,3
              IF(DSSEST(I).EQ.GAMOPN) THEN
                WRITE(6,9022) I,(DSSSNM(K,I),K=1,SSNMS_LEN/4)
              ENDIF
            ENDDO
            WRITE(6,910)
            GOTO 9999
        ENDIF
        IF(DSSSTS.EQ.GAMCAN.OR.DSSSTS.EQ.GAMREF) THEN
            WRITE(6,9020) (DSSMNM(K),K=1,SSNMS_LEN/4)
        ELSE
            WRITE(6,902)  (DSSMNM(K),K=1,SSNMS_LEN/4)
        ENDIF
        WRITE(6,811)   DSSWPA(1,PRWON) 
        DO I=1,3
           IF(DSSEST(I).NE.GAMOPN) GOTO 10
           IF(DSSSTS.EQ.GAMCAN.OR.DSSSTS.EQ.GAMREF) THEN
              WRITE(6,9022) I,(DSSSNM(K,I),K=1,SSNMS_LEN/4)
           ELSE
              WRITE(6,9021) I,(DSSSNM(K,I),K=1,SSNMS_LEN/4),
     *                      DSSWIN(1,I),DSSWIN(2,I)
           ENDIF
        ENDDO
10      CONTINUE
C
        IF(DSSWON.NE.0) THEN
          WRITE(6,904)  DSSODS/100,MOD(DSSODS,100)
        ELSE
          WRITE(6,904)  0,0
        ENDIF
C
C CALCULATE PERCENTAGES AND DISPLAY TOTALS
C
        IF(DSSSAL.GT.0) THEN
           IF(DSSSAL.EQ.DSSREF) THEN
              WINPER = 0.0
              WONPER = 0.0
           ELSE
              WINPER = (FLOAT(DSSWON-DSSREF)/(DSSSAL-DSSREF)) * 100
              WONPER = (FLOAT((DSSWON-DSSREF)-DSSPOL(1))/(DSSSAL-DSSREF)) * 100
           ENDIF
           IF(DSSWON.EQ.0) THEN
              ROLPER = (FLOAT(DSSTPL-DSSREF)/(DSSSAL-DSSREF))*100
           ELSE
              ROLPER = 0.0
           ENDIF
        ELSE
           WINPER = 0.0
           WONPER = 0.0
           ROLPER = 0.0
        ENDIF
C
        WRITE(6,901) DSSWPO(1,PRWON),DSSWRO(1,PRWON),
     *               DSSWPR(1,PRWON),CMONY(DSSSAL,12,BETUNIT),
     *               CMONY(DSSREF,12,BETUNIT),
     *               CMONY(DSSSAL-DSSREF,12,BETUNIT),
     *               CMONY((DSSWON-DSSREF)-DSSPOL(1),12,BETUNIT),WONPER,
     *               CMONY(DSSPOL(1),12,BETUNIT),
     *               CMONY(DSSWON-DSSREF,12,BETUNIT),
     *               CMONY(DSSPOL(2),12,BETUNIT),ROLPER
C
9999    CONTINUE
        CALL USRCLOS1(6)
C
        CALL SPOOL(REPNAM,COPY,ST)
C
        CALL CLOSEFIL(FDB)

C
800     FORMAT('MV',I1,'WINR.REP') 
801     FORMAT(3A4,' VOITTOLASKELMA, PÄÄTTYNEET KOHTEET')
802     FORMAT(//,1X,4A4,2X,'Draw: ',I4,4X,'Kierros ',I2.2,'/',I4,3X,
     *         7A2,//)
804     FORMAT(132(' '))
810     FORMAT('VALVONTARAPORTTI')
811     FORMAT(19X,'Osallistuneet pelit / riviä:',22X,I10,//)
901     FORMAT(//,1X,50X,'Voittorivejä kpl',I12,/,
     *            1X,50X,'Pal.rivejä kpl  ',I12,//,
     *            1X,50x,'Voittoja yht    ',I12,//,
     *            1X,50X,'Vaihto          ',A12,/,
     *            1X,50X,'Palautuksia     ',A12,/,
     *            1X,50X,'Nettovaihto     ',A12,5X,'100.00%',//,
     *            1X,50X,'Voittoihin      ',A12,5X,F6.2,'%'//,
     *            1X,50X,'Lisätty         ',A12,5X,//,
     *            1X,50X,'Jaetaan         ',A12,5X,//,
     *            1X,50X,'Siirtyvä        ',A12,5X,F6.2,'%')
902     FORMAT(/,19X,<SSNMS_LEN/4>A4,//)
9020    FORMAT(/,19X,<SSNMS_LEN/4>A,4X,'*** KOHDE PERUTTU ***',//)
9021    FORMAT(19X,I1,'. ',<SSNMS_LEN/4>A4,22X,I2.2,' - ',I2.2)
9022    FORMAT(19X,I1,'. ',<SSNMS_LEN/4>A4)
903     FORMAT(/,19X,'Tulos   : ',I2,' - ',I2)
904     FORMAT(/,51X,'Kerroin:         ',I8,'.',I2.2)
910     FORMAT(//,19X,'Results not IN')

812     FORMAT(1X,'            ',10X,'Voittoja kpl',/)
813     FORMAT(1X,A10,13X,I10)
814     FORMAT(1X,23X,'            ',/,
     *         1X,'Yhteensä -->',11X,I10,////)
815     FORMAT(10X,'Tuloksen vahvisti               Vantaa    ',
     *         '_____/_____  _____  _______________________________')
        RETURN
        END  

