C
C SUBROUTINE TSHRRPT
C
C V09 15-FEB-1999 UXN Fix for DTSSAL .EQ. DTSREF
C V08 27-NOV-1997 UXN Changes for PITKA (singles,doubles added)
C V07 04-SEP-1996 RXK Rfss 296. Total number of winners cleared 
C V06 26-JAN-1996 HXK Reorder results
C V05 26-OCT-1993 GXA Add refunds by division to total # of winners by division.
C V04 18-OCT-1993 GXA Hardcoded sales % to 100% (don't ask why....). 
C                     Added # rows played.
C V03 18-OCT-1993 GXA Added second report (broken down by division).
C V02 17-OCT-1993 GXA Added Spooling of report.
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
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TSHRRPT(GNUM,GIND,DRAW,COPY)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DTSREC.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4 NUMROW
        PARAMETER(NUMROW = 10)
C
        REAL*4    NETPER                ! Net Sales Percentage
        REAL*4    WINPER                ! Winning Amount Percentage.
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
        INTEGER*4 FINODS(4)             ! Final Odds (4 at a time)
        INTEGER*4 ROW                   ! Actual Row Number.
        INTEGER*4 NUMWIN                ! Number of winners
        INTEGER*4 BEG                   ! Beginning String index
        INTEGER*4 END                   ! Ending String index
        INTEGER*4 TOTWON                ! Total # of winners.
        INTEGER*4 TOTSAL                ! Total # of rows sold.
C
        INTEGER*2 DATE(LDATE_LEN)       ! 
        INTEGER*4 YEAR
C
        CHARACTER*47 HEAD               ! Report Header
        CHARACTER*40 HEAD2              ! Report Header2
        CHARACTER*12 REPNAM             ! Report Name
        CHARACTER*132 STRING            ! Report String
        CHARACTER*10 DIVNAME(TSLDIV)    ! Division Names
C
        DATA    DIVNAME/'triple    ','kvartetti ','kvintetti ',
     *                  'sekstetti ','single    ','tupla     '/
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
        NUMWIN = 0
        CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
        CALL IOINIT(FDB,3,DTSSEC*256)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)

        CALL READW(FDB,DRAW,DTSREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
        CDC = DTSESD           
        CALL FIGWEK(CDC-WEEK_OFFSET,WEKNO,YEAR)  
        DATE(VCDC) = CDC  
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
        CALL TITLE(HEAD,'TWINRP  ',GIND,6,PAGE,CDC)
C
        WRITE(6,802) (SCFLGN(I,GNUM),I=1,4),DRAW,WEKNO,YEAR,
     *               (DATE(K),K=7,13)
C
        WRITE(6,803)
C
C LOOP FOR ALL ROWS, DISPLAY 4 COLUMS OF X ROWS.
C
        DO I = 1,MAXSRW/4
           WRITE(STRING,804)
           BEG = 0
           END = 1
           DO K = 0,3
              BEG = END + 1
              END = BEG + 25
              ROW = I+(K*10)     !I+K
              FINODS(K+1) = 0
              IF(DTSWIN(ROW).EQ.ROWCAN) THEN
                 FINODS(K+1) = 100
              ELSE
                 FINODS(K+1) = 0
                 IF(DTSWIN(ROW).EQ.ROWWIN) FINODS(K+1) = DTSODS(1,ROW)
                 IF(DTSWIN(ROW).EQ.ROWLOS) FINODS(K+1) = DTSODS(2,ROW)
                 IF(DTSWIN(ROW).EQ.ROWTIE) FINODS(K+1) = DTSODS(3,ROW)
              ENDIF
C
              IF(FINODS(K+1).EQ.0) THEN
                 WRITE(STRING(BEG:END),902) ROW
              ELSE
                 WRITE(STRING(BEG:END),900) ROW,FINODS(K+1)/100,
     *                                      MOD(FINODS(K+1),100)
              ENDIF
           END DO
C
           WRITE(6,903) STRING
C
        END DO
C
        DO I = 1,TSLDIV
           NUMWIN = NUMWIN + DTSWBD(I,1) + DTSRBD(I,1)
        END DO
C
C CALCULATE PERCENTAGES AND DISPLAY TOTALS
C
        IF(DTSSAL.GT.0) THEN
           NETPER = (FLOAT(DTSSAL-DTSREF)/DTSSAL)*100
           IF(DTSSAL.NE.DTSREF) THEN
               WINPER = (FLOAT(DTSWON-DTSREF)/(DTSSAL-DTSREF))*100
           ELSE
	       WINPER = 0
	   ENDIF
        ELSE
           NETPER = 0.0
           WINPER = 0.0
        ENDIF
C
        WRITE(6,901) CMONY(DTSSAL,12,BETUNIT),
     *               CMONY(DTSREF,12,BETUNIT),
     *               CMONY(DTSSAL-DTSREF,12,BETUNIT),
     *               DFLOAT(DTSBRK)/100.D0,
     *               NUMWIN,CMONY(DTSWON-DTSREF,12,BETUNIT),WINPER
C
C============================================================================
C
C SECOND PART OF REPORT (FOR STATE CONTROLLER)
C
        WRITE(HEAD2,810)
        CALL TITLE(HEAD2,'TWINRP  ',GIND,6,PAGE,CDC)
C
        WRITE(6,802) (SCFLGN(I,GNUM),I=1,4),DRAW,WEKNO,YEAR,
     *               (DATE(K),K=7,13)
C
        DO I = 1,TSLDIV
           TOTSAL = TOTSAL + DTSSBD(I,1)
        END DO
C
        WRITE(6,811) TOTSAL 
        WRITE(6,812) 
C
        WRITE(6,813) DIVNAME(5),DTSWBD(5,1)+DTSRBD(5,1)
        WRITE(6,813) DIVNAME(6),DTSWBD(6,1)+DTSRBD(6,1)
        DO I = 1,TSLDIV
           IF(I.LT.5) WRITE(6,813) DIVNAME(I),DTSWBD(I,1)+DTSRBD(I,1)
           TOTWON = TOTWON + DTSWBD(I,1) + DTSRBD(I,1)
        END DO
C
        WRITE(6,814) TOTWON
        WRITE(6,815)
C
        CALL USRCLOS1(6)
C
        CALL SPOOL(REPNAM,COPY,ST)
C
        CALL CLOSEFIL(FDB)

C
800     FORMAT('PI',I1,'WINRP.REP') 
801     FORMAT(3A4,' VOITTOLASKELMA, PÄÄTTYNEET KOHTEET')
802     FORMAT(//,1X,4A4,1X,I4,4X,'Kierros ',I2.2,'/',I4,4X,
     *         'Voittotilanne ',7A2,//)
803     FORMAT(1X,4('Kohde',4X,'Kerroin',10X))
804     FORMAT(132(' '))
810     FORMAT('VALVONTARAPORTTI')
811     FORMAT(1X,'Osallistuneet pelit ',I10,' riviä',/)
812     FORMAT(1X,'Voittoluokka',10X,'Voittoja kpl',/)
813     FORMAT(1X,A10,13X,I10)
814     FORMAT(1X,23X,'============',/,
     *         1X,'Yhteensä  ',13X,I10,////)
815     FORMAT(10X,'Tuloksen vahvisti               Vantaa    ',
     *         '_____/_____  _____  _______________________________')
C** 900 FORMAT(1X,4(3X,I2,4X,1X,I3,'.',I2.2,10X))
900     FORMAT('   ',I2,'     ',I3,'.',I2.2,10(' '))
901     FORMAT(//,1X,50X,'Vaihto         ',A12,/,
     *            1X,50X,'Palautuksia    ',A12,/,
     *            1X,50X,'Nettovaihto    ',A12,5X,'100.00%',//,
     *            1X,50X,'Voittorahastoon',F12.2,/,
     *            1X,10X,'Voittoja yhteensä ',I10,' kpl',8X,
     *                   'Voitot         ',A12,2X,F9.2,'%')
902     FORMAT('   ',I2,21(' '))
903     FORMAT(132A)
        RETURN
        END  
