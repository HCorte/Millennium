C PROGRAM WEEKTSHR
C $Log:   GXAFXT:[GOLS]WEEKTSHR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:59:12   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   15 Mar 1996 10:47:08   HXK
C  Initial revision.
C  
C     Rev 1.0   08 Mar 1996 17:34:22   RXK
C  Initial revision.
C
C PITKAVETO WEEKLY SHARES REPORT
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
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM WEEKTSHR
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DTSREC.DEF'
        INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	INTEGER*4 NUMROW
	PARAMETER(NUMROW = 10)
C
	REAL*4    WINPER		! Winning Amount Percentage.
        INTEGER*4 DRAW			! draw number
        INTEGER*4 COPY			! number of report copies
C
        INTEGER*4 I,J,K			! Loop variables
        INTEGER*4 ST			! status
        INTEGER*4 PAGE	                ! Page Number
        INTEGER*4 FDB(7)		! file description block
        INTEGER*4 GFDB(7)	
	INTEGER*4 CDC			! Results valid for CDC.
	INTEGER*4 FINODS(4)		! Final Odds (4 at a time)
	INTEGER*4 ROW			! Actual Row Number.
	INTEGER*4 NUMWIN		! Number of winners
	INTEGER*4 BEG			! Beginning String index
	INTEGER*4 END			! Ending String index
	INTEGER*4 TOTWON		! Total # of winners.
	INTEGER*4 TOTSAL		! Total # of rows sold.
C
	INTEGER*4 YEAR,WEEK,EXT,SCDC,LCDC,GAM,GTYP,LAST_DRAW
	INTEGER*4 TOTVAIHTO/0/
	INTEGER*4 TOTPALA/0/
	INTEGER*4 TOTNUM/0/	
	INTEGER*4 TOTVOITOT/0/	
C
        INTEGER*2 BEGSAL(LDATE_LEN)		! 
        INTEGER*2 ENDSAL(LDATE_LEN)		! 
C
	CHARACTER*47 HEAD		! Report Header
	CHARACTER*11 REPNAM		! Report Name
	CHARACTER*132 STRING		! Report String
        CHARACTER*10 DIVNAME(TSLDIV)    ! Division Names
C
        DATA    DIVNAME/'triple    ','kvartetti ','kvintetti ',
     *                  'sekstetti ','single    ','tupla     '/
C
C
        CALL COPYRITE
C
        TYPE*,IAM()
        TYPE*,IAM(),' <<<<<  PITKAVETO WEEKLY SHARES REPORT  >>>>>'
        TYPE*,IAM()
C
C
C GET SYSTEM CONFIGURATION INFO.
C
	CALL GETSCONF(SCFREC,ST)
	IF(ST.NE.0) THEN
	   TYPE*,IAM(),'Unable to get system configuration info. '
	   CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
5       CALL PRMNUM('Enter week number:',WEEK,1,53,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
        CALL PRMNUM('Enter year (e.g.1996):',YEAR,1977,2099,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)

        CALL WKNUMCDC(WEEK,MOD(YEAR,100),SCDC)

        IF(SCDC.GT.DAYCDC) THEN
           TYPE*,IAM(),'Invalid week! Try again.'
           GOTO 5
        ENDIF
	SCDC = SCDC + 1
	LCDC = SCDC + 6
C
        COPY   = 1
        PAGE = 0

        WRITE(REPNAM,800)
        DO 15 GAM = 1,MAXGAM
           GTYP = GNTTAB(GAMTYP,GAM)
           IF(GTYP.NE.TTSL) GOTO 15
           WRITE(HEAD,801) (SCFLGN(I,GAM),I=1,3),WEEK,YEAR
           CALL ROPEN(REPNAM,6,ST)
           IF(ST.NE.0) THEN
               TYPE*,IAM(),REPNAM,' Open error  st - ',ST
               CALL USRCLOS1(6)
               CALL GSTOP(GEXIT_FATAL)
           ENDIF
           GOTO 16
15      CONTINUE
C
16      CONTINUE
        CALL TITLE(HEAD,'  TWINRPX',1,6,PAGE,CDC)
        WRITE(6,905)
        WRITE(6,805) WEEK,YEAR
C
C OPEN DAILY ACTIVITY FILE
C
        CALL OPENW(1,SFNAMES(1,DAF),4,0,0,ST)
        CALL IOINIT(FDB,1,DAFSEC*256)
        IF(ST.NE.0) THEN
            CALL CLOSEFIL(FDB)
            CALL FILERR(SFNAMES(1,DAF),1,ST,0)
        ENDIF
C
        DO 20 GAM = 1,MAXGAM
           GTYP = GNTTAB(GAMTYP,GAM)
           IF(GTYP.NE.TTSL) GOTO 20
           DO 30 CDC = SCDC, SCDC + 6
              IF(CDC.LE.0) GOTO 20
              CALL READW(FDB,CDC,DAFREC,ST)
              IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),1,ST,CDC)
              DRAW = DAFDRW(GAM)
              CALL OPENW(3,GFNAMES(1,GAM),4,0,0,ST)
              CALL IOINIT(GFDB,3,DTSSEC*256)
              IF(ST.NE.0) THEN
                 CALL FILERR(SCFGFN(1,GAM),1,ST,0)
                 CALL GSTOP(GEXIT_FATAL)
              ENDIF
              DO 40 J= DRAW,DRAW-20,-1  !read draws for previous 20 draws
                 IF(DRAW.LE.0) GOTO 40
                 IF(DRAW.EQ.LAST_DRAW) GOTO 40
                 LAST_DRAW = DRAW
                 CALL READW(GFDB,DRAW,DTSREC,ST)
                    IF(ST.NE.0) THEN
                       CALL FILERR(GFNAMES(1,GAM),2,ST,DRAW)
                       CALL GSTOP(GEXIT_FATAL)
                    ENDIF
                 IF(DTSESD.LT.SCDC.OR.DTSESD.GT.LCDC) GOTO 40
                 BEGSAL(5) = DTSBSD
                 CALL LCDATE(BEGSAL)
                 ENDSAL(5) = DTSESD
                 CALL LCDATE(ENDSAL)
                 NUMWIN = 0
C
C LOOP FOR ALL ROWS, DISPLAY 4 COLUMS OF X ROWS.
C
                 WRITE(6,904)
                 WRITE(6,802) DRAW,(BEGSAL(I),I=7,13),
     *                        (ENDSAL(I),I=7,13)
                 WRITE(6,803)
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
                          IF(DTSWIN(ROW).EQ.ROWWIN) FINODS(K+1) =
     *                       DTSODS(1,ROW)
		          IF(DTSWIN(ROW).EQ.ROWLOS) FINODS(K+1) = 
     *                       DTSODS(2,ROW)
                          IF(DTSWIN(ROW).EQ.ROWTIE) FINODS(K+1) = 
     *                       DTSODS(3,ROW)
	               ENDIF
C
	               IF(FINODS(K+1).EQ.0) THEN
	                  WRITE(STRING(BEG:END),902) ROW
	               ELSE
	                  WRITE(STRING(BEG:END),900) ROW,FINODS(K+1)/100,
     *                          MOD(FINODS(K+1),100)
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
   	            WINPER = (FLOAT(DTSWON-DTSREF)/(DTSSAL-DTSREF))*100
	         ELSE
	            WINPER = 0.0
	         ENDIF
C
	         WRITE(6,901) CMONY(DTSSAL,12,BETUNIT),
     *               CMONY(DTSREF,12,BETUNIT),
     *               CMONY(DTSSAL-DTSREF,12,BETUNIT),
     *               DRAW,
     *               NUMWIN,
     *               CMONY(DTSWON-DTSREF,12,BETUNIT),
     *               WINPER
C
C #WINNERS IN EACH DIVISION. 
C
C
	          TOTSAL = 0
		  TOTWON = 0
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
                 ENDDO
                 WRITE(6,814) TOTWON
C
                 TOTVAIHTO = TOTVAIHTO + DTSSAL
                 TOTPALA   = TOTPALA   + DTSREF
                 TOTNUM = TOTNUM + NUMWIN
                 TOTVOITOT = TOTVOITOT + DTSWON
40            CONTINUE
              CALL CLOSEFIL(GFDB)
30         CONTINUE
20      CONTINUE
C
        CALL CLOSEFIL(FDB)
C
C
        WRITE(6,905)
	IF(TOTVAIHTO.GT.0) THEN
	   WINPER = (FLOAT(TOTVOITOT-TOTPALA)/(TOTVAIHTO-TOTPALA))*100
	ELSE
	   WINPER = 0.0
	ENDIF
	WRITE(6,906) CMONY(TOTVAIHTO,12,BETUNIT),
     *               CMONY(TOTPALA,12,BETUNIT),
     *               CMONY(TOTVAIHTO-TOTPALA,12,BETUNIT),
     *               TOTNUM,
     *               CMONY(TOTVOITOT-TOTPALA,12,BETUNIT),
     *               WINPER
C
C
        CALL USRCLOS1(6)
        CALL SPOOL('TWINRPX.REP',COPY,ST)
        CALL GSTOP(GEXIT_SUCCESS)
C
C
800     FORMAT('TWINRPX.REP') 
801     FORMAT(3A4,' VOITTOLASKELMA, WEEK ',I2.2,'/',I4.4)
802	FORMAT(/,1X,'Draw  ',I4,4X,7A2,' - ',7A2)
803	FORMAT(/,1X,4('Kohde',4X,'Kerroin',10X))
804	FORMAT(132(' '))
805	FORMAT(/1X,'Kierros ',I2.2,'/',I4.4)
811     FORMAT(1X,'Osallistuneet pelit ',I10,' riviä',/)
812     FORMAT(1X,'Voittoluokka',10X,'Voittoja kpl',/)
813     FORMAT(1X,A10,13X,I10)
814     FORMAT(1X,23X,'============',/,
     *         1X,'Yhteensä  ',13X,I10,/)
900	FORMAT('   ',I2,'     ',I3,'.',I2.2,10(' '))
901	FORMAT(//,1X,50X,'Vaihto      ',A12,/,
     *            1X,50X,'Palautuksia ',A12,/,
     *            1X,50X,'Nettovaihto ',A12,5X,'100.00%',//,
     *            1X,'Draw ',I4, 
     *            2X,'Voittoja yhteensä',I10,' kpl',8X,
     *                   'Voitot      ',A12,5X,F6.2,'%',//)
902	FORMAT('   ',I2,21(' '))
903	FORMAT(132A)
904	FORMAT(/1X,100('-'))
905	FORMAT(/1X,100('='))
906	FORMAT(//,1X,50X,'Vaihto      ',A12,/,
     *            1X,50X,'Palautuksia ',A12,/,
     *            1X,50X,'Nettovaihto ',A12,5X,'100.00%',//,
     *            1X,11X,'Voittoja yhteensä',I10,' kpl',8X,
     *                   'Voitot      ',A12,5X,F6.2,'%')
C
	END  
