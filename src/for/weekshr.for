C PROGRAM WEEKSHR
C
C V14 01-DEC-1999 OXK Fix for total number of boards. (overflow in printing)
C V13 12-AUG-1999 UXN Fix for winning percantage.
C V12 28-MAY-1999 UXN Todays Trio modified. DTRCMB added.
C V11 26-MAY-1999 UXN Super Triple added.
C V10 14-APR-1999 UXN Fix for big odds.
C V09 26-JAN-1998 UXN Super Score and Todays Triple added.
C V08 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting, 
C                     Instant Pass Thru Phase 1
C V07 15-MAR-1996 HXK Putting Rita's fixes (RXK) into PVCS archive
C V06 05-MAR-1996 RXK Various fixes 
C V05 16-FEB-1996 RXK Row number of event B fixed   
C V04 02-FEB-1996 RXK Rfss 94150 + Today's couple and Super Double added.
C V03 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V02 06-JUN-1994 HXK CHANGED TO HANDLE CANCELLED TWIT GAMES.
C V01 05-JUN-1994 HXK Initial revision.
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
	PROGRAM WEEKSHR
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'	
        INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:PRMVAL.DEF'
        INCLUDE 'INCLIB:DSCREC.DEF'
        INCLUDE 'INCLIB:DWIREC.DEF'
        INCLUDE 'INCLIB:DCPREC.DEF'
        INCLUDE 'INCLIB:DDBREC.DEF'
	INCLUDE 'INCLIB:DSSREC.DEF'
	INCLUDE 'INCLIB:DTRREC.DEF'
	INCLUDE 'INCLIB:DSTREC.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C

	INTEGER*4 FDB(7)              !
        INTEGER*4 GFDB(7)             !
	INTEGER*4 I,J,K,L
	INTEGER*4 PAGE                !
	INTEGER*4 GAM                 !
	INTEGER*4 IND                 !
	INTEGER*4 CDC                 !
	INTEGER*4 WEEK                !
        INTEGER*4 YEAR                !
	INTEGER*4 GAMCNT              !
	INTEGER*4 SCDC,LCDC           !
	INTEGER*4 EXT                 !
	INTEGER*4 COPY                !
	INTEGER*4 ST                  !
	INTEGER*4 GTYP                !

        INTEGER*4 GIND, DRAW, LAST_DRAW, LAST_GAME, REPLU

        REAL*4    WINPER
	INTEGER*4 YESNO

	INTEGER*2 DATE(LDATE_LEN)/LDATE_LEN*0/      
        INTEGER*2 D_DATE(LDATE_LEN)

        INTEGER*4 VALID_STS(MAXGAM,7)
	INTEGER*4 VALID_EVENT_STS(3,MAXGAM,7)
        INTEGER*4 VALID_DRW(MAXGAM,7)
        INTEGER*4 VALID_ENM(WENM_LEN/4,MAXGAM,7)
        INTEGER*4 VALID_DENM(DBLENM_LEN/4,MAXGAM,7)
        INTEGER*4 VALID_STRENM(STRENM_LEN/4,MAXGAM,7)
        INTEGER*4 VALID_C1ENM(CPLENM_LEN/4,MAXGAM,7)
        INTEGER*4 VALID_C2ENM(CPLENM_LEN/4,MAXGAM,7)
        INTEGER*4 VALID_NMS(WNMS_LEN/4,4,MAXGAM,7)
        INTEGER*4 VALID_NM1(SNMS_LEN/4,MAXGAM,7)
        INTEGER*4 VALID_NM2(SNMS_LEN/4,MAXGAM,7)
        INTEGER*4 VALID_DNMS(DBLNMS_LEN/4,4,MAXGAM,7)
        INTEGER*4 VALID_D2NMS(DBLNMS_LEN/4,4,MAXGAM,7)
        INTEGER*4 VALID_STRNMS(STRNMS_LEN/4,MAXTRPTI,MAXGAM,7)
        INTEGER*4 VALID_STR2NMS(STRNMS_LEN/4,MAXTRPTI,MAXGAM,7)
        INTEGER*4 VALID_STR3NMS(STRNMS_LEN/4,MAXTRPTI,MAXGAM,7)
        INTEGER*4 VALID_CNMS(CPLNMS_LEN/4,4,MAXGAM,7)
        INTEGER*4 VALID_C2NMS(CPLNMS_LEN/4,4,MAXGAM,7)
	INTEGER*4 VALID_SSC_NMS(SSNMS_LEN/4,4,MAXGAM,7)
	INTEGER*4 VALID_TRP_ENM(TRPENM_LEN/4,3,MAXGAM,7)
	INTEGER*4 VALID_TRP_NMS(TRPNMS_LEN/4,MAXTRPTI,3,MAXGAM,7)
        INTEGER*4 VALID_ODS(VMAX,MAXGAM,7)
        INTEGER*4 VALID_2ODS(VMAX,MAXGAM,7)
	INTEGER*4 VALID_TRP_ODS(MAXTRPTI,MAXGAM,7)
	INTEGER*4 VALID_CMB(MAXGAM,7)
        INTEGER*4 VALID_DAT(MAXGAM,7)
        INTEGER*4 VALID_WIN(VMAX,MAXGAM,7)
        INTEGER*4 VALID_2ND(VMAX,MAXGAM,7)
        INTEGER*4 VALID_3RD(VMAX,MAXGAM,7)
        INTEGER*4 VALID_WON(MAXGAM,7)
        INTEGER*4 VALID_SAL(MAXGAM,7)
        INTEGER*4 VALID_REF(MAXGAM,7)
        INTEGER*4 VALID_WPA(MAXGAM,7)
        INTEGER*4 VALID_WPR(MAXGAM,7)
        INTEGER*4 TOT_SAL(MAXTYP)/MAXTYP*0/
        INTEGER*4 TOT_REF(MAXTYP)/MAXTYP*0/
        INTEGER*4 TOT_WON(MAXTYP)/MAXTYP*0/
        INTEGER*4 TOT_WON_C(MAXTYP)/MAXTYP*0/
        REAL*4    TOTPER

        INTEGER*4 SCR_COUNT/0/,WIT_COUNT/0/,CPL_COUNT/0/,DBL_COUNT/0/
	INTEGER*4 SSC_COUNT/0/,TRP_COUNT/0/,STR_COUNT/0/
	CHARACTER HEAD*50

        LOGICAL FIRST_SSC
        LOGICAL FIRST_SCR
        LOGICAL FIRST_WIT
        LOGICAL FIRST_DBL
        LOGICAL FIRST_CPL
        LOGICAL FIRST_TRP
        LOGICAL FIRST_STR

        CHARACTER CNAME1(SNMS_LEN),CNAME2(SNMS_LEN)
        EQUIVALENCE (CNAME1,DSCNM1),(CNAME2,DSCNM2)

	CALL COPYRITE

        TYPE*,IAM()
        TYPE*,IAM(),' <<<<<  WEEKLY SHARES REPORT  >>>>>'
        TYPE*,IAM()

        REPLU  = 7
	PAGE   = 0
	GAMCNT = 0
	COPY   = 1

        DO I=1,MAXGAM
           DO J=1,7
              VALID_STS(I,J) = 0
              VALID_EVENT_STS(1,I,J) = 0
              VALID_EVENT_STS(2,I,J) = 0
              VALID_EVENT_STS(3,I,J) = 0
              VALID_DRW(I,J) = 0
              VALID_DAT(I,J) = 0
              VALID_WON(I,J) = 0
              VALID_SAL(I,J) = 0
              VALID_REF(I,J) = 0
              VALID_WPR(I,J) = 0
              VALID_WPA(I,J) = 0
              DO K=1,VMAX
                 VALID_ODS(K,I,J) = 0
                 VALID_2ODS(K,I,J) = 0
                 VALID_WIN(K,I,J) = 0
                 VALID_2ND(K,I,J) = 0
                 VALID_3RD(K,I,J) = 0
              ENDDO
              DO K=1,MAXTRPTI
		 VALID_TRP_ODS(K,I,J)=0
	      ENDDO
	      VALID_CMB(I,J)=0
              DO K=1,WENM_LEN/4
                 VALID_ENM(K,I,J) = 0
              ENDDO
              DO K=1,4
                 DO L=1,WNMS_LEN/4
                    VALID_NMS(L,K,I,J) = 0
                 ENDDO
              ENDDO
              DO K=1,SNMS_LEN/4
                 VALID_NM1(K,I,J) = 0
                 VALID_NM2(K,I,J) = 0
              ENDDO
           ENDDO
	ENDDO

5       CONTINUE

        CALL PRMNUM('Enter week number:',WEEK,1,53,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
        CALL PRMNUM('Enter year (e.g. 1996):',YEAR,1989,2099,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)

        CALL WKNUMCDC(WEEK,MOD(YEAR,100),SCDC)
C
        IF(SCDC.GT.DAYCDC) THEN
           TYPE*,IAM(),'Invalid week! Try again.'
           GOTO 5
        ENDIF
C
C Display first and last CDC and confirm it.
C
	SCDC = SCDC + 1		    ! round starts on Tuesday
	LCDC = SCDC + 6		    ! and ends on Monday
C
7	CONTINUE
	DATE(VCDC) = SCDC
	CALL LCDATE(DATE)
	WRITE(6,9700) IAM(),SCDC,(DATE(K),K=7,13)
	DATE(VCDC) = LCDC
	CALL LCDATE(DATE)
	WRITE(6,9701) IAM(),LCDC,(DATE(K),K=7,13)
	CALL INPYESNO('Is this correct [Y/N] ?',YESNO)
	IF(YESNO.EQ.1) GOTO 10
	CALL INPNUM('Enter starting CDC ',SCDC,1,9999,EXT)
	IF(EXT.NE.0) CALL GSTOP(GEXIT_OPABORT)
	CALL INPNUM('Enter ending   CDC ',LCDC,1,9999,EXT)
	IF(EXT.NE.0) CALL GSTOP(GEXIT_OPABORT)
	GOTO 7		! Ask for confirmation
10	CONTINUE
C
C OPEN DAILY ACTIVITY FILE
C
	CALL OPENW(1,SFNAMES(1,DAF),4,0,0,ST)
	CALL IOINIT(FDB,1,DAFSEC*256)
	IF(ST.NE.0) THEN
	    CALL CLOSEFIL(FDB)
	    CALL FILERR(SFNAMES(1,DAF),1,ST,0)
	ENDIF
        DATE(VCDC)=SCDC
        CALL LCDATE(DATE)
        CALL FIGWEK(SCDC-WEEK_OFFSET,WEEK,YEAR)
15	CONTINUE
        DO 30 GAM = 1,MAXGAM
	   DO 20 CDC = SCDC, LCDC
 	          IF(CDC.LE.0) GOTO 20
	          CALL READW(FDB,CDC,DAFREC,ST)
	          IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),1,ST,0)
	          IND = CDC - SCDC + 1
	          GTYP = GNTTAB(GAMTYP,GAM)
	          IF(GTYP.NE.TSCR.AND.GTYP.NE.TWIT.AND.
     *		     GTYP.NE.TDBL.AND.GTYP.NE.TCPL.AND.
     *               GTYP.NE.TSSC.AND.GTYP.NE.TTRP.AND.
     *               GTYP.NE.TSTR) GOTO 30
                  GIND = GNTTAB(GAMIDX,GAM)
                  DRAW = DAFDRW(GAM) 
                  DO 40 J= DRAW,DRAW-20,-1  !read draws for previous 20 draws
                     IF(DRAW.LE.0) GOTO 40
                     IF(GAM.EQ.LAST_GAME.AND.DRAW.EQ.LAST_DRAW) GOTO 40
                     LAST_GAME = GAM
                     LAST_DRAW = DRAW
                     IF(GTYP.EQ.TSCR) THEN
                        CALL OPENW(3,GFNAMES(1,GAM),4,0,0,ST)
                        CALL IOINIT(GFDB,3,DSCSEC*256)
                        IF(ST.NE.0) THEN
                           CALL FILERR(GFNAMES(1,GAM),1,ST,0)
                           CALL GSTOP(GEXIT_FATAL)
                        ENDIF
                        CALL READW(GFDB,DRAW,DSCREC,ST)
                        IF(ST.NE.0) THEN
                           CALL FILERR(GFNAMES(1,GAM),2,ST,DRAW)
                           CALL GSTOP(GEXIT_FATAL)
                        ENDIF
			CALL CLOSEFIL(GFDB)
                        IF(DSCDAT.GT.LCDC)   GOTO 40
                        IF(DSCDAT.LT.SCDC)   GOTO 40
                        VALID_STS(GAM,IND) = DSCSTS
                        VALID_DRW(GAM,IND) = DRAW
                        CNAME1(15) = ' '
                        CNAME1(16) = ' '
                        CNAME2(15) = ' '
                        CNAME2(16) = ' '
                        DO I=1,SNMS_LEN/4
                           VALID_NM1(I,GAM,IND) = DSCNM1(I)
                           VALID_NM2(I,GAM,IND) = DSCNM2(I)
                        ENDDO
                        VALID_ODS(1,GAM,IND) = DSCODS
                        VALID_DAT(GAM,IND) = DSCDAT
                        VALID_WIN(1,GAM,IND) = DSCWIN(1)
                        VALID_WIN(2,GAM,IND) = DSCWIN(2)
                        VALID_WON(GAM,IND) = DSCWON
		        VALID_SAL(GAM,IND) = DSCSAL
                        VALID_REF(GAM,IND) = DSCREF
                        VALID_WPR(GAM,IND) = DSCWPR(1,PRWON)
                        VALID_WPA(GAM,IND) = DSCWPA(1,PRWON)
                     ENDIF  

                     IF(GTYP.EQ.TSSC) THEN
                        CALL OPENW(3,GFNAMES(1,GAM),4,0,0,ST)
                        CALL IOINIT(GFDB,3,DSSSEC*256)
                        IF(ST.NE.0) THEN
                           CALL FILERR(GFNAMES(1,GAM),1,ST,0)
                           CALL GSTOP(GEXIT_FATAL)
                        ENDIF
                        CALL READW(GFDB,DRAW,DSSREC,ST)
                        IF(ST.NE.0) THEN
                           CALL FILERR(GFNAMES(1,GAM),2,ST,DRAW)
                           CALL GSTOP(GEXIT_FATAL)
                        ENDIF
			CALL CLOSEFIL(GFDB)
                        IF(DSSDAT.GT.LCDC)   GOTO 40
                        IF(DSSDAT.LT.SCDC)   GOTO 40
                        VALID_STS(GAM,IND) = DSSSTS
			VALID_EVENT_STS(1,GAM,IND) = DSSEST(1)
			VALID_EVENT_STS(2,GAM,IND) = DSSEST(2)
			VALID_EVENT_STS(3,GAM,IND) = DSSEST(3)
                        VALID_DRW(GAM,IND) = DRAW
			VALID_WIN(1,GAM,IND) = DSSWIN(1,1)
			VALID_WIN(2,GAM,IND) = DSSWIN(2,1)
			VALID_WIN(3,GAM,IND) = DSSWIN(1,2)
			VALID_WIN(4,GAM,IND) = DSSWIN(2,2)
			VALID_WIN(5,GAM,IND) = DSSWIN(1,3)
			VALID_WIN(6,GAM,IND) = DSSWIN(2,3)
			DO K=1,SSNMS_LEN/4
			   VALID_SSC_NMS(K,1,GAM,IND) = DSSSNM(K,1)
			   VALID_SSC_NMS(K,2,GAM,IND) = DSSSNM(K,2)
			   VALID_SSC_NMS(K,3,GAM,IND) = DSSSNM(K,3)
			   VALID_SSC_NMS(K,4,GAM,IND) = DSSMNM(K)
			ENDDO
                        VALID_ODS(1,GAM,IND) = DSSODS
                        VALID_DAT(GAM,IND) = DSSDAT
                        VALID_WON(GAM,IND) = DSSWON
		        VALID_SAL(GAM,IND) = DSSSAL
                        VALID_REF(GAM,IND) = DSSREF
                        VALID_WPR(GAM,IND) = DSSWPR(1,PRWON)
                        VALID_WPA(GAM,IND) = DSSWPA(1,PRWON)
                     ENDIF  

                     IF(GTYP.EQ.TWIT) THEN
                        CALL OPENW(3,GFNAMES(1,GAM),4,0,0,ST)
                        CALL IOINIT(GFDB,3,DWISEC*256)
                        IF(ST.NE.0) THEN
                           CALL FILERR(GFNAMES(1,GAM),1,ST,0)
                           CALL GSTOP(GEXIT_FATAL)
                        ENDIF
                        CALL READW(GFDB,DRAW,DWIREC,ST)
                        IF(ST.NE.0) THEN
                           CALL FILERR(GFNAMES(1,GAM),2,ST,DRAW)
                           CALL GSTOP(GEXIT_FATAL)
                        ENDIF
			CALL CLOSEFIL(GFDB)
                        IF(DWIDAT.GT.LCDC)   GOTO 40
                        IF(DWIDAT.LT.SCDC)   GOTO 40
                        VALID_STS(GAM,IND) = DWISTS
                        VALID_DRW(GAM,IND) = DRAW
                        DO I=1,WENM_LEN/4
                           VALID_ENM(I,GAM,IND) = DWIENM(I)
                        ENDDO
                        VALID_DAT(GAM,IND) = DWIDAT
                        VALID_WIN(1,GAM,IND) = DWIWIN(1)
                        VALID_WIN(2,GAM,IND) = DWIWIN(2)
                        VALID_WIN(3,GAM,IND) = DWIWIN(3)
                        VALID_WIN(4,GAM,IND) = DWIWIN(4)
                        VALID_WON(GAM,IND) = DWIWON
                        VALID_SAL(GAM,IND) = DWISAL
                        VALID_REF(GAM,IND) = DWIREF
                        VALID_WPR(GAM,IND) = DWIWPR(1,PRWON)
                        VALID_WPA(GAM,IND) = DWIWPA(1,PRWON)
                        DO I=1,4
                           VALID_ODS(I,GAM,IND) = DWIODS(I)
                        ENDDO
                        DO K=1,4
                           IF(VALID_WIN(K,GAM,IND).NE.0) THEN
                              DO I=1,WNMS_LEN/4
                                 VALID_NMS(I,K,GAM,IND) = DWINMS(I,DWIWIN(K))
                              ENDDO
                           ENDIF
                        ENDDO
                     ENDIF

                     IF(GTYP.EQ.TDBL) THEN
                        CALL OPENW(3,GFNAMES(1,GAM),4,0,0,ST)
                        CALL IOINIT(GFDB,3,DDBSEC*256)
                        IF(ST.NE.0) THEN
                           CALL FILERR(GFNAMES(1,GAM),1,ST,0)
                           CALL GSTOP(GEXIT_FATAL)
                        ENDIF
                        CALL READW(GFDB,DRAW,DDBREC,ST)
                        IF(ST.NE.0) THEN
                           CALL FILERR(GFNAMES(1,GAM),2,ST,DRAW)
                           CALL GSTOP(GEXIT_FATAL)
                        ENDIF
			CALL CLOSEFIL(GFDB)
                        IF(DDBDAT.GT.LCDC)   GOTO 40
                        IF(DDBDAT.LT.SCDC)   GOTO 40
                        VALID_STS(GAM,IND) = DDBSTS
                        VALID_DRW(GAM,IND) = DRAW
                        DO I=1,DBLENM_LEN/4
                           VALID_DENM(I,GAM,IND) = DDBENM(I)
                        ENDDO
                        DO I=1,4
                           VALID_ODS(I,GAM,IND) = DDBODS(I)
                        ENDDO
                        VALID_DAT(GAM,IND) = DDBDAT
                        VALID_CMB(GAM,IND) = DDBCMB
			DO I=1,MAXDBLTI
                           VALID_WIN(I,GAM,IND) = DDBWIN(1,I)
                           VALID_2ND(I,GAM,IND) = DDBWIN(2,I)
                           IF(DDBWIN(1,I).GT.0) THEN
                              CALL FASTMOV(DDBNMS(1,DDBWIN(1,I)),
     *                                     VALID_DNMS(1,I,GAM,IND),
     *                                     DBLNMS_LEN/4)
                           ENDIF
                           IF(DDBWIN(2,I).GT.0) THEN
                              CALL FASTMOV(DDBNMS(1,DDBWIN(2,I)),
     *                                     VALID_D2NMS(1,I,GAM,IND),
     *                                     DBLNMS_LEN/4)
                           ENDIF
                        ENDDO			     
                        VALID_WON(GAM,IND) = DDBWON
                        VALID_SAL(GAM,IND) = DDBSAL(DOLAMT)
                        VALID_REF(GAM,IND) = DDBREF
                        VALID_WPR(GAM,IND) = DDBWPR(1,PRWON)
                        VALID_WPA(GAM,IND) = DDBWPA(1,PRWON)
                     ENDIF

                     IF(GTYP.EQ.TSTR) THEN
                        CALL OPENW(3,GFNAMES(1,GAM),4,0,0,ST)
                        CALL IOINIT(GFDB,3,DSTSEC*256)
                        IF(ST.NE.0) THEN
                           CALL FILERR(GFNAMES(1,GAM),1,ST,0)
                           CALL GSTOP(GEXIT_FATAL)
                        ENDIF
                        CALL READW(GFDB,DRAW,DSTREC,ST)
                        IF(ST.NE.0) THEN
                           CALL FILERR(GFNAMES(1,GAM),2,ST,DRAW)
                           CALL GSTOP(GEXIT_FATAL)
                        ENDIF
			CALL CLOSEFIL(GFDB)
                        IF(DSTDAT.GT.LCDC)   GOTO 40
                        IF(DSTDAT.LT.SCDC)   GOTO 40
                        VALID_STS(GAM,IND) = DSTSTS
                        VALID_DRW(GAM,IND) = DRAW
                        DO I=1,STRENM_LEN/4
                           VALID_STRENM(I,GAM,IND) = DSTENM(I)
                        ENDDO
                        DO I=1,MAXSTRTI
                           VALID_ODS(I,GAM,IND) = DSTODS(I)
                        ENDDO
                        VALID_DAT(GAM,IND) = DSTDAT
                        VALID_CMB(GAM,IND) = DSTCMB
			DO I=1,MAXSTRTI
			   VALID_WIN(I,GAM,IND) = DSTWIN(1,I)
                           VALID_2ND(I,GAM,IND) = DSTWIN(2,I)
                           VALID_3RD(I,GAM,IND) = DSTWIN(3,I)
			   IF(DSTWIN(1,I).GT.0) THEN
			      CALL FASTMOV(DSTNMS(1,DSTWIN(1,I)), 
     *                                     VALID_STRNMS(1,I,GAM,IND),
     *                                     STRNMS_LEN/4)
			   ENDIF
			   IF(DSTWIN(2,I).GT.0) THEN
			      CALL FASTMOV(DSTNMS(1,DSTWIN(2,I)), 
     *                                     VALID_STR2NMS(1,I,GAM,IND),
     *                                     STRNMS_LEN/4)
			   ENDIF
			   IF(DSTWIN(3,I).GT.0) THEN
			      CALL FASTMOV(DSTNMS(1,DSTWIN(3,I)), 
     *                                     VALID_STR3NMS(1,I,GAM,IND),
     *                                     STRNMS_LEN/4)
			   ENDIF
 			ENDDO
                        VALID_WON(GAM,IND) = DSTWON
                        VALID_SAL(GAM,IND) = DSTSAL(DOLAMT)
                        VALID_REF(GAM,IND) = DSTREF
                        VALID_WPR(GAM,IND) = DSTWPR(1,PRWON)
                        VALID_WPA(GAM,IND) = DSTWPA(1,PRWON)
                     ENDIF

                     IF(GTYP.EQ.TCPL) THEN
                        CALL OPENW(3,GFNAMES(1,GAM),4,0,0,ST)
                        CALL IOINIT(GFDB,3,DCPSEC*256)
                        IF(ST.NE.0) THEN
                           CALL FILERR(GFNAMES(1,GAM),1,ST,0)
                           CALL GSTOP(GEXIT_FATAL)
                        ENDIF
                        CALL READW(GFDB,DRAW,DCPREC,ST)
                        IF(ST.NE.0) THEN
                           CALL FILERR(GFNAMES(1,GAM),2,ST,DRAW)
                           CALL GSTOP(GEXIT_FATAL)
                        ENDIF
			CALL CLOSEFIL(GFDB)
                        IF(DCPDAT.GT.LCDC)   GOTO 40
                        IF(DCPDAT.LT.SCDC)   GOTO 40
                        VALID_STS(GAM,IND) = DCPSTS
                        VALID_DRW(GAM,IND) = DRAW
                        DO I=1,CPLENM_LEN/4
                           VALID_C1ENM(I,GAM,IND) = DCPENM(I,1)
                           VALID_C2ENM(I,GAM,IND) = DCPENM(I,2)
                        ENDDO
                        DO I=1,4
                           VALID_ODS(I,GAM,IND) = DCPODS(I)
                        ENDDO
                        IF (DCPWIN(1,2).NE.0.AND.DCPWIN(2,2).EQ.0) THEN
                           VALID_ODS(3,GAM,IND) = DCPODS(2)
                           VALID_ODS(2,GAM,IND) = 0
                        ENDIF
                        VALID_DAT(GAM,IND) = DCPDAT
                        VALID_CMB(GAM,IND) = DCPCMB
			DO I=1,MAXCPLTI
                           VALID_WIN(I,GAM,IND) = DCPWIN(1,I)
                           VALID_2ND(I,GAM,IND) = DCPWIN(2,I)
                           IF(DCPWIN(1,I).GT.0) THEN
                              CALL FASTMOV(DCPNMS(1,DCPWIN(1,I)),
     *                                     VALID_CNMS(1,I,GAM,IND),
     *                                     CPLNMS_LEN/4)
                           ENDIF
                            IF(DCPWIN(2,I).GT.0) THEN
                              CALL FASTMOV(DCPNMS(1,DCPWIN(2,I)),
     *                                     VALID_C2NMS(1,I,GAM,IND),
     *                                     CPLNMS_LEN/4)
                           ENDIF
                        ENDDO
                        VALID_WON(GAM,IND) = DCPWON
                        VALID_SAL(GAM,IND) = DCPSAL(DOLAMT)
                        VALID_REF(GAM,IND) = DCPREF
                        VALID_WPR(GAM,IND) = DCPWPR(1,PRWON)
                        VALID_WPA(GAM,IND) = DCPWPA(1,PRWON)
                     ENDIF

                     IF(GTYP.EQ.TTRP) THEN
                        CALL OPENW(3,GFNAMES(1,GAM),4,0,0,ST)
                        CALL IOINIT(GFDB,3,DTRSEC*256)
                        IF(ST.NE.0) THEN
                           CALL FILERR(GFNAMES(1,GAM),1,ST,0)
                           CALL GSTOP(GEXIT_FATAL)
                        ENDIF
                        CALL READW(GFDB,DRAW,DTRREC,ST)
                        IF(ST.NE.0) THEN
                           CALL FILERR(GFNAMES(1,GAM),2,ST,DRAW)
                           CALL GSTOP(GEXIT_FATAL)
                        ENDIF
			CALL CLOSEFIL(GFDB)
                        IF(DTRDAT.GT.LCDC)   GOTO 40
                        IF(DTRDAT.LT.SCDC)   GOTO 40
                        VALID_STS(GAM,IND) = DTRSTS
                        VALID_DRW(GAM,IND) = DRAW
                        DO I=1,TRPENM_LEN/4
                           VALID_TRP_ENM(I,1,GAM,IND) = DTRENM(I,1)
                           VALID_TRP_ENM(I,2,GAM,IND) = DTRENM(I,2)
                           VALID_TRP_ENM(I,3,GAM,IND) = DTRENM(I,3)
                        ENDDO
                        DO I=1,MAXTRPTI
                           VALID_TRP_ODS(I,GAM,IND) = DTRODS(I)
                        ENDDO
                        VALID_DAT(GAM,IND) = DTRDAT
			VALID_CMB(GAM,IND) = DTRCMB
			DO I=1,DTRCMB
                           VALID_WIN(I,GAM,IND) = DTRWIN(1,I)
                           VALID_2ND(I,GAM,IND) = DTRWIN(2,I)
                           VALID_3RD(I,GAM,IND) = DTRWIN(3,I)
                        ENDDO
                        VALID_WON(GAM,IND) = DTRWON
                        VALID_SAL(GAM,IND) = DTRSAL(DOLAMT)
                        VALID_REF(GAM,IND) = DTRREF
                        VALID_WPR(GAM,IND) = DTRWPR(1,PRWON)
                        VALID_WPA(GAM,IND) = DTRWPA(1,PRWON)
                        DO K=1,DTRCMB
                           IF(VALID_WIN(K,GAM,IND).NE.0) THEN
		              CALL FASTMOV(DTRNMS(1,DTRWIN(1,K),1),
     *                                VALID_TRP_NMS(1,K,1,GAM,IND),
     *                                TRPNMS_LEN/4)
                           ENDIF
                           IF(VALID_2ND(K,GAM,IND).NE.0) THEN
		              CALL FASTMOV(DTRNMS(1,DTRWIN(2,K),2),
     *                                VALID_TRP_NMS(1,K,2,GAM,IND),
     *                                TRPNMS_LEN/4)
                           ENDIF
                           IF(VALID_3RD(K,GAM,IND).NE.0) THEN
		              CALL FASTMOV(DTRNMS(1,DTRWIN(3,K),3),
     *                                VALID_TRP_NMS(1,K,3,GAM,IND),
     *                                TRPNMS_LEN/4)
                           ENDIF
                        ENDDO
                     ENDIF
40                CONTINUE
20	   CONTINUE
30	CONTINUE

        CALL CLOSEFIL(FDB)

	CALL ROPEN('WEEKSHR.REP',REPLU,ST)
	DATE(VCDC)=SCDC
	CALL LCDATE(DATE)

        FIRST_SCR = .TRUE.
        FIRST_SSC = .TRUE.
        FIRST_WIT = .TRUE.
        FIRST_DBL = .TRUE.
        FIRST_CPL = .TRUE.
        FIRST_TRP = .TRUE.
        FIRST_STR = .TRUE.

C---- Score game.

        DO IND = 1,7      ! day of the week
           DO GAM = 1,MAXGAM
              GTYP = GNTTAB(GAMTYP,GAM)
              IF(GTYP.EQ.TSCR.AND.VALID_DRW(GAM,IND).NE.0) THEN
                 IF(FIRST_SCR) THEN
                     WRITE (HEAD,9000) 'TULOSVETO  ',WEEK, YEAR
                     CALL TITLE(HEAD,'VEDONLY',1,REPLU,PAGE,DAYCDC)
                     WRITE(REPLU,9100)
                     FIRST_SCR = .FALSE.
                 ENDIF
                 GIND = GNTTAB(GAMIDX,GAM)
                 CALL FASTSET(0,D_DATE,6)
                 D_DATE(VCDC) = VALID_DAT(GAM,IND)
                 CALL LCDATE(D_DATE)
                 IF(VALID_WIN(1,GAM,IND).GE.0) THEN
                    IF(VALID_STS(GAM,IND).EQ.GFINAL) THEN
                       SCR_COUNT = SCR_COUNT + 1
                       TOT_SAL(GTYP) = TOT_SAL(GTYP) + VALID_SAL(GAM,IND)
                       TOT_REF(GTYP) = TOT_REF(GTYP) + VALID_REF(GAM,IND)
                       TOT_WON(GTYP) = TOT_WON(GTYP) + VALID_WON(GAM,IND)
                       TOT_WON_C(GTYP) = TOT_WON_C(GTYP) + VALID_WPR(GAM,IND)
                       IF(VALID_SAL(GAM,IND).EQ.VALID_REF(GAM,IND)) THEN
                          WINPER = 0.0
                       ELSE
                          WINPER = (FLOAT(VALID_WON(GAM,IND))/
     *                                   (VALID_SAL(GAM,IND)-
     *                                    VALID_REF(GAM,IND)))*100
                       ENDIF
                       WRITE(REPLU,9101) SCR_COUNT,
     *                    GIND,VALID_DRW(GAM,IND),
     *                    (D_DATE(K),K=7,13),
     *                    CMONY(VALID_SAL(GAM,IND),12,BETUNIT),
     *                    CMONY(VALID_REF(GAM,IND),12,BETUNIT),
     *                    CMONY(VALID_SAL(GAM,IND)-VALID_REF(GAM,IND),12,
     *                          BETUNIT),
     *                    VALID_WPA(GAM,IND),
     *                    VALID_WPR(GAM,IND),
     *                    CMONY(VALID_WON(GAM,IND)-VALID_REF(GAM,IND),12,
     *                          VALUNIT),
     *                     WINPER,
     *                    (VALID_NM1(K,GAM,IND),K=1,SNMS_LEN/4),
     *                    (VALID_NM2(K,GAM,IND),K=1,SNMS_LEN/4),
     *                    VALID_WIN(1,GAM,IND),
     *                    VALID_WIN(2,GAM,IND),
     *                    VALID_ODS(1,GAM,IND)/100,
     *                    MOD(VALID_ODS(1,GAM,IND),100)
                    ELSE
                       SCR_COUNT = SCR_COUNT + 1
                       WRITE(REPLU,91011)SCR_COUNT,  !not final
     *                    GIND,VALID_DRW(GAM,IND),
     *                    (D_DATE(K),K=7,13),
     *                    (VALID_NM1(K,GAM,IND),K=1,SNMS_LEN/4),
     *                    (VALID_NM2(K,GAM,IND),K=1,SNMS_LEN/4)
                    ENDIF
                 ELSE
                    SCR_COUNT = SCR_COUNT + 1
                    TOT_SAL(GTYP) = TOT_SAL(GTYP) + VALID_SAL(GAM,IND)
                    TOT_REF(GTYP) = TOT_REF(GTYP) + VALID_REF(GAM,IND)
                    WINPER = 0
                    WRITE(REPLU,91012) SCR_COUNT,   !cancelled
     *                 GIND,VALID_DRW(GAM,IND),
     *                 (D_DATE(K),K=7,13),
     *                 CMONY(VALID_SAL(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_REF(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_SAL(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 VALID_WPA(GAM,IND),
     *                 VALID_WPR(GAM,IND),
     *                 CMONY(VALID_WON(GAM,IND)-VALID_REF(GAM,IND),12,VALUNIT),
     *                 WINPER,
     *                 (VALID_NM1(K,GAM,IND),K=1,SNMS_LEN/4),
     *                 (VALID_NM2(K,GAM,IND),K=1,SNMS_LEN/4),
     *                 VALID_ODS(1,GAM,IND)/100,
     *                 MOD(VALID_ODS(1,GAM,IND),100)
                 ENDIF
              ENDIF
           ENDDO
        ENDDO

        IF(TOT_SAL(TSCR).EQ.TOT_REF(TSCR)) THEN
           TOTPER = 0.0
        ELSE
           TOTPER = FLOAT(TOT_WON(TSCR))/
     *              FLOAT(TOT_SAL(TSCR)-TOT_REF(TSCR))*100.0
        ENDIF
        IF(.NOT.FIRST_SCR)
     *     WRITE(REPLU,9001) CMONY(TOT_SAL(TSCR),12,BETUNIT),
     *                    CMONY(TOT_REF(TSCR),12,BETUNIT),
     *                    CMONY(TOT_SAL(TSCR)-TOT_REF(TSCR),12,BETUNIT),
     *                    TOT_WON_C(TSCR),
     *                    CMONY(TOT_WON(TSCR),12,VALUNIT),
     *                    TOTPER
C
C Super Score game...
C
        DO IND = 1,7      ! day of the week
           DO 100 GAM = 1,MAXGAM
              GTYP = GNTTAB(GAMTYP,GAM)
              IF(GTYP.NE.TSSC.OR.VALID_DRW(GAM,IND).EQ.0) GOTO 100
	      IF(VALID_STS(GAM,IND).EQ.GAMNUL) GOTO 100
              IF(FIRST_SSC) THEN
                     WRITE (HEAD,9000) 'MONIVETO   ',WEEK, YEAR
                     CALL TITLE(HEAD,'VEDONLY',1,REPLU,PAGE,DAYCDC)
                     WRITE(REPLU,9100)
                     FIRST_SSC = .FALSE.
              ENDIF
                 GIND = GNTTAB(GAMIDX,GAM)
                 CALL FASTSET(0,D_DATE,6)
                 D_DATE(VCDC) = VALID_DAT(GAM,IND)
                 CALL LCDATE(D_DATE)
                 IF(VALID_STS(GAM,IND).EQ.GFINAL) THEN
                       SSC_COUNT = SSC_COUNT + 1
                       TOT_SAL(GTYP) = TOT_SAL(GTYP) + VALID_SAL(GAM,IND)
                       TOT_REF(GTYP) = TOT_REF(GTYP) + VALID_REF(GAM,IND)
                       TOT_WON(GTYP) = TOT_WON(GTYP) + VALID_WON(GAM,IND)
                       TOT_WON_C(GTYP) = TOT_WON_C(GTYP) + VALID_WPR(GAM,IND)
                       IF(VALID_SAL(GAM,IND).EQ.VALID_REF(GAM,IND)) THEN
                          WINPER = 0.0
                       ELSE
                          WINPER = (FLOAT(VALID_WON(GAM,IND))/
     *                                   (VALID_SAL(GAM,IND)-
     *                                    VALID_REF(GAM,IND)))*100
                       ENDIF
                       WRITE(REPLU,9501) SSC_COUNT,
     *                    GIND,VALID_DRW(GAM,IND),
     *                    (D_DATE(K),K=7,13),
     *                    CMONY(VALID_SAL(GAM,IND),12,BETUNIT),
     *                    CMONY(VALID_REF(GAM,IND),12,BETUNIT),
     *                    CMONY(VALID_SAL(GAM,IND)-VALID_REF(GAM,IND),12,
     *                          BETUNIT),
     *                    VALID_WPA(GAM,IND),
     *                    VALID_WPR(GAM,IND),
     *                    CMONY(VALID_WON(GAM,IND)-VALID_REF(GAM,IND),12,
     *                          VALUNIT),
     *                     WINPER
			DO I=1,3
			   IF(VALID_EVENT_STS(I,GAM,IND).NE.GAMNUL) THEN
			    WRITE(REPLU,9502)
     *                       (VALID_SSC_NMS(K,I,GAM,IND),K=1,SSNMS_LEN/4),
     *                       (VALID_WIN(K,GAM,IND),K=2*I-1,2*I)
			  ENDIF
	                ENDDO
	                WRITE(REPLU,9503)
     *                    (VALID_SSC_NMS(K,4,GAM,IND),K=1,SSNMS_LEN/4),
     *                    VALID_ODS(1,GAM,IND)/100,
     *                    MOD(VALID_ODS(1,GAM,IND),100)
                  ELSEIF(VALID_STS(GAM,IND).LT.GFINAL) THEN
                       SSC_COUNT = SSC_COUNT + 1
                       WRITE(REPLU,95011)SSC_COUNT,  !not final
     *                    GIND,VALID_DRW(GAM,IND),
     *                    (D_DATE(K),K=7,13),
     *                    (VALID_SSC_NMS(K,1,GAM,IND),K=1,SSNMS_LEN/4),
     *                    (VALID_SSC_NMS(K,2,GAM,IND),K=1,SSNMS_LEN/4),
     *                    (VALID_SSC_NMS(K,3,GAM,IND),K=1,SSNMS_LEN/4),
     *                    (VALID_SSC_NMS(K,4,GAM,IND),K=1,SSNMS_LEN/4)
                  ELSEIF(VALID_STS(GAM,IND).EQ.GAMCAN.OR.
     *                   VALID_STS(GAM,IND).EQ.GAMREF) THEN
                    SSC_COUNT = SSC_COUNT + 1
                    WINPER = 0
                    TOT_SAL(GTYP) = TOT_SAL(GTYP) + VALID_SAL(GAM,IND)
                    TOT_REF(GTYP) = TOT_REF(GTYP) + VALID_REF(GAM,IND)
                    WRITE(REPLU,95012) SSC_COUNT,   !cancelled
     *                 GIND,VALID_DRW(GAM,IND),
     *                 (D_DATE(K),K=7,13),
     *                 CMONY(VALID_SAL(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_REF(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_SAL(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 VALID_WPA(GAM,IND),
     *                 VALID_WPR(GAM,IND),
     *                 CMONY(VALID_WON(GAM,IND)-VALID_REF(GAM,IND),12,VALUNIT),
     *                 WINPER,
     *                 (VALID_SSC_NMS(K,1,GAM,IND),K=1,SSNMS_LEN/4),
     *                 (VALID_SSC_NMS(K,2,GAM,IND),K=1,SSNMS_LEN/4),
     *                 (VALID_SSC_NMS(K,3,GAM,IND),K=1,SSNMS_LEN/4),
     *                 (VALID_SSC_NMS(K,4,GAM,IND),K=1,SSNMS_LEN/4),
     *                 VALID_ODS(1,GAM,IND)/100,
     *                 MOD(VALID_ODS(1,GAM,IND),100)
                ENDIF
100	   CONTINUE
        ENDDO

        IF(TOT_SAL(TSSC).EQ.TOT_REF(TSSC)) THEN
           TOTPER = 0.0
        ELSE
           TOTPER = FLOAT(TOT_WON(TSSC))/
     *              FLOAT(TOT_SAL(TSSC)-TOT_REF(TSSC))*100.0
        ENDIF
        IF(.NOT.FIRST_SSC)
     *     WRITE(REPLU,9001) CMONY(TOT_SAL(TSSC),12,BETUNIT),
     *                    CMONY(TOT_REF(TSSC),12,BETUNIT),
     *                    CMONY(TOT_SAL(TSSC)-TOT_REF(TSSC),12,BETUNIT),
     *                    TOT_WON_C(TSSC),
     *                    CMONY(TOT_WON(TSSC),12,VALUNIT),
     *                    TOTPER

C---- Winner tip game.

        DO IND = 1,7      ! day of the week
           DO GAM = 1,MAXGAM
              GTYP = GNTTAB(GAMTYP,GAM)
              IF(GTYP.EQ.TWIT.AND.VALID_DRW(GAM,IND).NE.0) THEN
                 IF(FIRST_WIT) THEN
                     WRITE (HEAD,9000) 'VOITTAJAVETO  ',WEEK,YEAR
                     CALL TITLE(HEAD,'VEDONLY',2,REPLU,PAGE,DAYCDC)
                     WRITE(REPLU,9200)
                     FIRST_WIT = .FALSE.
                 ENDIF
                 GIND = GNTTAB(GAMIDX,GAM)
                 CALL FASTSET(0,D_DATE,6)
                 D_DATE(VCDC) = VALID_DAT(GAM,IND)
                 CALL LCDATE(D_DATE)
                 IF(VALID_STS(GAM,IND).EQ.GFINAL) THEN
                    IF(VALID_SAL(GAM,IND).EQ.VALID_REF(GAM,IND)) THEN
                       WINPER = 0.0
                    ELSE
                       WINPER = (FLOAT(VALID_WON(GAM,IND))/
     *                                 (VALID_SAL(GAM,IND)-
     *                                  VALID_REF(GAM,IND)))*100
                    ENDIF
                    TOT_SAL(GTYP) = TOT_SAL(GTYP) + VALID_SAL(GAM,IND)
                    TOT_REF(GTYP) = TOT_REF(GTYP) + VALID_REF(GAM,IND)
                    TOT_WON(GTYP) = TOT_WON(GTYP) + VALID_WON(GAM,IND)
                    TOT_WON_C(GTYP) = TOT_WON_C(GTYP) + VALID_WPR(GAM,IND)
                    WIT_COUNT = WIT_COUNT + 1
                    WRITE(REPLU,9201) WIT_COUNT,
     *                 GIND,VALID_DRW(GAM,IND),
     *                 (D_DATE(K),K=7,13),
     *                 CMONY(VALID_SAL(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_REF(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_SAL(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 VALID_WPA(GAM,IND),
     *                 VALID_WPR(GAM,IND),
     *                 CMONY(VALID_WON(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 WINPER,
     *                 (VALID_ENM(K,GAM,IND),K=1,WENM_LEN/4),
     *                 VALID_WIN(1,GAM,IND),
     *                 (VALID_NMS(K,1,GAM,IND),K=1,WNMS_LEN/4),
     *                 VALID_ODS(1,GAM,IND)/100,
     *                 MOD(VALID_ODS(1,GAM,IND),100)
                    DO I=2,4
                       IF(VALID_WIN(I,GAM,IND).NE.0)
     *                    WRITE(REPLU,92011)VALID_WIN(I,GAM,IND),
     *                       (VALID_NMS(K,I,GAM,IND),K=1,WNMS_LEN/4),
     *                       VALID_ODS(I,GAM,IND)/100,
     *                       MOD(VALID_ODS(I,GAM,IND),100)
                    ENDDO
                 ELSEIF(VALID_STS(GAM,IND).EQ.GAMCAN.OR.
     *                  VALID_STS(GAM,IND).EQ.GAMREF) THEN
                    WIT_COUNT = WIT_COUNT + 1
                    TOT_SAL(GTYP) = TOT_SAL(GTYP) + VALID_SAL(GAM,IND)
                    TOT_REF(GTYP) = TOT_REF(GTYP) + VALID_REF(GAM,IND)
                    WRITE(REPLU,92014) WIT_COUNT,   !cancelled
     *                 GIND,VALID_DRW(GAM,IND),
     *                 (D_DATE(K),K=7,13),
     *                 CMONY(VALID_SAL(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_REF(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_SAL(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 VALID_WPA(GAM,IND),
     *                 (VALID_ENM(K,GAM,IND),K=1,WENM_LEN/4),
     *                 VALID_ODS(1,GAM,IND)/100,
     *                 MOD(VALID_ODS(1,GAM,IND),100)
                 ELSE
                    WIT_COUNT = WIT_COUNT + 1
                    WRITE(REPLU,92015) WIT_COUNT,           !not final
     *                    GIND,VALID_DRW(GAM,IND),
     *                    (D_DATE(K),K=7,13),
     *                    (VALID_ENM(K,GAM,IND),K=1,WENM_LEN/4)
                 ENDIF
              ENDIF
           ENDDO
        ENDDO

        IF(TOT_SAL(TWIT).EQ.TOT_REF(TWIT)) THEN
           TOTPER = 0.0
        ELSE
           TOTPER = FLOAT(TOT_WON(TWIT))/
     *              FLOAT(TOT_SAL(TWIT)-TOT_REF(TWIT))*100
        ENDIF
        IF(.NOT.FIRST_WIT)
     *     WRITE(REPLU,9001) CMONY(TOT_SAL(TWIT),12,BETUNIT),
     *                    CMONY(TOT_REF(TWIT),12,BETUNIT),
     *                    CMONY(TOT_SAL(TWIT)-TOT_REF(TWIT),12,BETUNIT),
     *                    TOT_WON_C(TWIT),
     *                    CMONY(TOT_WON(TWIT),12,VALUNIT),
     *                    TOTPER

C---- Super double game.

        DO IND = 1,7      ! day of the week
           DO GAM = 1,MAXGAM
              GTYP = GNTTAB(GAMTYP,GAM)
              IF(GTYP.EQ.TDBL.AND.VALID_DRW(GAM,IND).NE.0) THEN
                 IF(FIRST_DBL) THEN
                     WRITE (HEAD,9000) 'SUPER KAKSARI ',WEEK,YEAR
                     CALL TITLE(HEAD,'VEDONLY',3,REPLU,PAGE,DAYCDC)
                     WRITE(REPLU,9300)
                     FIRST_DBL = .FALSE.
                 ENDIF
                 GIND = GNTTAB(GAMIDX,GAM)
                 CALL FASTSET(0,D_DATE,6)
                 D_DATE(VCDC) = VALID_DAT(GAM,IND)
                 CALL LCDATE(D_DATE)
                 IF(VALID_STS(GAM,IND).EQ.GFINAL) THEN
                    IF(VALID_SAL(GAM,IND).EQ.VALID_REF(GAM,IND)) THEN
                       WINPER = 0.0
                    ELSE
                       WINPER = (FLOAT(VALID_WON(GAM,IND))/
     *                                 (VALID_SAL(GAM,IND)-
     *                                  VALID_REF(GAM,IND)))*100
                    ENDIF
                    TOT_SAL(GTYP) = TOT_SAL(GTYP) + VALID_SAL(GAM,IND)
                    TOT_REF(GTYP) = TOT_REF(GTYP) + VALID_REF(GAM,IND)
                    TOT_WON(GTYP) = TOT_WON(GTYP) + VALID_WON(GAM,IND)
                    TOT_WON_C(GTYP) = TOT_WON_C(GTYP) + VALID_WPR(GAM,IND)
                    DBL_COUNT = DBL_COUNT + 1
                    WRITE(REPLU,9301) DBL_COUNT,
     *                 GIND,VALID_DRW(GAM,IND),
     *                 (D_DATE(K),K=7,13),
     *                 CMONY(VALID_SAL(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_REF(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_SAL(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 VALID_WPA(GAM,IND),
     *                 VALID_WPR(GAM,IND),
     *                 CMONY(VALID_WON(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 WINPER
	            DO I=1,VALID_CMB(GAM,IND)
                       WRITE(REPLU,9302)
     *                 (VALID_DENM(K,GAM,IND),K=1,DBLENM_LEN/4),
     *                 VALID_WIN(I,GAM,IND),
     *                 (VALID_DNMS(K,I,GAM,IND),K=1,DBLNMS_LEN/4),
     *                 VALID_ODS(I,GAM,IND)/100,
     *                 MOD(VALID_ODS(I,GAM,IND),100),
     *                 VALID_2ND(I,GAM,IND),
     *                 (VALID_D2NMS(K,I,GAM,IND),K=1,DBLNMS_LEN/4)
                    ENDDO
                 ELSEIF(VALID_STS(GAM,IND).EQ.GAMCAN.OR.
     *                  VALID_STS(GAM,IND).EQ.GAMREF) THEN
                    DBL_COUNT = DBL_COUNT + 1
                    TOT_SAL(GTYP) = TOT_SAL(GTYP) + VALID_SAL(GAM,IND)
                    TOT_REF(GTYP) = TOT_REF(GTYP) + VALID_REF(GAM,IND)
                    WRITE(REPLU,93014) DBL_COUNT,   !cancelled
     *                 GIND,VALID_DRW(GAM,IND),
     *                 (D_DATE(K),K=7,13),
     *                 CMONY(VALID_SAL(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_REF(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_SAL(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 VALID_WPA(GAM,IND),
     *                 (VALID_DENM(K,GAM,IND),K=1,DBLENM_LEN/4),
     *                 VALID_ODS(1,GAM,IND)/100,
     *                 MOD(VALID_ODS(1,GAM,IND),100)
                 ELSE
                    DBL_COUNT = DBL_COUNT + 1
                    WRITE(REPLU,93015) DBL_COUNT,      !not final
     *                   GIND,VALID_DRW(GAM,IND),
     *                   (D_DATE(K),K=7,13),
     *                   (VALID_DENM(K,GAM,IND),K=1,DBLENM_LEN/4)
                 ENDIF
              ENDIF
           ENDDO
        ENDDO

        IF(TOT_SAL(TDBL).EQ.TOT_REF(TDBL)) THEN
           TOTPER = 0.0
        ELSE
           TOTPER = FLOAT(TOT_WON(TDBL))/
     *              FLOAT(TOT_SAL(TDBL)-TOT_REF(TDBL))*100
        ENDIF
        IF(.NOT.FIRST_DBL)
     *     WRITE(REPLU,9001) CMONY(TOT_SAL(TDBL),12,BETUNIT),
     *                    CMONY(TOT_REF(TDBL),12,BETUNIT),
     *                    CMONY(TOT_SAL(TDBL)-TOT_REF(TDBL),12,BETUNIT),
     *                    TOT_WON_C(TDBL),
     *                    CMONY(TOT_WON(TDBL),12,VALUNIT),
     *                    TOTPER

C---- Super Triple game.

        DO IND = 1,7      ! day of the week
           DO GAM = 1,MAXGAM
              GTYP = GNTTAB(GAMTYP,GAM)
              IF(GTYP.EQ.TSTR.AND.VALID_DRW(GAM,IND).NE.0) THEN
                 IF(FIRST_STR) THEN
                     WRITE (HEAD,9000) 'SUPER TRIPLA ',WEEK,YEAR
                     CALL TITLE(HEAD,'VEDONLY',3,REPLU,PAGE,DAYCDC)
                     WRITE(REPLU,9303)
                     FIRST_STR = .FALSE.
                 ENDIF
                 GIND = GNTTAB(GAMIDX,GAM)
                 CALL FASTSET(0,D_DATE,6)
                 D_DATE(VCDC) = VALID_DAT(GAM,IND)
                 CALL LCDATE(D_DATE)
                 IF(VALID_STS(GAM,IND).EQ.GFINAL) THEN
                    IF(VALID_SAL(GAM,IND).EQ.VALID_REF(GAM,IND)) THEN
                       WINPER = 0.0
                    ELSE
                       WINPER = (FLOAT(VALID_WON(GAM,IND))/
     *                                 (VALID_SAL(GAM,IND)-
     *                                  VALID_REF(GAM,IND)))*100
                    ENDIF
                    TOT_SAL(GTYP) = TOT_SAL(GTYP) + VALID_SAL(GAM,IND)
                    TOT_REF(GTYP) = TOT_REF(GTYP) + VALID_REF(GAM,IND)
                    TOT_WON(GTYP) = TOT_WON(GTYP) + VALID_WON(GAM,IND)
                    TOT_WON_C(GTYP) = TOT_WON_C(GTYP) + VALID_WPR(GAM,IND)
                    STR_COUNT = STR_COUNT + 1

                    WRITE(REPLU,9801) STR_COUNT,
     *                 GIND,VALID_DRW(GAM,IND),
     *                 (D_DATE(K),K=7,13),
     *                 CMONY(VALID_SAL(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_REF(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_SAL(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 VALID_WPA(GAM,IND),
     *                 VALID_WPR(GAM,IND),
     *                 CMONY(VALID_WON(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 WINPER
		    DO I=1,VALID_CMB(GAM,IND)
		       WRITE(REPLU,9802) 
     *                 (VALID_STRENM(K,GAM,IND),K=1,STRENM_LEN/4),
     *                 VALID_WIN(I,GAM,IND),
     *                 (VALID_STRNMS(K,I,GAM,IND),K=1,STRNMS_LEN/4),
     *                 VALID_ODS(I,GAM,IND)/100,
     *                 MOD(VALID_ODS(I,GAM,IND),100),
     *                 VALID_2ND(I,GAM,IND),
     *                 (VALID_STR2NMS(K,I,GAM,IND),K=1,STRNMS_LEN/4),
     *                 VALID_3RD(I,GAM,IND),
     *                 (VALID_STR3NMS(K,I,GAM,IND),K=1,STRNMS_LEN/4)
		    ENDDO
                 ELSEIF(VALID_STS(GAM,IND).EQ.GAMCAN.OR.
     *                  VALID_STS(GAM,IND).EQ.GAMREF) THEN
                    STR_COUNT = STR_COUNT + 1
                    TOT_SAL(GTYP) = TOT_SAL(GTYP) + VALID_SAL(GAM,IND)
                    TOT_REF(GTYP) = TOT_REF(GTYP) + VALID_REF(GAM,IND)
                    WRITE(REPLU,93014) STR_COUNT,   !cancelled
     *                 GIND,VALID_DRW(GAM,IND),
     *                 (D_DATE(K),K=7,13),
     *                 CMONY(VALID_SAL(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_REF(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_SAL(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 VALID_WPA(GAM,IND),
     *                 (VALID_STRENM(K,GAM,IND),K=1,STRENM_LEN/4),
     *                 VALID_ODS(1,GAM,IND)/100,
     *                 MOD(VALID_ODS(1,GAM,IND),100)
                 ELSE
                    STR_COUNT = STR_COUNT + 1
                    WRITE(REPLU,93015) STR_COUNT,      !not final
     *                   GIND,VALID_DRW(GAM,IND),
     *                   (D_DATE(K),K=7,13),
     *                   (VALID_STRENM(K,GAM,IND),K=1,STRENM_LEN/4)
                 ENDIF
              ENDIF
           ENDDO
        ENDDO

        IF(TOT_SAL(TSTR).EQ.TOT_REF(TSTR)) THEN
           TOTPER = 0.0
        ELSE
           TOTPER = FLOAT(TOT_WON(TSTR))/
     *              FLOAT(TOT_SAL(TSTR)-TOT_REF(TSTR))*100
        ENDIF
        IF(.NOT.FIRST_STR)
     *     WRITE(REPLU,9001) CMONY(TOT_SAL(TSTR),12,BETUNIT),
     *                    CMONY(TOT_REF(TSTR),12,BETUNIT),
     *                    CMONY(TOT_SAL(TSTR)-TOT_REF(TSTR),12,BETUNIT),
     *                    TOT_WON_C(TSTR),
     *                    CMONY(TOT_WON(TSTR),12,VALUNIT),
     *                    TOTPER

C---- Todays Couple game.

        DO IND = 1,7      ! day of the week
           DO GAM = 1,MAXGAM
              GTYP = GNTTAB(GAMTYP,GAM)
              IF(GTYP.EQ.TCPL.AND.VALID_DRW(GAM,IND).NE.0) THEN
                 IF(FIRST_CPL) THEN
                     WRITE (HEAD,9000) 'PAIVAN PARI ',WEEK,YEAR
                     CALL TITLE(HEAD,'VEDONLY',2,REPLU,PAGE,DAYCDC)
                     WRITE(REPLU,9400)
                     FIRST_CPL = .FALSE.
                 ENDIF
                 GIND = GNTTAB(GAMIDX,GAM)
                 CALL FASTSET(0,D_DATE,6)
                 D_DATE(VCDC) = VALID_DAT(GAM,IND)
                 CALL LCDATE(D_DATE)
                 IF(VALID_STS(GAM,IND).EQ.GFINAL) THEN
                    IF(VALID_SAL(GAM,IND).EQ.VALID_REF(GAM,IND)) THEN
                       WINPER = 0.0
                    ELSE
                       WINPER = (FLOAT(VALID_WON(GAM,IND))/
     *                                 (VALID_SAL(GAM,IND)-
     *                                  VALID_REF(GAM,IND)))*100
                    ENDIF
                    TOT_SAL(GTYP) = TOT_SAL(GTYP) + VALID_SAL(GAM,IND)
                    TOT_REF(GTYP) = TOT_REF(GTYP) + VALID_REF(GAM,IND)
                    TOT_WON(GTYP) = TOT_WON(GTYP) + VALID_WON(GAM,IND)
                    TOT_WON_C(GTYP) = TOT_WON_C(GTYP) + VALID_WPR(GAM,IND)
                    CPL_COUNT = CPL_COUNT + 1
                    WRITE(REPLU,9401) CPL_COUNT,
     *                 GIND,VALID_DRW(GAM,IND),
     *                 (D_DATE(K),K=7,13),
     *                 CMONY(VALID_SAL(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_REF(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_SAL(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 VALID_WPA(GAM,IND),
     *                 VALID_WPR(GAM,IND),
     *                 CMONY(VALID_WON(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 WINPER
		    DO I=1,VALID_CMB(GAM,IND)
		       WRITE(REPLU,9402)	 
     *                 (VALID_C1ENM(K,GAM,IND),K=1,CPLENM_LEN/4),
     *                 VALID_WIN(I,GAM,IND),
     *                 (VALID_CNMS(K,I,GAM,IND),K=1,CPLNMS_LEN/4),
     *                 VALID_ODS(I,GAM,IND)/100,
     *                 MOD(VALID_ODS(I,GAM,IND),100),
     *                 (VALID_C2ENM(K,GAM,IND),K=1,CPLENM_LEN/4),
     *                 VALID_2ND(I,GAM,IND)-MAXCPLRW/2,
     *                 (VALID_C2NMS(K,I,GAM,IND),K=1,CPLNMS_LEN/4)
                    ENDDO
                 ELSEIF(VALID_STS(GAM,IND).EQ.GAMCAN.OR.
     *                  VALID_STS(GAM,IND).EQ.GAMREF) THEN
                    CPL_COUNT = CPL_COUNT + 1
                    TOT_SAL(GTYP) = TOT_SAL(GTYP) + VALID_SAL(GAM,IND)
                    TOT_REF(GTYP) = TOT_REF(GTYP) + VALID_REF(GAM,IND)
                    WRITE(REPLU,94014) CPL_COUNT,   !cancelled
     *                 GIND,VALID_DRW(GAM,IND),
     *                 (D_DATE(K),K=7,13),
     *                 CMONY(VALID_SAL(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_REF(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_SAL(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 VALID_WPA(GAM,IND),
     *                 VALID_WPR(GAM,IND),
     *                 CMONY(VALID_WON(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 WINPER,
     *                 'A',(VALID_C1ENM(K,GAM,IND),K=1,CPLENM_LEN/4),
     *                 VALID_ODS(1,GAM,IND)/100,
     *                 MOD(VALID_ODS(1,GAM,IND),100),
     *                 'B',(VALID_C2ENM(K,GAM,IND),K=1,CPLENM_LEN/4)
                 ELSE
                    CPL_COUNT = CPL_COUNT + 1
                    WRITE(REPLU,94015) CPL_COUNT,          !not final
     *                    GIND,VALID_DRW(GAM,IND),
     *                    (D_DATE(K),K=7,13),
     *                    'A',(VALID_C1ENM(K,GAM,IND),K=1,CPLENM_LEN/4),
     *                    'B',(VALID_C2ENM(K,GAM,IND),K=1,CPLENM_LEN/4)
                 ENDIF
              ENDIF
           ENDDO
        ENDDO

        IF(TOT_SAL(TCPL).EQ.TOT_REF(TCPL)) THEN
           TOTPER = 0.0
        ELSE
           TOTPER = FLOAT(TOT_WON(TCPL))/
     *              FLOAT(TOT_SAL(TCPL)-TOT_REF(TCPL))*100
        ENDIF
        IF(.NOT.FIRST_CPL)
     *     WRITE(REPLU,9001) CMONY(TOT_SAL(TCPL),12,BETUNIT),
     *                    CMONY(TOT_REF(TCPL),12,BETUNIT),
     *                    CMONY(TOT_SAL(TCPL)-TOT_REF(TCPL),12,BETUNIT),
     *                    TOT_WON_C(TCPL),
     *                    CMONY(TOT_WON(TCPL),12,VALUNIT),
     *                    TOTPER


C---- Todays Triple game.

        DO IND = 1,7      ! day of the week
           DO GAM = 1,MAXGAM
              GTYP = GNTTAB(GAMTYP,GAM)
              IF(GTYP.EQ.TTRP.AND.VALID_DRW(GAM,IND).NE.0) THEN
                 IF(FIRST_TRP) THEN
                     WRITE (HEAD,9000) 'PAIVAN TRIO ',WEEK,YEAR
                     CALL TITLE(HEAD,'VEDONLY',2,REPLU,PAGE,DAYCDC)
                     WRITE(REPLU,9600)
                     FIRST_TRP = .FALSE.
                 ENDIF
                 GIND = GNTTAB(GAMIDX,GAM)
                 CALL FASTSET(0,D_DATE,6)
                 D_DATE(VCDC) = VALID_DAT(GAM,IND)
                 CALL LCDATE(D_DATE)
                 IF(VALID_STS(GAM,IND).EQ.GFINAL) THEN
                    IF(VALID_SAL(GAM,IND).EQ.VALID_REF(GAM,IND)) THEN
                       WINPER = 0.0
                    ELSE
                       WINPER = (FLOAT(VALID_WON(GAM,IND))/
     *                                 (VALID_SAL(GAM,IND)-
     *                                  VALID_REF(GAM,IND)))*100
                    ENDIF
                    TOT_SAL(GTYP) = TOT_SAL(GTYP) + VALID_SAL(GAM,IND)
                    TOT_REF(GTYP) = TOT_REF(GTYP) + VALID_REF(GAM,IND)
                    TOT_WON(GTYP) = TOT_WON(GTYP) + VALID_WON(GAM,IND)
                    TOT_WON_C(GTYP) = TOT_WON_C(GTYP) + VALID_WPR(GAM,IND)
                    TRP_COUNT = TRP_COUNT + 1
C
                    WRITE(REPLU,9601) TRP_COUNT,
     *                 GIND,VALID_DRW(GAM,IND),
     *                 (D_DATE(K),K=7,13),
     *                 CMONY(VALID_SAL(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_REF(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_SAL(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 VALID_WPA(GAM,IND),
     *                 VALID_WPR(GAM,IND),
     *                 CMONY(VALID_WON(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 WINPER
		    DO I = 1, VALID_CMB(GAM,IND)
	              WRITE(REPLU,9602) 
     *                 'A',(VALID_TRP_ENM(K,1,GAM,IND),K=1,TRPENM_LEN/4),
     *                 'A',VALID_WIN(I,GAM,IND),
     *                 (VALID_TRP_NMS(K,I,1,GAM,IND),K=1,TRPNMS_LEN/4),
     *                 VALID_TRP_ODS(I,GAM,IND)/100,
     *                 MOD(VALID_TRP_ODS(I,GAM,IND),100),
     *                 'B',(VALID_TRP_ENM(K,2,GAM,IND),K=1,TRPENM_LEN/4),
     *                 'B',VALID_2ND(I,GAM,IND),
     *                 (VALID_TRP_NMS(K,I,2,GAM,IND),K=1,TRPNMS_LEN/4),
     *                 'C',(VALID_TRP_ENM(K,3,GAM,IND),K=1,TRPENM_LEN/4),
     *                 'C',VALID_3RD(I,GAM,IND),
     *                 (VALID_TRP_NMS(K,I,3,GAM,IND),K=1,TRPNMS_LEN/4)
		    ENDDO
                 ELSEIF(VALID_STS(GAM,IND).EQ.GAMCAN.OR.
     *                  VALID_STS(GAM,IND).EQ.GAMREF) THEN
                    TRP_COUNT = TRP_COUNT + 1
                    TOT_SAL(GTYP) = TOT_SAL(GTYP) + VALID_SAL(GAM,IND)
                    TOT_REF(GTYP) = TOT_REF(GTYP) + VALID_REF(GAM,IND)
                    WRITE(REPLU,96014) TRP_COUNT,   !cancelled
     *                 GIND,VALID_DRW(GAM,IND),
     *                 (D_DATE(K),K=7,13),
     *                 CMONY(VALID_SAL(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_REF(GAM,IND),12,BETUNIT),
     *                 CMONY(VALID_SAL(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 VALID_WPA(GAM,IND),
     *                 VALID_WPR(GAM,IND),
     *                 CMONY(VALID_WON(GAM,IND)-VALID_REF(GAM,IND),12,BETUNIT),
     *                 WINPER,
     *                 'A',(VALID_TRP_ENM(K,1,GAM,IND),K=1,TRPENM_LEN/4),
     *                 VALID_TRP_ODS(1,GAM,IND)/100,
     *                 MOD(VALID_TRP_ODS(1,GAM,IND),100),
     *                 'B',(VALID_TRP_ENM(K,2,GAM,IND),K=1,TRPENM_LEN/4),
     *                 'C',(VALID_TRP_ENM(K,3,GAM,IND),K=1,TRPENM_LEN/4)
                 ELSE
                    TRP_COUNT = TRP_COUNT + 1
                    WRITE(REPLU,96015) TRP_COUNT,          !not final
     *                    GIND,VALID_DRW(GAM,IND),
     *                    (D_DATE(K),K=7,13),
     *                    'A',(VALID_TRP_ENM(K,1,GAM,IND),K=1,TRPENM_LEN/4),
     *                    'B',(VALID_TRP_ENM(K,2,GAM,IND),K=1,TRPENM_LEN/4),
     *                    'C',(VALID_TRP_ENM(K,3,GAM,IND),K=1,TRPENM_LEN/4)
                 ENDIF
              ENDIF
           ENDDO
        ENDDO

        IF(TOT_SAL(TTRP).EQ.TOT_REF(TTRP)) THEN
           TOTPER = 0.0
        ELSE
           TOTPER = FLOAT(TOT_WON(TTRP))/
     *              FLOAT(TOT_SAL(TTRP)-TOT_REF(TTRP))*100
        ENDIF
        IF(.NOT.FIRST_TRP)
     *     WRITE(REPLU,9001) CMONY(TOT_SAL(TTRP),12,BETUNIT),
     *                    CMONY(TOT_REF(TTRP),12,BETUNIT),
     *                    CMONY(TOT_SAL(TTRP)-TOT_REF(TTRP),12,BETUNIT),
     *                    TOT_WON_C(TTRP),
     *                    CMONY(TOT_WON(TTRP),12,VALUNIT),
     *                    TOTPER


	CALL SPOOL('WEEKSHR.REP',COPY,ST)
C
C     ===================== Format Statements =================
C
9000    FORMAT(A15,'VOITTOLASKELMA, WEEK ',I2.2,'/',I4.4)
9001    FORMAT(1X,130('-'),/,
     *         1X,'YHTEENSA',T49,3(1X,A12),T98,I7,1X,A12,2X,F8.2)

9100    FORMAT(1X,'Num Game Draw      Tulospaiva',T56,
     *        'Vaihto  Palautuksia  Nettovaihto   Osall.     Voittoja',/,
     *        1X,T4,'Kohde',T41,'Tulos',T62,'Kerroin',T92,'pelit',
     *        T102,'kpl        mk         %',/,1X,130('-')) 

9101    FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2,T49,3(1X,A12),
     *         T89,I8,T98,I7,1X,A12,2X,F8.2,/,
     *         T4,<SNMS_LEN/4>A4,T20,'-',T22,<SNMS_LEN/4>A4,T40,
     *         I2,' - 'I2,T60,I7,'.',I2.2)

91011   FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2/,
     *         T4,<SNMS_LEN/4>A4,
     *         T20,<SNMS_LEN/4>A4,T40,'Ei viela tuloksia')

91012   FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2,T49,3(1X,A12),
     *         T89,I8,T98,I7,1X,A12,2X,F8.2,/,
     *         T4,<SNMS_LEN/4>A4,T20,'-',T22,<SNMS_LEN/4>A4,T40,
     *         'Peruttu',T60,I7,'.',I2.2)

9200    FORMAT(1X,'Num Game Draw       Tulospaiva',T56,
     *        'Vaihto  Palautuksia  Nettovaihto   Osall.     Voittoja',/,
     *        1X,T4,'Kohde',T41,'Voittaja',T62,'Kerroin',T92,'pelit',
     *        T102,'kpl        mk         %',/,1X,130('-')) 

9201     FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2,T49,3(1X,A12),
     *         T89,I8,T98,I7,1X,A12,2X,F8.2,/,
     *         T4,<WENM_LEN/4>A4,T40,I2,1X,<WNMS_LEN/4>A4,T60,I7,'.',I2.2)

92011   FORMAT(1X,T40,I2,1X,<WNMS_LEN/4>A4,T60,I7,'.',I2.2)

92014   FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2,T49,3(1X,A12),T55,I7,/,
     *         T4,<WENM_LEN/4>A4,T40,'Peruttu',T60,I7,'.',I2.2)

92015   FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2,8X,'Ei viela tuloksia',/,
     *          T4,<WENM_LEN/4>A4)

9300    FORMAT(1X,'Num Game Draw'7X,'Tulospaiva',T41,'Voittaja',T56,
     *        'Vaihto  Palautuksia  Nettovaihto   Osall.     Voittoja',/,
     *        1X,T4,'Kohde',T41,'Toinen',T62,'Kerroin',T92,'pelit',
     *        T102,'kpl        mk         %',/,1X,130('-')) 

9301     FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2,T49,3(1X,A12),
     *         T89,I8,T98,I7,1X,A12,2X,F8.2)
9302     FORMAT(T4,<DBLENM_LEN/4>A4,
     *          T37,'1. ',I2,1X,<DBLNMS_LEN/4>A4,T60,I7,'.',I2.2,/,
     *          T37,'2. ',I2,1X,<DBLNMS_LEN/4>A4,/)

9303    FORMAT(1X,'Num Game Draw'7X,'Tulospaiva',T41,'Voittaja',T56,
     *        'Vaihto  Palautuksia  Nettovaihto   Osall.     Voittoja',/,
     *        1X,T4,'Kohde',T41,'Toinen',T62,'Kerroin',T92,'pelit',
     *        T102,'kpl        mk         %',/,T41,'Kolmas',/,1X,130('-')) 

93011   FORMAT(1X,T37,I1,'. ',I2,1X,<DBLNMS_LEN/4>A4,T60,I7,'.',I2.2,/,
     *            T37,I1,'. ',I2,1X,<DBLNMS_LEN/4>A4)

93014   FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2,T49,3(1X,A12),T55,I7,/,
     *         T4,<DBLENM_LEN/4>A4,T37,'Peruttu',T60,I7,'.',I2.2)

93015   FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2,8X,'Ei viela tuloksia',/,
     *          T4,<DBLENM_LEN/4>A4)

9400    FORMAT(1X,'Num Game Draw      Tulospaiva',T56,
     *        'Vaihto  Palautuksia  Nettovaihto   Osall.     Voittoja',/,
     *        1X,T4,'Kohde A',T41,'Voittaja',T62,'Kerroin',T92,'pelit',
     *        T102,'kpl        mk         %',/
     *        1X,T4,'Kohde B',T41,'Voittaja',/,1X,130('-')) 

9401    FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2,T49,3(1X,A12),
     *         T89,I8,T98,I7,1X,A12,2X,F8.2)
9402    FORMAT(T2,'A ',<CPLENM_LEN/4>A4,T38,'A ',I2,1X,<CPLNMS_LEN/4>A4,
     *         T60,I7,'.',I2.2,/,
     *         T2,'B ',<CPLENM_LEN/4>A4,T38,'B ',I2,1X,<CPLNMS_LEN/4>A4,/)

94011   FORMAT(1X,T38,A1,1X,I2,1X,<CPLNMS_LEN/4>A4,T60,I7,'.',I2.2,/,
     *            T38,A1,1X,I2,1X,<CPLNMS_LEN/4>A4)

94014   FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2,T49,3(1X,A12),
     *         T89,I8,T98,I7,1X,A12,2X,F8.2,/,
     *         T2,A1,1X,<CPLENM_LEN/4>A4,T40,'Peruttu',T60,I7,'.',I2.2,/,
     *         T2,A1,1X,<CPLENM_LEN/4>A4)

94015   FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2,8X,'Ei viela tuloksia',/,
     *         T2,A1,1X,<CPLENM_LEN/4>A4,/,T2,A1,1X,<CPLENM_LEN/4>A4)

9501    FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2,T49,3(1X,A12),
     *         T89,I8,T98,I7,1X,A12,2X,F8.2)
9502	FORMAT(4X,<SSNMS_LEN/4>A4,T40,I2,' - ',I2)
9503 	FORMAT(4X,<SSNMS_LEN/4>A4,T57,I8,'.',I2.2)
95011   FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2/,
     *         T4,<SSNMS_LEN/4>A4,/,
     *         T4,<SSNMS_LEN/4>A4,/,
     *         T4,<SSNMS_LEN/4>A4,/,
     *         T4,<SSNMS_LEN/4>A4,T60,'Ei viela tuloksia')
95012   FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2,T49,3(1X,A12),
     *         T89,I8,T98,I7,1X,A12,2X,F8.2,/,
     *         T4,<SSNMS_LEN/4>A4,/,
     *         T4,<SSNMS_LEN/4>A4,/,
     *         T4,<SSNMS_LEN/4>A4,/,
     *         T4,<SSNMS_LEN/4>A4,T40,
     *         'Peruttu',T60,I7,'.',I2.2)

9600    FORMAT(1X,'Num Game Draw      Tulospaiva',T56,
     *        'Vaihto  Palautuksia  Nettovaihto   Osall.     Voittoja',/,
     *        1X,T4,'Kohde A',T41,'Voittaja',T62,'Kerroin',T92,'pelit',
     *        T102,'kpl        mk         %',/,
     *        1X,T4,'Kohde B',T41,'Voittaja',/,
     *        1X,T4,'Kohde C',T41,'Voittaja',/,1X,130('-')) 

9601    FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2,T49,3(1X,A12),
     *         T89,I8,T98,I7,1X,A12,2X,F8.2)
9602	FORMAT(/,1X,T2,A1,1X,<TRPENM_LEN/4>A4,T38,A1,1X,I2,1X,<TRPNMS_LEN/4>A4,
     *         T60,I7,'.',I2.2,/,
     *         T2,A1,1X,<TRPENM_LEN/4>A4,T38,A1,1X,I2,1X,<TRPNMS_LEN/4>A4,/,
     *         T2,A1,1X,<TRPENM_LEN/4>A4,T38,A1,1X,I2,1X,<TRPNMS_LEN/4>A4)
96014   FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2,T49,3(1X,A12),
     *         T89,I8,T98,I7,1X,A12,2X,F8.2,/,
     *         T2,A1,1X,<TRPENM_LEN/4>A4,T40,'Peruttu',T60,I7,'.',I2.2,/,
     *         T2,A1,1X,<TRPENM_LEN/4>A4,/,T2,A1,1X,<TRPENM_LEN/4>A4)
96015   FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2,8X,'Ei viela tuloksia',/,
     *         T2,A1,1X,<TRPENM_LEN/4>A4,/,
     *         T2,A1,1X,<TRPENM_LEN/4>A4,/,
     *         T2,A1,1X,<TRPENM_LEN/4>A4)
9700	FORMAT(1X,A,'Starting CDC ',I4,2X,7A2)	
9701	FORMAT(1X,A,'Ending   CDC ',I4,2X,7A2)	
9801    FORMAT(/,1X,I2,3X,I2,1X,I4,6X,7A2,T49,3(1X,A12),
     *         T89,I8,T98,I7,1X,A12,2X,F8.2)
9802    FORMAT(T4,<STRENM_LEN/4>A4,
     *         T37,'1. ',I2,1X,<STRNMS_LEN/4>A4,T60,I7,'.',I2.2,/,
     *         T37,'2. ',I2,1X,<STRNMS_LEN/4>A4,/,
     *         T37,'3. ',I2,1X,<STRNMS_LEN/4>A4,/)
	END
