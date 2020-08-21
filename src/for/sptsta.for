C PROGRAM SPTSTA
C
C V13 05-DIC-2003 FRP Modify for Batch2 Totobola Changes.
C V12 31-JAN-2000 OXK Removed GIND=1 hardcoding etc. (Vakio changes)
C V11 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting,
C			Instant Pass Thru Phase 1
C V10 16 Jan 1996 RXK Rfss 95178. # of total boards calculated from sales now
C V09 02 Sep 1994 HXK Merge of May,June RFSS batch
C V08 27 Apr 1994 JXP COPY=0
C V07 17 Oct 1993 HXK ADDED GSTOP.
C V06 03 Sep 1993 SXH FIX PROBLEM WITH DRAW FILE VOLUME NAME
C V05 26 Aug 1993 SXH Added IAM() etc
C V04 19 Aug 1993 HXK fixed bugs, added carryover read
C V03 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C V02 01-AUG-1990 XXX RELEASED FOR VAX
C V01 03-MAR-1990 TDM INITIAL RELEASE FOR DENMARK
C
C SPORT STATISTICS REPORTS FOR ALL SPORT GAMES
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM SPTSTA
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

C
	INTEGER*4  TOTSEL                 !
	PARAMETER (TOTSEL = 1594323)
C
	INTEGER*4  K                      !
	INTEGER*4  PAGE                   !
	INTEGER*4  LENGTH                 !
	INTEGER*4  XTYPE                  !
	INTEGER*4  IND                    !
	INTEGER*4  EOF                    !
	INTEGER*4  BLOCK                  !
	INTEGER*4  CDC                    !
	INTEGER*4  GNUM                   !
	INTEGER*4  LMCNT                  !
	INTEGER*4  LCFCNT                 !
	INTEGER*4  DRWCDC                 !
	INTEGER*4  OFFST                  !
	INTEGER*4  VOLN                   !
	INTEGER*4  DRAW                   !
	INTEGER*4  ENDCDC                 !
	INTEGER*4  STCDC                  !
	INTEGER*4  EXT                    !
	INTEGER*4  COPY                   !
	INTEGER*4  I                      !
	INTEGER*4  ST                     !
	INTEGER*4  TOTBDS                 !
	INTEGER*4  SPTBETS(SPGNBR,8)      !
	INTEGER*4  TFDB(7)                !
	INTEGER*4  FDB(7)                 !
	INTEGER*4  TMFBUF(8192)           !
	INTEGER*4  LOGBUF(LREC*3)         !
	INTEGER*4  REPLU/7/               !
	INTEGER*4  DRAWD(3)               !
C	INTEGER*4  TOTAL                  !
        INTEGER*4  I4TEMP                 !
C       INTEGER*4  GAM                    !
        INTEGER*4  DRWFILE(5)             !
        INTEGER*4  LUNC                   ! LOGICAL UNIT FOR CARRYOVER FILE
        INTEGER*4  INLEN                  !
        INTEGER*4  DSP_SALES,TOT_BOARDS
	INTEGER*4  GIND

	INTEGER*2  XDATE(LDATE_LEN)       !

        CHARACTER*4  CVOLN                !

        BYTE       I1TEMP(4)              !

        LOGICAL    EOFFLG   /.FALSE./     !
	LOGICAL    ERR_STAT /.FALSE./     !
        LOGICAL    READLCF  /.FALSE./     !

        EQUIVALENCE(I4TEMP,I1TEMP)
        EQUIVALENCE(VOLN,CVOLN)

        DATA       DRWFILE/'XXXX',':VAK','IXXX','X.FI','L   '/
	DATA       TOTBDS /1/                        !ARRAY SUBSCRIPT
        DATA       LUNC   /1/
C
	CALL COPYRITE

        TYPE *  
        TYPE *,IAM(),'<<<<< SPTSTA Sports Statistics Report >>>>>'
        TYPE *,IAM(),'<<<<<      V A K I O   1 X 2          >>>>>'
        TYPE *  
C
C
C READ SCF RECORD
C
        CALL GETSCONF(SCFREC,ST)
        IF (ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
C	CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)
C	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
        COPY=0

	CALL PRMNUM('Enter game index',GIND,1,NUMSPT,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)

	CALL PRMNUM('Enter the starting CDC:       ',STCDC,0,DAYCDC,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
	CALL PRMNUM('Enter the ending   CDC:       ',ENDCDC,
     *	                                             STCDC,DAYCDC,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
	CALL PRMNUM('Enter the draw number:        ',DRAW,
     *	                                             1,5000,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
	CALL PRMTEXT('Enter volume for draw files:  ',CVOLN,INLEN)
C
C     Initialize counters
C
	OFFST     = 0
	DRWCDC    = 0
	LCFCNT    = 0
	LMCNT     = 0
C
	CALL ROPEN('SPTSTA.REP',REPLU,ST)
	IF(ST.NE.0)THEN
	  TYPE *,IAM(),'Error opening SPTSTA.REP > ',ST
	  CALL GPAUSE
	END IF
C
C OPEN SPORT GAME FILE
C
	GNUM=SCFGTN(TSPT,GIND)
	CALL OPENW(8,SCFGFN(1,GNUM),4,0,0,ST)
	CALL IOINIT(FDB,8,DSPSEC*256)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
C
C READ SPORT GAME FILE
C
	CALL READW(FDB,DRAW,DSPREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
	CALL CLOSEFIL(FDB)
C
C CALCULATE NUMBER OF TOTAL BOARDS
C
        IF(ENDCDC.EQ.DSPDAT(1)) THEN
           DO K=1,SPGENT
              DSP_SALES = DSP_SALES + DSPSAL(K)
           ENDDO
        ELSE
           DO K=DSPDAT(1)-ENDCDC+3,DSPDAT(1)-STCDC+3
              DSP_SALES = DSP_SALES + DSPSAL(K)
           ENDDO
        ENDIF
        TOT_BOARDS = IDNINT( DFLOAT(DSP_SALES / (SPTPRC(1)/P(PRFACTOR))) )
C
C INITIALIZE DRAW DATE VARIABLES
C
	DRWCDC   = DSPDAT(1)
	XDATE(5) = DRWCDC
	CALL LCDATE(XDATE)
	DRAWD(1) = XDATE(1)   !DAY
	DRAWD(2) = XDATE(2)   !MONTH
	DRAWD(3) = XDATE(14)   !YEAR

        ! set up draw file name
        DRWFILE(1) = VOLN
        CALL MOVBYT(SCFSGN(GNUM),1,DRWFILE(1),6,4)

C
C     The draw number and date have been found now go through the LM
C     files and check the wagers.
C
	WRITE(6,9009) IAM(),GIND,DRAW,DRWCDC,DRAWD(1),DRAWD(2),DRAWD(3)
C
C     ================================================================
C                            Main processing Loop
C     ================================================================
C
C	DO 200 CDC  = STCDC,ENDCDC+1
	DO 200 CDC  = STCDC,ENDCDC
	   IF(CDC.GT.STCDC) CALL CLOSEFIL(TFDB)
	   ERR_STAT = .TRUE.
           READLCF  = .FALSE.
           IF(CDC.EQ.ENDCDC+1) READLCF=.TRUE.
C
C OPEN LOTTO DRAW FILE OR CARRYOVER FILE
C
           IF(READLCF) THEN
             CLOSE(UNIT=PTMF)
             CALL IOPEN(SCFSFN(1,TCF),LUNC,LREC*2,LCDC,LSER*2-1,ST)
             IF(ST.NE.0) CALL FILERR(SCFSFN(1,TCF),1,ST,0)
           ELSE
C
C OPEN DRAW FILE
C
 	     CALL OPNDRW(CDC,PTMF,DRWFILE)
	     CALL IOINIT(TFDB,PTMF,128*256)
	     BLOCK=0
	     EOF=0
	     IND=8192
           ENDIF
C
           IF(READLCF) TYPE*,IAM(),'Processing TCF'
C
2030	   CONTINUE
           IF(READLCF) THEN
             CALL READTCF(LOGBUF,LUNC,EOFFLG)
             IF(EOFFLG) GOTO 200
           ELSE
  	      IF(IND.GT.8157) THEN
                 ST=0
	         BLOCK=BLOCK+1
	         IND=1
	         CALL READW(TFDB,BLOCK,TMFBUF,ST)
	         IF(ST.NE.0) THEN
	            WRITE(6,8002) IAM(),CDC,ST,BLOCK
	            CALL GPAUSE
	         ENDIF
	      ENDIF
           ENDIF
C
           IF(READLCF) THEN
              CALL LOGTRA(TRABUF,LOGBUF)
           ELSE
 	      IF(EOF.GT.1000) GOTO 200
	      IF(TMFBUF(IND).EQ.0) THEN
	         EOF=EOF+1
	         IND=IND+LREC
	         GOTO 2030
	      ENDIF
	      EOF=0
	      I4TEMP=TMFBUF(IND+LREC-1)
              XTYPE=ZEXT(I1TEMP(4))
	      IF(XTYPE.NE.LONE .AND. XTYPE.NE.LREG) THEN
	         TYPE*,IAM(),'Bad record type > ',XTYPE,' index > ',IND
	         IND=IND+LREC
	         GOTO 2030
	      ENDIF
  	      LENGTH=LREC
	      IF(XTYPE.EQ.LONE) THEN
	         I4TEMP=TMFBUF(IND+LREC*2-1)
	         XTYPE=ZEXT(I1TEMP(4))
	         IF(XTYPE.EQ.LEND) LENGTH=LREC*2
	         IF(XTYPE.EQ.LTWO) LENGTH=LREC*3
	      ENDIF
	      CALL FASTMOV(TMFBUF(IND),LOGBUF,LENGTH)
	      CALL LOGTRA(TRABUF,LOGBUF)
	      IND=IND+LENGTH
           ENDIF
C
C VALIDATE TRANSACTION CDC
C
	    IF(TRABUF(TCDC).NE.CDC.AND.ERR_STAT.AND..NOT.READLCF)THEN
	       TYPE*,IAM(),' CDC of transaction: ',TRABUF(TCDC),
     *	             ' Does not equal expected CDC of: ',CDC
	       CALL GPAUSE
	       ERR_STAT = .FALSE.
	    ENDIF
C
C VALIDATION TRANSACTION STATUS
C
	    IF (TRABUF(TGAMTYP).NE.TSPT) GOTO 2030
     	    IF (TRABUF(TGAMIND).NE.GIND) GOTO 2030
C            IF (TRABUF(TFRAC).NE.10)     GOTO 2030
	    IF (TRABUF(TTYP) .NE.TWAG)   GOTO 2030
     	    IF (TRABUF(TSTAT).NE.GOOD.AND.
     *          TRABUF(TSTAT).NE.FRAC)   GOTO 2030
            IF (TRABUF(TFIL).EQ.CDEAD)   GOTO 2030
C
C VALIDATE DRAW NUMBER
C
	    IF ((TRABUF(TWBEG) .LT. DRAW) .AND.
     *	        (TRABUF(TWEND) .LT. DRAW)) GOTO 2030
	    IF ((TRABUF(TWBEG) .GT. DRAW) .AND.
     *	        (TRABUF(TWEND) .GT. DRAW)) GOTO 2030
C
C UPDATE BETTING INFORMATION
C
	    CALL SPTSTAT(SPTBETS,TRABUF,TOTBDS)
	    GOTO 2030
C
200	CONTINUE

        IF(READLCF) THEN
           CLOSE(UNIT=PTMF)
        ELSE
           CLOSE(UNIT=LUNC)
        ENDIF

C
C CALCULATE TOTAL NUMBER OF COMBINATIONS PLAYED
C
C       GAM=2+GIND        !1X2 IS DEFINED AS 3RD .. 8TH GAM IN THE POOL
C	CALL CMBPTOT(TOTAL,GAM,LTPAGE,PAGESIZE)
C
C GENERATE REPORT
C
	CALL TITLE('1 X 2  STATISTICS REPORT','SPTSTA1 ',1,REPLU,
     *	            PAGE,DAYCDC)
C
	WRITE(REPLU,9000) GIND,DRAW,(XDATE(K),K=7,13),STCDC,ENDCDC,TOTSEL,
     *	                  TOT_BOARDS,TOTBDS,TOTSEL-TOTBDS,
     *	                  DFLOAT(TOT_BOARDS)/DFLOAT(TOTSEL)*100.0D0,
     *	                  DFLOAT(TOTBDS)/DFLOAT(TOTSEL)*100.0D0
	WRITE(REPLU,7000)
	WRITE(REPLU,7001)
	DO 500 I=1, DSPMAX
	   IF(SPTBETS(I,8).LE.0) SPTBETS(I,8) = 1
	   WRITE(REPLU,7002) I,(SPTBETS(I,K),
     *	                     DFLOAT(SPTBETS(I,K))/DFLOAT(SPTBETS(I,8))
     *	                     *100.0D0,K=1,7)
500	CONTINUE
C
	CALL SPOOL('SPTSTA.REP',COPY,ST)
	TYPE *,IAM(),'Sport Statistics Report Generated ...'

        CALL GSTOP(GEXIT_SUCCESS)


C
C     =================== Format Statements ======================
C
8001	FORMAT(A4)
8002	FORMAT(1X,A,1X,' Error reading SPORTS draw file  cdc:',
     *         I4.4,' > ',I4,' Block: ',I10)
C
7000	FORMAT(1X,/,131('='))
7001	FORMAT(///,8X,47('-'),'   C O M B I N A T I O N S  ',48('-'),
     *	       //,14X,'(1--)',13X,'(-X-)',13X,'(1X-)',13X,'(--2)',
     *	       13X,'(1-2)',13X,'(-X2)',13X,'(1X2)',//,1X,'EVENT',
     *	       7(2X,'SELECTED PERCENT'),/)
7002	FORMAT(2X,I2,2X,7(2X,I8,2X,F6.2))
C
9000	FORMAT(1X,131('='),//,1X,T41,'STATISTICS FOR INDEX: ',I1,/
     *       ,1X,T41,'PRODUCED FOR DRAW: ',I4,
     *       T71,'DATE:',T79,7(A2),/,1X,T41,'STARTING CDC:',T59,I5,T71,
     *	      'ENDING CDC:',T88,I5,//,1X,T41,'AVAILABLE COMBINATIONS:',
     *	       T81,I12,/,1X,T41,'TOTAL BOARDS:',T81,I12,/,1X,T41,
     *	      'COMBINATIONS PLAYED:',T81,I12,/,1X,T41,
     *	      'COMBINATIONS NOT PLAYED:',T81,I12,/,1X,T41,
     *	      'TOTAL BOARDS / AVAILABLE COMBINATIONS:',T87,F6.2,' %',/,
     *	      1X,T41,'COMBINATIONS PLAYED / AVAILABLE COMBINATIONS:',
     *	       T87,F6.2,' %',/)
9009	FORMAT(/,1X,A,1X,'Processing  Gind : ',I1,'  Draw # : ',I4,
     *         '  Draw CDC : ',I4,'  Draw Date : ',I2.2,'/',I2.2,'/',I4.4,/)
C
	END
