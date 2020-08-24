C
C PROGRAM TCFSUM
C
C TCFSUM.FOR
C  
C V12 26-JUN-2010 FJG OFB Issue in LCF
C V11 01-FEB-2000 UXN TWFFLG added.
C V10 10-FEB-1999 UXN XCHD tickets not counted.
C V09 13-FEB-1996 RXK Rfss 256, change of some formats.
C V08 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V07 29-APR-1994 JXP COPY=0
C V06 16-SEP-1993 SXH Give it the same functionality as the CONCURRENT report
C V05 31-AUG-1993 SXH PUT COPY=1
C V04 15-JUL-1993 SXH Released for Finland
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 02-APR-1991 MTK INITIAL RELEASE FOR MARYLAND
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C
C PROGRAM TO PRINT A SUMMARY REPORT FROM THE TCF FILE
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM TCFSUM
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
C
C
        ! variables                               !
	INTEGER*4  K                              !
	INTEGER*4  PAGE                           !
	INTEGER*4  GIND                           !
	INTEGER*4  GTYP                           !
	INTEGER*4  GAM                            !
	INTEGER*4  DUMMY                          !
	INTEGER*4  IND3                           !
	INTEGER*4  IND2                           !
	INTEGER*4  IND1                           !
	INTEGER*4  GNUM                           !
	INTEGER*4  I                              !
	INTEGER*4  ST                             !
	INTEGER*4  COPY                           !
	INTEGER*4  LCF(10,11,MAXGAM)              !
	INTEGER*4  LOGREC(LREC*3)                 !
	INTEGER*4  TOTAL(11,MAXGAM)               !
	INTEGER*4  SDRAW(MAXGAM)                  !
	INTEGER*4  CDRAW(MAXGAM)                  !
	INTEGER*4  REPLU/7/                       !
        INTEGER*4  TOT                            !

	CHARACTER  HEAD*38                        !

	LOGICAL    EOF /.FALSE./                  !
                                                  
C
	CALL COPYRITE
c
C
	TYPE*,IAM(),' Generating carryover summary report'
C	CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)
C	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
        COPY=0
C
C OPEN REPORT FILE
C
	CALL ROPEN('TCFSUM.REP',REPLU,ST)
	IF(ST.NE.0)THEN
	    TYPE *,IAM(),'Error openning TCFSUM.REP > ',ST
	    CALL GPAUSE
	ENDIF
C
C READ SCF RECORD
C
	CALL GETSCONF(SCFREC,ST)
C
C OPEN CARRYOVER FILE
C
	CALL IOPEN(SCFSFN(1,TCF),1,LREC*2,LCDC,LSER*2-1,ST)
	IF(ST.NE.0)  CALL FILERR(SCFSFN(1,TCF),1,ST,0)
C
C
	DO 100 GIND=1,NUMLTO
	    GNUM = SCFGTN(TLTO,GIND)
	    IF(GNUM.LE.0) GOTO 100
	    SDRAW(GNUM) = LTODRW(GIND)-10
            CDRAW(GNUM) = LTODRW(GIND)
	    IF(SDRAW(GNUM).LT.1) SDRAW(GNUM) = 1
100	CONTINUE
C
	DO 110 GIND=1,NUMSPT
	    GNUM = SCFGTN(TSPT,GIND)
	    IF(GNUM.LE.0) GOTO 110
	    SDRAW(GNUM) = SPTDRW(GIND)-10
	    CDRAW(GNUM) = SPTDRW(GIND)
	    IF(SDRAW(GNUM).LT.1) SDRAW(GNUM) = 1
110	CONTINUE

	DO 130 GIND=1,NUMKIK
	    GNUM = SCFGTN(TKIK,GIND)
	    IF(GNUM.LE.0) GOTO 130
	    SDRAW(GNUM) = KIKDRW(GIND)-10
	    CDRAW(GNUM) = KIKDRW(GIND)
	    IF(SDRAW(GNUM).LT.1) SDRAW(GNUM) = 1
130	CONTINUE
C
C CLEAR LCF TRANSACTION TABLE
C
	CALL FASTSET(0,LCF,110*MAXGAM)
	CALL FASTSET(0,TOTAL,11*MAXGAM)
C
C READ ALL TRANSACTIONS IN TCF
C
20	CONTINUE
C
C     ====================== Main Loop =======================
C
	CALL READTCF(LOGREC,1,EOF)
	IF(EOF) GOTO 1000
C
C ADD TRANSACTION TO APPROPIATE SLOT IN LCF TABLE
C
	CALL LOGTRA(TRABUF,LOGREC)
	IF(TRABUF(TWFFLG).EQ.1) GOTO 20
	IF(TRABUF(TSTAT).NE.GOOD .AND. 
     *	   TRABUF(TSTAT).NE.EXCH .AND.
     *     TRABUF(TSTAT).NE.FRAC) GOTO 20
	IF(TRABUF(TGAMTYP).NE.TLTO .AND. TRABUF(TGAMTYP).NE.TSPT .AND.
     *     TRABUF(TGAMTYP).NE.TKIK) GOTO 20

	IF(TRABUF(TGAMTYP).EQ.TKIK) GOTO 30

	IND1=TRABUF(TWBEG)-SDRAW(TRABUF(TGAM))
	IF(IND1.LT.1) IND1=1

	IND2=TRABUF(TWEND)-CDRAW(TRABUF(TGAM))+1
        IND3=CDRAW(TRABUF(TGAM))-TRABUF(TWBEG)        
        IND3=10-IND3                                  

        IF(IND3.GT.0) THEN
	  DO I=1,IND2
	    LCF(IND3,I,TRABUF(TGAM)) = LCF(IND3,I,TRABUF(TGAM)) +
     *	                               TRABUF(TWAMT)
	    TOTAL(I,TRABUF(TGAM)) = TOTAL(I,TRABUF(TGAM)) + TRABUF(TWAMT)
          END DO
        ENDIF
C
C UPDATE KICKER GAME
C
30	CONTINUE
	IF(TRABUF(TWKGME).EQ.0) GOTO 20

	IND1=TRABUF(TWKBEG)-SDRAW(TRABUF(TWKGME))
	IF(IND1.LT.1) IND1=1

	IND2=TRABUF(TWKEND)-CDRAW(TRABUF(TWKGME))+1

        IND3=CDRAW(TRABUF(TWKGME))-TRABUF(TWKBEG)    
        IND3=10-IND3                                 


	DO I=1,IND2
	    LCF(IND3,I,TRABUF(TWKGME)) = LCF(IND3,I,TRABUF(TWKGME))+
     *	                                 TRABUF(TWKAMT)
	    TOTAL(I,TRABUF(TWKGME)) = TOTAL(I,TRABUF(TWKGME))+
     *	                              TRABUF(TWKAMT)
        END DO
C
C     ====================== End of Loop ======================
C
	GOTO 20
C
C CLOSE FILE AND PRINT REPORT.
C
1000	CONTINUE
	CALL ICLOSE(1,DUMMY,ST)
C
C
	DO 500 GAM=1,MAXGAM
	    GTYP = SCFGNT(GAMTYP,GAM)
	    GIND = SCFGNT(GAMIDX,GAM)
	    IF(GTYP.LE.0) GOTO 500

C                                                                               
C           TEST IF GAME HAS MULTIVEEK COUPONS   
C                                                                               
            TOT = 0                                                                
            DO 450 I=1,10                                                          
                TOT = TOT + TOTAL(I,GAM)                                            
450         CONTINUE                                                               
            IF (TOT.EQ.0) GOTO 500                                                 

            TOT = TOT - TOTAL(1,GAM)     !COUNT MONEY FOR FUTURE 

	    IF(GTYP.NE.TLTO .AND. GTYP.NE.TSPT .AND. GTYP.NE.TKIK) GOTO 500

	    WRITE(HEAD,8000) (SCFLGN(K,GAM),K=1,4)

	    PAGE=0
	    CALL TITLE(HEAD,'  TCFSUM',1,REPLU,PAGE,DAYCDC)
	    WRITE(REPLU,9002) GTNAMES(GTYP),GIND
	    WRITE(REPLU,9003) (K,K=CDRAW(GAM),CDRAW(GAM)+9)
	    DO I=1,10
                IF (CDRAW(GAM)+I-10.GT.0)                 
     *	          WRITE(REPLU,9004) CDRAW(GAM)+I-10,
     *	                         (CMONY(LCF(I,K,GAM),12,BETUNIT),K=1,10)
            END DO

	    WRITE(REPLU,9005) (CMONY(TOTAL(K,GAM),12,BETUNIT),K=2,10),
     *                         CMONY(TOTAL(1,GAM),12,BETUNIT),
     *                         CMONY(TOT,12,BETUNIT)
500	CONTINUE
C
	CALL USRCLOS1(REPLU)
	CALL SPOOL('TCFSUM.REP',COPY,ST)
	CALL GSTOP(GEXIT_SUCCESS)
C
C     ===================== Format Statements =================
C
8000	FORMAT(4A4,' CARRYOVER FILE REPORT')
C
9002	 FORMAT(/,1X,131('='),//,' GAME: ',A8,I1,/,
     *	        /,1X,'START',5X,50('-'),'EXPIRATION DRAW',
     *	          1X,52('-'),/,'  DRAW')
9003	FORMAT(5X,10(8X,I4),/)
9004	FORMAT(1X,I4,2X,10(A12))
9005	FORMAT(/,1X,'TOTAL             ',9(A12),///,
     *       1X,'TO POT ',A12,87X,'TO FUTURE',A12) 

	END
