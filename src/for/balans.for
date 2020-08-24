C
C BALANS.FOR
C
C V09 13-MAY-2010 RXK Changes for ePassive.
C V08 20-OCT-2000 UXN Release for Alpha baseline.
C V07 01-MAR-1993 PP  CHANGED REPORT CODES (LIABS ARE ONE REP.CODE)
C V06 01-FEB-1993 PP  CHANGED CURGAM TO 7 (VIKING LOTTO)
C                     ADDED VIKING LIABILITY REP
C V05 01-OCT-1992 PP  INTERCHANGED THE CONTENTS OF ROWS AND COLUMNS
C                     COLUMNS USED TO SHOW GAMES, NOW THEY SHOW REPORTS
C                     ROWS USED TO SHOW THE REPORTS, NOW THEY SHOW THE
C                     GAMES
C                     (THIS ENABLES THE ADDITION OF N NUMBER OF GAMES)
C V04 12-FEB-1992 PP  CHANGED THE ORDER OF HTRANS SUMS
C VO3 17-SEP-1991 HJK ADDED SCF ACCESS FOR FILE NAMES
C V02 13-AUG-1991 PP  CORRECTED BUG: COMPARING REAL-DATA AND
C                     INTEGER (LIABILITY SUMS) CHANGED TO COMPARING
C                     TWO INTEGERS
C V01 22-MAY-1091 PP  INITIAL RELEASE FOR FINLAND
C
C PROGRAM TO WRITE A BALANS REPORT USING THE FIGURES IN BALANS.FIL
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, W.Greenwich, Rhode
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
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
        PROGRAM BALANS
        IMPLICIT NONE
 
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECDAF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:BALANS.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'

        INTEGER*4  RPLU/4/
        INTEGER*4  WAGERS
        INTEGER*4  VOIDS
        INTEGER*4  VALIDS
        INTEGER*4  RETURNS

        INTEGER*4  SALES(NUMTOT,NUMFIN,MAXGAM)
        INTEGER*4  TOTALS(NUMTOT,NUMFIN)
        !INTEGER*4  MINUSSUMS(MAXGAM+1)
        !INTEGER*4  MINUSSUMS_TEMP(MAXGAM+1)
        INTEGER*4  FDB(7)
        INTEGER*4  DFDB(7)
        INTEGER*4  TASMAYS(MAXGAM+1)
        INTEGER*4  ST
        INTEGER*4  EXT
        INTEGER*4  I
        INTEGER*4  COPY
        INTEGER*4  GTYP
        INTEGER*4  GIND
        INTEGER*4  GNUM
        INTEGER*4  GAM
        INTEGER*4  PAGE
        INTEGER*4  LINCNT
        INTEGER*4  IND
        INTEGER*4  RAP
        INTEGER*4  OFF

        INTEGER*2  DATE(LDATE_LEN) /LDATE_LEN*0/

        CHARACTER  RAPORTIT(22)*6                 ! V06
        CHARACTER  HEAD*19
C        CHARACTER  EROTUS*6
        CHARACTER  ERRTEXT*3
        CHARACTER  TEXT*3
        CHARACTER  BLTEXT*3
        CHARACTER  TEXTAU(MAXGAM+1)*3
        CHARACTER  TOT*6/'TOTAL ' /

        REAL*8     TOTSUM
	real*8	   R_BALCSUM

        LOGICAL EITASM /.FALSE./
        LOGICAL OK/.TRUE./

        DATA RAPORTIT/'SYSSUM','AGTACT','CARACT','CLKREP','GAMTOT',
     *                'CSHREP','CANREP','LIABS ','CSHPAS','      ',
     *                '      ','      ','      ','FBNKWN','      ',
     *                'INSRPT','MISRPT','SALSNP','SYSSNP','      ',
     *                '      ','      '/
        DATA HEAD/'BALANS REPORT '/
C        DATA EROTUS/'CSH-BN'/
        DATA ERRTEXT/'-E-'/
        DATA BLTEXT /'   '/


        COMMON BALREC

C******** Begining of source code. *************************************

C---- Call Copyrite subroutine

        CALL COPYRITE

        ST = -1

	CALL GETSCONF(SCFREC,ST)

        TYPE *,IAM()
        TYPE *,IAM(),'<<<<< BALANS  REPORT >>>>>'
        TYPE *,IAM()

        CALL OPENW(8,SCFSFN(1,BAL),4,0,0,ST)
	CALL IOINIT(FDB,8,BALSEC*256)
        IF (ST .NE. 0) CALL FILERR(SCFSFN(1,BAL),1,ST,0)

C  READ BALANS RECORD
C
        CALL READW(FDB,1,BALREC,ST)
        IF (ST .NE. 0) CALL FILERR(SCFSFN(1,BAL),2,ST,1)

        COPY=0

C     OPEN REPORT FILE

        CALL ROPEN('BALANS.REP',RPLU,ST)

        IF (ST .NE. 0) THEN
          TYPE *,IAM(),'BALANS.REP',' REPORT FILE OPEN ERROR > ',ST
	  CALL GSTOP(GEXIT_SUCCESS)
C----     CALL GPAUSE ** REPLACED WITH GSTOP.
        END IF

C
C GET THE SALSNP AND SYSSNP SUMS
C
        CALL FASTSET(0, SALES, NUMTOT*NUMFIN*MAXGAM)
        CALL FASTSET(0, TOTALS,NUMTOT*NUMFIN)
        WAGERS = 0
        VOIDS  = 0
        VALIDS = 0
        RETURNS = 0
C
C CALCULATE SYSSNP SUMS
C                                                                               
        DO  I = 1, MAXGAM 
          WAGERS = WAGERS + DAYTYP(DOLAMT,TWAG,I)
          VOIDS  = VOIDS  + DAYTYP(DOLAMT,TCAN,I)
          VALIDS = VALIDS + DAYTYP(DOLAMT,TVAL,I)+DAYTYP(DOLAMT,TREF,I)
          RETURNS = RETURNS + DAYTYP(DOLAMT,TRET,I) 
        END DO
C
C CALCULATE SALES SNAPSHOT SUMS
C
        CALL FASTMOV(DAYSTS,DAFSTS,DAFLEN)
C
C READ DATA FROM FILE
C
        CALL OPENW(1,SCFSFN(1,DAF),4,0,0,ST)
        CALL IOINIT(DFDB,1,DAFSEC*256)
        IF (ST .NE. 0) CALL FILERR(SCFSFN(1,DAF),1,ST,0)

        CALL READW(DFDB,BALCDC,DAFREC,ST)
        IF (ST .NE. 0) CALL FILERR(SCFSFN(1,DAF),2,ST,BALCDC)
        CALL CLOSEFIL(DFDB)
C
C GET TOTAL SALES BY GAME TYPE
C
        DO 230 GTYP = 1, MAXTYP
            DO 240 GIND = 1, MAXIND
                GNUM = GTNTAB(GTYP,GIND)
                IF(GNUM.LT.1) GOTO 240
                DO I = 1, NUMFIN
                    OFF = I
                    IF(OFF.EQ.TREF) OFF = TVAL
                    SALES(TRACNT,OFF,GNUM) = SALES(TRACNT,OFF,GNUM)+
     *                                     DAFTYP(TRACNT,I,GNUM)
                    SALES(DOLAMT,OFF,GNUM) = SALES(DOLAMT,OFF,GNUM)+
     *                                     DAFTYP(DOLAMT,I,GNUM)
                    TOTALS(TRACNT,OFF)     = TOTALS(TRACNT,OFF)+
     *                                     DAFTYP(TRACNT,I,GNUM)
                    TOTALS(DOLAMT,OFF)     = TOTALS(DOLAMT,OFF)+
     *                                     DAFTYP(DOLAMT,I,GNUM)
                END DO
240     CONTINUE
230     CONTINUE
C
C CLEAR TOTALS
C
        PAGE   =  0
        LINCNT = 70
        TOTSUM = 0.0D0
C
        DATE(5) = BALCDC
        CALL LCDATE(DATE)
C
C MAKE THE REPORT FROM THE SUMS OF THE BALANS FILE RECORD
C
        CALL TITLE(HEAD,'BALANS  ', 1, RPLU, PAGE, BALCDC)
        WRITE(RPLU,8003)
        LINCNT = 5
C
        DO 400 I = 1, TRET
            IF (I.EQ.1) THEN
                WRITE(RPLU,8005) 'S A L E S  '
                WRITE(RPLU,8006)  RAPORTIT(1),
     *                            RAPORTIT(3),
     *                            RAPORTIT(5),
     *                           (RAPORTIT(RAP),RAP=18,19),   !V06
     *                            RAPORTIT(15),
     *                            '      ',      !V06
     *                            '      '
            ELSEIF (I.EQ.2) THEN
                LINCNT=70
                CALL TITLE(HEAD,'BALANS  ',1,RPLU,PAGE,BALCDC)
                WRITE(RPLU,8003)

                WRITE(RPLU,8005) 'C A N C E L S        '
                WRITE(RPLU,8006) (RAPORTIT(RAP),RAP=1,2),     !V05
     *                           (RAPORTIT(RAP),RAP=18,19),   !V06
     *                            RAPORTIT(7),
     *                            '      ',
     *                            '      ',
     *                            '      '
            ELSEIF (I.EQ.3) THEN
                LINCNT=70
                CALL TITLE(HEAD,'BALANS  ',1,RPLU,PAGE,BALCDC)
                WRITE(RPLU,8003)
                WRITE(RPLU,8005) 'C A S H E S          '
                WRITE(RPLU,8010)  RAPORTIT(6),                  !V05
     *                            RAPORTIT(9),
C     *                            RAPORTIT(14),                 !V06
C     *                            RAPORTIT(20),
C     *                            EROTUS,
C    *                           (RAPORTIT(RAP),RAP=1,3),
     *                           (RAPORTIT(RAP),RAP=1,2),
     *                            RAPORTIT(18),                  !V06
     *                            RAPORTIT(19)
            ELSEIF (I.EQ.4) THEN
               !nothing
            ELSEIF (I.EQ.TRET) THEN
                LINCNT=70
                CALL TITLE(HEAD,'BALANS  ',1,RPLU,PAGE,BALCDC)
                WRITE(RPLU,8003)

                WRITE(RPLU,8005) 'R E T U R N S        '
                WRITE(RPLU,8006) (RAPORTIT(RAP),RAP=1,2),   
     *                           (RAPORTIT(RAP),RAP=18,19),  
     *                            '      ',
     *                            '      ',
     *                            '      ',
     *                            '      '
            ENDIF
            WRITE(RPLU,9000)
C
C CHECK IF SUMS BALANCE
C
            EITASM = .FALSE.
            CALL FASTSET(0,TASMAYS,MAXGAM+1)
            DO IND = 1, MAXGAM+1
                TEXTAU(IND) = BLTEXT
            END DO

            CALL BALCHK(BALREC,WAGERS,VOIDS,VALIDS,SALES,TOTALS,
     *                  I,TASMAYS,EITASM)

            IF (EITASM) THEN
                DO IND = 1,MAXGAM+1
                    IF (TASMAYS(IND).EQ.1) THEN
                        TEXTAU(IND) = ERRTEXT
                    ENDIF
                END DO
                OK=.FALSE.
            ENDIF
C
C SALES
C
            IF (I.EQ.1) THEN
                DO 300 GAM = 1, MAXGAM+1			      
                    IF (GAM.LT.MAXGAM+1) THEN			      
                        IF (GNTTAB(GAMIDX,GAM).EQ.0) GOTO 300
                        WRITE(RPLU,9010) (GSNAMES(GAM)),	      
     *                                   BALGSUMS(1,GAM,I,1),
     *                                  (BALGSUMS(RAP,GAM,I,1) -
     *                                   BALGSUMS(RAP,GAM,2,1),RAP=3,3),
     *                                   SALES(1,I,GAM),
     *                                   TEXTAU(GAM)

                        WRITE(RPLU,9011)			      
     *                    CMONY(BALGSUMS(1,GAM,I,2),12,VALUNIT),
     *                   (CSMONY((BALGSUMS(RAP,GAM,I,2) -
     *                           BALGSUMS(RAP,GAM,2,2)),12,VALUNIT),RAP=3,3),
     *                    CMONY(BALGSUMS(5,GAM,I,2),12,VALUNIT),
     *                    CMONY(SALES(2,I,GAM),12,VALUNIT),
     *                    TEXTAU(GAM)
                    ELSE					      
                        WRITE(RPLU,90101) TOT,			      
     *                                   BALTSUMS(1,I),
     *                                  (BALTSUMS(RAP,I) -
     *                                   BALTSUMS(RAP,3),RAP=3,3),
     *                                   TOTALS(1,I),
     *                                   TEXTAU(GAM)

                        WRITE(RPLU,9012)			             
     *                    CMONY(BALTSUMS(1,I+1),12,VALUNIT),
     *                   (CSMONY((BALTSUMS(RAP,I+1)-
     *                           BALTSUMS(RAP,I+3)),12,VALUNIT),RAP=3,3),
     *                    CMONY(BALTSUMS(5,I+1),12,VALUNIT),
     *                    CMONY(TOTALS(2,I),12,VALUNIT),	      !SALSNP
     *                    CMONY(WAGERS,12,VALUNIT),		      !SYSSNP
     *                    TEXTAU(GAM)
                    ENDIF
300             CONTINUE
	     ENDIF
C
C CANCELLATIONS
C
            IF (I.EQ.2) THEN
                DO 320 GAM = 1, MAXGAM+1
                    IF (GAM.LT.MAXGAM+1) THEN			      
                        IF (GNTTAB(GAMIDX,GAM).EQ.0) GOTO 320
                        WRITE(RPLU,9020) (GSNAMES(GAM)),	         
     *                                  (BALGSUMS(RAP,GAM,I,1),RAP=1,2),
     *                                   SALES(1,I,GAM),
     *                                   TEXTAU(GAM)

                        WRITE(RPLU,9021)			             
     *                   (CMONY(BALGSUMS(RAP,GAM,I,2),12,VALUNIT),RAP=1,2),
     *                    CMONY(SALES(2,I,GAM),12,VALUNIT),
     *                    TEXTAU(GAM)
                                                                                
                    ELSE					              
                        WRITE(RPLU,90201) TOT,			          
     *                                  (BALTSUMS(RAP,3),RAP=1,2),
     *                                   TOTALS(1,I),
     *                                   TEXTAU(GAM)

                        WRITE(RPLU,9022)			             
     *                            (CMONY(BALTSUMS(RAP,4),12,VALUNIT),RAP=1,2),
     *                             CMONY(TOTALS(2,I),12,VALUNIT),     !SALSNP
     *                             CMONY(VOIDS,12,VALUNIT),	      !SYSSNP
     *                             CMONY(BALTSUMS(7,4),12,VALUNIT),   !CANREP
     *                             TEXTAU(GAM)
                    ENDIF
320             CONTINUE
            ENDIF
C
C VALIDATIONS
C
            IF (I.EQ.3) THEN
                !DO GAM = 1, MAXGAM+1
                !    MINUSSUMS_TEMP(GAM) = BALCSUMS(1,GAM) - BALCSUMS(2,GAM)
                !    MINUSSUMS(GAM) = MINUSSUMS_TEMP(GAM) - BALCSUMS(3,GAM)
                !END DO
                                                                                
                DO 330 GAM = 1,MAXGAM+1
                    IF (GAM.LT.MAXGAM+1) THEN			               
                        IF (GNTTAB(GAMIDX,GAM).EQ.0) GOTO 330
                        TEXTAU(GAM) = BLTEXT
                        ! NB! CARACT (in header AGTACT) is left out from comparison
                        IF(BALGSUMS(1,GAM,4,1).NE.SALES(TRACNT,TVAL,GAM)) THEN
                           TEXTAU(GAM) = ERRTEXT
                           OK = .FALSE.
                        ENDIF 
                        WRITE(RPLU,9030) (GSNAMES(GAM)),	          
     *                                   BALGSUMS(1,GAM,4,1),	      !SYSSUM
C    *                                 ! (BALGSUMS(RAP,GAM,I,1),RAP=2,3),
     *                                   BALGSUMS(3,GAM,I,1),         !CARACT
     *                                   SALES(TRACNT,TVAL,GAM),
     *                                   TEXTAU(GAM)

                        TEXTAU(GAM) = BLTEXT
                        IF(SALES(DOLAMT,TVAL,GAM).NE.(BALCSUMS(1,GAM)+BALCSUMS(3,GAM)).OR.
     *                     SALES(DOLAMT,TVAL,GAM).NE.BALGSUMS(1,GAM,4,2)) THEN
                           TEXTAU(GAM) = ERRTEXT 
                           OK = .FALSE.
                        ENDIF 
                        WRITE(RPLU,9031)			             
     *                    CMONY(BALCSUMS(1,GAM)+BALCSUMS(3,GAM),12,VALUNIT),
C    *                 ! (CMONY(BALCSUMS(R,GAM),12,VALUNIT),R=1,3),
C    *                 !  CSMONY(MINUSSUMS(GAM),12,VALUNIT),
     *                    CMONY(BALGSUMS(1,GAM,4,2),12,VALUNIT),      !SYSSUM   
C    *                 ! (CMONY(BALGSUMS(RAP,GAM,I,2),12,VALUNIT),RAP=2,3),!A+C
     *                    CMONY(BALGSUMS(3,GAM,I,2),12,VALUNIT),      !CARACT
     *                    CMONY(SALES(DOLAMT,TVAL,GAM),12,VALUNIT),   !SAL
     *                    TEXTAU(GAM)
                    ELSE
                        TEXTAU(GAM) = BLTEXT
                        IF(BALTSUMS(1,7).NE.TOTALS(TRACNT,TVAL)) THEN
                           TEXTAU(GAM) = ERRTEXT			
                           OK = .FALSE.
                        ENDIF 
                        WRITE(RPLU,90301) TOT,			          
     *                                    BALTSUMS(1,7),	      !SYSSUM
C    *                                  ! (BALTSUMS(RAP,5),RAP=2,3),
     *                                    BALTSUMS(3,5),              !CARACT
     *                                    TOTALS(TRACNT,TVAL),        !SALSNP
     *                                    TEXTAU(GAM)

                        TEXTAU(GAM) = BLTEXT
                        IF(VALIDS.NE.(BALCSUMS(1,GAM)+BALCSUMS(3,GAM)).OR.
     *                     VALIDS.NE.BALTSUMS(1,8).OR.
     *                     VALIDS.NE.TOTALS(DOLAMT,TVAL)) THEN
                           TEXTAU(GAM) = ERRTEXT         
                           OK = .FALSE.
                        ENDIF 
                        WRITE(RPLU,9032)
     *                    CMONY(BALCSUMS(1,GAM)+BALCSUMS(3,GAM),12,VALUNIT),
C    *                  !(CMONY(BALCSUMS(R,GAM),12,VALUNIT),R=1,3),
C    *                  ! CSMONY(MINUSSUMS(GAM),12,VALUNIT),
     *                    CMONY(BALTSUMS(1,8),12,VALUNIT),	      !SYSSUM
C    *                  !(CMONY(BALTSUMS(RAP,6),12,VALUNIT),RAP=2,3),
     *                    CMONY(BALTSUMS(3,6),12,VALUNIT),            !CARACT
     *                    CMONY(TOTALS(DOLAMT,TVAL),12,VALUNIT),      !SALSNP
     *                    CMONY(VALIDS,12,VALUNIT),		      !SYSSNP
     *                    TEXTAU(GAM)
                    ENDIF
330             CONTINUE
            ENDIF                                                              
C
C RETURNS
C
            IF (I.EQ.TRET) THEN
                DO 350 GAM = 1, MAXGAM+1
                    IF (GAM.LT.MAXGAM+1) THEN			      
                        IF (GNTTAB(GAMTYP,GAM).NE.TPAS) GOTO 350  
                        TEXTAU(GAM) = BLTEXT
                        IF(SALES(TRACNT,TRET,GAM).NE.BALGSUMS(1,GAM,TRET,1).OR.
     *                     SALES(TRACNT,TRET,GAM).NE.BALGSUMS(2,GAM,4,1)) THEN
                           TEXTAU(GAM) = ERRTEXT 
                           OK = .FALSE.
                        ENDIF 
                        WRITE(RPLU,9040) (GSNAMES(GAM)),	         
     *                                   BALGSUMS(1,GAM,TRET,1),
     *                                   BALGSUMS(2,GAM,4,1),
     *                                   SALES(TRACNT,TRET,GAM),
     *                                   TEXTAU(GAM)
                        TEXTAU(GAM) = BLTEXT
                        IF(SALES(DOLAMT,TRET,GAM).NE.BALGSUMS(1,GAM,TRET,2).OR.
     *                     SALES(DOLAMT,TRET,GAM).NE.BALGSUMS(2,GAM,4,2)) THEN
                           TEXTAU(GAM) = ERRTEXT    
                           OK = .FALSE.
                        ENDIF 
                        WRITE(RPLU,9041)			             
     *                    CMONY(BALGSUMS(1,GAM,TRET,2),12,VALUNIT),
     *                    CMONY(BALGSUMS(2,GAM,4,2),12,VALUNIT),
     *                    CMONY(SALES(DOLAMT,TRET,GAM),12,VALUNIT),
     *                    TEXTAU(GAM)
                                                                                
                    ELSE					         
                        TEXTAU(GAM) = BLTEXT
                        IF(TOTALS(TRACNT,TRET).NE.BALTSUMS(1,9).OR.
     *                     TOTALS(TRACNT,TRET).NE.BALTSUMS(2,9)) THEN       
                           TEXTAU(GAM) = ERRTEXT
                           OK = .FALSE.
                        ENDIF 
                        WRITE(RPLU,90401) TOT,			          
     *                                  (BALTSUMS(RAP,9),RAP=1,2),
     *                                   TOTALS(TRACNT,TRET),
     *                                   TEXTAU(GAM)
                        TEXTAU(GAM) = BLTEXT
                        IF(RETURNS.NE.TOTALS(DOLAMT,TRET).OR.
     *                     RETURNS.NE.BALTSUMS(1,10).OR.
     *                     RETURNS.NE.BALTSUMS(2,10)) THEN
                           TEXTAU(GAM) = ERRTEXT
                           OK = .FALSE.
                        ENDIF 
                        WRITE(RPLU,9042)			             
     *                            (CMONY(BALTSUMS(RAP,10),12,VALUNIT),RAP=1,2),
     *                             CMONY(TOTALS(DOLAMT,TRET),12,VALUNIT),!SALSNP
     *                             CMONY(RETURNS,12,VALUNIT),	      !SYSSNP
     *                             TEXTAU(GAM)
                    ENDIF
350             CONTINUE
            ENDIF
C
400     CONTINUE
C
C GROSS SALES
C
        LINCNT=70
        CALL TITLE(HEAD,'BALANS  ',1, RPLU, PAGE, BALCDC)
        WRITE(RPLU,8003)
        WRITE(RPLU,8004) 'T O T A L   S A L E S '
C       WRITE(RPLU,8009) (RAPORTIT(RAP),RAP=2,3)
        WRITE(RPLU,8009) (RAPORTIT(RAP),RAP=2,2)
        WRITE(RPLU,9006)
C
C CHECK IF GROSS SALES BALANCE
C
        EITASM = .FALSE.
        CALL FASTSET(0, TASMAYS, MAXGAM+1)
        DO IND = 1, MAXGAM+1
            TEXTAU(IND) = BLTEXT
        END DO

        IF (BALRAPC(2).NE.0.AND.BALRAPC(3).NE.0) THEN
            DO GAM = 1, MAXGAM
                IF (BALGSUMS(2,GAM,1,1) .NE.
     *              BALGSUMS(3,GAM,1,1) .OR.
     *              BALGSUMS(2,GAM,1,2) .NE.
     *              BALGSUMS(3,GAM,1,2)) THEN
                    TASMAYS(GAM) = 1
                    EITASM = .TRUE.
                 ENDIF
            END DO
            IF (BALTSUMS(2,1).NE.BALTSUMS(3,1).OR.
     *          BALTSUMS(2,2).NE.BALTSUMS(3,2)) THEN
                TASMAYS(MAXGAM+1) = 1
                 EITASM = .TRUE.
            ENDIF

            IF(EITASM) THEN
                DO IND = 1,MAXGAM+1
                    IF (TASMAYS(IND).EQ.1) THEN
                        TEXTAU(IND) = ERRTEXT
                    ENDIF
                END DO
                OK = .FALSE.
            ENDIF
        ENDIF
C
        DO 520 GAM = 1, MAXGAM+1
            IF (GAM.LT.MAXGAM+1) THEN                                 
                IF (GNTTAB(GAMIDX,GAM).EQ.0) GOTO 520
                WRITE (RPLU,9050) (GSNAMES(GAM)),
C    *                           (BALGSUMS(RAP,GAM,1,1),RAP=2,3),
     *                           (BALGSUMS(RAP,GAM,1,1),RAP=2,2),
     *                            TEXTAU(GAM)
                WRITE (RPLU,9051)
C    *            (CMONY(BALGSUMS(RAP,GAM,1,2),12,VALUNIT),RAP=2,3),
     *            (CMONY(BALGSUMS(RAP,GAM,1,2),12,VALUNIT),RAP=2,2),
     *            TEXTAU(GAM)
            ELSE                                                   
                WRITE (RPLU,90501) TOT,
C    *                           (BALTSUMS(RAP,1),RAP=2,3),
     *                           (BALTSUMS(RAP,1),RAP=2,2),
     *                            TEXTAU(GAM)
                WRITE (RPLU,9051)
C    *            (CMONY(BALTSUMS(RAP,2),12,VALUNIT),RAP=2,3),
     *            (CMONY(BALTSUMS(RAP,2),12,VALUNIT),RAP=2,2),
     *            TEXTAU(GAM)
            ENDIF
520     CONTINUE
C
C COMISSIONS
C
        WRITE(RPLU,8004) 'C O M I S S I O N S'
        WRITE(RPLU,8009) RAPORTIT(2)
        WRITE(RPLU,9006)
        DO 540 GAM = 1, MAXGAM
            IF (GNTTAB(GAMIDX,GAM).EQ.0) GOTO 540
            WRITE (RPLU,9002) (GSNAMES(GAM)),
     *                        CMONY(BALCOMS(2,GAM),12,VALUNIT)
540     CONTINUE
        WRITE (RPLU,90020) TOT,
     *                     CMONY(BALTCOM(2),12,VALUNIT)
C
        WRITE (RPLU,9033) (CSMONY(BALDUE(2,IND),14,VALUNIT),IND=1,2)
C                                                                               
C LIABILITIES AND CSHREP SUMS
C
        WRITE(RPLU,8004) 'C A S H E S'
        LINCNT=5
        WRITE (RPLU,8008)
        WRITE (RPLU,9006)
        DO 600 GAM = 1, MAXGAM

	  IF (GSNAMES(GAM) .GT. ' ') THEN
            RAP = 8                               !V07
            TOTSUM = TOTSUM + BALLSUMS(GAM)
            TEXT = BLTEXT

	    R_BALCSUM = DFLOAT(BALCSUMS(1,GAM))

	    IF (BALLSUMS(GAM) .NE. R_BALCSUM) THEN
              TEXT = ERRTEXT
              OK = .FALSE.
            END IF

            WRITE (RPLU,90071) (GSNAMES(GAM)),
     *                        BALLSUMS(GAM)*FLOAT(DYN_VALUNIT)/FLOAT(DOLL_BASE),
     *                        CMONY(BALCSUMS(1,GAM),11,VALUNIT),
     *                        TEXT
	  END IF
600     CONTINUE

        TEXT = BLTEXT

	R_BALCSUM = DFLOAT(BALCSUMS(1,MAXGAM+1))

        IF (TOTSUM .NE. R_BALCSUM) THEN
          TEXT = ERRTEXT
          OK = .FALSE.
        END IF

        WRITE (RPLU,90072) TOT,
     *                     TOTSUM*FLOAT(DYN_VALUNIT)/FLOAT(DOLL_BASE),
     *                     CMONY(BALCSUMS(1,MAXGAM+1),11,VALUNIT),
     *                     TEXT  !V05

        IF(OK) THEN
            WRITE (RPLU,9035)
        ELSE
            WRITE (RPLU,9036)
        ENDIF

        WRITE (RPLU,9038)
        WRITE (RPLU,9037)
        WRITE (RPLU,9038)
        WRITE (RPLU,9039)

        CALL CLOSEFIL(FDB)
        CLOSE(UNIT=RPLU)
        CALL SPOOL('BALANS.REP',COPY,EXT)

	CALL GSTOP(GEXIT_SUCCESS)
C
C       ===================== Format Statements =================
                                                                               
8003    FORMAT (1X,'PRIMARY/SECONDARY  ', /, X, 131('='))
8004    FORMAT (///,18X,A28)
8005    FORMAT (///,55X,A28)
8006    FORMAT (/,11X,8(7X,A6))
8007    FORMAT(/,15X,A12,4X,A12,4X,A12,4X,A12)
8008    FORMAT(/,7X,'LIABILITY:PAID TODAY ',11X,'CSHREP:GAME TOTAL')
8009    FORMAT (/,14X,1(11X,A6))
8010    FORMAT (/,11X,A6,'/',A6,4(7X,A6))
8011    FORMAT (/,11X,2(7X,A6))

901     FORMAT(1X,A,1X,A14,' open error ',I4)
902     FORMAT(1X,A,1X,A14,' read error ',I4,' for rec ',I4)

9000    FORMAT(1X,131('='))
9002    FORMAT(1X,A4,2X,9X,A12)                    
90020   FORMAT(1X,A6,9X,A12)
9006    FORMAT(1X,80('='))
90071   FORMAT(1X,A4,2x,F11.2,18X,A11,4X,A20)                        
90072   FORMAT(1X,A6,F11.2,18X,A11,4X,A20)                         
9008    FORMAT(1X,A15,A11)

C     - SALES -
9010    FORMAT(1X,A4,2X,'    ',2(6X,I7),19X,I7,42X,A3)
90101   FORMAT(1X,A6,'    ',2(6X,I7),19X,I7,42X,A3)
9011    FORMAT(7X,'    ',4(1X,A12),42X,A3)
9012    FORMAT(7X,'    ',5(1X,A12),29X,A3)

C     - CANCELS -
9020    FORMAT(1X,A4,2X,'    ',3(6X,I7),55X,A3)
90201   FORMAT(1X,A6,'    ',3(6X,I7),55X,A3)
9021    FORMAT(7X,'    ',3(1X,A12),55X,A3)
9022    FORMAT(7X,'    ',5(1X,A12),29X,A3)

C     - CASHES      -
9030    FORMAT(1X,A4,2X,'    ',13X,3(6X,I7),16X,A3)
90301   FORMAT(1X,A6,'    ',13X,3(6X,I7),16X,A3)
9031    FORMAT(7X,'    ',4(1X,A12),16X,A3)
9032    FORMAT(7X,'    ',5(1X,A12),3X,A3)
90302   FORMAT(1X,A6,'    ',1X,A12,16X,A3,//)

C     -RETURNS
9040    FORMAT(1X,A4,2X,'    ',3(6X,I7),16X,A3)
90401   FORMAT(1X,A6,'    ',3(6X,I7),16X,A3)
9041    FORMAT(7X,'    ',3(1X,A12),16X,A3)
9042    FORMAT(7X,'    ',4(1X,A12),3X,A3)
C
9033    FORMAT(////,1X,'DUE LOTTERY  ',A14,'   DUE AGENTS ',A14)
9035    FORMAT(////,27X,'--------- REPORT BALANSES ! -----')
9036    FORMAT(////,17X,'--- ERROR: REPORT DOES NOT BALANS! ---')
9037    FORMAT(////,17X,'PRIMARY AND SECONDARY BALANS/ DONT BALANS')
9038    FORMAT(////,17X,'APPROVED BY: _______________________')
9039    FORMAT(/,17X,'DATE:      __/__/____')
90006    FORMAT(1X,45('='))

C     - TOTAL SALES   -
9050    FORMAT(1X,A4,2X,'COUNT :',1(10X,I7),1X,A3)
90501   FORMAT(1X,A6,'COUNT :',1(10X,I7),1X,A3)
9051    FORMAT(7X,'AMOUNT:',1(5X,A12),1X,A3)

C     - TELEBETTING -
9060    FORMAT(1X,A4,2X,'COUNT :',10X,I7,10X,I7)
9061    FORMAT(7X,'AMOUNT:',5X,A12,5X,A12,15X,A3)
9062    FORMAT(1X,A6,'COUNT :',10X,I7,10X,I7,15X,A3)
9063	FORMAT(1X,A15,A11,5X,A11,34X,A3)        ! misc.sales,petty cash 
9064	FORMAT(1X,A15,A11,50X,A3)	        ! inst.cash,inst.bank
9065	FORMAT(1X,A15,A11,22X,A11,4X,A11,3X,A3) ! inst.val.total
C
        END

