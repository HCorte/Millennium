C SPTDIS.FOR                                                                   
C 
C V18 14-MAR-2000 OXK Event name added & general cleanup
C V17 07-JAN-2000 UXN Several fixes for VAKIO.
C V16 09-SEP-1998 RXK Changed due new layout of recrep.def
C V15 26-JAN-1995 HXK Print zero sales count 
C V14 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V13 27-APR-1994 JXP COPY=0
C V12 26-OCT-1993 HXK fix from Huy.
C V11 19-OCT-1993 HXK REALIGNED GAME NAME.
C V10 19-OCT-1993 HXK FIX GAME NAME BUG, PUT IN GSTOP.
C V09 26-SEP-1993 GXA Do not print systems with no sales.
C V08 16-SEP-1993 SXH COPY=1, added IAM()
C V07 18-AUG-1993 HXN Initial revision.
C V06 23-APR-1993 HHe *Prints now empty system lines too 
C V05 13-OCT-1991 HJK CHANGED TO EXCLUDE SPEDEN (SPORTS 2) 
C V04 24-FEB-1991 HJK ALLOW FREE SYSTEMS > 256    
C V03 10-DEC-1991 HJK ADDED 2,3,5 WEEK COUPONS    
C V02 02-MAY-1991 MTK FIXED REDUCED SYSTEM PROBLEM
C V01 04-OCT-1989 MGM INITIAL RELEASE FOR FINLAND
C                                                                               
C ALL ACTIVE SPORT GAMES DISTRIBUTION OF COUPONS/BOARDS SOLD FOR THIS WEEK
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
C Copyright 2000 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM SPTDIS
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:RECREP.DEF'
C                                  
      INTEGER*4 MAXWK, MAXBDS
      PARAMETER(MAXWK=4)                !for 1,2,3 or 5 weeks
      PARAMETER(MAXBDS=10)              !MAXIMUM NUMBER OF BOARDS 

      INTEGER*4 FDBREP(7)
      INTEGER*4 GIND
      INTEGER*4 TOTCUP(NUMSPT,MAXWK,SYSTYPS),TOTBDS(NUMSPT,MAXWK,SYSTYPS)
      INTEGER*4 GRANDCUP(NUMSPT,MAXWK),GRANDBDS(NUMSPT,MAXWK)
      INTEGER*4 CUPS(MAXCMB,NUMSPT,SYSTYPS,QPTYPS,MAXWK)
      INTEGER*4 SYSCUP(MAXCMB,NUMSPT,MAXWK),SYSBDS(MAXCMB,NUMSPT,MAXWK)
      INTEGER*4 REDCUP(MAXCMB,NUMSPT,MAXWK),REDBDS(MAXCMB,NUMSPT,MAXWK)
      INTEGER*4 REDROW(MAXCMB),SYSROW(MAXCMB)                                   

      INTEGER*4 EVNAME(4,NUMSPT)
      INTEGER*4 K

      CHARACTER HEAD1*42                                                        

      INTEGER*4 RECLU/01/,
     *          REPLU/07/,
     *          REV/05/,
     *          SPTCNT /0/


      INTEGER*4 COPY,ST,PAGE,REC,
     *            GAM,AGAM,SYS,DAYS,
     *		  WEEK,WK,SYSNUM,ROW,GAME_NB,
     *		  YEAR,QPT,SYST,MAXCNT,TRIP,DOUB
      CHARACTER*20 QPTEXT(3) /'Normal              ',
     *                        'Quick Picks         ',
     *                        'Weighted Quick Picks'/
      CHARACTER*16 SYSTEXT(9)/'NORMAL SIMPLE   ',
     *                        'QP SIMPLE       ',
     *                        'WEIGHTED QP SIMP',
     *                        'NORMAL SYSTEM   ',
     *                        'QP SYSTEM       ',
     *                        'WEIGHTED QP SYST', 
     *                        'NORMAL H-SYSTEM ',
     *                        'QP H-SYSTEM     ',
     *                        'WEIGHTED QP H-S '/


C BEGIN CODE -----------------------------------------

      CALL COPYRITE                                                             

      TYPE *                                                                    
      TYPE *,'<<<<< SPTDIS Sports Coupon Distribution >>>>>'           
      TYPE *                                                                    

C      CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)
C      IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)                     
       COPY=0

C OPEN OFFLINE REPORT FILE
C ------------------------
      WRITE (6,501) IAM(),(SFNAMES(ST,REP),ST=1,5)
 501  FORMAT(1X,A,'  Opening & Reading Distribution file...',5A4,'...')

      CALL OPENQW (RECLU,SFNAMES(1,REP),4,0,0,ST)
      CALL IOQINIT(FDBREP,RECLU,REPSEC*256)
      IF(ST.NE.0) THEN
         CALL FILERR(SFNAMES(1,REP),1,ST,0)                                    
         CALL CLOSEQFILE (FDBREP)    ! CLOSE REP.FIL
         CALL GPAUSE
      ENDIF

C CLEAR / SET VARIABLES
C ---------------------
      PAGE=0                                                                    
      CALL FASTSET(0,CUPS,MAXCMB*NUMSPT*SYSTYPS*QPTYPS*MAXWK)
      CALL FASTSET(0,TOTCUP,NUMSPT*MAXWK*SYSTYPS)     
      CALL FASTSET(0,TOTBDS,NUMSPT*MAXWK*SYSTYPS)     
      CALL FASTSET(0,GRANDCUP,NUMSPT*MAXWK)     
      CALL FASTSET(0,GRANDBDS,NUMSPT*MAXWK)     
      CALL FASTSET(0,SYSCUP,MAXCMB*NUMSPT*MAXWK)
      CALL FASTSET(0,SYSBDS,MAXCMB*NUMSPT*MAXWK)
      CALL FASTSET(0,REDCUP,MAXCMB*NUMSPT*MAXWK)
      CALL FASTSET(0,REDBDS,MAXCMB*NUMSPT*MAXWK)

      CALL FASTSET('20202020'X,EVNAME,NUMSPT*4)
      DO GIND=1,NUMSPT
	IF (SPTEVN(1,GIND).NE.0) CALL FASTMOV(SPTEVN(1,GIND),EVNAME(1,GIND),4)
      ENDDO

C READ REPORT STATISTICS FILE
C ---------------------------
      REC=1
      CALL READQIO (FDBREP,REC,REPREC,REPSEC*256,ST)
      IF(ST.NE.0) THEN
         CALL FILERR(SFNAMES(1,REP),2,ST,REC)
         CALL CLOSEQFILE (FDBREP)    ! CLOSE REP.FIL
         CALL GPAUSE
      ENDIF

      CALL CLOSEQFILE (FDBREP)    ! CLOSE REP.FIL

C LOOP THROUGH ALL SPORT GAMES
C ----------------------------
      DO 300 GAM=1,MAXGAM                                                       

         IF (GNTTAB (GAMTYP,GAM).NE.TSPT) GOTO 300 !ONLY SPORT (VAKIO)
         SPTCNT=SPTCNT+1                                                        
         AGAM = SPTCNT + NUMLTO                                                 

         DO 350 SYS=1,MAXCMB                                                    

C           MULTI WEEK COUPONS AVAILABL TO SPORTS (1, 2, 3 AND 5 WEEKS)
C           -----------------------------------------------------------

            DO 320 DAYS = 1,7                   
            DO 320 QPT  = 1,QPTYPS                                
            DO 320 SYST = 1,SYSTYPS                                

               CUPS(SYS,SPTCNT,SYST,QPT,1) = 
     *                 CUPS(SYS,SPTCNT,SYST,QPT,1) + 
     *                 REPN1W(SYS,AGAM,SYST,QPT,DAYS) 
               CUPS(SYS,SPTCNT,SYST,QPT,2) = 
     *                 CUPS(SYS,SPTCNT,SYST,QPT,2) + 
     *                 REPN2W(SYS,AGAM,SYST,QPT,DAYS) 
               CUPS(SYS,SPTCNT,SYST,QPT,3) = 
     *                 CUPS(SYS,SPTCNT,SYST,QPT,3) + 
     *                 REPN3W(SYS,AGAM,SYST,QPT,DAYS) 
               CUPS(SYS,SPTCNT,SYST,QPT,4) = 
     *                 CUPS(SYS,SPTCNT,SYST,QPT,4) + 
     *                 REPN5W(SYS,AGAM,SYST,QPT,DAYS) 

320         CONTINUE                                                            
350      CONTINUE                                                            

300   CONTINUE                                                                  
      MAXCNT = SPTCNT

C OPEN REPORT FILE
C ----------------
      CALL ROPEN('SPTDIS.REP',REPLU,ST)                                         
      IF(ST.NE.0) THEN                                                          
         TYPE*,IAM(),'SPTDIS.REP Open error  st > ',ST
         CLOSE(REPLU)                                                           
         PAUSE                                                                  
      ENDIF                                                                     

C GET DATE IN YEAR AND WEEK FORMAT
C --------------------------------
C
      CALL FIGWEK(DAYCDC,WEEK,YEAR)  
      ENCODE(42,9001,HEAD1) WEEK, YEAR

C LOOP THROUGH ALL GAMES AND PRINT REPORT                                       

      DO 500 SPTCNT = 1,MAXCNT 
	GAME_NB = GTNTAB (TSPT,SPTCNT)   !determine the Game number
        DO 480 QPT = 1,QPTYPS 

C SPORT FULL SYSTEMS BETS 
C ------------------------

         SYST = 2
         CALL FASTSET(0,TOTCUP,NUMSPT*MAXWK*SYSTYPS)     
         CALL FASTSET(0,TOTBDS,NUMSPT*MAXWK*SYSTYPS)     
         CALL FASTSET(0,SYSCUP,MAXCMB*NUMSPT*MAXWK)
         CALL FASTSET(0,SYSBDS,MAXCMB*NUMSPT*MAXWK)
         CALL FASTSET(0,SYSROW,MAXCMB)

         DO 510 SYS=1,256
            DO 515 WK=1,MAXWK
               IF(CUPS(SYS,SPTCNT,SYST,QPT,WK).GT.0) THEN
                  SYSROW(SYS) = SYS
                  SYSCUP(SYS,SPTCNT,WK) = CUPS(SYS,SPTCNT,SYST,QPT,WK)
                  SYSBDS(SYS,SPTCNT,WK) = SYS*CUPS(SYS,SPTCNT,SYST,QPT,WK) 
                  TOTCUP(SPTCNT,WK,SYST) = TOTCUP(SPTCNT,WK,SYST) + 
     *                                     SYSCUP(SYS,SPTCNT,WK)
                  TOTBDS(SPTCNT,WK,SYST) = 
     *                TOTBDS(SPTCNT,WK,SYST) + SYSBDS(SYS,SPTCNT,WK)
               ENDIF
515         CONTINUE   
510      CONTINUE                                                               

         DO 516 SYS=257,MAXCMB
            DO 517 WK=1,MAXWK
               SYSNUM = SYS
               CALL BIGFRSYS(SYSNUM,1)                                          
               IF(CUPS(SYS,SPTCNT,SYST,QPT,WK).GT.0) THEN
                  SYSROW(SYS) = SYSNUM 
                  SYSCUP(SYS,SPTCNT,WK) = CUPS(SYS,SPTCNT,SYST,QPT,WK)
                  SYSBDS(SYS,SPTCNT,WK) = SYSNUM*CUPS(SYS,SPTCNT,SYST,QPT,WK)
                  TOTCUP(SPTCNT,WK,SYST) = TOTCUP(SPTCNT,WK,SYST) + 
     *                                     SYSCUP(SYS,SPTCNT,WK)
                  TOTBDS(SPTCNT,WK,SYST) = TOTBDS(SPTCNT,WK,SYST) +
     *                                     SYSBDS(SYS,SPTCNT,WK)
               ENDIF
517	    CONTINUE
516      CONTINUE                                                               

C PRINT FULL SYSTEM RESULTS REPORT
C ----------------------------------

         CALL TITLE(HEAD1,'SPTDIS',REV,REPLU,PAGE,DAYCDC)                       
         WRITE(REPLU,9000)                                                      
         WRITE(REPLU,9002) (GLNAMES(WK,GAME_NB),WK=1,MAXWK),'Full systems ',
     *                     QPTEXT(QPT)
	 GIND=GNTTAB(GAMIDX,GAME_NB)
	 WRITE(REPLU,9011) (EVNAME(K,GIND),K=1,4)
         WRITE(REPLU,9009)
         WRITE(REPLU,9003) 'SYSTEM'

         SYST=2
         DO 525 ROW=1,MAXCMB                                                    
           IF(SYSROW(ROW).GT.0) THEN
            WRITE(REPLU,9004) SYSROW(ROW),
     *                        (SYSCUP(ROW,SPTCNT,WK),         
     *                         SYSBDS(ROW,SPTCNT,WK),WK=1,MAXWK)
           ENDIF                                 
525      CONTINUE                                                               

         WRITE(REPLU,9006) SYSTEXT((SYST-1)*QPTYPS+QPT),
     *                     (TOTCUP(SPTCNT,WK,SYST),
     *                      TOTBDS(SPTCNT,WK,SYST),WK=1,MAXWK)

         DO WK=1,MAXWK
            GRANDCUP(SPTCNT,WK) = GRANDCUP(SPTCNT,WK) + TOTCUP(SPTCNT,WK,SYST)
            GRANDBDS(SPTCNT,WK) = GRANDBDS(SPTCNT,WK) + TOTBDS(SPTCNT,WK,SYST)
         ENDDO

C SPORTS REDUCED SYSTEM BETS
C --------------------------

         SYST = 3
         CALL FASTSET(0,TOTCUP,NUMSPT*MAXWK*SYSTYPS)     
         CALL FASTSET(0,TOTBDS,NUMSPT*MAXWK*SYSTYPS)     
         CALL FASTSET(0,REDCUP,MAXCMB*NUMSPT*MAXWK)
         CALL FASTSET(0,REDBDS,MAXCMB*NUMSPT*MAXWK)
         CALL FASTSET(0,REDROW,MAXCMB)

         DO 520 SYS=1,SPGSYS                                       
            DO 524 WK=1,MAXWK  
               IF(CUPS(SYS,SPTCNT,SYST,QPT,WK).GT.0) THEN
                  REDROW(SYS) = SPSNUM(5,SYS)
                  REDCUP(SYS,SPTCNT,WK) = CUPS(SYS,SPTCNT,SYST,QPT,WK)
                  REDBDS(SYS,SPTCNT,WK) = 
     *                  REDROW(SYS)*CUPS(SYS,SPTCNT,SYST,QPT,WK)
                  TOTCUP(SPTCNT,WK,SYST) = TOTCUP(SPTCNT,WK,SYST) + 
     *                                     REDCUP(SYS,SPTCNT,WK)
                  TOTBDS(SPTCNT,WK,SYST) = TOTBDS(SPTCNT,WK,SYST) +
     *                                     REDBDS(SYS,SPTCNT,WK)
               ENDIF
524	    CONTINUE 
520      CONTINUE                                                               


C PRINT REDUCED SYSTEM RESULTS REPORT
C ---------------------------------------

         CALL TITLE(HEAD1,'SPTDIS',REV,REPLU,PAGE,DAYCDC)                       
         WRITE(REPLU,9000)                                                      
         WRITE(REPLU,9002) (GLNAMES(WK,GAME_NB),WK=1,MAXWK),'Reduced systems ',
     *                     QPTEXT(QPT)
	 GIND=GNTTAB(GAMIDX,GAME_NB)
	 WRITE(REPLU,9011) (EVNAME(K,GIND),K=1,4)
         WRITE(REPLU,9009)                                                      
         WRITE(REPLU,90031) 'SYSTEM'

         SYST=3
         DO 526 ROW=1,SPGSYS  
          IF(REDROW(ROW).GT.0) THEN
            TRIP=SPSNUM(2,ROW)-1
            DOUB=SPSNUM(3,ROW)-1-TRIP
            WRITE(REPLU,90051) DOUB,TRIP,
     *                         REDROW(ROW),
     *                         SPSGAR(ROW),
     *                        (REDCUP(ROW,SPTCNT,WK),
     *                         REDBDS(ROW,SPTCNT,WK),WK=1,MAXWK)
          ENDIF               
526      CONTINUE                                                               

         WRITE(REPLU,9006) SYSTEXT((SYST-1)*QPTYPS+QPT),
     *                     (TOTCUP(SPTCNT,WK,SYST),
     *                      TOTBDS(SPTCNT,WK,SYST),WK=1,MAXWK) 
C 
         DO WK=1,MAXWK
            GRANDCUP(SPTCNT,WK) = GRANDCUP(SPTCNT,WK) + TOTCUP(SPTCNT,WK,SYST)
            GRANDBDS(SPTCNT,WK) = GRANDBDS(SPTCNT,WK) + TOTBDS(SPTCNT,WK,SYST)
         ENDDO

480    CONTINUE      

C SIMPLE BETS
C --------------
       CALL TITLE(HEAD1,'SPTDIS',REV,REPLU,PAGE,DAYCDC)                       
       WRITE(REPLU,9000)                                                      
       WRITE(REPLU,9002) (GLNAMES(WK,GAME_NB),WK=1,MAXWK)
	 GIND=GNTTAB(GAMIDX,GAME_NB)
	 WRITE(REPLU,9011) (EVNAME(K,GIND),K=1,4)
        
       DO 490 QPT=1,QPTYPS
         WRITE(REPLU,90021),QPTEXT(QPT)
         WRITE(REPLU,9009)                                                      
         WRITE(REPLU,9003) 'SIMPLE'                                             

         SYST = 1
         CALL FASTSET(0,TOTCUP,NUMSPT*MAXWK*SYSTYPS)     
         CALL FASTSET(0,TOTBDS,NUMSPT*MAXWK*SYSTYPS)     

         DO 530 SYS=1,MAXBDS
            DO 528 WK=1,MAXWK
               IF(CUPS(SYS,SPTCNT,SYST,QPT,WK).GT.0) THEN
                  TOTCUP(SPTCNT,WK,SYST) = TOTCUP(SPTCNT,WK,SYST) + 
     *                                     CUPS(SYS,SPTCNT,SYST,QPT,WK)
                  TOTBDS(SPTCNT,WK,SYST) = 
     *               TOTBDS(SPTCNT,WK,SYST) + SYS*CUPS(SYS,SPTCNT,SYST,QPT,WK)
               ENDIF
528         CONTINUE   
530      CONTINUE                                                               

         DO 527 ROW=1,MAXBDS   
            WRITE(REPLU,9004) ROW,                                              
     *                        (CUPS(ROW,SPTCNT,SYST,QPT,WK),
     *                        CUPS(ROW,SPTCNT,SYST,QPT,WK)*ROW, WK=1,MAXWK)
527      CONTINUE                                                               
                                                                                
         WRITE(REPLU,9006) SYSTEXT((SYST-1)*QPTYPS+QPT),
     *                     (TOTCUP(SPTCNT,WK,SYST),TOTBDS(SPTCNT,WK,SYST),
     *                      WK=1,MAXWK)
         DO WK=1,MAXWK
            GRANDCUP(SPTCNT,WK) = GRANDCUP(SPTCNT,WK) + TOTCUP(SPTCNT,WK,SYST)
            GRANDBDS(SPTCNT,WK) = GRANDBDS(SPTCNT,WK) + TOTBDS(SPTCNT,WK,SYST)
         ENDDO

490    CONTINUE

C GRAND TOTAL FOR ALL BETS
C ------------------------

         WRITE(REPLU,9008)  (GRANDCUP(SPTCNT,WK), WK=1,MAXWK),
     *                      (GRANDBDS(SPTCNT,WK), WK=1,MAXWK)

C GRAND TOTAL FOR ALL BETS FOR ALL WEEKS
C --------------------------------------

         WRITE(REPLU,9010)  GRANDCUP(SPTCNT,1) + GRANDCUP(SPTCNT,2) +
     *                      GRANDCUP(SPTCNT,3) + GRANDCUP(SPTCNT,4),
     *                      GRANDBDS(SPTCNT,1) + GRANDBDS(SPTCNT,2) +
     *                      GRANDBDS(SPTCNT,3) + GRANDBDS(SPTCNT,4)

500   CONTINUE                                                                  

C SPOOL REPORT TO THE PRINTER
C ---------------------------
      CLOSE(REPLU)
      CALL SPOOL('SPTDIS.REP',COPY,ST)
      IF(ST.NE.0) THEN
         TYPE*,IAM(),'Error while spooling report  st - ',ST
         CALL GSTOP(GEXIT_SUCCESS)
      ENDIF                                                                     

C     ================== FORMAT STATEMENTS ======================               
C                                                                               
9000  FORMAT(1X,131('='),/)                                                    
9001  FORMAT('SPORT COUPON DISTRIBUTION FOR WEEK ',I2.2,'/',I4.4)               
9002  FORMAT(2X,'GAME: ',4A4, 4X,A,1X,A)                      
90021 FORMAT(//,8X,'Simple bets ',A)
9003  FORMAT(/,5X,A6,8X,4('COUPONS',4X,'NUMBER OF',5X),                       
     *       /,8X,'BET',8X,4(3X,'SOLD',7X,'BOARDS',5X),/)                       
90031 FORMAT(/,5X,A6,8X,4('COUPONS',4X,'NUMBER OF',5X),                       
     *       /,1X,' TYPE SIZE GUAR',3X,4(3X,'SOLD',7X,'BOARDS',5X),/)
9004  FORMAT(4X,I7,8X,4(I7,4X,I9,5X))                                           
9005  FORMAT(6X,I4,A,8X,4(I7,6X,I7,5X))                                         
90051 FORMAT(1X,I2,'-',I2,1X,I4,1X,I4,3X,4(I7,6X,I7,5X))          
9006  FORMAT(/,18X,<MAXWK>(8('-'),5X,8('-'),4X),
     *       /,1X,A16,1X,4(I8,5X,I8,4X))
9008  FORMAT(///,' TOTAL COUPONS:'2X,4(13X,I9,3X),//,                           
     *           ' TOTAL BOARDS: '2X,4(13X,I9,3X))                              
9009  FORMAT(33X,'1-Week',19X,'2-Week',19X,'3-Week',19X,'5-Week',/,
     *       18X,4(21('-'),4X))
9010  FORMAT(///,' OVERALL TOTAL COUPONS:',9X,I10,/,                           
     *           ' OVERALL TOTAL BOARDS: ',9X,I10)                              
9011  FORMAT(1X,4A4)
C                                                                               
      END                                                                       
