C LTODIS.FOR
C $Log:   GXAFXT:[GOLS]LTODIS.FOV  $
C  
C V08 09-SEP-98 RXK Changed due new layout of recrep.def
C 
C     Rev 1.0   17 Apr 1996 13:58:12   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.3   02 Sep 1994 18:07:48   HXK
C  Merge of May,June RFSS batch 
C  
C     Rev 1.3   27 Apr 1994 16:34:18   JXP
C  COPY=0
C  
C     Rev 1.2   16 Sep 1993 16:24:52   SXH
C  COPY=1, Added IAM()
C  
C     Rev 1.1   18 Aug 1993 18:39:06   HXN
C  Initial version, based on Concurrent System.
C                                                                               
C V02 17-OCT-90 MTK CHANGED FOR TWO/THREE WEEK TICKETS                          
C V01 04-OCT-89 MGM INITIAL RELEASE FOR FINLAND                                 
C                                                                               
C ALL ACTIVE LOTTO GAMES DISTRIBUTION OF COUPONS/BOARDS                         
C SOLD FOR THIS WEEK                                                            
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
C Copyright 1998 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM LTODIS               
        IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECREP.DEF'
C	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:LSYSCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'


      INTEGER*4 FDBREP(7),SCFFDB(7)                                             
      INTEGER*4 CUPCNT(MAXCMB,NUMLTO,3,5), GIND(NUMLTO)                         
      INTEGER*4 TOTCUP(NUMLTO,5,3),TOTBDS(NUMLTO,5,3)                           
      INTEGER*4 MAXBDS                                                          
      INTEGER*4 YEAR         ! Year in 4 digits
      CHARACTER HEAD1*42                                                        

      INTEGER*4 RECLU  /01/,
     *          REPLU  /07/,
     *          SCFNAM(5) /'SCF.','FIL ',3*'    '/,
     *		REV       /01/
C                                                                               
      PARAMETER(MAXBDS=12) !MAXIMUM NUMBER OF BOARDS BET                        


	INTEGER*4 COPY,EXT,ST,PAGE,REC,SYSLIM,
     *            GAM,LTOCNT,DAYS,WEEK,SYS,
     *            CHECK,MAXSIMBET,ROWNUM,XTYP,XIND,
     *            I,W,S,QPS,STYP

C BEGIN CODE -----------------------------------------

      CALL COPYRITE                                                             


      TYPE *                                                                    
      TYPE *,'<<<<< LTODIS Lotto Coupon Distribution       V01 >>>>>'           
      TYPE *                                                                    

C      CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)
C      IF(EXT.LT.0) STOP                                          
       COPY=0



C OPEN OFFLINE REPORT FILE                                                      
C ------------------------
      WRITE (5,501) IAM(),(SFNAMES(ST,REP),ST=1,5)
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
      CALL FASTSET(0,CUPCNT,MAXCMB*NUMLTO*3*5)                                  
      CALL FASTSET(0,TOTCUP,NUMLTO*5*3)                                         
      CALL FASTSET(0,TOTBDS,NUMLTO*5*3)                                         


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

C LOOP THROUGH ALL ACTIVE LOTTO GAMES
C -----------------------------------
      SYSLIM = 256                                                              

      DO 300 GAM=1,MAXGAM                                                       

         XTYP = GNTTAB (GAMTYP,GAM)
         XIND = GNTTAB (GAMIDX,GAM)
         IF (XTYP.NE.TLTO) GOTO 300 !ONLY LOTTO                   
         IF (XIND.LE.0)    GOTO 300 !ONLY ACTIVE                  

         LTOCNT=LTOCNT+1                                                        
         GIND(LTOCNT)=GAM                                                       

         DO 350 SYS=1,MAXCMB                                                    
         DO 370 DAYS = 1,7                                                   
         DO 371 I=1,3        !For all types of system bets  (1=full,2=reduced,3=u)
         DO 372 QPS=1,2      !For non-qp and normal qp

C              1 WEEK COUPONS
C              --------------
               CUPCNT(SYS,LTOCNT,I,1) = CUPCNT(SYS,LTOCNT,I,1) +
     *                                  REPN1W(SYS,LTOCNT,I,QPS,DAYS)
C              2 WEEK COUPONS
C              --------------
               CUPCNT(SYS,LTOCNT,I,2) = CUPCNT(SYS,LTOCNT,I,2) +
     *                                  REPN2W(SYS,LTOCNT,I,QPS,DAYS)
C              3 WEEK COUPONS
C              --------------
               CUPCNT(SYS,LTOCNT,I,3) = CUPCNT(SYS,LTOCNT,I,3) +
     *                                  REPN3W(SYS,LTOCNT,I,QPS,DAYS)
C              5 WEEK COUPONS
C              --------------
               CUPCNT(SYS,LTOCNT,I,4) = CUPCNT(SYS,LTOCNT,I,4) +
     *                                  REPN5W(SYS,LTOCNT,I,QPS,DAYS)
C              10 WEEK COUPONS
C              ---------------
               CUPCNT(SYS,LTOCNT,I,5) = CUPCNT(SYS,LTOCNT,I,5) +
     *                                  REPNAW(SYS,LTOCNT,I,QPS,DAYS)
372      CONTINUE                                                            
371      CONTINUE                                                            
370      CONTINUE                                                            
350      CONTINUE                                                               
300   CONTINUE                                                                  

C OPEN REPORT FILE
C ----------------
      CALL ROPEN('LTODIS.REP',REPLU,ST)                                         
      IF(ST.NE.0) THEN                                                          
         TYPE*,IAM(),'LTODIS.REP Open error  st - ',ST 
         CLOSE(REPLU)                                                           
         CALL GPAUSE
      ENDIF                                                                     

C GET DATE IN YEAR AND WEEK FORMAT                                              
C --------------------------------
      CALL FIGWEK(DAYCDC,WEEK,YEAR)

C     ===================== Main Loop =======================                   
C                                                                               
C LOOP THROUGH ALL GAMES AND PRINT REPORT                                       
C                                                                               
      DO 500 GAM=1,LTOCNT                                                       
C                                                                               
C BUILD HEADERS                                                                 
C                                                                               
         ENCODE(42,9001,HEAD1) WEEK, YEAR
         CALL TITLE(HEAD1,'LTODIS',REV,REPLU,PAGE,DAYCDC)                       
         WRITE(REPLU,9000)                                                      
         WRITE(REPLU,9002)  GTNAMES(TLTO),GAM                                   
         WRITE(REPLU,9003) 'SYSTEM'                                             

C LOTTO FULL SYSTEMS BETS
C -----------------------
         STYP = 2
         DO 600 SYS=1,LSYSMAX
            CHECK = CUPCNT(SYS,GAM,STYP,1) + CUPCNT(SYS,GAM,STYP,2) +
     *              CUPCNT(SYS,GAM,STYP,3) + CUPCNT(SYS,GAM,STYP,4) +
     *              CUPCNT(SYS,GAM,STYP,5)                           
            IF(CHECK.EQ.0) GOTO 600                                             
            WRITE(REPLU,9004) LSYS_BOARD(SYS),CUPCNT(SYS,GAM,STYP,1),  
     *                        CUPCNT(SYS,GAM,STYP,1)*LSYS_BOARD(SYS),  
     *                        CUPCNT(SYS,GAM,STYP,2),                  
     *                        CUPCNT(SYS,GAM,STYP,2)*LSYS_BOARD(SYS),  
     *                        CUPCNT(SYS,GAM,STYP,3),                  
     *                        CUPCNT(SYS,GAM,STYP,3)*LSYS_BOARD(SYS),  
     *                        CUPCNT(SYS,GAM,STYP,4),                  
     *                        CUPCNT(SYS,GAM,STYP,4)*LSYS_BOARD(SYS),  
     *                        CUPCNT(SYS,GAM,STYP,5),                  
     *                        CUPCNT(SYS,GAM,STYP,5)*LSYS_BOARD(SYS)   
            DO 630 W=1,5                                                        
               TOTBDS(GAM,W,STYP) = TOTBDS(GAM,W,STYP) +               
     *                           CUPCNT(SYS,GAM,STYP,W)*LSYS_BOARD(SYS)
               TOTCUP(GAM,W,STYP) = TOTCUP(GAM,W,STYP)+CUPCNT(SYS,GAM,STYP,W)
630         CONTINUE                                                            
600      CONTINUE                                                               

C LOTTO REDUCED SYSTEMS BETS
C --------------------------
         STYP = 3
         DO 601 SYS=1,LSYSMAX
            CHECK = CUPCNT(SYS,GAM,STYP,1) + CUPCNT(SYS,GAM,STYP,2) +
     *              CUPCNT(SYS,GAM,STYP,3) + CUPCNT(SYS,GAM,STYP,4) +
     *              CUPCNT(SYS,GAM,STYP,5)                           
            IF(CHECK.EQ.0) GOTO 601                                             
            WRITE(REPLU,9014) LSYS_BOARD(SYS),'R',CUPCNT(SYS,GAM,STYP,1),
     *                        CUPCNT(SYS,GAM,STYP,1)*LSYS_BOARD(SYS),
     *                        CUPCNT(SYS,GAM,STYP,2),                
     *                        CUPCNT(SYS,GAM,STYP,2)*LSYS_BOARD(SYS),
     *                        CUPCNT(SYS,GAM,STYP,3),                
     *                        CUPCNT(SYS,GAM,STYP,3)*LSYS_BOARD(SYS),
     *                        CUPCNT(SYS,GAM,STYP,4),                
     *                        CUPCNT(SYS,GAM,STYP,4)*LSYS_BOARD(SYS),
     *                        CUPCNT(SYS,GAM,STYP,5),                
     *                        CUPCNT(SYS,GAM,STYP,5)*LSYS_BOARD(SYS) 
            DO 631 W=1,5                                                        
               TOTBDS(GAM,W,STYP) = TOTBDS(GAM,W,STYP) +      
     *                           CUPCNT(SYS,GAM,STYP,W)*LSYS_BOARD(SYS)
               TOTCUP(GAM,W,STYP) = TOTCUP(GAM,W,STYP)+CUPCNT(SYS,GAM,STYP,W)
631         CONTINUE                                                            
601      CONTINUE                                                               
C                                                                               
         WRITE(REPLU,9005) TOTCUP(GAM,1,2)+TOTCUP(GAM,1,3),
     *                     TOTBDS(GAM,1,2)+TOTBDS(GAM,1,3),
     *                     TOTCUP(GAM,2,2)+TOTCUP(GAM,2,3), 
     *                     TOTBDS(GAM,2,2)+TOTBDS(GAM,2,3),
     *                     TOTCUP(GAM,3,2)+TOTCUP(GAM,3,3), 
     *                     TOTBDS(GAM,3,2)+TOTBDS(GAM,3,3),
     *                     TOTCUP(GAM,4,2)+TOTCUP(GAM,4,3), 
     *                     TOTBDS(GAM,4,2)+TOTBDS(GAM,4,3),
     *                     TOTCUP(GAM,5,2)+TOTCUP(GAM,5,3), 
     *                     TOTBDS(GAM,5,2)+TOTBDS(GAM,5,3)

C LOTTO SIMPLE BETS
C -----------------
         WRITE(REPLU,9003) 'SIMPLE'                                             
C                                                                               
         STYP = 1
         DO 700 SYS=1,MAXBDS
            WRITE(REPLU,9004) SYS,(CUPCNT(SYS,GAM,STYP,S),      
     *                        CUPCNT(SYS,GAM,STYP,S)*SYS,S=1,5) 
            DO 710 W=1,5                                                        
               TOTCUP(GAM,W,STYP)=TOTCUP(GAM,W,STYP)+CUPCNT(SYS,GAM,STYP,W)
               TOTBDS(GAM,W,STYP)=TOTBDS(GAM,W,STYP)+ 
     *                        (SYS*CUPCNT(SYS,GAM,STYP,W))
710         CONTINUE                                                            
700      CONTINUE                                                               
         WRITE(REPLU,9005) TOTCUP(GAM,1,STYP),TOTBDS(GAM,1,STYP),
     *                     TOTCUP(GAM,2,STYP),TOTBDS(GAM,2,STYP),
     *                     TOTCUP(GAM,3,STYP),TOTBDS(GAM,3,STYP),
     *                     TOTCUP(GAM,4,STYP),TOTBDS(GAM,4,STYP),
     *                     TOTCUP(GAM,5,STYP),TOTBDS(GAM,5,STYP)

C GRAND TOTAL FOR ALL BETS
C ------------------------
         WRITE(REPLU,9006)  TOTCUP(GAM,1,1)+TOTCUP(GAM,1,2)+TOTCUP(GAM,1,3),
     *                      TOTBDS(GAM,1,1)+TOTBDS(GAM,1,2)+TOTBDS(GAM,1,3),
     *                      TOTCUP(GAM,2,1)+TOTCUP(GAM,2,2)+TOTCUP(GAM,2,3),
     *                      TOTBDS(GAM,2,1)+TOTBDS(GAM,2,2)+TOTBDS(GAM,2,3),
     *                      TOTCUP(GAM,3,1)+TOTCUP(GAM,3,2)+TOTCUP(GAM,3,3),
     *                      TOTBDS(GAM,3,1)+TOTBDS(GAM,3,2)+TOTBDS(GAM,3,3),
     *                      TOTCUP(GAM,4,1)+TOTCUP(GAM,4,2)+TOTCUP(GAM,4,3),
     *                      TOTBDS(GAM,4,1)+TOTBDS(GAM,4,2)+TOTBDS(GAM,4,3),
     *                      TOTCUP(GAM,5,1)+TOTCUP(GAM,5,2)+TOTCUP(GAM,5,3),
     *                      TOTBDS(GAM,5,1)+TOTBDS(GAM,5,2)+TOTBDS(GAM,5,3)    
C                                                                               
C     ================== End of Main Loop =====================                 

500   CONTINUE                                                                  

C SPOOL REPORT TO THE PRINTER                                                   
C ---------------------------
      CLOSE(REPLU)                                                              
      CALL SPOOL('LTODIS.REP',COPY,ST)                                          
      IF(ST.NE.0) THEN                                                          
         TYPE*,IAM(),'Error while spooling report  st - ',ST
         STOP                                                                   
      ENDIF                                                                     

C     ================== FORMAT STATEMENTS ======================               
C                                                                               
9000  FORMAT(1X,131('='),//)                                                    
9001  FORMAT('LOTTO COUPON DISTRIBUTION FOR WEEK ',I2.2,'/',I4.4)               
9002  FORMAT(2X,'GAME: ',A8,I2)                         
9003  FORMAT(///,18X,'1-WEEK',17X,'2-WEEK',17X,'3-WEEK',                        
     *          17X,'5-WEEK',16X,'10-WEEK',/,7X,5(4X,19('-')),/,
     *       4X,A6,5(' NUMBER ',3X,'NUMBER OF',3X),/,                           
     *       6X,'BET',4X,5('SOLD',6X,'BOARDS',7X),/)                            
9004  FORMAT(5X,I4,2X,5(I7,4X,I8,4X))                                           
9014  FORMAT(5X,I4,A1,1X,5(I7,4X,I8,4X))                                        
9005  FORMAT(/,' TOTAL ',2X,5(I9,3X,I9,2X))                                     
9006  FORMAT(/,' GND TOT ',5(I9,3X,I9,2X))                                      
C                                                                               
      END                                                                       
