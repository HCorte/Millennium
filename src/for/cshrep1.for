C
C $Log:   GXAFXT:[GOLS]CSHREP1.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:45:10   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.16   07 Feb 1996 10:16:32   RXK
C  CASH.FIL size increased from 10000 to 12000
C  
C     Rev 1.14   31 Jan 1996 15:29:04   RXK
C  Sort table increased from 80000 to 100000
C  
C     Rev 1.13   12 Dec 1994 10:13:50   PXB
C  Bug fixes. Now totals up correctly.
C  
C     Rev 1.12   02 Sep 1994 18:02:36   HXK
C  Merge of May,June RFSS batch 
C  
C     Rev 1.12   27 Apr 1994 16:23:44   JXP
C  COPY=0
C  
C     Rev 1.11   22 Jan 1994 15:06:40   JXP
C  Increased sort table from 60000 to 80000
C  
C     Rev 1.10   18 Oct 1993 16:52:14   HXK
C  FIX FOR REFUND IN GAME TOTALS.
C CSHREP1.FCC                                                                    
C            
C Taken from CSHREP.FTN (FINLAND) - 19/08/93
C                                                                   
C V09 16-JAN-01 EPH REMOVE VK2PAMT
C V08 20-OCT-92 HHE OWN REPORT FOR TOTAL PAGE                                   
C V07 ??-MAR-92 HJK FIXED TOTALS ERROR                                          
C V06 21-NOV-91 STC MODIFIED FOR DOUBLE JOKERI                                  
C V05 05-JUN-91 PP  ADDED SUBROUTINE CALL (BALWRI)                              
C V04 28-SEP-90 PP -REMOVED SORT BY CARTEL NUMBER                               
C                  -CHANGED FORMAT STATEMENT (9001) TO PRINT                    
C                   AGENT NUMBER WITH LEADING ZEROES                            
C V03 22-AUG-90 HHE INCORPORATED SORT BY CARTEL NUMBER REMOVED                  
C V02 05-FEB-90 MGM INCORPORATED SORT BY CARTEL NUMBER                          
C V01 04-OCT-89 MGM INITIAL RELEASE FOR FINLAND                                 
C                                                                               
C CASHED TICKET REPORT                                                          
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
C Copyright 1991 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT                                                                       
        PROGRAM CSHREP1                                                            
        IMPLICIT NONE
C       

        INCLUDE 'INCLIB:SYSPARAM.DEF'                                                           
        INCLUDE 'INCLIB:SYSEXTRN.DEF'                                                           
                                                                        
        INCLUDE 'INCLIB:GLOBAL.DEF'                                                           
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:HSHCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:CSHREC.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
C                               
        ! parameters                                        
        INTEGER*4  TUBSIZ                 !
        PARAMETER (TUBSIZ=I4BUCSIZ*7)                                          

        INTEGER*4  CSHSIZ                 !
        PARAMETER (CSHSIZ=20000)                                                
C                 
        ! variables
        INTEGER*4  SCFFDB(7)              !
        INTEGER*4  FDB(7)                 !
        INTEGER*4  SCFNAM(5)              !                   
        INTEGER*4  VLFBUF(TUBSIZ)         !                                      
C       INTEGER*4  CARTEL(NUMAGT)       ;V03               
        INTEGER*4  SORT(5,150000)         !
        INTEGER*4  CSHFIL(5)              !
	EQUIVALENCE (SFNAMES(1,CASHFIL),CSHFIL)                       
        INTEGER*4  PAIDCNT /0/            !
        INTEGER*4  LINCNT /70/            !
        INTEGER*4  COUNT /0/              !            
        INTEGER*4  PAIDCNT2 /0/           !
        INTEGER*4  LINCNT2 /70/           !
        INTEGER*4  COUNT2 /0/             !          
                                          
        INTEGER*4  TOTPAID /0/            !
        INTEGER*4  TOTOTHR /0/            !
        INTEGER*4  OLDBLK/0/              !            
        INTEGER*4  TOTREF/0/              !
        INTEGER*4  TOTPAID2 /0/           !
        INTEGER*4  TOTOTHR2 /0/           !                        
        INTEGER*4  TOT2KIK  /0/           !
        INTEGER*4  TOT2KIK2 /0/           !                        
        INTEGER*4  TOTREF2/0/             !
        INTEGER*4  GIND /0/               !
        INTEGER*4  KIND /0/               !
        INTEGER*4  BLK /1/                !
        INTEGER*4  IND /0/                !         
        INTEGER*4  NEWSIZE                                                      
        INTEGER*4  GAMPAID(MAXGAM) /MAXGAM*0/    !                               
        INTEGER*4  GAMPAID2(MAXGAM) /MAXGAM*0/   !                               
        INTEGER*4  NOCHECK0               !
        INTEGER*4  PAGE                   !
        INTEGER*4  COPY                   !
        INTEGER*4  EXT                    !
        INTEGER*4  ANSWER                 !
        INTEGER*4  CDCOVR                 !
        INTEGER*4  ST                     !
        INTEGER*4  I                      !
        INTEGER*4  REC                    !
        INTEGER*4  K                      !
        INTEGER*4  REFAMT                 !
        INTEGER*4  AMOUNT1                !
        INTEGER*4  AMOUNT2                !
        INTEGER*4  AMOUNT3                !
        INTEGER*4  STATUS                 !
        INTEGER*4  PAGE1                  !
        INTEGER*4  PAGE2                  !
        INTEGER*4  WEEK                   !
	INTEGER*4  YEAR2		  !
        INTEGER*4  JUL                    !
        INTEGER*4  SER                    !
        INTEGER*4  CHK                    !
        INTEGER*4  START                  !
        INTEGER*4  SCNT                   !
        INTEGER*4  SSER                   !
        INTEGER*4  SCHK                   !
        INTEGER*4  SCDC                   !
        INTEGER*4  STER                   !
        INTEGER*4  CSER                   !
        INTEGER*4  CCHK                   !
        INTEGER*4  CCDC                   !
        INTEGER*4  GAM                    !
        INTEGER*4  CSHTER                 !
        INTEGER*4  SAGNT                  !
        INTEGER*4  CAGNT                  !

        INTEGER*4  GAMESUMS(MAXGAM,NUMFIN,NUMTOT) !
        INTEGER*4  TOTSUMS(NO_BALSUMS)            !

        INTEGER*4  RAPCODE                !

        REAL*8     TOTREAL                !

        INTEGER*2  I2SORT(10,150000)       !
        INTEGER*2  DATE(LDATE_LEN)         !                    


        LOGICAL    OVERIDE /.FALSE./      !                                      

        CHARACTER  KGAME*4 /' '/          !
        CHARACTER  HEAD*38                !                       
        CHARACTER  MGAME*4 /' '/          !
        CHARACTER  HEAD2*38               !                       
        CHARACTER  RNAME*12               !                                      

        DATA   SCFNAM/'SCF.','FIL ',3*'    '/                               
        DATA   PAGE/0/,PAGE1/0/,PAGE2/0/                                                     

        EQUIVALENCE (SORT,I2SORT)                                                 

        COMMON       SCFREC                                                       
        COMMON      /NOCHECK0/ NOCHECK0                                           
        NOCHECK0=-1                                                               
C                                                                               
C CALL  COPYRITE  SUBROUTINE                                                    
C                                                                               
        CALL COPYRITE                                                             
C                                                                               
C                                                                               
        IF(DAYSTS.NE.DSCLOS) THEN                                                 
            TYPE*,IAM(),'Invalid day status ',DAYSTS                                      
            CALL GPAUSE                                                                   
        ENDIF                                                                     

        TYPE *                                                                    
        TYPE *,'<<<<< CSHREP1 Tickets Cashed Report           V02 >>>>>'           
        TYPE *                                                                    

C       CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)               
C       IF(EXT.LT.0) STOP                                                         
        COPY = 0

        CALL PRMYESNO('Overide CDC date to report on (Y/N) ? ',ANSWER)                      
        IF (ANSWER.EQ.1) OVERIDE = .TRUE.                                         
        IF (OVERIDE) THEN                                                         
            CALL PRMDAT(CDCOVR,EXT)                                                   
        ELSE                                                                      
            CDCOVR = DAYCDC                                                        
        ENDIF                                                                     
C                                                                               
C READ SCF RECORD                                                               
C                                                                               
        CALL OPENW(1,SCFNAM,4,0,0,ST)                                             
        CALL IOINIT(SCFFDB,1,SCFSEC*256)                                              
        IF(ST.NE.0) CALL FILERR(SCFNAM,1,ST,0)                                    

        CALL READW(SCFFDB,1,SCFREC,ST)                                            
        IF(ST.NE.0) CALL FILERR(SCFNAM,2,ST,1)                                    
        CALL CLOSEFIL(SCFFDB)                                                     

        DO I=1,MAXFIL                                                          
            IF(SCFSFN(1,I).EQ.'    ') CALL SYSVOL(SCFSFN(1,I))                     
        END DO
        IND=0                                                                     
C                                                                               
C OPEN THE AGENT SALES FILE AND BUILD CARTEL TABLE, COMMENTED V03               
C                                                                               
C       CALL OPENASF(ASF)                                                        
C       DO REC=1,NUMAGT                                                      
C           CALL READASF(REC,ASFREC,ST)                                           
C           CARNUM=0                                                              
C           CALL ASCBIN(ASFINF,SCART,LCART,CARNUM,CERR)                           
C           CARTEL(REC)=CARNUM                                                     
C       END DO
C       CALL CLOSASF                                                             
C                                                                               
C ALLOCATE A CASHED TICKET WORK FILE                                            
C                                                                               
        NEWSIZE=SFSIZES(CASHFIL)
510     CONTINUE                                                                  
        CALL CRTFIL(CSHFIL,NEWSIZE,ST)                                            
        IF(ST.NE.0) THEN                                                          
            TYPE*,IAM(),'CASH.FIL allocation error '                                      
            CALL PRMNUM('Enter new work file size: ',NEWSIZE,1,20000,ST)            
            IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)                                                        
            GOTO 510                                                                
        ENDIF                                                                     
C                                                                               
C OPEN CASHED TICKETS WORK FILE                                                 
C                                                                               
        CALL OPENW(1,CSHFIL,4,0,0,ST)                                         
        CALL IOINIT(FDB,1,128*256)                                                    
        IF(ST.NE.0) THEN                                                          
            TYPE*,IAM(),'CASH.FIL open error - ',ST                                       
            CALL GPAUSE                                                                   
        ENDIF                                                                     
C                                                                               
C OPEN VALIDATION FILE FOR SEQUENTIAL READ                                      
C                                                                               
        CALL IOPEN(SCFSFN(1,VLF),VLF,VFLEN*2,VFSCDC,VFSSER*2-1,ST)                
        IF(ST.NE.0) CALL FILERR(SCFSFN(1,VLF),1,ST,0)                             
        CALL ITUBSIZE(VLF,TUBSIZ)                                                 
C                                                                               
C OPEN REPORT FILE(S)                                                           
C                                                                               
        WRITE(RNAME,8003) DAYCDC                                                 
        CALL ROPEN(RNAME,6,ST)                                                    
        IF(ST.NE.0) THEN                                                          
            TYPE*,IAM(),RNAME,' open error - ',ST                                         
            CALL GPAUSE                                                                   
        ENDIF                                                                     
C                                                                               
        CALL ROPEN('CSHREP1.REP',7,ST)                                            
        IF(ST.NE.0) THEN                                                          
            TYPE*,IAM(),'CSHREP1.REP open error - ',ST                                    
            CALL GPAUSE                                                                   
        ENDIF                                                                     
C                                                                               
        CALL ROPEN('CSHREP.REP',8,ST)                                             
        IF(ST.NE.0) THEN                                                          
            TYPE*,IAM(),'CSHREP.REP open error - ',ST                                     
            CALL GPAUSE                                                                   
        ENDIF                                                                     
C                                                                               
C                                                                               
        DATE(VCDC) = CDCOVR                                                       
        CALL LCDATE(DATE)                                                          
        CALL FIGWEK(CDCOVR,WEEK,YEAR2)                                                  
        WRITE(HEAD,8000) WEEK,YEAR2                                             
        WRITE(HEAD2,8002) WEEK,YEAR2                                            
C                                                                               
C READ THE VALIDATION FILE                                                      
C                                                                               
        TYPE*,IAM(),'Extracting cashed tickets from the VLF.FIL'                        
100     CONTINUE                                                                  

        CALL ISREAD(V4BUF,VLF,VLFBUF,ST)                                          
        IF(ST.EQ.ERREND) GOTO 1000                                                
        IF(ST.NE.0) THEN                                                          
            TYPE*,IAM(),'VLF.FIL read error - ',ST                                        
            CALL GPAUSE                                                                   
        ENDIF                                                                     
                                                                                
        CALL LOGVAL(VALREC,V4BUF)                                                 
        IF(VALREC(VCCDC).NE.CDCOVR) GOTO 100                                      
        IF((VALREC(VPAMT)+VALREC(VKPAMT)+VALREC(VRAMT)).EQ.0) GOTO 100                          

C                                                                               
C WRITE CASHED TICKET TO WORK FILE WHEN RECORD IS FULL                          
C                                                                               
        COUNT=COUNT+1                                                             
        IND=IND+1                                                                 
        IF(IND.GT.CSHBLK) THEN                                                    
            CALL WRITEW(FDB,BLK,CSHREC,ST)                                         
            IF(ST.NE.0) THEN                                                       
                TYPE*,IAM(),'CASH.FIL write error - ',ST,' record ',BLK                   
                CALL GPAUSE                                                               
            ENDIF                                                                  
            BLK=BLK+1                                                              
            IND=1                                                                  
            CALL FASTSET(0,CSHREC,CSHLEN)                                          
        ENDIF                                                                     
C                                                                               
        CALL FASTMOV(V4BUF,CSHBUF(1,IND),VFLEN)                                   
C                                                                               
C WRITE RECORD TO MEMORY SORT TABLE                                             
C                                                                               
        IF(COUNT.GT.150000) THEN                                                   
            TYPE*,IAM(),'Memory sort table overflow '                                     
            CALL GPAUSE                                                                   
        ENDIF                                                                     
C                                                                               
        DATE(VCDC)=VALREC(VSCDC)                                                  
        CALL LCDATE(DATE)                                                          
        JUL=DATE(VJUL)                                                            
        CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SER,CHK)                          
C                                                                               
        SER = SER*100+CHK                                                           
        SORT(1,COUNT) = JUL                                                         
        SORT(2,COUNT) = SER                                                         
        SORT(3,COUNT) = 0      ! 0 INSTEAD OF CARTEL(VALREC(VSTER)), V03            

        I2SORT(7,COUNT) = BLK                                                       
        I2SORT(8,COUNT) = IND                                                       
C                                                                               
        GOTO 100                                                                  
C                                                                               
C CLOSE VALIDATION FILE AND WRITE LAST BLOCK TO WORK FILE                       
C                                                                               
1000    CONTINUE   
                                                               
        CALL ICLOSE(VLF,VLFBUF,ST)                                                
        CALL WRITEW(FDB,BLK,CSHBUF,ST)                                            
        IF(ST.NE.0) THEN                                                          
            TYPE*,IAM(),'CASH.FIL write error - ',ST,' record - ',BLK                     
            CALL GPAUSE                                                                   
        ENDIF                                                                     
C                                                                               
C GENERATE REPORT SORTED BY (CARTEL AND) TICKET NUMBER                          
C                                                                               
        TYPE*,IAM(),'Generating report(s) sorted by day and ticket number'              
C                                                                               
C     SORT BY CARTEL NUMBER TAKEN OUT FROM HERE  ;V04                           
        CALL ISORT5(SORT,COUNT,1)                                                 

2010    CONTINUE                                                                  

        TYPE*,IAM(),'Sort pass 1 complete'                                              
        START=1                                                                   
        SCNT=1                                                                    
        DO I=2,COUNT+1                                                       
            IF(SORT(1,I).NE.SORT(1,I-1)) THEN                                      
                CALL ISORT5(SORT(1,START),SCNT,2)                                   
                SCNT=0                                                              
                START=I                                                             
            ENDIF                                                                  
            SCNT=SCNT+1                                                            
        END DO
        TYPE*,IAM(),'Sort completed'                                                    
C                                                                               
        DO 3000 I=1,COUNT                                                         
            BLK=I2SORT(7,I)                                                        
            IND=I2SORT(8,I)                                                        
            IF(BLK.NE.OLDBLK) THEN                                                 
                CALL READW(FDB,BLK,CSHBUF,ST)                                       
                IF(ST.NE.0) THEN                                                    
                    TYPE*,IAM(),'CASH.FIL read error - ',ST                                
                    CALL GPAUSE                                                            
                ENDIF                                                               
                OLDBLK=BLK                                                          
            ENDIF                                                                  
            CALL LOGVAL(VALREC,CSHBUF(1,IND))                                      
C                                                                               
C   GET SERIAL NUMBER FOR THE WAGER                                             
C                                                                               
            CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SSER,SCHK)                     
            DATE(VCDC)=VALREC(VSCDC)                                               
            CALL LCDATE(DATE)                                                       
            SCDC = DATE(VJUL)                                                      
            STER=VALREC(VSTER)                                                     
            SAGNT=0                                                                
            IF(STER.GT.0) SAGNT=AGTTAB(AGTNUM,STER)                                
C                                                                               
C   GET SERIAL NUMBER FOR THE CASHED WAGER                                      
C                                                                               
            CALL OUTGEN(VALREC(VCCDC),VALREC(VCSER),CSER,CCHK)                     
            DATE(VCDC) = VALREC(VCCDC)                                             
            CALL LCDATE(DATE)                                                       
            CCDC = DATE(VJUL)                                                      
            CSHTER=VALREC(VCTER)                                                   
            CAGNT=0                                                                
            IF(CSHTER.GT.0) CAGNT=AGTTAB(AGTNUM,CSHTER)                            
C                                                                               
C     PRINT DETAIL LINE OF REPORT                                               
C                                                                               
C KICKER BOUGHT ALONE                                                           
C                                                                               
            IF(VALREC(VPAMT).EQ.0.AND.VALREC(VRAMT).EQ.0) THEN                                            
                GIND = 0                                                            
                WRITE(KGAME,8001) SCFSGN(VALREC(VKGME))                            
                KIND = SCFGNT(GAMIDX,VALREC(VKGME))                                 
                GAM=VALREC(VKGME)                                                   

                AMOUNT1 = 0                                                         
                AMOUNT2 = VALREC(VKPAMT)   !-VALREC(VK2PAMT)   !V09
                AMOUNT3 = 0                !VALREC(VK2PAMT)    !V09   
                REFAMT  = VALREC(VRAMT)                        
                WRITE(MGAME,8001) '----'                                           
            ELSE                                                                   
C                                                                               
C OTHERS GAMES WITH POSSIBLY KICKER                                             
C                                                                               
                GIND = SCFGNT(GAMIDX,VALREC(VGAM))                                  
                AMOUNT1 = VALREC(VPAMT)                                             
                GAM =  VALREC(VGAM)                                                 
                AMOUNT2 = VALREC(VKPAMT)        !-VALREC(VK2PAMT)    !V09
                AMOUNT3 = 0                     !VALREC(VK2PAMT)     !V09
                REFAMT = VALREC(VRAMT)                      
                WRITE(MGAME,8001) SCFSGN(VALREC(VGAM))                             

                IF(VALREC(VKPAMT).GT.0)THEN                                         
                    KIND = SCFGNT(GAMIDX,VALREC(VKGME))                              
                    WRITE(KGAME,8001) SCFSGN(VALREC(VKGME))                         
                ELSE                                                                
                    WRITE(KGAME,8001) '----'                                        
                    KIND = 0                                                         
                ENDIF                                                               
            ENDIF                                                                  
C                                                                               
            IF(LINCNT.GT.50) THEN                                                  
                CALL TITLE(HEAD,'CSHREP  ',1,6,PAGE,DAYCDC)                                            
                WRITE(6,9000)                                                       
                LINCNT = 7                                                          
            ENDIF                                                                  
C                                                                               
            IF(LINCNT2.GT.50) THEN                                                 
                CALL TITLE(HEAD2,'CSHREP1 ',1,7,PAGE1,DAYCDC)                                           
                WRITE(7,9000)                                                       
                LINCNT2 = 7                                                         
            ENDIF                                                                  
C                                                                               
            WRITE(6,9001) CAGNT,
     *                    VALREC(VCTER),                    
     *                    SAGNT,
     *                    VALREC(VSTER),                    
     *                    MGAME,
     *                    GIND,                                              
     *                    KGAME,
     *                    KIND,                                              
     *                    SCDC,
     *                    SSER,
     *                    SCHK/10,
     *                    CCDC,
     *                    CSER,
     *                    CCHK/10,    
     *                    CMONY(REFAMT,11,VALUNIT),
     *                    CMONY(AMOUNT1,11,VALUNIT),
     *                    CMONY(AMOUNT2,11,VALUNIT),                
     *                    CMONY(AMOUNT3,11,VALUNIT)
            LINCNT=LINCNT+1                                                        
C          
            TOTREF  = TOTREF  + REFAMT                                                         
            TOTPAID = TOTPAID + AMOUNT1                                            
            TOTOTHR = TOTOTHR + AMOUNT2                                            
            TOT2KIK = TOT2KIK + AMOUNT3                                            
            PAIDCNT = PAIDCNT + 1            
            GAMPAID(VALREC(VGAM))=GAMPAID(VALREC(VGAM)) + VALREC(VPAMT) +
     *                            REFAMT
            IF(VALREC(VKGME).GT.0)                                                 
     *        GAMPAID(VALREC(VKGME))=GAMPAID(VALREC(VKGME))+VALREC(VKPAMT)           
C                                                                               
            IF(AMOUNT1+AMOUNT2+AMOUNT3 .LT. 1000*DOLL_BASE/DYN_VALUNIT)GOTO 3000                        
C                                                                               
C PRINT ALSO ON OTHER REPORT                                                    
C                                                                               
            WRITE(7,9001) CAGNT,
     *                    VALREC(VCTER),                    
     *                    SAGNT,
     *                    VALREC(VSTER),                    
     *                    MGAME,
     *                    GIND,                                              
     *                    KGAME,
     *                    KIND,                                              
     *                    SCDC,
     *                    SSER,
     *                    SCHK/10,
     *                    CCDC,
     *                    CSER,
     *                    CCHK/10,                 
     *                    CMONY(REFAMT,11,VALUNIT),    
     *                    CMONY(AMOUNT1,11,VALUNIT),
     *                    CMONY(AMOUNT2,11,VALUNIT),                
     *                    CMONY(AMOUNT3,11,VALUNIT)
            LINCNT2=LINCNT2+1                                                      
C                              
            TOTREF2  = TOTREF2  + REFAMT                                     
            TOTPAID2 = TOTPAID2 + AMOUNT1                                          
            TOTOTHR2 = TOTOTHR2 + AMOUNT2                                          
            TOT2KIK2 = TOT2KIK2 + AMOUNT3                                          
            PAIDCNT2 = PAIDCNT2 + 1                                                
            GAMPAID2(VALREC(VGAM))=GAMPAID2(VALREC(VGAM))+VALREC(VPAMT)+
     *                             REFAMT            
            IF(VALREC(VKGME).GT.0)                                                 
     *        GAMPAID2(VALREC(VKGME))=GAMPAID2(VALREC(VKGME))+                       
     *        VALREC(VKPAMT)                                                         

3000    CONTINUE                                                                  
C                                                                               
C REPORT TOTALS                                                                 
C                                                                               
        IF(LINCNT.GT.50) THEN                                                     
            CALL TITLE(HEAD,'CSHREP  ',1,6,PAGE,DAYCDC)                                               
            WRITE(6,9000)                                                          
            LINCNT = 7                                                             
        ENDIF                                                                     
C                                                                               
        IF(LINCNT2.GT.50) THEN                                                    
            CALL TITLE(HEAD2,'CSHREP1 ',1,7,PAGE1,DAYCDC)                                              
            WRITE(7,9000)                                                          
            LINCNT2 = 7                                                            
        ENDIF                                                                     
C                                                                               
        WRITE(6,9002) PAIDCNT, 
     *                CMONY(TOTREF,11,VALUNIT),
     *                CMONY(TOTPAID,11,VALUNIT),
     *                CMONY(TOTOTHR,11,VALUNIT),
     *                CMONY(TOT2KIK,11,VALUNIT)

        WRITE(7,9002) PAIDCNT2, 
     *                CMONY(TOTREF2,11,VALUNIT),
     *                CMONY(TOTPAID2,11,VALUNIT),
     *                CMONY(TOTOTHR2,11,VALUNIT),
     *                CMONY(TOT2KIK2,11,VALUNIT)

        CALL TITLE(HEAD,'CSHREP  ',1,6,PAGE,DAYCDC)                               
        CALL TITLE(HEAD2,'CSHREP2 ',1,7,PAGE1,DAYCDC)                             
        CALL TITLE(HEAD,'CSHREP  ',1,8,PAGE2,DAYCDC)                                  
        WRITE(6,9003)                                                             
        WRITE(7,9003)                                                             
        WRITE(8,9003)                                                             
C                                                                               
        DO 4000 I=1, MAXGAM                                                       
            IF (GAMPAID(I).LE.0) GOTO 3500                                         
            WRITE(6,9004) (SCFLGN(K,I),K=1,4), 
     *                     CMONY(GAMPAID(I),13,VALUNIT)

            WRITE(8,9004) (SCFLGN(K,I),K=1,4), 
     *                     CMONY(GAMPAID(I),13,VALUNIT)
3500        CONTINUE                                                               

            IF (GAMPAID2(I).LE.0) GOTO 4000                                        
            WRITE(7,9004) (SCFLGN(K,I),K=1,4), 
     *                     CMONY(GAMPAID2(I),13,VALUNIT)
4000    CONTINUE                                                                  
C                                                                               
        WRITE(6,9005) CMONY((TOTPAID+TOTOTHR+TOT2KIK+TOTREF),14,VALUNIT)

        WRITE(8,9005) CMONY((TOTPAID+TOTOTHR+TOT2KIK+TOTREF),14,VALUNIT)

        WRITE(7,9005) CMONY((TOTPAID2+TOTOTHR2+TOT2KIK2+TOTREF2),14,VALUNIT)
C                                                                               
        CALL USRCLOS1(1)                                                             
        CALL USRCLOS1(9)                                                             
        CALL USRCLOS1(7)                                                             
        CALL USRCLOS1(8)                                                             
C                                                                               
C       TOTALS TO BALANSFILE             V05                                      
C                                                                               
        DO GAM = 1,MAXGAM                                                      
            GAMESUMS(GAM,3,2) = GAMPAID(GAM)                                       
        END DO
        TOTSUMS(6) = TOTPAID + TOTOTHR + TOT2KIK + TOTREF
        RAPCODE = 6                                                               
        CALL BALWRI(RAPCODE,GAMESUMS,TOTSUMS,TOTREAL)                             
C                                                                               
        CALL SPOOL('CSHREP.REP',COPY,STATUS)                                      
        CALL SPOOL('CSHREP1.REP',COPY,STATUS)                                     
        CALL SPOOL(RNAME,COPY,STATUS)                                     

C                                                                               
C       =================== FORMAT STATEMENTS =====================               
C                                                                               
8000    FORMAT('CASHED TICKETS REPORT FOR WEEK ',I2.2,'/',I4.4)                   
8001    FORMAT(A4)                                                                
8002    FORMAT('CASHES OVER 1,000.00 MK FOR    ',I2.2,'/',I4.4)                   
8003    FORMAT('CSHD',I4.4,'.REP')                                                
9000    FORMAT(2X,'CASHING CASHING  SELLING SELLING',/,                         
     *         2X,' AGENT  TERMINAL  AGENT  TERMINAL GAME/IND'                  
     *         ' OTH/IND',2X,'SELLING SERIAL',4X,'CASHING SERIAL',
     *         5X,'REF',
     *         4X,'GAME PAID',2X,'JOKER1 PAID',1X,'JOKER2 PAID',/,                
     *         1X,131('='),/)                                                     
9001    FORMAT(1X,I8.8,1X,I5,2X,I8.8,2X,I5,4X,A4,'/',I2.2,            
     *         2X,A4,'/',I2.2,2X,I3.3,'-',I8.8,'-',I2.2,'*',2X,I3.3,              
     *         '-',I8.8,'-',I2.2,'*',4(A11))                              
9002    FORMAT(/,2X,'TOTAL TICKETS',4X,I8,T89,4(A11))                     
9003    FORMAT(1X,131('='),///,T48,'GAME',T68,'TOTAL PAID')                       
9004    FORMAT(/,T45,4A4,4X,A13)                                         
9005    FORMAT(/,T48,'TOTAL',T64,A14)                                    
C                                                                               
        END                                                                       
