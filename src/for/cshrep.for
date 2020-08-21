C CSHREP.FOR
C
C V03 16-JAN-2001 EPH VK2PAMT WAS REMOVED
C V02 06-SEP-1999 UXN ISSUBPROC LOGIC ADDED. 
C V01 13-JAN-1999 GLS NEW RELEASE FOR FINLAND (FROM CSHREP1.FOR)      
C                                                                               
C CASHED TICKET REPORT - FULL VERSION + VMS SORT                     
C CASH.FIL REMOVED                                                   
C                                                                               
C PHASE 1 : WRITE SELECTED RECORDS TO CSHDnnn.REP & CSHREP1.REP (BIG PRIZES)
C PHASE 2 : SORT BY VMS BOTH FILES; OUTPUT = CSHDnnnn_PC.REP & CSHREP1_PC.REP
C PHASE 3 : ROPEN (DELETE) CSHDnnnn.REP & CSHREP1.REP
C PHASE 4 : REWRITE BOTH PC FILES TO CSHDnnnn.REP AND CSHREP1.REP ADDING 
C           HEADINGS AND TOTALS 
C           THE THIRD REPORT (CSHREP.REP) IS NOT MODIFIED
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
C Copyright 1999 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT                                                                       
        PROGRAM CSHREP                                                            
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
        ! variables
        INTEGER*4  VLFBUF(TUBSIZ)         !                                      
        INTEGER*4  PAIDCNT /0/            !
        INTEGER*4  LINCNT /70/            !
        INTEGER*4  COUNT /0/              !            
        INTEGER*4  PAIDCNT2 /0/           !
        INTEGER*4  LINCNT2 /70/           !
        INTEGER*4  COUNT2 /0/             !          
C                                          
        INTEGER*4  TOTPAID /0/            !
        INTEGER*4  TOTOTHR /0/            !
        INTEGER*4  TOTREF/0/              !
        INTEGER*4  TOTPAID2 /0/           !
        INTEGER*4  TOTOTHR2 /0/           !                        
        INTEGER*4  TOT2KIK  /0/           !
        INTEGER*4  TOT2KIK2 /0/           !                        
        INTEGER*4  TOTREF2/0/             !
        INTEGER*4  GIND /0/               !
        INTEGER*4  KIND /0/               !
        INTEGER*4  IND /0/                !         
        INTEGER*4  GAMPAID(MAXGAM) /MAXGAM*0/    !                               
        INTEGER*4  GAMPAID2(MAXGAM) /MAXGAM*0/   !                               
        INTEGER*4  PAGE                   !
        INTEGER*4  COPY                   !
        INTEGER*4  EXT                    !
        INTEGER*4  ANSWER                 !
        INTEGER*4  CDCOVR                 !
        INTEGER*4  ST                     !
        INTEGER*4  I                      !
        INTEGER*4  K                      !
        INTEGER*4  REFAMT                 !
        INTEGER*4  AMOUNT1                !
        INTEGER*4  AMOUNT2                !
        INTEGER*4  AMOUNT3                !
        INTEGER*4  PAGE1                  !
        INTEGER*4  PAGE2                  !
        INTEGER*4  WEEK                   !
	INTEGER*4  YEAR2		  !
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
        INTEGER*2  DATE(LDATE_LEN)        !                    
        LOGICAL    ISTHERE                ! FLAG TO KNOW IF FILE IS IN DISK
        LOGICAL    OVERIDE /.FALSE./      !                                      
        CHARACTER  KGAME*4 /' '/          !
        CHARACTER  HEAD*38                !                       
        CHARACTER  MGAME*4 /' '/          !
        CHARACTER  HEAD2*38               !                       
        CHARACTER  RNAME*12               !                                      
        CHARACTER  FILIN*20               !INPUT FILE NAME USED BY VMS SORT 
        CHARACTER  FILOUT*20              !OUTPUT FILE NAME USED BY VMS SORT
        CHARACTER  OUTREC*132             !RECORD BUFFER
        CHARACTER  VMSCMD*80              !VMS COMMAND BUFFER
C
        DATA   PAGE/0/,PAGE1/0/,PAGE2/0/
                                                     
C
        COMMON       SCFREC                                                       
C                                                                               
        CALL COPYRITE                                                             
C                                                                               
        IF(DAYSTS.NE.DSCLOS) THEN                                                 
            TYPE*,IAM(),'Invalid day status ',DAYSTS                                      
            CALL GPAUSE                                                                   
        ENDIF                                                                     

        TYPE *                                                                    
        TYPE *,IAM(),'<<<<< CSHREP Tickets Cashed Report           V03 >>>>>'           
        TYPE *                                                                    
C
C       CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)               
C       IF(EXT.LT.0) STOP                                                         
        COPY = 0
C
	CALL STTSK(8HSTSYSTEM,EXT,ST)
	IF(ST.NE.4.AND.ISSUBPROC()) THEN
	    ANSWER = 0      ! DEFAULT
	ELSE
            CALL PRMYESNO('Overide CDC date to report on (Y/N) ? ',ANSWER)
        ENDIF
        IF (ANSWER.EQ.1) OVERIDE = .TRUE.                                         
        IF (OVERIDE) THEN                                                         
            CALL PRMDAT(CDCOVR,EXT)                                                   
        ELSE                                                                      
            CDCOVR = DAYCDC                                                        
        ENDIF                                                                     
C                                                                               
C READ SCF RECORD                                                               
C      
	CALL GETSCONF(SCFREC,ST) 
        IND=0                                                                     
C                                                                               
C OPEN VALIDATION FILE FOR SEQUENTIAL READ                                      
C                                                                               
        CALL IOPEN(SCFSFN(1,VLF),VLF,VFLEN*2,VFSCDC,VFSSER*2-1,ST)                
        IF(ST.NE.0) CALL FILERR(SCFSFN(1,VLF),1,ST,0)                             
        CALL ITUBSIZE(VLF,TUBSIZ)                                                 
C                                                                               
C OPEN REPORT FILE(S)                                                           
C                                                                               
        WRITE(RNAME,8003) CDCOVR                                                 
        CALL ROPEN(RNAME,6,ST) !CSHDnnnn.FIL
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
        IF(ST.EQ.ERREND) GOTO 3000                                                
        IF(ST.NE.0) THEN                                                          
            TYPE*,IAM(),'VLF.FIL read error - ',ST                                        
            CALL GPAUSE                                                                   
        ENDIF                                                                     
C                                                                                
        CALL LOGVAL(VALREC,V4BUF)                                                 
        IF(VALREC(VCCDC).NE.CDCOVR) GOTO 100                                      
        IF((VALREC(VPAMT)+VALREC(VKPAMT)+VALREC(VRAMT)).EQ.0) GOTO 100                          
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
C KICKER BOUGHT ALONE                                                           
C                                                                               
        IF(VALREC(VPAMT).EQ.0.AND.VALREC(VRAMT).EQ.0) THEN                                            
          GIND = 0                                                            
          WRITE(KGAME,8001) SCFSGN(VALREC(VKGME))                            
          KIND = SCFGNT(GAMIDX,VALREC(VKGME))                                 
          GAM=VALREC(VKGME)                                                   
          AMOUNT1 = 0                                                         
          AMOUNT2 = VALREC(VKPAMT)      !-VALREC(VK2PAMT)    !V03
          AMOUNT3 = 0                   !VALREC(VK2PAMT)   !V03
          REFAMT  = VALREC(VRAMT)                        
          WRITE(MGAME,8001) '----'                                           
        ELSE                                                                   
C                                                                               
C OTHERS GAMES WITH POSSIBLY KICKER                                             
C                                                                               
          GIND = SCFGNT(GAMIDX,VALREC(VGAM))                                  
          AMOUNT1 = VALREC(VPAMT)                                             
          GAM =  VALREC(VGAM)                                                 
          AMOUNT2 = VALREC(VKPAMT)            !-VALREC(VK2PAMT)    !V03                            
          AMOUNT3 = 0                         !VALREC(VK2PAMT)     !V03     
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
C WRITE CASHED TICKET TO THE BUFFER                          
C                                                                               
        WRITE(OUTREC,9001)CAGNT,
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
C
C     PRINT DETAIL LINE OF REPORT                                               
C       
        WRITE(6,9010) OUTREC
        COUNT=COUNT+1                                                             
C
C GET TOTALS
C
        TOTREF  = TOTREF  + REFAMT                                                         
        TOTPAID = TOTPAID + AMOUNT1                                            
        TOTOTHR = TOTOTHR + AMOUNT2                                            
        TOT2KIK = TOT2KIK + AMOUNT3                                            
        PAIDCNT = PAIDCNT + 1            
        GAMPAID(VALREC(VGAM))=GAMPAID(VALREC(VGAM)) + VALREC(VPAMT) +
     *                        REFAMT
        IF(VALREC(VKGME).GT.0)                                                 
     *    GAMPAID(VALREC(VKGME))=GAMPAID(VALREC(VKGME))+VALREC(VKPAMT)           
C                                                                             
        IF(AMOUNT1+AMOUNT2+AMOUNT3 .LT. 1000*DOLL_BASE/DYN_VALUNIT)GOTO 100                        
C                                                                              
C PRINT ALSO ON OTHER REPORT                                                    
C                                                                               
        WRITE(7,9010) OUTREC
        COUNT2=COUNT2+1                                                             
C                             
C GET TOTALS
C
        TOTREF2  = TOTREF2  + REFAMT                                     
        TOTPAID2 = TOTPAID2 + AMOUNT1                                          
        TOTOTHR2 = TOTOTHR2 + AMOUNT2                                          
        TOT2KIK2 = TOT2KIK2 + AMOUNT3                                          
        PAIDCNT2 = PAIDCNT2 + 1                                                
        GAMPAID2(VALREC(VGAM))=GAMPAID2(VALREC(VGAM))+VALREC(VPAMT)+
     *                         REFAMT            
        IF(VALREC(VKGME).GT.0)                                                 
     *    GAMPAID2(VALREC(VKGME))=GAMPAID2(VALREC(VKGME))+                       
     *    VALREC(VKPAMT)                                                         
C
        GOTO 100                                                                  
C
3000    CONTINUE                                                                  
C
        CALL ICLOSE(VLF,VLFBUF,ST)                                                
        CALL USRCLOS1(6)                                                             
        CALL USRCLOS1(7)  
C
        TYPE *,IAM(),COUNT,' cash records listed in ',
     *                                    RNAME(1:8),'_PC.REP file'
        TYPE *,IAM(),COUNT2,' cash records listed in ',
     *                                    'CSHREP1_PC.REP file'
                                                           
C
C REMOVE OLD VERSION OF PC REPORTS
C
        VMSCMD=' '
        VMSCMD(1:10)='DEL/NOLOG '
        VMSCMD(11:30)=RNAME(1:8)//'_PC.REP.*'
        INQUIRE(FILE = VMSCMD(11:30), EXIST = ISTHERE)
        IF(ISTHERE .EQ. .TRUE.) CALL DCLCOM1(VMSCMD)
C
        VMSCMD=' '
        VMSCMD(1:10)='DEL/NOLOG '
        VMSCMD(11:30)='CSHREP1_PC.REP.*'
        INQUIRE(FILE = VMSCMD(11:30), EXIST = ISTHERE)
        IF(ISTHERE .EQ. .TRUE.) CALL DCLCOM1(VMSCMD)
C
C SORT BY VMS
C
        TYPE *,IAM(),'Sorting file ',RNAME
        VMSCMD=' '
        VMSCMD(1 :30)='$SORT/KEY=(POS:55,SIZ:15)     '
        FILIN=' '
        FILIN=RNAME
        FILOUT=' '
        FILOUT=RNAME(1:8)//'_PC.REP'
        VMSCMD(31:50)=FILIN
        VMSCMD(51:70)=FILOUT
        CALL DCLCOM1(VMSCMD)
C
        TYPE *,IAM(),'Sorting file CSHREP1.REP'
        VMSCMD=' '
        VMSCMD(1 :30)='$SORT/KEY=(POS:55,SIZ:15)     '
        FILIN=' '
        FILIN='CSHREP1.REP'
        FILOUT=' '
        FILOUT='CSHREP1_PC.REP'
        VMSCMD(31:50)=FILIN
        VMSCMD(51:70)=FILOUT
        CALL DCLCOM1(VMSCMD)
C                                                                               
C OPEN AGAIN REPORT FILES FOR WRITE                                                          
C                                                                               
        CALL ROPEN(RNAME,6,ST) !CSHDnnnn.FIL
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
C OPEN PC REPORT FILES FOR READ                                                           
C                                                                               

        OPEN(16,FILE=RNAME(1:8)//'_PC.REP',ACCESS='SEQUENTIAL',
     *       STATUS='OLD',RECL=133,IOSTAT=ST)                 !CSHDnnnn_PC.FIL
        IF(ST.NE.0) THEN                                                          
            TYPE*,IAM(),RNAME(1:8),'_PC.REP',' open error - ',ST                                         
            CALL GPAUSE                                                                   
        ENDIF                                                                     
C                                                                               
        OPEN(17,FILE='CSHREP1_PC.REP',ACCESS='SEQUENTIAL',
     *       STATUS='OLD',RECL=133,IOSTAT=ST)   
        IF(ST.NE.0) THEN                                                          
            TYPE*,IAM(),'CSHREP1_PC.REP open error - ',ST                                    
            CALL GPAUSE                                                                   
        ENDIF                                                                     
C
C REWRITE RECORDS FORM PC FILES TO ORIGINAL FILES ADDING HEADINGS
C
        TYPE *,IAM(),'Re-writing report files'
3100    CONTINUE
        IF(LINCNT.GT.50) THEN                                              
          CALL TITLE(HEAD,'CSHREP  ',1,6,PAGE,DAYCDC)                    
          WRITE(6,9000)                                                  
          LINCNT = 7                                                     
        ENDIF                                                              
C
        READ(16,9010,END=3200)OUTREC
        WRITE(6,9010)OUTREC
        LINCNT=LINCNT+1                                                    
        GOTO 3100 
C
3200    CONTINUE
        IF(LINCNT2.GT.50) THEN                                                    
          CALL TITLE(HEAD2,'CSHREP1 ',1,7,PAGE1,DAYCDC)                                              
          WRITE(7,9000)                                                          
          LINCNT2 = 7                                                            
        ENDIF                                                                     
C
        READ(17,9010,END=3250)OUTREC
        WRITE(7,9010)OUTREC
        LINCNT2=LINCNT2+1                                                    
        GOTO 3200 
C
C ADD REPORT TOTALS                                                                 
C                                                                               
3250    CONTINUE
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
     *                   CMONY(GAMPAID(I),13,VALUNIT)
          WRITE(8,9004) (SCFLGN(K,I),K=1,4), 
     *                   CMONY(GAMPAID(I),13,VALUNIT)
3500      CONTINUE                                                               
          IF (GAMPAID2(I).LE.0) GOTO 4000                                        
          WRITE(7,9004) (SCFLGN(K,I),K=1,4), 
     *                   CMONY(GAMPAID2(I),13,VALUNIT)
4000    CONTINUE                                                                  
C                                                                               
        WRITE(6,9005) CMONY((TOTPAID+TOTOTHR+TOT2KIK+TOTREF),14,VALUNIT)
        WRITE(8,9005) CMONY((TOTPAID+TOTOTHR+TOT2KIK+TOTREF),14,VALUNIT)
        WRITE(7,9005) CMONY((TOTPAID2+TOTOTHR2+TOT2KIK2+TOTREF2),14,VALUNIT)
C                                                                               
        CALL USRCLOS1(6)                                                             
        CALL USRCLOS1(7)                                                             
        CALL USRCLOS1(8)                                                             
        CALL USRCLOS1(16)                                                             
        CALL USRCLOS1(17)                                                             
C                                                                               
C       TOTALS TO BALANSFILE                                        
C                                                                               
        DO GAM = 1,MAXGAM                                                      
            GAMESUMS(GAM,3,2) = GAMPAID(GAM)                                       
        END DO
        TOTSUMS(6) = TOTPAID + TOTOTHR + TOT2KIK + TOTREF
        RAPCODE = 6                                                               
        CALL BALWRI(RAPCODE,GAMESUMS,TOTSUMS,TOTREAL)                             
C                                                                               
CCC        CALL SPOOL('CSHREP.REP',COPY,STATUS)                                      
CCC        CALL SPOOL('CSHREP1.REP',COPY,STATUS)                                     
CCC        CALL SPOOL(RNAME,COPY,STATUS)                                     
C
        CALL GSTOP(GEXIT_SUCCESS)
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
9010    FORMAT(A132)
C                                                                               
        END                                                                       
C
C
C SUBROUTINE TO EXECUTE VMS COMMANDS FROM FORTRAN PROGRAM 
C
      SUBROUTINE DCLCOM(STR)
      IMPLICIT NONE
C
      INCLUDE '($SMGDEF)'
      INCLUDE '(SMG$ROUTINES)'
C
      CHARACTER * (*) STR
      INTEGER * 4 S,VIS_ID,RCNT,CCNT
      INTEGER * 4 ATR
C
      DATA RCNT/1/
      DATA CCNT/80/
C
      ATR=SMG$M_REVERSE
      S=SMG$CREATE_VIRTUAL_DISPLAY(RCNT,CCNT,VIS_ID, ,ATR, )
      IF(.NOT.S) GO TO 9999
C
      S=SMG$CREATE_SUBPROCESS(VIS_ID, , )
      IF(.NOT.S) GO TO 9999
C
      S=SMG$EXECUTE_COMMAND(VIS_ID,STR, , )
      IF(.NOT.S) GO TO 9999
C
      S=SMG$DELETE_SUBPROCESS(VIS_ID)
      IF(.NOT.S) GO TO 9999
      RETURN
C
9999  CONTINUE
      CALL LIB$SIGNAL(%VAL(S))
      RETURN
      END
C
C+++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE DCLCOM1(STR)
      IMPLICIT NONE
C
      CHARACTER * 10 CMDFIL/'CSHREP.CMD'/
      CHARACTER * (*) STR
      INTEGER * 4 L,ST
C
      L=LEN(STR)
      CALL ROPEN(CMDFIL,11,ST)
      WRITE(11,1000) STR(1:L)
 1000 FORMAT(A<L>)
      CALL USRCLOS1(11)
      CALL RUNCMD(CMDFIL,'CMDREP  ')
      CALL XWAIT(1,2,ST)
      RETURN
      END
C
