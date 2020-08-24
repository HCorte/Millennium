C CSHWEEK.FOR
C 
C V06 16-OCT-2013 SCML Adding new OP Generation Flag
C V05 19-MAI-2006 CPH ALTERAÇÃO DAS VALIDACOES PARA PREMIOS COM OPS
C V04 08-JUN-2005 CPH ALTERAÇAO DE CSHREP PARA NOVO EXECUTAVEL 
C                     CSHWEEK - VALIDAÇOES SEMANAIS >50 EUROS
C V03 16-JAN-2001 EPH VK2PAMT WAS REMOVED
C V02 06-SEP-1999 UXN ISSUBPROC LOGIC ADDED. 
C V01 13-JAN-1999 GLS NEW RELEASE FOR FINLAND (FROM CSHREP1.FOR)      
C                                                                               
C CASHED TICKET REPORT - FULL VERSION + VMS SORT                     
C CASH.FIL REMOVED                                                   
C                                                                               
C PHASE 1 : WRITE SELECTED RECORDS TO CSHWEEKnnn.REP
C PHASE 2 : SORT BY VMS BOTH FILES; OUTPUT = CSHWEEK_PC.REP
C PHASE 3 : ROPEN (DELETE) CSHWEEKnnnn.REP
C PHASE 4 : REWRITE BOTH PC FILES TO CSHWEEKnnnn.REP AND ADDING 
C           HEADINGS 
C           
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C This item is the property of GTECH Corporation, W.Greenwich, Rhode            
C Island, and contains confidential and trade secret information. It            
C may not be transferred from the custody or control of GTECH except            
C nor the information it contains may be used, transferred,                     
C reproduced, published, or disclosed, in whole or in part, and                 
C directly or indirectly, except as expressly authorized by an                  
C officer of GTECH, pursuant to written agreement.                              
C                                                                               
C Copyright 1999 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT                                                                       
        PROGRAM CSHWEEK                                                            
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
        INTEGER*4  COUNT2 /0/             !          
        INTEGER*4  GIND /0/               !
        INTEGER*4  KIND /0/               !
        INTEGER*4  IND /0/                !         
        INTEGER*4  COPY                   !
        INTEGER*4  EXT                    !
        INTEGER*4  FIRSTCDC
        INTEGER*4  LASTCDC
        INTEGER*4  ST                     !
        INTEGER*4  AMOUNT1                !
        INTEGER*4  AMOUNT2                !
        INTEGER*4  PAGE1                  !
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
        INTEGER*2  DATE(LDATE_LEN)        ! 
        INTEGER*4  ANSWER                 !
C                   
        LOGICAL    ISTHERE                ! FLAG TO KNOW IF FILE IS IN DISK
        CHARACTER  KGAME*4 /' '/          !
        CHARACTER  MGAME*4 /' '/          !
        CHARACTER  HEAD2*40               !V05                       
        CHARACTER  RNAME1*16              !V04 CPH                                      
        CHARACTER  FILIN*20               !INPUT FILE NAME USED BY VMS SORT 
        CHARACTER  FILOUT*20              !OUTPUT FILE NAME USED BY VMS SORT
        CHARACTER  OUTREC*132             !RECORD BUFFER
        CHARACTER  VMSCMD*80              !VMS COMMAND BUFFER
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
        TYPE *,IAM(),'<< CSHWEEK-RELATORIO SEMANAL VALIDAÇOES DE PREMIOS COM OPS >>'           
        TYPE *                                                                    
C
        COPY = 0
C
        TYPE *,IAM(),'CDC -->',DAYCDC  
        ANSWER = 0      ! DEFAULT
        CALL PRMYESNO('CONFIRMA QUE O CDC É DE DOMINGO (Y/N) ? ',ANSWER)
        IF (ANSWER.EQ.1) THEN                                       
         LASTCDC = DAYCDC                                                 
        ELSE                                                                                                                   
         CALL INPNUM('Insira CDC do último Domingo ',LASTCDC,1,9999,EXT)
	   IF (LASTCDC .GT. DAYCDC) THEN
	      TYPE *,IAM(),'CDC INSERIDO INVÁLIDO'
	      CALL GPAUSE
           ENDIF                                                                     
        ENDIF
	FIRSTCDC = (LASTCDC - 6)
 
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
C OPEN REPORT FILE    
C 
        WRITE(RNAME1,8004) LASTCDC 
        CALL ROPEN(RNAME1,6,ST)  
        IF(ST.NE.0) THEN                                                          
            TYPE*,IAM(),RNAME1,' open error - ',ST   
            CALL GPAUSE                                                                   
        ENDIF
        
        DATE(VCDC) = LASTCDC                                                     
        WRITE(HEAD2,8002)                                            
C                                                                               
C READ THE VALIDATION FILE    
C                                                                               
        TYPE*,IAM(),'Leitura no VLF.FIL dos bilhetes validados'     
100     CONTINUE  
        CALL ISREAD(V4BUF,VLF,VLFBUF,ST)          
        IF(ST.EQ.ERREND) GOTO 3000 
        IF(ST.NE.0) THEN     
            TYPE*,IAM(),'VLF.FIL read error - ',ST
            CALL GPAUSE
        ENDIF
C                      
        CALL LOGVAL(VALREC,V4BUF)
                
        IF (VALREC(VCCDC).LT.FIRSTCDC .OR. VALREC(VCCDC).GT.LASTCDC) GOTO 100
        IF ((VALREC(VPAMT)+VALREC(VKPAMT)+VALREC(VRAMT)).EQ.0) GOTO 100 
C----+------------------------------------------------------------------
C V06| Adding New OP Gen Flag
C----+------------------------------------------------------------------
        IF ( (VALREC(VOPSAMT)+VALREC(VKOPSAMT)).EQ.0 
     *   .OR. VALREC(VOPSCNT) .EQ. 0) GOTO 100 !V05
C----+------------------------------------------------------------------
C V06| Adding New OP Gen Flag
C----+------------------------------------------------------------------
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
C----+------------------------------------------------------------------
C V06| Adding New OP Gen Flag
C----+------------------------------------------------------------------
C       IF(VALREC(VOPSAMT).EQ.0.AND.VALREC(VRAMT).EQ.0) THEN !V05                                           
        IF (  (VALREC(VOPSAMT) .EQ.0 .OR. VALREC(VOPSCNT) .EQ. 0)
     *   .AND. VALREC(VRAMT).EQ.0) THEN
C----+------------------------------------------------------------------
C V06| Adding New OP Gen Flag
C----+------------------------------------------------------------------
          GIND = 0                                                            
          WRITE(KGAME,8001) SCFSGN(VALREC(VKGME))                            
          KIND = SCFGNT(GAMIDX,VALREC(VKGME))                                 
          GAM=VALREC(VKGME)                                                   
          AMOUNT1 = 0                                                         
          AMOUNT2 = VALREC(VKOPSAMT)      !V05    
          WRITE(MGAME,8001) '----'                                           
        ELSE                                                                   
C                                                                               
C OTHERS GAMES WITH POSSIBLY KICKER                                             
C                                                                               
          GIND = SCFGNT(GAMIDX,VALREC(VGAM))                                  
          AMOUNT1 = VALREC(VOPSAMT)   !V05
          GAM =  VALREC(VGAM)                                                 
          AMOUNT2 = VALREC(VKOPSAMT)  !V05
          WRITE(MGAME,8001) SCFSGN(VALREC(VGAM))                             
          IF(VALREC(VKOPSAMT).GT.0)THEN !V05
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
     *                    CMONY(AMOUNT1,11,VALUNIT),
     *                    CMONY(AMOUNT2,11,VALUNIT)                
C                                   
C       IF(AMOUNT1+AMOUNT2 .LT.150*DOLL_BASE/DYN_VALUNIT)GOTO 100 
C	IF(AMOUNT1+AMOUNT2 .LT.P(VALORDER)/DYN_VALUNIT .and. VALREC(VOPSAMT)+VALREC(VKOPSAMT).EQ.0)GOTO 100
C                                                   
C     PRINT DETAIL LINE OF REPORT                                               
C                                                                               
        WRITE(6,9010) OUTREC
        COUNT2=COUNT2+1                 
C
        GOTO 100                                                                  
C
3000    CONTINUE                                                                  
C
        CALL ICLOSE(VLF,VLFBUF,ST)                                                
        CALL USRCLOS1(6)                                                             
C
        TYPE *,IAM(),COUNT2,' Registos lidos em ',
     *                                    RNAME1(1:8),'PC.REP file'   !V04
                                                           
C
C REMOVE OLD VERSION OF PC REPORT
C
        VMSCMD=' '
        VMSCMD(1:10)='DEL/NOLOG '
        VMSCMD(11:30)='CSHWEEK_PC.REP;*    '	
        INQUIRE(FILE = VMSCMD(11:30), EXIST = ISTHERE)
        IF(ISTHERE .EQ. .TRUE.) CALL DCLCOM1(VMSCMD)
C
C SORT BY VMS
C
        TYPE *,IAM(),'Sorting file ',RNAME1	  !V04
        VMSCMD=' '
        VMSCMD(1 :30)='$SORT/KEY=(POS:82,SIZ:15)     '
        FILIN=' '
        FILIN=RNAME1				  !V04
        FILOUT=' '
        FILOUT=RNAME1(1:8)//'PC.REP'		  !V04
        VMSCMD(31:50)=FILIN
        VMSCMD(51:70)=FILOUT
        CALL DCLCOM1(VMSCMD)
C                                                                               
C OPEN AGAIN REPORT FILES FOR WRITE                                                                                                                               
C                                                                               
        CALL ROPEN(RNAME1,6,ST)			      !V04                                            
        IF(ST.NE.0) THEN                                                          
            TYPE*,IAM(),RNAME1,' open error - ',ST    !V04                               
            CALL GPAUSE                                                                   
        ENDIF                                                                     
C                                                                               
C OPEN PC REPORT FILES FOR READ                                                                                                                  
C                                                                               
        OPEN(16,FILE=RNAME1(1:8)//'PC.REP',ACCESS='SEQUENTIAL',      !V04
     *       STATUS='OLD',RECL=133,IOSTAT=ST)   
        IF(ST.NE.0) THEN                                                          
            TYPE*,IAM(),RNAME1(1:8),'PC.REP open error - ',ST        !V04                            
            CALL GPAUSE                                                                   
        ENDIF                                                                     
C
C REWRITE RECORDS FORM PC FILES TO ORIGINAL FILES ADDING HEADINGS
C
        TYPE *,IAM(),'A Escrever para o Relatorio'
           CALL TITLE(HEAD2,'CSHWEEK ',1,6,PAGE1,DAYCDC)                                              
           WRITE(6,9000)                                                          
C
3200    CONTINUE
        READ(16,9010,END=3250)OUTREC
        WRITE(6,9010)OUTREC
        GOTO 3200 

3250    CONTINUE
                                    
        CALL USRCLOS1(6)                                                             
        CALL USRCLOS1(16)                                                             

        CALL GSTOP(GEXIT_SUCCESS)
C                                                                               
C       =================== FORMAT STATEMENTS =====================               
C                                                                                                  
8001    FORMAT(A4)                                                                               
8002    FORMAT('VALIDACOES DE SEG-DOM DE PREMIOS COM OPS') !V04                      
8004    FORMAT('CSHWEEK_',I4.4,'.REP')  
9000    FORMAT(2X,'   PAGAMENTO          VENDIDO    ',/,                         
     *         2X,'AGENTE  TERMINAL  AGENTE  TERMINAL  JOGO/IND'                  
     *         '    JKR/IND',4X,' N.SERIE VENDA',6X,' N.SERIE PAGAMENTO',
     *         6X,'JOGO PAGO',2X,'JOKER PAGO',1X,/,                
     *         1X,131('='),/)                                                     
9001    FORMAT(1X,I8.8,2X,I5,3X,I8.8,2X,I5,4X,A4,'/',I2.2,            
     *         5X,A4,'/',I2.2,4X,I3.3,'-',I8.8,'-',I2.2,'*',5X,I3.3,              
     *         '-',I8.8,'-',I2.2,'*',5X,2(A11))                              
9010    FORMAT(A132)
C                                                                               
        END                                                                       
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
      CHARACTER * 11 CMDFIL/'CSHWEEK.CMD'/
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
