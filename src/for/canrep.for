C
C V08 13-APR-2000 UXN Fix for JOKER amount
C V07 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V05 27-APR-1994 JXP COPY=0
C V04 26-AUG-1993 SXH Added IAM() etc
C V03 17-AUG-1993 SXH Initial revision.
C V02 05-JUN-1991 PP  ADDED SUBROUTINE CALL BALWRI                            
C V01 11-JAN-1990 GCN INITIAL RELEASE FOR FINLAND                             
C                                                                               
C GENERATES A CANCELLED WAGER REPORT                                            
C                                                                               
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
C=======OPTIONS/CHECK=NOOVERFLOW/EXT                       
        PROGRAM CANREP                                    
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C                                                                               
C                                                                               
        INTEGER*4  REPLU                         !
        PARAMETER(REPLU=7)                       

        INTEGER*4  LINMAX                        !
        PARAMETER(LINMAX=48)                     

        INTEGER*4  CNTMAX                        !
        PARAMETER(CNTMAX=20000)                  
C                                                                               

        INTEGER*4  LOGREC(LMUREC)                !
        INTEGER*4  SORT(10,CNTMAX)               !
        INTEGER*4  TOTPAD(DOLAMT+1)              !
        INTEGER*4  NOCHECK0                      !
        INTEGER*4  PAGE                          !
        INTEGER*4  SER                           !
        INTEGER*4  SRTCNT                        !
        INTEGER*4  LINCNT                        !
        INTEGER*4  CDCTRA                        !
        INTEGER*4  JULTRA                        !
        INTEGER*4  ST                            !
        INTEGER*4  COPY                          !
C        INTEGER*4  EXT                           !
        INTEGER*4  REV                           !
        INTEGER*4  WSER                          !
        INTEGER*4  WCHK                          !
        INTEGER*4  START                         !
        INTEGER*4  SCNT                          !
        INTEGER*4  I                             !
        INTEGER*4  REC                           !
        INTEGER*4  CSER                          !
        INTEGER*4  CCHK                          !
        INTEGER*4  KGNUM                         !
        INTEGER*4  MGNUM                         !
        INTEGER*4  MGTYP                         !
        INTEGER*4  MGIND                         !
        INTEGER*4  KGIND                         !
        INTEGER*4  GAMPAD                        !
        INTEGER*4  KGTYP                         !
        INTEGER*4  S                             !

        INTEGER*4  TOTSUMS(NO_BALSUMS)            !
        INTEGER*4  GAMESUMS(MAXGAM,NUMFIN,NUMTOT) !
        INTEGER*4  RAPCODE                        !


        INTEGER*2  DATBUF(LDATE_LEN)             !

        REAL*8     TOTREAL                       !

        CHARACTER*1  BELL                        !
        CHARACTER*4  MGAME                       !
        CHARACTER*4  KGAME                       !
        CHARACTER*12 REPNAM                      !
        CHARACTER*41 HEAD                        !
        CHARACTER*8 IAGT_NO                      ! FUNCTION: FORMAT AGT NUMBER
C                                                 
        LOGICAL EOF/.FALSE./                     !
        COMMON /NOCHECK0/ NOCHECK0 

        DATA REPNAM/'  CANREP.REP'/
        DATA BELL/Z07/             
        DATA REV/01/               


C                                                                               
C CALL  COPYRITE  SUBROUTINE                                                    
C                                  
        CALL COPYRITE              
C                                                                               
C SET / CLEAR VARIABLES                                                         
C                                  
        NOCHECK0=-1                
        PAGE=0                     
        SER=1                      
        SRTCNT=0                   
        LINCNT=70                  
        CDCTRA=0                   
        JULTRA=0                   
        CALL FASTSET(0,TOTPAD,DOLAMT+1)       
C                                                                               
        TYPE*,IAM(),' '                       
        TYPE*,IAM(),'<<<<< CANREP  V',REV,'  CANCELLED WAGER REPORT >>>>> '
        TYPE*,IAM(),' '                                                    
C                                                                               
C GET SYSTEM CONTROL INFORMATION                                               
C                                                                               
        CALL GETSCONF(SCFREC,ST)                                           
        IF(ST.NE.0) THEN                                                   
            TYPE*,IAM(),' Unable to get system control info. ',BELL        
            CALL GPAUSE                                                    
        ENDIF                                                              
C                                                                               
C GET NUMBER OF REPORT COPIES                                                   
C                                                                               
C        CALL INPNUM('Enter number of report copies: ',COPY,0,20,EXT)    
C        IF(EXT.NE.0) STOP                                               
        COPY=0
C                                                                               
C OPEN THE TRANSACTION FILE                                                     
C                                                                               
        CALL OPENW(PTMF,SCFSFN(1,PTMF),4,0,0,ST)                         
        IF(ST.NE.0) CALL FILERR(SCFSFN(1,PTMF),1,ST,0)                   
        CALL TOPEN(PTMF)                                                 
C                                                                               
C LOOP THROUGH THE TM                                                           
C                                                                               
100     CONTINUE                                                         
        CALL READTMF(LOGREC,SER,EOF)                                     
        IF(EOF) GOTO 1000                                                
C                                                                               
C CONVERT TO INTERNAL TRANSACTION FORMAT                                        
C                                                                               
        CALL LOGTRA(TRABUF,LOGREC)                                       
C                                                                               
C PROCESS ALL GOOD CANCELLATIONS                                                
C                                                                               
        IF(TRABUF(TSTAT).NE.VOID) GOTO 100                               
        IF(TRABUF(TTYP).NE.TWAG)  GOTO 100                               
        IF(CDCTRA.LE.0) THEN                                             
            CDCTRA=TRABUF(TCDC)                                          
            DATBUF(VCDC)=CDCTRA                                          
            CALL LCDATE(DATBUF)                                          
            JULTRA=DATBUF(VJUL)                                          
        ENDIF                                                            
        CALL OUTGEN(CDCTRA,TRABUF(TSER),WSER,WCHK)                       
C                                                                               
C STUFF SORT ARRAY                                                              
C                                                                               
        SRTCNT = SRTCNT+1                                                
        SORT(1,SRTCNT) = TRABUF(TAGT)                                    
        SORT(2,SRTCNT) = TRABUF(TTER)                                    
        SORT(3,SRTCNT) = TRABUF(TGAM)                                    
        SORT(4,SRTCNT) = TRABUF(TWKGME)                                  
        SORT(5,SRTCNT) = WSER                                            
        SORT(6,SRTCNT) = WCHK                                            
        SORT(7,SRTCNT) = TRABUF(TWCSER)                                  
        SORT(8,SRTCNT) = TRABUF(TWTOT)                                   
        SORT(9,SRTCNT) = TRABUF(TWKAMT)*TRABUF(TWKDUR)                   
        GOTO 100                                                         
C                                                                               
C END OF FILE                                                                   
C                                                                               
1000    CONTINUE                                                         
        CALL USRCLOS1(PTMF)                                              

C                                                                               
C OPEN REPORT FILE                                                              
C                                                                               
        CALL ROPEN(REPNAM,REPLU,ST)                                      
        IF(ST.NE.0) THEN                                                 
            TYPE*,IAM(),REPNAM,' Open error  st - ',ST                   
            CALL GSTOP(GEXIT_FATAL)                                      
        ENDIF                                                            
C                                                                               
C ENCODE REPORT HEADER                                                          
C                                                                               
        DATBUF(VCDC)=CDCTRA                                              
        CALL LCDATE(DATBUF)                                              
        WRITE(HEAD,9000) DATBUF(VDAY),DATBUF(VMON),DATBUF(VYEAR2)        

        ! check if no cancelled wagers
        IF (SRTCNT .EQ. 0) THEN
            TYPE *, IAM(),' NO CANCELLED WAGERS FOUND'
            WRITE(REPLU,9060)
            CALL USRCLOS1(REPLU)                                         
            CALL SPOOL(REPNAM,COPY,ST)                                   
            CALL GSTOP(GEXIT_SUCCESS)
        END IF
C                                                                               
C SORT ON AGENT NUMBER AND MAIN GAME                                            
C                                                                               
        TYPE*,IAM(),' Generating report sorted by Agent and Game number '
        CALL ISORTA(SORT,SRTCNT,1)                                       
        TYPE*,IAM(),' Sort pass 1 complete '                             
C                                                                        
        START = 1                                                        
        SCNT  = 1                                                        
        DO I = 2, SRTCNT+1                                                      
            IF(SORT(1,I).NE.SORT(1,I-1)) THEN                            
                CALL ISORTA(SORT(1,START),SCNT,3)                        
                SCNT=0                                                   
                START=I                                                  
            ENDIF                                                        
            SCNT=SCNT+1                                                  
        END DO
C                                                                               
        START=1                                                          
        SCNT=1                                                           
        DO I = 2, SRTCNT+1                                               
            IF(SORT(1,I).NE.SORT(1,I-1).OR.                              
     *         SORT(3,I).NE.SORT(3,I-1)) THEN                            
                CALL ISORTA(SORT(1,START),SCNT,5)                        
                SCNT=0                                                   
                START=I                                                  
            ENDIF                                                        
            SCNT=SCNT+1                                                  
        END DO
        TYPE*,IAM(),' Sort completed '                                   
C                                                                               
C ENCODE REPORT                                                                 
C                                                                               
        DO I=1,SRTCNT                                                        
            LINCNT=LINCNT+1                                              
            IF(LINCNT.GT.LINMAX) THEN                                    
                CALL TITLE(HEAD,REPNAM(1:8),REC,REPLU,PAGE,DAYCDC)       
                WRITE(REPLU,*)                                           
                WRITE(REPLU,9010)                                        
                WRITE(REPLU,9020)                                        
                WRITE(REPLU,*)                                           
                LINCNT=7                                                 
            ENDIF                                                        
C                                                                               
C GET CANCELLING SERIAL NUMBER                                                  
C                                                                        
            CALL OUTGEN(CDCTRA,SORT(7,I),CSER,CCHK)                      
C                                                                        
            KGNUM=SORT(4,I)                                              
            MGNUM=SORT(3,I)                                              
            MGTYP=SCFGNT(GAMTYP,MGNUM)                                   
            MGIND=SCFGNT(GAMIDX,MGNUM)                                   
            WRITE(MGAME,9030) SCFSGN(MGNUM)  
                                      
            IF(SORT(9,I).LE.0.OR.MGTYP.EQ.TKIK) THEN                     
                KGAME='----'                                             
                KGIND=0                                                  
                GAMPAD=SORT(8,I)                                         
                SORT(9,I)=0                                              
            ELSE                                                         
                KGTYP=SCFGNT(GAMTYP,KGNUM)                               
                KGIND=SCFGNT(GAMIDX,KGNUM)                               
                WRITE(KGAME,9030) SCFSGN(KGNUM)                          
                GAMPAD=SORT(8,I)-SORT(9,I)                               
            ENDIF                                                        

            TOTPAD(TRACNT)   = TOTPAD(TRACNT)+1                          
            TOTPAD(DOLAMT)   = TOTPAD(DOLAMT)+GAMPAD                     
            TOTPAD(DOLAMT+1) = TOTPAD(DOLAMT+1)+SORT(9,I)                
C                                                                               
            WRITE(REPLU,9040) IAGT_NO(SORT(1,I)),
     *                        SORT(2,I),          
     *                        MGAME,
     *                        MGIND,
     *                        KGAME,
     *                        KGIND,                                 
     *                        JULTRA,
     *                        SORT(5,I),
     *                        SORT(6,I),
     *                        JULTRA,
     *                        CSER,
     *                        CCHK,             
     *                        CMONY(GAMPAD,10,BETUNIT),
     *                        CMONY(SORT(9,I),10,BETUNIT)                
        END DO
C                                                                               
C WRITE TOTAL LINE                                                              
C                                                                               
        WRITE(REPLU,9050) TOTPAD(TRACNT),                                
     *                    (CMONY(TOTPAD(S),10,BETUNIT),S=DOLAMT,DOLAMT+1)
C                                                                               
C CLOSE REPORT FILE AND SPOOL REPORT                                            
C                                                                               
        CALL USRCLOS1(REPLU)                                             
C                                                                               
C       TOTAL TO BALANSFILE (BALWRI)     V02                             
C                                                                               
        TOTSUMS(4) = TOTPAD(DOLAMT) + TOTPAD(DOLAMT+1)                   
        RAPCODE = 7                                                      
        CALL BALWRI(RAPCODE,GAMESUMS,TOTSUMS,TOTREAL)                    
C                                                                               
        CALL SPOOL(REPNAM,COPY,ST)                                       
C                                                                               
C FORMAT STATEMENTS FOR CANREP REPORT                                           
C                                                                               
9000    FORMAT('CANCELLED TICKETS REPORT FOR: ',I2.2,'.',I2.2,'.',I4.4)  
9010    FORMAT(2X,'AGENT#',2X,'TERMINAL#',2X,'GAME/IND',                 
     *         2X,'OTHER/IND',6X,'SELLING SERIAL#',                      
     *         6X,'CANCELLING SERIAL#',6X,'GAME PAID',                   
     *         6X,'OTHER PAID')                                          
9020    FORMAT(1X,132('='))                                              
9030    FORMAT(A4)                                                       
9040    FORMAT(2X,A8,5X,I4,2X,A4,'/',I2.2,3X,A4,'/',I2.2,         
     *         8X,I3.3,'-',I8.8,'-',I3.3,5X,I3.3,'-',I8.8,'-',I3.3,      
     *         7X,A10,6X,A10)                                            
9050    FORMAT(//,50X,' Total wagers cancelled: ',I8,' for: ',1X,        
     *         A10,6X,A10)                                                    
9060    FORMAT(1X,'NO CANCELLED WAGERS FOUND IN TMF FILE')

        END                                                              
