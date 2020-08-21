C CLKREP.FTN                                                                    
C                                                                               
C $Log:   GXAFXT:[GOLS]CLKREP.FOV  
C  
C     Rev 1.0   17 Apr 1996 12:35:10   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   14 Sep 1993 21:05:12   GXA
C  Corrected format for displaying CMONY (F11.2 to A11).
C
C V03 15-MAR-2011 GPW NUMAGT=12288
C V02 17-JUN-91 PP  ADDED CALL TO SUBROUTINE BALWRI                             
C V01 13-NOV-89 MGM INITIAL RELEASE FOR FINLAND                                 
C                                                                               
C PRODUCE A REPORT TO DISPLAY INFORMATION IN AGENTS CLERK FILE                  
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
        PROGRAM CLKREP                                                            
        IMPLICIT NONE
C                                                                               
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'                                                           
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:CLERK.DEF'
C                                                                               
        INTEGER*4  CLKFDB(7)                         !
        INTEGER*4  SCFFDB(7)                         !                   
        INTEGER*4  DAYSALS(9,MAXGAM)                 !                              
        INTEGER*4  SPESAL(ASPELEN,MAXGAM)            !                              
        INTEGER*4  MISSAL(AMISLEN,NUMTOT)            !                              
        INTEGER*4  TOTSAL(9,9)                       !
        INTEGER*4  LINCNT /70/                       !                  
        INTEGER*4  GRDSAL(9,9)                       !                              
        INTEGER*4  CARTOT(9)                         !                              
        INTEGER*4  SORT(NUMAGT)                      !                              
        INTEGER*4  SCFNAM(5)                         !
        INTEGER*4  DIS(8)                            !
        INTEGER*4  REPLU /7/                         !           
        INTEGER*4  NOCHECK0                          !
        INTEGER*4  EXT                               !
        INTEGER*4  COPY                              !
        INTEGER*4  ST                                !
        INTEGER*4  I                                 !
        INTEGER*4  GAM                               !
        INTEGER*4  ADAY                              !
        INTEGER*4  CLRK                              !
        INTEGER*4  GAMESUMS(MAXGAM,NUMFIN,NUMTOT)    !
        INTEGER*4  TOTSUMS(NO_BALSUMS)               !
        INTEGER*4  RAPCODE                           !
        INTEGER*4  WEEK                              !
        INTEGER*4  CNT                               !
        INTEGER*4  RECS                              !
        INTEGER*4  XREC                              !
        INTEGER*4  CARTEL                            !
        INTEGER*4  CERR                              !
        INTEGER*4  PRECAR                            !
        INTEGER*4  ASPE                              !
        INTEGER*4  AMIS                              !
        INTEGER*4  PAGE                              !
        INTEGER*4  OFF                               !
        INTEGER*4  K                                 !

        REAL*8     TOTREAL                           !

        INTEGER*2  DATE(12) /12*0/                   !                              
	INTEGER*4  YEAR2

        CHARACTER  DISCRP(8)*30                      !
        CHARACTER  HEAD*48                           !                
        CHARACTER  CZERO /Z0/                        !                              
C                                                                               
        LOGICAL   FIRST                              !                             
C                                                                               
        DATA      DISCRP /'STANDARD SALES DATA ARRAY     ',                       
     *                    'SPECIAL SALES DATA ARRAY      ',                       
     *                    'MISCELLANEOUS SALES DATA ARRAY',                       
     *                    '----------  N O N E  ---------',                       
     *                  4*'                              '/                       
C                                                                               
        DATA      SCFNAM /'SCF.','FIL ',3*'    '/                                 
C                                                                               
        COMMON /NOCHECK0/ NOCHECK0                                                
        NOCHECK0=-1                                                               
C                                                                               
C CALL  COPYRITE  SUBROUTINE                                                    
C                                                                               
        CALL COPYRITE                                                             
C                                                                               
C                                                                               
        TYPE *                                                                    
        TYPE *,'<<<<< CLKREP Clerk Activity Balancing Report V02 >>>>>'           
        TYPE *                                                                    
        CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)               
        IF(EXT.LT.0) STOP                                                         
C                                                                               
C OPEN REPORT FILE                                                              
C                                                                               
        CALL ROPEN('CLKREP.REP',REPLU,ST)                                         
        IF(ST.NE.0)THEN                                                           
            TYPE *,'Error openning CLKREP.REP > ',ST                               
            CALL GPAUSE                                                                  
        ENDIF                                                                     

        CALL FIGWEK(DAYCDC,WEEK,YEAR2)                                                  
        WRITE(HEAD,8000) WEEK, YEAR2                                             
C                                                                               
C READ SCF RECORD                                                               
C                                                                               
        CALL OPENW(1,SCFNAM,4,0,0,ST)                                             
        CALL IOINIT(SCFFDB,1,SCFSEC*256)                                              
        IF(ST.NE.0) CALL FILERR(SCFNAM,1,ST,0)                                    

        CALL READW(SCFFDB,1,SCFREC,ST)                                            
        IF(ST.NE.0) CALL FILERR(SCFNAM,2,ST,1)                                    

        CALL CLOSEFIL(SCFFDB)                                                     

        DO I = 1, MAXFIL                                                          
            IF(SCFSFN(1,I).EQ.'    ') CALL SYSVOL(SCFSFN(1,I))                     
        END DO
C                                                                               
C SORT BY CARTEL AND AGENT NUMBER                                               
C                                                                               
        CALL SRTFLD(61,1,SORT,CNT)                                                
C                                                                               
C OPEN AGENT SALES FILE                                                         
C                                                                               
        CALL OPENASF(ASF)                                                         
C                                                                               
C OPEN AGENT CLERK FILE                                                         
C                                                                               
        CALL OPENW(1,SCFSFN(1,CLK),4,0,0,ST)                                      
        CALL IOINIT(CLKFDB,1,CLRKSEC*256)                                             
        IF(ST.NE.0) CALL FILERR(SCFSFN(1,CLK),1,ST,0)                             
C                                                                               
        CALL FASTSET(0,GRDSAL,9*9)                                                
        CALL FASTSET(0,CARTOT,9)                                                  
        FIRST = .TRUE.                                                            
C                                                                               
C READ CLERK FILE FOR EVERY AGENT                                               
C                                                                               
        DO 100 RECS = 1, CNT                                                         
C                                                                               
            XREC=SORT(RECS)                                                        
C                                                                               
            IF(AGTTAB(AGTNCL,XREC).EQ.0) GOTO 100   !NO CLERK                      
            CALL READW(CLKFDB,XREC,CLRKREC,ST)                                     
            IF(ST.NE.0) CALL FILERR(SCFSFN(1,CLK),2,ST,0)                          
C                                                                               
            CALL READASF(XREC,ASFREC,ST)                                           
            CARTEL = 0                                                             
            CALL ASCBIN(ASFINF,SCHAN,LCHAN,CARTEL,CERR)                            
            IF(FIRST)THEN                                                          
                PRECAR =  CARTEL                                                     
                FIRST  = .FALSE.                                                     
            ENDIF                                                                  
C                                                                               
            CALL FASTSET(0,DAYSALS,9*MAXGAM)                                        
            CALL FASTSET(0,TOTSAL,9*9)                                             
            CALL FASTSET(0,SPESAL,ASPELEN*MAXGAM)                                  
            CALL FASTSET(0,MISSAL,AMISLEN*NUMTOT)                                  
            CALL FASTSET(8,DIS,8)                                                  
C                                                                               
            DO 210 GAM = 1, MAXGAM                                                    
                DO ADAY = 1, 9                                                     
                    DO CLRK = 1, 8                                                  
                        DAYSALS(ADAY,GAM) = DAYSALS(ADAY,GAM) +                         
     *                                     CLRKDAY(ADAY,GAM,CLRK)                     

                        TOTSAL(ADAY,9)   = TOTSAL(ADAY,9) +                           
     *                                     CLRKDAY(ADAY,GAM,CLRK)                     

                        CARTOT(ADAY)     = CARTOT(ADAY) +                             
     *                                     CLRKDAY(ADAY,GAM,CLRK)                     

                        TOTSAL(ADAY,CLRK)= TOTSAL(ADAY,CLRK) +                        
     *                                     CLRKDAY(ADAY,GAM,CLRK)                     

                        GRDSAL(ADAY,CLRK)= GRDSAL(ADAY,CLRK) +                        
     *                                     CLRKDAY(ADAY,GAM,CLRK)                     

                        GRDSAL(ADAY,9)   = GRDSAL(ADAY,9) +                           
     *                                     CLRKDAY(ADAY,GAM,CLRK)                     
                    END DO

                IF(DAYSALS(ADAY,GAM).NE.ASFDAY(ADAY,GAM,1)) DIS(1)=1              

                END DO
C                                                                               
                DO ASPE = 1, ASPELEN                                               
                    DO CLRK = 1, 8                                                  
                        SPESAL(ASPE,GAM) = SPESAL(ASPE,GAM) +                         
     *                                     CLRKSPE(ASPE,GAM,CLRK)                     
                    END DO

                    IF(SPESAL(ASPE,GAM).NE.ASFSPE(ASPE,GAM,1)) DIS(2)=2              
                END DO
C                                                                               
                IF(GAM.GT.NUMTOT) GOTO 210                                          
                DO AMIS = 1, AMISLEN                                               
                    DO CLRK = 1, 8                                                  
                        MISSAL(AMIS,GAM) = MISSAL(AMIS,GAM) +                         
     *                                     CLRKMIS(AMIS,GAM,CLRK)                     
                    END DO

                    IF(MISSAL(AMIS,GAM).NE.ASFMIS(AMIS,GAM,1)) DIS(3)=3              
                END DO
C                                                                               
210         CONTINUE                                                               
C                                                                               
C PRINT REPORT DETAIL LINE                                                      
C                                                                               
            IF(LINCNT.GT.50) THEN                                                  
                CALL TITLE(HEAD,'  CLKREP',1,REPLU,PAGE,DAYCDC)                     
                WRITE(REPLU,9000)                                                   
                LINCNT=7                                                            
            ENDIF                                                                  

            LINCNT=LINCNT+2                                                        
            WRITE(REPLU,9001) AGTTAB(AGTNUM,XREC)/10,                              
     *                        MOD(AGTTAB(AGTNUM,XREC),10),                         
     *                        PRECAR,                                              
     *                        XREC,(ASFBYT(I),I = SPAS1, EPAS1),                      
     *                        TOTSAL(GSCNT,1),
     *                        CMONY(TOTSAL(GSAMT,1),11,VALUNIT),               
     *                        TOTSAL(GCCNT,1),
     *                        CMONY(TOTSAL(GCAMT,1),11,VALUNIT),               
     *                        TOTSAL(GVCNT,1),
     *                        CMONY(TOTSAL(GVAMT,1),11,VALUNIT),               
     *                        DISCRP(DIS(1))                                       
C                                                                               
            OFF = SPAS1                                                            
            DO CLRK = 2, 8                                                        
                OFF = OFF + 4                                                       
                DO K=0,3                                                        
                    IF(ASFBYT(OFF+K).EQ.CZERO) ASFBYT(OFF+K) = ' '                   
                END DO

                IF(DIS(1).EQ.8.AND.DIS(2).EQ.8.AND.DIS(3).EQ.8)                     
     *            CALL FASTSET(4,DIS,8)                                            

                WRITE(REPLU,9002) CLRK,(ASFBYT(I),I=OFF,OFF+3),                     
     *                            TOTSAL(GSCNT,CLRK),                               
     *                            CMONY(TOTSAL(GSAMT,CLRK),11,VALUNIT),                         
     *                            TOTSAL(GCCNT,CLRK),                               
     *                            CMONY(TOTSAL(GCAMT,CLRK),11,VALUNIT),                         
     *                            TOTSAL(GVCNT,CLRK),                               
     *                            CMONY(TOTSAL(GVAMT,CLRK),11,VALUNIT),                         
     *                            DISCRP(DIS(CLRK))                                 
                LINCNT=LINCNT+1                                                     
            END DO
C                                                                               
C WRITE AGENT TOTAL LINE                                                        
C                                                                               
            WRITE(REPLU,9003)  TOTSAL(GSCNT,9),                                    
     *                         CMONY(TOTSAL(GSAMT,9),11,VALUNIT),                              
     *                         TOTSAL(GCCNT,9),                                    
     *                         CMONY(TOTSAL(GCAMT,9),11,VALUNIT),                              
     *                         TOTSAL(GVCNT,9),                                    
     *                         CMONY(TOTSAL(GVAMT,9),11,VALUNIT)                               
            LINCNT=LINCNT+2                                                        
C                                                                               
            IF(PRECAR.NE.CARTEL.AND.RECS.NE.1)THEN                                 
                WRITE(REPLU,9005)  PRECAR,CARTOT(GSCNT),                            
     *                             CMONY(CARTOT(GSAMT),11,VALUNIT),                             
     *                             CARTOT(GCCNT),                                   
     *                             CMONY(CARTOT(GCAMT),11,VALUNIT),                             
     *                             CARTOT(GVCNT),                                   
     *                             CMONY(CARTOT(GVAMT),11,VALUNIT)                              

                LINCNT=LINCNT+2                                                     
                PRECAR=CARTEL                                                       
                IF(LINCNT.GT.50) THEN                                               
                    CALL TITLE(HEAD,'  CLKREP',1,REPLU,PAGE,DAYCDC)                  
                    WRITE(REPLU,9000)                                                
                    LINCNT=7                                                         
                ENDIF                                                               
            ENDIF                                                                  
C                                                                               
100     CONTINUE                                                                  
C                                                                               
C PROCESS CARTEL                                                                
C                                                                               
        WRITE(REPLU,9005)  PRECAR,CARTOT(GSCNT),                                  
     *                     CMONY(CARTOT(GSAMT),11,VALUNIT),                                   
     *                     CARTOT(GCCNT),                                         
     *                     CMONY(CARTOT(GCAMT),11,VALUNIT),                                   
     *                     CARTOT(GVCNT),                                         
     *                     CMONY(CARTOT(GVAMT),11,VALUNIT)                                    
C                                                                               
        WRITE(REPLU,9004)  GRDSAL(GSCNT,9),                                       
     *                     CMONY(GRDSAL(GSAMT,9),11,VALUNIT),                                 
     *                     GRDSAL(GCCNT,9),                                       
     *                     CMONY(GRDSAL(GCAMT,9),11,VALUNIT),                                 
     *                     GRDSAL(GVCNT,9),                                       
     *                     CMONY(GRDSAL(GVAMT,9),11,VALUNIT)                                  
C                                                                               
        CALL SPOOL('CLKREP.REP',COPY,ST)                                          
C                                                                               
C GRAND TOTAL TO BALANSFILE            V02                                      
C                                                                               
        TOTSUMS(1) =  GRDSAL(GSCNT,9)                                            
        TOTSUMS(2) =  GRDSAL(GSAMT,9)                                            
        TOTSUMS(3) =  GRDSAL(GCCNT,9)                                            
        TOTSUMS(4) =  GRDSAL(GCAMT,9)                                            
        TOTSUMS(5) =  GRDSAL(GVCNT,9)                                            
        TOTSUMS(6) =  GRDSAL(GVAMT,9)                                            
        RAPCODE = 4                                                              

        CALL BALWRI(RAPCODE,GAMESUMS,TOTSUMS,TOTREAL)                            
C                                                                               
C     ====================== Format Statements ===================              
C                                                                               
8000    FORMAT('CLERK ACTIVITY BALANCING REPORT FOR WEEK ',                       
     *          I2.2,'/',I4.4)                                                    
C                                                                               
9000    FORMAT(32X,'----- SALES -----  ---- CANCELS -----',                       
     *         2X,'----- VALIDS -----',/,2X,'AGENT',3X,'CARTEL',2X,               
     *        'TERM',5X,'CLERK',3X,3('COUNT   AMOUNT',6X),                        
     *        'DISCREPANCIES DETECTED IN',/,1X,131('='),/)                        
9001    FORMAT(/,1X,I6.6,'-',I1,2X,I4.4,2X,I5,3X,'1.  ',4A1,2X,                   
     *         3(I6,1X,A11,2X),1X,A30)                                          
9002    FORMAT(25X,I1,'.  ',4A1,2X,3(I6,1X,A11,2X),6X,A30)                      
9003    FORMAT(/,25X,'TOTAL',5X,3(I6,1X,A11,2X))                                
9004    FORMAT(/,19X,'GRAND TOTAL',5X,3(I6,1X,A11,2X))                          
9005    FORMAT(/,34X,97('-'),/,3X,'CARTEL ',I4.4,' TOTAL',T21,15X,                
     *         3(I6,1X,A11,2X))                                                 
C                                                                               
        END                                                                       
