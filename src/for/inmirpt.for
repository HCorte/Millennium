C INMIRPT.FOR
C
C V17 13-NOV-97 UXN Using of TMFREP.DEF added.
C
C $Log:   GXAFIP:[GOLS]INMIRPT.FOV  $
C  
C     Rev 1.1   06 Mar 1997 13:15:24   RXK
C  Instant report removed
C  
C     Rev 1.0   17 Apr 1996 13:36:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.6   02 Sep 1994 18:06:02   HXK
C  Merge of May,June RFSS batch 
C  
C     Rev 1.6   27 Apr 1994 16:26:08   JXP
C  COPY=0
C  
C     Rev 1.5   17 Oct 1993 14:49:24   HXK
C  FIX FOR CARTELS.
C  
C     Rev 1.4   02 Oct 1993 11:29:10   HXK
C  Don't increment petty cash type as it should already be correctly
C  numbered in the TMF.
C  
C     Rev 1.3   27 Sep 1993 20:27:08   GXA
C  Enabled negative amount on report.
C  
C     Rev 1.2   07 Sep 1993 13:58:56   SXH
C  Corrected BALWRI CALL
C  
C     Rev 1.1   03 Sep 1993 12:13:58   SXH
C  Copy=1, added IAM()
C  
C     Rev 1.0   18 Aug 1993 15:46:58   HXN
C  Initial revision.
C  
C                                                                               
C V08 FEB-93 PP  CHANGED RAPCODES FOR BALANCE                                   
C V07  5-NOV-92 JWE  Fix CARTAB subscript errors                                
C V06 09-NOV-92 HJK  ADDED COUNT TOTALS TO MISC REPORT.                         
C V05 28-AUG.91 HHE  ADDED INSTANT SALE COUNT FIELD                             
C V04 05-JUN-91 PP   ADDED SUBROUTINE CALL: BALWRI                              
C V03 03-MAY-90 HHE  REPORTS INSTANT AND MISC BY CARTELS                        
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
                                                                               
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
        SUBROUTINE INMIRPT
        IMPLICIT NONE
	INCLUDE '(LIB$ROUTINES)'
	INCLUDE 'INCLIB: SYSPARAM.DEF'
        INCLUDE 'INCLIB: SYSEXTRN.DEF'

	INCLUDE 'INCLIB: GLOBAL.DEF'
	INCLUDE 'INCLIB: CONCOM.DEF'
	INCLUDE 'INCLIB: DESTRA.DEF'
	INCLUDE 'INCLIB: PRMLOG.DEF'
	INCLUDE 'INCLIB: AGTCOM.DEF'

	INCLUDE 'INCLIB: AGTINF.DEF'
	INCLUDE 'INCLIB: RECAGT.DEF'
	INCLUDE 'INCLIB: DATBUF.DEF'
	INCLUDE 'INCLIB: TMFREP.DEF'
C
	INTEGER*4   LUN
C	
        INTEGER*4  MAXCART
        PARAMETER (MAXCART=10)

        ! BALWRI arguments
        INTEGER*4  RAPCODE
        INTEGER*4  GAMESUMS(MAXGAM,NUMFIN,NUMTOT)
        INTEGER*4  TOTSUMS(NO_BALSUMS)

        REAL*8     TOTREAL

        ! variables

        INTEGER*4  MISSLS(10,2)
        INTEGER*4  PTYCSH(10,2)
        INTEGER*4  CMISSLS(10,2,MAXCART)
        INTEGER*4  CPTYCSH(10,2,MAXCART)

        CHARACTER*20 MISPET(2)  /'MISCELLANEOUS SALES ','PETTY CASH          '/

        LOGICAL FIRST  /.TRUE./
        INTEGER*4  C_LEN /12/      !used by CMONY

        INTEGER*4  COPY
        INTEGER*4  EXT
        INTEGER*4  PAGE
        INTEGER*4  OFF1
        INTEGER*4  CARTEL
        INTEGER*4  ST
        INTEGER*4  PAGES
        INTEGER*4  TOT0
        INTEGER*4  TOT1
        INTEGER*4  TOT2
        INTEGER*4  TOT3
        INTEGER*4  K
        INTEGER*4  I
C
      IF (EOF) GOTO 200                                                         

      IF (FIRST) THEN                                                           
         FIRST = .FALSE.                                                        

         TYPE *                                                                 
         TYPE *,IAM(), '<<< TMREPS (INMIRPT) V01                      >>>'
         TYPE *,IAM(), '<<< Miscellaneous Sales and Petty Cash Report >>>'
         TYPE *                                                                 
         COPY=0

C        CLEAR/SET VARIABLES                                                    
C        -------------------
         PAGE=0                                                                 
         CALL FASTSET(0,MISSLS,10*2)                                            
         CALL FASTSET(0,PTYCSH,10*2)                                            
         CALL FASTSET(0,CMISSLS,10*2*MAXCART)                                   
         CALL FASTSET(0,CPTYCSH,10*2*MAXCART)
      ENDIF                                                                     

C ACCUMULATE STATISCTICS FOR INSTANT, PETTY CASH, AND MISC ITEMS.               
C --------------------------------------------------------------                                                                               
      OFF1 = TRABUF(TSDT1)                                                      

      IF (TRABUF(TSFUN).EQ.TITEMS) THEN         ! IF MISC ITEMS             
         CARTEL = CARTAB(1,TRABUF(TTER)) + 1                                    

         MISSLS(OFF1,1)=MISSLS(OFF1,1)+TRABUF(TSDT3)    ! QUANTITY              
         CMISSLS(OFF1,1,CARTEL)=                                                
     *      CMISSLS(OFF1,1,CARTEL)+TRABUF(TSDT3)        ! QUANTITY              

         MISSLS(OFF1,2)=MISSLS(OFF1,2)+TRABUF(TSDT2)    ! AMOUNT                
         CMISSLS(OFF1,2,CARTEL)=                                                
     *      CMISSLS(OFF1,2,CARTEL)+TRABUF(TSDT2)        ! AMOUNT                

      ELSEIF(TRABUF(TSFUN).EQ.TPCSH) THEN           ! IF PETTY CASH             
         CARTEL = CARTAB(1,TRABUF(TTER)) + 1                                    
         OFF1=TRABUF(TSDT2)

         PTYCSH(OFF1,1)=PTYCSH(OFF1,1)+TRABUF(TSDT3)    ! QUANTITY              
         CPTYCSH(OFF1,1,CARTEL)=                                                
     *      CPTYCSH(OFF1,1,CARTEL)+TRABUF(TSDT3)        ! QUANTITY              

         PTYCSH(OFF1,2)=PTYCSH(OFF1,2)+TRABUF(TSDT1)    ! AMOUNT                
         CPTYCSH(OFF1,2,CARTEL)=                                                
     *      CPTYCSH(OFF1,2,CARTEL)+TRABUF(TSDT1)        ! AMOUNT                
      ENDIF                                                                     
      RETURN                                                                    

200   CONTINUE                                                                  

C PRODUCE THE MISCELLANEOUS SALES REPORT                                        
C --------------------------------------
      TYPE*,IAM(),'Producing Misc. Sales / Petty Cash Report'
      ST = LIB$GET_LUN(LUN)
      IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
      CALL ROPEN('MISCRPT.REP',LUN,ST)                                        
      IF(ST.NE.0) THEN                                                          
         TYPE*,IAM(),'Error opening MISCRPT.REP > ',ST
         CLOSE(LUN)                                                           
         CALL GSTOP(GEXIT_FATAL)
      ENDIF                                                                     

      PAGE=0                                                                    
      PAGES=0                                                                   

      DO 450 CARTEL=1,MAXCART                                                   
         TOT0=0                                                                 
         TOT1=0                                                                 
         TOT2=0                                                                 
         TOT3=0                                                                 

         DO 410 K=1,10                                                          
            TOT0=TOT0+CMISSLS(K,1,CARTEL)                                       
            TOT1=TOT1+CMISSLS(K,2,CARTEL)                                       
            TOT2=TOT2+CPTYCSH(K,1,CARTEL)                                       
            TOT3=TOT3+CPTYCSH(K,2,CARTEL)                                       
410      CONTINUE                                                               

         IF (TOT0.EQ.0.AND.TOT1.EQ.0.AND.TOT2.EQ.0.AND.                         
     *       TOT3.EQ.0) GOTO 450

         IF (PAGES.EQ.0) THEN                                                   
            CALL TITLE('MISCELLANEOUS SALES AND PETTY CASH REPORT',             
     *                 ' TMSCAN',1,LUN,PAGE,DAYCDC)                           
            PAGES=1                                                             
         ELSE                                                                   
            PAGES=0                                                             
         ENDIF                                                                  

         WRITE(LUN,9020) CARTEL - 1     ! CARTELS START FROM 0                

         WRITE(LUN,9015) MISPET(1),MISPET(2)                                  

         DO 430 K=1,10                                                          
            WRITE(LUN,9016) K-1,CMISSLS(K,1,CARTEL),                          
     *                        CSMONY( CMISSLS(K,2,CARTEL),C_LEN,VALUNIT),
     *                        K-1,CPTYCSH(K,1,CARTEL),                          
     *                        CSMONY( CPTYCSH(K,2,CARTEL),C_LEN,VALUNIT)
430      CONTINUE                                                               

         WRITE(LUN,9017) TOT0,CSMONY(TOT1,C_LEN,VALUNIT),TOT2,
     *                          CSMONY(TOT3,C_LEN,VALUNIT)                    
450   CONTINUE                                                                  

      CALL TITLE('MISCELLANEOUS SALES AND PETTY CASH REPORT',                   
     *           ' TMSCAN',1,LUN,PAGE,DAYCDC)                                 
      WRITE(LUN,9021)                                                         
      WRITE(LUN,9015) MISPET(1),MISPET(2)                                     
      TOT0=0                                                                    
      TOT1=0                                                                    
      TOT2=0                                                                    
      TOT3=0                                                                    

      DO 470 K=1,10                                                             
         WRITE(LUN,9016) K-1,MISSLS(K,1),CSMONY( MISSLS(K,2),C_LEN,VALUNIT ),
     *                     K-1,PTYCSH(K,1),CSMONY( PTYCSH(K,2),C_LEN,VALUNIT )
         TOT0=TOT0+MISSLS(K,1)                                                  
         TOT1=TOT1+MISSLS(K,2)                                                  
         TOT2=TOT2+PTYCSH(K,1)                                                  
         TOT3=TOT3+PTYCSH(K,2)                                                  
470   CONTINUE                                                                  

      WRITE(LUN,9017) TOT0,CSMONY(TOT1,C_LEN,VALUNIT),TOT2,
     *                       CSMONY(TOT3,C_LEN,VALUNIT)                    
C
C CLOSE THE REPORT FILE AND SPOOL TO THE PRINTER.
C
      CLOSE(UNIT=LUN)                                                         
      ST = LIB$FREE_LUN(LUN)	
      CALL SPOOL('MISCRPT.REP',COPY,ST)
C
C TOTALS TO BALANSFILE          V04                                         
C --------------------
      TOTSUMS(1) = TOT1                                                         
      TOTSUMS(2) = TOT3                                                         
      RAPCODE = 17                !V08                                          
      CALL BALWRI(RAPCODE,GAMESUMS,TOTSUMS,TOTREAL)

      RETURN                                                                    

C     ======================= FORMAT STATEMENTS ====================            

9012  FORMAT(///,25X,'SALES COUNT',4X,'SALES MK',                               
     *       15X,'CASHES MK',/,25X,47('-'),/)                                   

9013  FORMAT(1X,5A4,5X,I10,A12,12X,A12,/)
9023  FORMAT(14X,'TOTAL',7X,I10,A12,12X,A12,/)

9014  FORMAT(25X,47('='),/)                                                     
9015  FORMAT(//,2(20X,A20,10X),//,2(20X,'ITEM #',4X,'QUANTITY',                 
     *       10X,'MK'),/,2(20X,30('-')),/)                                      
9016  FORMAT(2(20X,I6,4X,I8,A12))                                             
9017  FORMAT(/,2(20X,30('=')),/,2(20X,'TOTAL',3X,I10,A12))
9020  FORMAT(///,5X,'CARTEL ',I8,/,5X,15('_'))                                  
9021  FORMAT(///,5X,'TOTAL',/,5X,5('_'),///)                                    
      END                                                                       
