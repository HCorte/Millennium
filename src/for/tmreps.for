C TMREPS.FOR
C $Log:   GXAFXT:[GOLS]TMREPS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:35:36   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   07 Sep 1993 13:58:00   SXH
C  Tidied up variable declarations
C  
C     Rev 1.1   03 Sep 1993 12:13:14   SXH
C  Fiexed DCL errors
C  
C     Rev 1.0   18 Aug 1993 15:50:52   HXN
C  Initial revision.
C  
C  
C V04 17-OCT-90 MTK CHANGED FOR KENO GAME                                       
C               PP  Added ONLGAMS report                                        
C V02 15-SEP-90 GCAN FIXED BUG, (LOGREC SIZE)                                   
C V01 30-AUG-90 HHE  Instatnt Sales & Validations Report,                       
C                    Other Sales & Petty Cash Report and                        
C                    SYSSUM Report moved here from TMSCAN                       
C                                                                               
C FILES PRODUCED :   INSRPT.REP
C                    MISCRPT.REP
C                    SYSSUM.REP
C                    ONLGAMS.REP
C                    BALANS.FIL
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
        PROGRAM TMREPS                                                            
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB: GLOBAL.DEF'
	INCLUDE 'INCLIB: CONCOM.DEF'
	INCLUDE 'INCLIB: DESTRA.DEF'
	INCLUDE 'INCLIB: PRMLOG.DEF'
C	INCLUDE 'INCLIB: RECREP.DEF'
	INCLUDE 'INCLIB: AGTCOM.DEF'
	INCLUDE 'INCLIB:TMFREP.DEF'




        INTEGER*4 LOGREC(LMUREC)
        INTEGER*4 SER,ST
                                                  
	INTEGER*4    FILNAM(5)
	CHARACTER*15 TMF_NAME /'SYSX:VAXTMF.FIL'/   !FOR TEST ONLY
	EQUIVALENCE  (FILNAM(1),TMF_NAME)

	INTEGER*4    COUNT /0/


C BEGIN CODE ---------------------------------------------

   	EOF = .FALSE.   
        CALL COPYRITE                                                             
        TYPE *                                                                    
        TYPE *,IAM(),'<<<< TMREPS Transaction Master File Reports V01 >>>>'             
        TYPE *                                                                    


C LOAD CARTEL TABLE                                                             
C -----------------                                  
        CALL LDCART( CARTAB )                                                     


C OPEN THE TMF FILE                                  
C -----------------                                  
        WRITE (5,501) IAM(),(SFNAMES(ST,PTMF),ST=1,5)
501     FORMAT(1X,A4,'  Opening & Reading TM file...',5A4,'...')
      
        CALL OPENW (PTMF,SFNAMES(1,PTMF),4,0,0,ST)     
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,PTMF),1,ST,0)

C       CALL OPENX(PTMF,TMF_NAME,4,0,0,ST)             !FOR TEST ONLY
C       IF(ST.NE.0) TYPE*,IAM(),' OPEN TMF FILE ERROR '
        CALL TOPEN(PTMF)                                                          




C LOOP AND READ THE TM FILE                                                     
C -------------------------                                                                               

        SER=1                                                                     

100     CONTINUE                                                                  

	IF (MOD(SER,50000).EQ.1) TYPE*,' Loading TM in progress ...',SER
	CALL READTMF(LOGREC,SER,EOF)                                              
	IF(EOF) GOTO 1000                                                         

	COUNT = COUNT + 1               !FOR TEST


	CALL LOGTRA(TRABUF,LOGREC)                                                
C
C       ACCUMULATE STATISCTICS FOR INSTANT, PETTY CASH, AND MISC ITEMS.
C        ---------------------------------------------------------------
C        IF(TRABUF(TTYP).EQ.TSPE.AND.TRABUF(TSTAT).EQ.GOOD)                        
C     *    CALL INMIRPT
C
C       COLLECT SYSSUM INFORMATION                                         
C       --------------------------              
        CALL SYSTRPT

C       COLLECT ONLINE SALES BY GAME INFORMATION                           
C       ----------------------------------------
        CALL ONLGAMS

        GOTO 100                                                                  



1000    CONTINUE                                                                  

        TYPE*,' '
        TYPE*,IAM(),' TM Eof encountered.'
        TYPE*,IAM(),' RECORDS READ : ',COUNT
C
C NOT USED FOR PORTUGAL
C
C       CALL INMIRPT
C
        CALL SYSTRPT
        CALL ONLGAMS
        CALL USRCLOS1 (PTMF)



        END   ! TMREPS.FCC
