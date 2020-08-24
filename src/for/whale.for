C SUBROUTINE WHALE
C
C V11 15-JUN-2000 UXN Unused variables removed.
C V10 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V09 29-APR-1994 JXP COPY=0
C V08 01-FEB-1994 HXK SET SCALE TO 200.
C V07 17-NOV-1993 SXH Uncommented CONCOM.DEF, removed initial reference to 
C                     GBTMSUM (for GBASE) (rest of GBASE code already 
C                     commented out)
C V06 07-SEP-1993 SXH De-commented SPOOL
C V05 03-SEP-1993 SXH Fix DCL errors, copy=1
C V04 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V03 28-MAY-1993 HHN INITIAL RELEASE FOR FINLAND
C V02 12-NOV-1991 MTK INITIAL RELEASE FOR NETHERLANDS
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
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
	SUBROUTINE WHALE
	IMPLICIT NONE
	INCLUDE '(LIB$ROUTINES)'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:TMFREP.DEF'

C Input parameters
C ----------------
      INTEGER*4  LUN
C
      INTEGER*4  MINUTE(1440,MAXGAM,4,2), MINOFF 

      CHARACTER  LINE(0:100)                                                    
      CHARACTER  TEXT*4, LIN*101                                                

      INTEGER*4  SCALE,TIMHI,TIMLO,TOTTRN /0/                                   
      INTEGER*4  TER,MAX,HR,NTX/0/,TIM,STEP                                     
      INTEGER*4  STATUS

      LOGICAL    FIRST/.TRUE./

      EQUIVALENCE(LIN(1:1),LINE(0))                                             

      INTEGER*4  COPY
      INTEGER*4  PAGE
      INTEGER*4  H
      INTEGER*4  K
      INTEGER*4  I
      INTEGER*4  ST

C      REAL*4     DLR                    !DOLLAR FUNCTION                        

C      DLR(X) = DFLOAT(X)/100.0D0                                                

C BEGIN CODE ------------------------------------------------------

      IF(FIRST)THEN                                                             
      	 TYPE *                                                                    
      	 TYPE *,IAM(),'<<<<< WHALE Transaction Distribution Analysis V01>>>>>'           
      	 TYPE *                                                                    

C      	 CALL INPNUM('Enter number of report copies      ',COPY,0,20,EXT)          
C      	 IF(EXT.LT.0) RETURN                                                       
         COPY=0

C***     CALL PRMNUM('Enter scale value:                 ',                        
C*** *		       SCALE,1,5000,EXT)                                           
C***     IF(EXT.LT.0) RETURN
         SCALE = 200
C
C Open the report file.
C
	 ST = LIB$GET_LUN(LUN)
	 IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))                                                                
	 CALL ROPEN('WHALE.REP',LUN,ST)
	 IF(ST.NE.0) THEN
	   TYPE*,IAM(),'Error opening WHALE.REP >',ST
	   CALL GSTOP(GEXIT_FATAL)
	 ENDIF
C
C REPORT HEADING                                                          
C        --------------------                                                                       
      	 CALL TITLE ('TRANSACTION DISTRIBUTION ANALYSIS','   WHALE',               
     *                1,LUN,PAGE,DAYCDC)                                         
      	 WRITE(LUN,9000) SCALE                                                   
      	 TER=0                                                                     
      	 MAX=12                                                                    
      	 HR=6                                                                      
      	 TEXT=' '                                                                  
      	 TIM=19800                                                                 
      	 TIMLO=19800                   ! 5:30 AM                                   
      	 TIMHI=TIMLO+600               ! 5:40 AM                                   
      	 STEP=MAX/100                                                              
      	 H=STEP*10                                                                 
      	 FIRST = .FALSE.                                                           

C      	 TYPE *                                                                    
C      	 TYPE *,IAM(),'<<<<< GBTMSUM GBASE Transaction Summary File V01 >>>>>'           
C      	 TYPE *                                                                    

      ENDIF                                                                     



C     ====================== Main Loop ========================                 

      IF(EOF) GOTO 1000                                                         


C BUILD TRANSACTION TIME TABLE FOR GBASE GBTMSUM.FIL                            
C --------------------------------------------------
      IF(MOD(TRABUF(TTIM),60).EQ.0)THEN                                         
         MINOFF = TRABUF(TTIM)/60                                               
      ELSE                                                                      
         MINOFF = (TRABUF(TTIM)/60) + 1                                         
      ENDIF                                                                     


C FOR WHALE REPORT ONLY INCLUDE  TYPE GOOD WAGERS                               
C -----------------------------------------------
C      IF(TRABUF(TSTAT).NE.GOOD.OR.    !TESTED IN TMSCAN YET.
C     *   TRABUF(TTYP).NE.TWAG) RETURN                 

      IF(TRABUF(TGAM).EQ.0) RETURN                                              
      IF(TRABUF(TTYP).GT.0.AND.TRABUF(TTYP).LE.4) THEN                          
         MINUTE(MINOFF,TRABUF(TGAM),TRABUF(TTYP),1) =                           
     *   MINUTE(MINOFF,TRABUF(TGAM),TRABUF(TTYP),1) + 1                         
         MINUTE(MINOFF,TRABUF(TGAM),TRABUF(TTYP),2) =                           
     *   MINUTE(MINOFF,TRABUF(TGAM),TRABUF(TTYP),2) +                           
     *   TRABUF(TWTOT)                                                          
      ENDIF                                                                     


C CHECK TRANSACTION TIME                                                        
C ----------------------
60    CONTINUE                                                                  
      IF(TRABUF(TTIM).LT.TIMHI) THEN                
          NTX=NTX+1                                                             
      ELSE                                                                      
        IF(NTX.LT.1)GOTO 67                                                     
        K=NTX/SCALE                                                             
        IF(K.GT.100) K=100                                                      
        IF(K.LT.0)   K=0                                                        

        DO 65 I=0,K-1                                                           
           LINE(I)='*'                                                          
65      CONTINUE                                                                

67      CONTINUE                                                                
        WRITE(LUN,9001) TIMLO/3600,MOD(TIMLO,3600)/60,NTX,LIN                 
        HR=HR+1                                                                 

        DO 70 I=0,100                                                           
          LINE(I)=' '                                                           
70      CONTINUE                                                                

        TOTTRN=TOTTRN+NTX                                                       
        NTX=0                                                                   
        TIM=TIM+600  !NEXT 10 MINUTE PERIOD                                     
        TIMHI=TIMHI+600                                                         
        TIMLO=TIMLO+600                                                         
        GOTO 60                                                                 
      ENDIF                                                                     

      RETURN                                                                    
C     ==================== End of Main Loop ===================                 
1000  CONTINUE
      CLOSE(LUN)
      ST = LIB$FREE_LUN(LUN)                                                              
      CALL SPOOL('WHALE.REP',COPY,STATUS)                                       
C
C OPEN GBTMSUM.FIL (GBASE TRANSACTION SUMMARY FILE)                             
C -------------------------------------------------
C      OPEN (UNIT=REPLU,FILE='GBTMSUM.FIL',IOSTAT=ST,                            
C     *      STATUS='RENEW',RECL=68,SHARE='SREW',SIZE=32)                        
C      IF(ST.NE.0)THEN                                                           
C         TYPE *,IAM(),'Error opening GBTMSUM.FIL > ',ST                               
C         PAUSE                                                                  
C      ENDIF                                                                     
C      CALL CARCON(REPLU,0) !NO FILE FORMATTING                                  



C BUILD GBASE GBTMSUM.FIL FROM TRANSACTION TIME SUMMARY TABLE                   
C -----------------------------------------------------------
C      TYPE *,IAM(),'Building GBASE Transaction Summary File ...'                      
C      DO 500 MINOFF=1,1440                                                      
C         DO 510 GAM = 1,MAXGAM
C            IF(GNTTAB(GAMIDX,GAM).LE.0) GOTO 510                                
C            WRITE(REPLU,8000) MINOFF, GAM,                                      
C     *                       (MINUTE(MINOFF,GAM,K,1),                           
C     *                        DLR(MINUTE(MINOFF,GAM,K,2)),K=1,4),               
C     *                        CHAR(Y'0D'),CHAR(Y'0A')                           
C            DO 530 TYP=1,4                                                      
C               TOTMIN(TYP,1) = TOTMIN(TYP,1) +                                  
C     *                         MINUTE(MINOFF,GAM,TYP,1)                         
C               TOTMIN(TYP,2) = TOTMIN(TYP,2) +                                  
C     *                         MINUTE(MINOFF,GAM,TYP,2)                         
C530         CONTINUE                                                            
C510      CONTINUE                                                               

C         GAM = 0                                                                
C         WRITE(REPLU,8000) MINOFF, GAM,                                         
C     *                    (TOTMIN(K,1),DLR(TOTMIN(K,2)),K=1,4),                 
C     *                     CHAR(Y'0D'),CHAR(Y'0A')                              
C         DO 540 I=1,4                                                           
C            TOTMIN(I,1) = 0                                                     
C            TOTMIN(I,2) = 0                                                     
C540      CONTINUE                                                               
C500   CONTINUE                                                                  


C      TYPE* ,IAM(),'GBASE Transaction Summary File Built ... '                        
C      TYPE*                                                                     
C      CLOSE(REPLU)                                                              


C     =================== Format Statements ====================                
8000  FORMAT(I4.4,I2.2,4(I6,F9.2),2A)                                           
9000  FORMAT(1X,131('='),//,56X,'* = ',I4,' TRANSACTIONS',/)                    
C9001 FORMAT(2X,'! ',I2,':',I2.2,' !',I4,' !',C100)                             
9001	FORMAT(2X,'| ',I2,':',I2.2,' |',I6,' |',A101)

      RETURN                                                                    
      END                                                                       
