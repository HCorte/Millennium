C VSYSRED.FOR
C $Log:   GXAFXT:[GOLS]VSYSRED.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:56:14   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   17 Jun 1993 15:59:40   HXN
C  Initial revision.
C                                                                               
C                                                                               
C This subroutine fills common with reduced system bets for LOTTO games,
C particularly for VIKING LOTTO.                                                                 
                                                                                
C input:                                                                        
C                                                                               
C       I4REC record from tape to be reduced ...                                
C       MARKS number of "marks" for reduced bet type                            
C       SYSNR system number
C       VALID_MARKS number of valid marks, eg 6 for Viking 6/48
C       RECCNT record count per system type                                     
C                                                                               
C output :                                                                      
C                                                                               
C       POINTER (pointer used when filling common)                              
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
      SUBROUTINE VSYSRED(I4REC,MARKS,SYSNR,VALID_MARKS,RECCNT,POINTER)
      IMPLICIT NONE
                                                                                
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:LSYSCOM.DEF'
                                                                                
C Input parameters
C ----------------                                 
      INTEGER*4 I4REC,      !system table record,                               
     *          MARKS,      !# of marks                                         
     *          SYSNR,      !System #,                                          
     *          VALID_MARKS,!number of valid marks,
     *          RECCNT      !records count,                                     

C Output parameters
C -----------------
      INTEGER*4 POINTER                                                         
                                                                                


                                                                                
      INTEGER*4 BET_DEF(LMXMARK), !lmxmark=24 max marks in red syst              
     *          REP_LU /5/        !report file LU                                     

      CHARACTER BITTAB(LMXMARK)                                                 

      INTEGER*4 I,OFF,BIT,COUNT


C-------------------------------------------                                    


      LSYS_TAB(POINTER)  =VALID_MARKS
      LSYS_TAB(POINTER+1)=0 !bet bit mask field.
                                                                                
      DO 10 I=1,LMXMARK          !initialize table for report ...               
         BITTAB(I) = '-'                                                        
10    CONTINUE                                                                  
                                                                                
C Determine which bits are set in the original record                           
C ---------------------------------------------------                           
      CALL FASTSET(0,BET_DEF,LMXMARK) !initialize ....                          
                                                                                
      OFF=0                                                                     
      DO 100 BIT = 1,MARKS     !determine which bits are set ...              
         IF ( BTEST(I4REC,BIT-1) ) THEN                                              
            OFF=OFF+1                                                           
            BET_DEF(OFF) = BIT-1
            BITTAB (BIT) = 'X' 
         ENDIF                                                                  
 100  CONTINUE                                                                  

      IF (OFF.NE.VALID_MARKS) THEN
          TYPE *,'Number of marks incorrect : ',OFF,'  # ',VALID_MARKS
          RETURN                                                                
      ENDIF                                                                   




      DO 320, OFF=1,VALID_MARKS                                                         
        IF (BET_DEF(OFF).LT.0   .OR.                    
     *      BET_DEF(OFF).GE.MARKS) THEN                                    
          TYPE *,'Invalid bet  offset ',OFF,' bet def ',BET_DEF(OFF)            
          PAUSE                                                                 
          RETURN                                                                
        ENDIF                                                                   

        CALL BSET(LSYS_TAB(POINTER+1),BET_DEF(OFF) )
 320  CONTINUE                                                                  
                                                                                
                                                                                
                                                                                
      CALL BITCNT(LSYS_TAB(POINTER+1),L_SYSBYTES,COUNT)                  
      IF (COUNT.NE.VALID_MARKS) THEN                                                    
        TYPE *,'Invalid bet bit off mark & count ',VALID_MARKS,' ',COUNT                
        PAUSE                                                                   
        RETURN                                                                  
      ENDIF                                                                     
                                                                                
      POINTER=POINTER+2  !position to next free data zone.
      WRITE(REP_LU,900) RECCNT, I4REC, (BITTAB(I),I=1,MARKS)                    


                                                                                
900   FORMAT(2X,I4,'  ',Z8,'  ',40A)                                            
      RETURN                                                                    
      END                                                                       
