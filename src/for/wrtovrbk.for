C WRTOVRBLK.FOR                                                                 
C                                                                               
C V01 11-MAR-93 JWE Initial release                                             
C                                                                               
C This program writes the overflow records passed by OVERDMP at the             
C sector offset given when called.                                              
C                                                                               
C INPUT:                                                                        
C     INTEGER*4   FDB(7) File descriptor block of dump file                     
C     INTEGER*4   OVERFLOW_RECORD(2,MAXOVR) record to be writen                 
C     INTEGER*4   SECTOR next free sector                                       
C OUTPUT:                                                                       
C     INTEGER*4   SECTOR is updated to be the next free sector                  
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
C Copyright 1991, 1993 GTECH Corporation. All rights reserved.             
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
      SUBROUTINE WRTOVRBK (FDB,OVERFLOW_RECORD,SECTOR)
      IMPLICIT NONE
                                                                                
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'

      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:POOLLTO.DEF'
                                                                                
      INTEGER*4   FDB(7)                                                        
      INTEGER*4   OVERFLOW_RECORD(2*MAXOVR)                                     
      INTEGER*4   SECTOR                                                        

      INTEGER*4 SMALL_BLOCK,ST

C                                                                               
C In this loop we write all of the OVERFLOW record in 1 sector chunks           
C                                                                               

100   CONTINUE                                                                  
      DO 200 SMALL_BLOCK=1,2*MAXOVR*4/512                                      
         CALL WRITEW(FDB,                                                       
     *      SECTOR,                                                             
     *      OVERFLOW_RECORD((SMALL_BLOCK-1)*512/4+1),                           
     *      ST)                                                                 
         IF (ST.NE.0) THEN                                                      
            TYPE *,IAM(),'Error writing sector ',SECTOR,' to dump file: ',ST,
     *         CHAR(7)                                                          
            TYPE *,IAM(),'CONTINUE to retry'                                          
            CALL GPAUSE        
            GOTO 100                                                            
         ENDIF                                                                  
         SECTOR = SECTOR + 1                                                    
200   CONTINUE                                                                  

      RETURN                                                                    
      END       ! WRTOVRBK.FCC
