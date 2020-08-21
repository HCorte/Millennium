C SHRTSK.FOR                                                                    
C                                                                               
C                                                                               
C PROCEDURE FOR SHARE CALCULATION                                               
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
         PROGRAM SHRTSK             
         IMPLICIT NONE

         INCLUDE 'INCLIB:SYSDEFINE.DEF'
         INCLUDE 'INCLIB:GLOBAL.DEF'

         INTEGER*4  FLAG       ! answer variable

         REAL*8     SHRLST(2)  ! share program names

         DATA SHRLST/'SHARECLC','SHARERPT'/         
C                                                                               
C                                                                               
C CALL  COPYRITE  SUBROUTINE                                                    
C                                                                               
        CALL COPYRITE                                             
C                                                                               
C                                                                               
        CALL WIMG(5,                                              
     *   'Are you sure you want share calculation (Y/N)? ')       
        CALL YESNO(FLAG)                                          
        IF(FLAG.NE.1) STOP                                        
C                                                                               
C LOAD AND START SHARECALC PROGRAM                                              
C                                                                               
        WRITE(5,901) SHRLST(1)                                    
        CALL RUNTSK(SHRLST(1))                                    
C                                                                               
C LOAD AND START SHRRPT PROGRAM                                                 
C                                                                               
        WRITE(5,901) SHRLST(2)                                    
        CALL RUNTSK(SHRLST(2))                                    
C                                                                               
C                                                                               
        TYPE*,'Verify share/tax values on share report'           
        TYPE*,'To override share values run SHAREUPD procedure'   
      
        CALL GSTOP(GEXIT_SUCCESS)                                 
C                                                                               
C                                                                               
901     FORMAT(' Beginning execution of ',A8)                     

        END                                                       
