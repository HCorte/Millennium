C
C HLIST.FCC                                                                              
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        FUNCTION HLIST(H1,H2)                                                     
        IMPLICIT NONE

        ! arguments
        INTEGER*4  H1               !
        INTEGER*4  H2               !

        ! function
        CHARACTER*5 HLIST           !                                              

        IF(H2.EQ.0) THEN                                                          
            WRITE(HLIST,900) H1                                                    
        ELSE                                                                      
            WRITE(HLIST,901) H1,H2                                                 
        ENDIF                                                                     

        RETURN                                                                    
C                                                                               
C                                                                               
900     FORMAT('   ',I2)                                                          
901     FORMAT(I2,',',I2)                                                         

        END                                                                       

