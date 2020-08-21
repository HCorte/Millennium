C ISORTA.FTN                                                                    
C                                                                               
C V03 04-JUN-93  HHN   INITIAL RELEASE FOR VAX FINLAND
C V02 28-AUG/89  GCAN  MOVED FROM SWDV EXPANDED TO 10 DIMENSIONS                
C V01 29-AUG-84  TKO   INITIAL RELEASE                                          
C                                                                               
C THIS WILL PERFORM A SHELL SORT OF A 10-DIMENSION I*4 ARRAY                    
C                                                                               
C                                                                               
C CALLING SEQUENCE:                                                             
C     CALL ISORTA(I4ARAY,CNT,IND)                                               
C INPUT                                                                         
C     CNT   - NUMBER OF ITEMS TO SORT                                           
C     I4ARAY - ARRAY OF ALPHAS IN I*4 FORMAT                                    
C     INDEX-INDEX TO TO SORT ON                                                 
C OUTPUT                                                                        
C     NONE                                                                      
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
      SUBROUTINE ISORTA(I4ARAY,CNT,IND)                                         
      IMPLICIT NONE


      INTEGER*4 CNT
      INTEGER*4 I4ARAY(10,CNT)                                                  


      INTEGER*4 I4TEMP,M,J,K,I,L,IND,XX


      M=CNT                                                                     

20    CONTINUE                                                                  
      M=M/2                                                                     
      IF(M.EQ.0) GO TO 8000                                                     
      J=1                                                                       
      K=CNT-M                                                                   


140   CONTINUE                                                                  
      I=J                                                                       

150   CONTINUE                                                                  
      L=I+M                                                                     
      IF(I4ARAY(IND,I) .LT. I4ARAY(IND,L))GO TO 210                             
      IF(I4ARAY(IND,I) .GT. I4ARAY(IND,L))GO TO 170                             
      GO TO 210                                                                 


170   CONTINUE                                                                  
      DO 175 XX=1,10                                                            
        I4TEMP=I4ARAY(XX,I)                                                     
        I4ARAY(XX,I)=I4ARAY(XX,L)                                               
        I4ARAY(XX,L)=I4TEMP                                                     
175   CONTINUE                                                                  
      I=I-M                                                                     
      IF(I.GE.1)GO TO 150                                                       


210   CONTINUE                                                                  
      J=J+1                                                                     
      IF(J.GT.K)GO TO 20                                                        
      GO TO 140                                                                 


8000  CONTINUE                                                                  


      RETURN                                                                    
      END                                                                       
