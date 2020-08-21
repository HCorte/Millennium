C STRBUF.FOR
C $Log:   GXAFXT:[GOLS]STRBUF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:20:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   03 Sep 1993 15:01:38   HXN
C  Initial revision.
C  
C
C     STRBUF(INTINP,LEN,IDX,BUF)                                                
C                                                                               
C     STORE BUFFER IS A SUBROUTINE THAT TAKES A I4 INTEGER AND STUFFS           
C     IT INTO A CHARACTER BUFFER FOR A GIVEN LENGTH STARTING AT                 
C     A GIVEN INDEX                                                             
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
C=======OPTIONS/CHECK/EXT
      SUBROUTINE STRBUF(INTINP,LEN,IDX,BUF)                                     
      IMPLICIT NONE

      INTEGER*4 TEMP(4),INTINP,LEN,IDX,I
      CHARACTER*2000 BUF                                                        



C***  DO 10 I=4-LEN+1,4                                                         
      DO 10 I=1,LEN                      !on the VAX, storage begins at byte 0
         CALL ILBYTE(TEMP(I),INTINP,I-1)                                        
         BUF(IDX:IDX)=CHAR(TEMP(I))                                             
         IDX=IDX+1                                                              
10    CONTINUE                                                                  



      RETURN                                                                    
      END     ! STRBUF.FCC
