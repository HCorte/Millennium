C BNKSUM.FOR
C $Log:   GXAFXT:[GOLS]BNKSUM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:21:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   13 Jul 1993 16:45:52   HXN
C  Initial revision.
C
C SUBROUTINE TO CALCULATE CHECKSUMS FOR BANK WIN REPORT                         
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
C=======OPTIONS/CHECK/EXT
      SUBROUTINE BNKSUM(TAB,CHKSUM)                                             
      IMPLICIT NONE


      INTEGER*4 CHKSUM
      INTEGER*4 TAB(6),CNT(6)                                                   
      DATA CNT/3,8,3,6,8,-1/                                                    


      INTEGER*4 TIKCHK,I,C,NUM,DIG,NUM1,NUM2,
     *          POWER,DIG1,DIG2,DIG3

C-----------------------------------------------------
C                                                                               
      TIKCHK=0                                                                  

      DO 100 I=1,6                                                              
      C=CNT(I)                                                                  
      NUM=IABS(TAB(I))                                                          
10    CONTINUE                                                                  

      DIG=MOD(NUM,10)                                                           
      IF(DIG.EQ.0) DIG=1                                                        

      TIKCHK=TIKCHK+DIG                                                         
      NUM=NUM/10                                                                
      C=C-1                                                                     
      IF(NUM.NE.0.OR.C.GT.0) GOTO 10                                            


100   CONTINUE                                                                  
C                                                                               
C                                                                               
      NUM1=CHKSUM                                                               
      NUM2=TIKCHK                                                               
      CHKSUM=0                                                                  
      POWER=1                                                                   

110   CONTINUE                                                                  
      DIG1=MOD(NUM1,10)                                                         
      DIG2=MOD(NUM2,10)                                                         
      DIG3=MOD(DIG1+DIG2,10)                                                    
      CHKSUM=CHKSUM+DIG3*POWER                                                  
      POWER=POWER*10                                                            
      NUM1=NUM1/10                                                              
      NUM2=NUM2/10                                                              
      IF(NUM1.EQ.0.AND.NUM2.EQ.0) RETURN                                        
      GOTO 110                                                                  

      END   ! BNKSUM.FCC
