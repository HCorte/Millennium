C BIGFRSYS.FTN                                                                  
C $Log:   GXAFXT:[GOLS]BIGFRSYS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:16:06   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   04 Jun 1993 12:00:46   HXN
C  Initial revision.
C                                                                               
C V01  24-FEB-91  HJK   INITIAL RELEASE FOR FINLAND                             
C                                                                               
C SUBROUTINE TO ASSIGN SYSNUM TO LARGE SPORTS SYSTEMS > 256.                    
C                                                                               
C     INPUT:   SYSNUM - SYSTEM NUMBER TO BE IDENTIFIED                          
C                 INV - 0 => SYSNUM -> SYSTEM                                   
C                       1 => SYSTEM -> SYSNUM                                   
C                                                                               
C     OUTPUT:  SYSNUM - IDENTIFIED SYSNUM OR SYSID                              
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
C Copyright 1998 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C                                                                               
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
      SUBROUTINE BIGFRSYS(SYSNUM,INV)                                           
      IMPLICIT NONE


	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF' 
	INCLUDE 'INCLIB:RECREP.DEF' 


      INTEGER*4 SYSNUM,SYSTEM(BIGSYS)                                           
      DATA SYSTEM/    288,    324,    384,    432,    486,                      
     *                512,    576,    648,    729,    768,                      
     *                864,    972,   1024,   1152,   1296,                      
     *               1458,   1536,   1728,   1944,   2048,                      
     *               2187,   2304,   2592,   2916,   3072,                      
     *               3456,   3888,   4096,   4374,   4608,   5184,              
     *               5832,   6144,   6561,   6912,   7776,                      
     *               8192,   8748,   9216,  10368,  11664,                      
     *              12288,  13122,  13824,  15552,  17496,                      
     *              18432,  19683,  20736,  23328,  26244,                      
     *              27648,  31104,  34992,  39366,  41472,                      
     *              46656,  52488,  59049,  62208,  69984,                      
     *              78732,  93312, 104976, 118098, 139968,                      
     *             157464, 177147, 209952, 236196, 314928,                      
     *             354294, 472392, 531441, 708588,1062882,                      
     *            1594323/                                                      

      INTEGER*4 INV,I


C------------------------------------------------


      IF(INV.NE.0) GOTO 200                                                     
      DO 100 I=1,BIGSYS                                                         
         IF(SYSTEM(I).EQ.SYSNUM) THEN                                           
            SYSNUM=(MAXCMB-BIGSYS)+I                                            
            RETURN                                                              
         ENDIF                                                                  
100   CONTINUE                                                                  
      RETURN                                                                    


200   CONTINUE                                                                  
      SYSNUM=SYSTEM(SYSNUM-(MAXCMB-BIGSYS))                                     

      RETURN                                                                    
      END                                                                       
