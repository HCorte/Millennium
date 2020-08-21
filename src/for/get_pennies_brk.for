C $Log:   GXAFXT:[GOLS]GET_PENNIES_BRK.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:24:10   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   02 Nov 1993 11:00:28   HXK
C  Initial revision.
C
C SUBROUTINE TO CALCULATE BREAKAGE, based on GETTAX.FTN                                            
C                                                                               
C V03 23-MAY-92 HJK  JULY 1st TAX CHANGES                                       
C V02 06-MAR-92 HJK  AMENDED TAX CALCULATIONS                                   
C V01 ??-???-91 MTK  INITIAL RELEASE FOR FINLAND                                
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
      SUBROUTINE GET_PENNIES_BRK(GROSS,NET,BREAKAGE)                                 
      IMPLICIT NONE

      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'

      ! arguments
      INTEGER*4  GROSS       ! Gross shares (in pennies)
      INTEGER*4  NET         ! Net shares   (in pennies, then 5 penny units)
      INTEGER*4  BREAKAGE    ! Breakage     (in pennies)

      BREAKAGE = 0                            
      NET = GROSS                             
C                                                                               
      NET = (GROSS/100)*100                     
      BREAKAGE = GROSS - NET

      NET = NET/DYN_VALUNIT 

      RETURN                                                                    

      END                                                                       
