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
C Copyright 1991 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C     
C=======OPTIONS/CHECK=NOOVERFLOW/EXT            
      SUBROUTINE GETBRK(GROSS,NET,BREAKAGE)                                 
      IMPLICIT NONE

      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'

      ! arguments
      INTEGER*4  GROSS       ! Gross shares
      INTEGER*4  NET         ! Net shares
      INTEGER*4  BREAKAGE    ! Breakage between gross and net (rounding)

      BREAKAGE = 0                            
      NET = GROSS                             
C                                                                               
      NET = GROSS/(100/DYN_VALUNIT)*(100/DYN_VALUNIT)                     
      BREAKAGE = GROSS - NET

      RETURN                                                                    

      END                                                                       
