C
C SUBROUTINE TO CALCULATE JOKER CDC OFFSETS
C
C  
C V02 28-FEB-2000 RXK Promotion ("add 1 free week") added.
C V01 05-Jul-1993 HXK ADDED POPULARITY LIST
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE KOFFGET(TRABUF,KOFFBEG,KOFFEND)                   
      IMPLICIT NONE                                                     

      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:DESTRA.DEF'
      INCLUDE 'INCLIB:KIKCOM.DEF'
C                                                                               

      ! arguments
      INTEGER*4 KOFFBEG         ! start CDC offset
      INTEGER*4 KOFFEND         ! end CDC offset
      INTEGER*4 GIND            ! game index
      INTEGER*4 DRAW            ! draw
      INTEGER*4 IND

      IF(TRABUF(TWKGME).NE.0) THEN                                              
          GIND=GNTTAB(GAMIDX,TRABUF(TWKGME))            
          DRAW=KIKDRW(GIND)                             
          IF(TRABUF(TWKEND)+TRABUF(TWADDFW).LT.DRAW) THEN               
              KOFFBEG='FF'X                             
              KOFFEND='FF'X                             
          ELSE                                          
              KOFFBEG=KIKDAT(CURDRW,GIND)-DAYCDC        
              IND=TRABUF(TWKEND)+TRABUF(TWADDFW)-DRAW+1                 
              KOFFEND=KIKDAT(IND,GIND)-DAYCDC           
          ENDIF                                         
      ELSE                                                                      
          KOFFBEG=0                                     
          KOFFEND=0                                     
      ENDIF                                                                     

      RETURN

      END 
