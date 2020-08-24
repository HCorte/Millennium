C WRFNUMB.FOR 
C                                                                               
C $Log:   GXAFXT:[GOLS]WRFNUMB.FOV  $
C  
C     Rev 1.0   17 Apr 1996 16:03:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   23 Sep 1993 19:28:48   HXK
C  Fixed input for WFRsnp.
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT                                                                      
        SUBROUTINE WRFNUMB(SLINE,POS,NUM)                                         
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'

        ! arguments
        INTEGER*4  SLINE(20)                 !
        INTEGER*4  POS                       !
        INTEGER*4  NUM                       !

        ! arguments                                   
        INTEGER*4  DLINE(20)                 !
        INTEGER*4  ERR                       !
        INTEGER*4  I                         !
        INTEGER*4  DPCNT                     !
        INTEGER*4  DIGIT                     !

        CHARACTER  CLINE(80)                 !
        CHARACTER  BLANK                     !
        CHARACTER  ZERO                      !
        CHARACTER  NINE                      !                

        EQUIVALENCE(DLINE,CLINE)                                                  
        DATA BLANK/' '/,ZERO/'0'/,NINE/'9'/                                       
C                                                                               
        NUM   = 0                                                                     
        DPCNT = 100                                                                 
        DO I = 1, 20                                                              
            DLINE(I)=SLINE(I)                                                         
        END DO
C                                                                               
20      CONTINUE   
                                                               
        IF(POS.GT.40) GOTO 50                                                     
        IF(CLINE(POS).NE.BLANK)GOTO 30                                            
        POS = POS + 1                                                                 
        GOTO 20                                                                   
C                                                                               
30      CONTINUE                                                                  
        IF((CLINE(POS).LT.ZERO.OR.CLINE(POS).GT.NINE).AND.                        
     *      CLINE(POS).NE.'.') GOTO 40                                               
        IF(CLINE(POS).EQ.'.') THEN                                                
            DPCNT = 0                                                                 
            POS = POS + 1                                                               
            GOTO 30                                                                 
        ENDIF                                                                     
        DPCNT = DPCNT + 1                                                             
        CALL ASCBIN(DLINE,POS,1,DIGIT,ERR)                                        
        NUM = NUM*10 + DIGIT                                                          
        POS = POS + 1                                                                 
        IF(POS.GT.40)GOTO 40                                                      
        GOTO 30                                                                   
C                                                                               
40      CONTINUE                                                                  

        IF(DPCNT.NE.2) NUM=-1                                                     

        NUM=NUM/DYN_BETUNIT

50      RETURN                                                                    

        END                                                                       

