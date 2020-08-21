C BALCHK.FTN                                                                    
C                                                                               
C V04 FEB-93  PP  CURGAM = 7                                                    
C V03 20-DEC-92 JWE Sales array should be MAXGAM not MAXTYP                     
C V02 OCT-92  PP   NEW GAME ADDED                                               
C V01 22-MAY-91 PP INITIAL RELEASE FOR FINLAND                                  
C                                                                               
C PROGRAM TO WRITE A BALANS REPORT USING THE FIGURES IN BALANS.FIL              
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
C 'Copyright 1991, 1992 GTECH Corporation. All rights reserved.'           
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            
C 
C=======OPTIONS/CHECK=NOOVERFLOW/EXT                                                                      
        SUBROUTINE BALCHK(BALRECORD,WAGERS,VOIDS,VALIDS,                          
     *                    SALES,TOTALS,I,TASMAYS,EITASM)                          
C                                                                               
        IMPLICIT NONE
C                    
                                                           
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:BALANS.DEF'
         
        ! arguments                                 
        INTEGER*4  BALRECORD(BALLEN)            !
        INTEGER*4  WAGERS                       !                                   
        INTEGER*4  VOIDS                        !                                   
        INTEGER*4  VALIDS                       !                                   
        INTEGER*4  SALES(NUMTOT,NUMFIN,MAXGAM)  !                                   
        INTEGER*4  TOTALS(NUMTOT,NUMFIN)        !                                   
        INTEGER*4  I                            !                                   
        INTEGER*4  TASMAYS(MAXGAM+1)            !                                   

        LOGICAL EITASM                          !                                  

        ! variables
        INTEGER*4  REPS(3)                      !                                   
        INTEGER*4  GAM                          !
        INTEGER*4  IND                          !
        INTEGER*4  IND2
        INTEGER*4  RAP                          !
        INTEGER*4  RAP1                         !
        INTEGER*4  RAP2                         !
        INTEGER*4  J                            !
        INTEGER*4  IX                           !
        INTEGER*4  JX                           !

        DATA REPS/1,2,3/                                                          
C                                                                               

                                                                                
        CALL FASTMOV(BALRECORD,BALREC,BALLEN)                                     
C                                                                               
C CHECK IF SUMS BALANCE                                                      
C                                                                               
        EITASM = .FALSE.                                                     
        CALL FASTSET(0,TASMAYS,6)                                            
        RAP1 = 0                                                             
C                                                                               
C    CALCULATE AGTACT AND CARACT NET SALES                                      
C                                                                               
        DO GAM = 1, MAXGAM                                                     
            DO IND = 1,2                                                          
                DO RAP = 2,3                                                          
                    BALGSUMS(RAP,GAM,1,IND) = BALGSUMS(RAP,GAM,1,IND) -                     
     *                                        BALGSUMS(RAP,GAM,2,IND)                       
                END DO
            END DO
        END DO

        DO  RAP = 2,4        !AGTACT,CARACT AND CLKREP                         
            BALTSUMS(RAP,1) = BALTSUMS(RAP,1) - BALTSUMS(RAP,3)                    
            BALTSUMS(RAP,2) = BALTSUMS(RAP,2) - BALTSUMS(RAP,4)                    
        END DO
C                                                                               
C       RAP1= the 1st rapcode to be compared                                   
C                                                                               
        J = 2 * I - 1                                                             
        IX = I                                                                    
        JX = J                                                                    
        DO 320 IND = 1,3                                                          
            RAP = REPS(IND)                                                        
            IF (BALRAPC(RAP).NE.0) THEN                                            
                RAP1 = RAP                                                         
                GOTO 325                                                           
            ENDIF                                                                  
320     CONTINUE                                                                  
        GOTO 331                                                                  

325     CONTINUE                                                                  
        IF (I.EQ.3 .AND. RAP1.EQ.1) THEN                                            
            IX = IX + 1                                                           
            JX = JX + 2                                                           
        ENDIF                                                                     

        DO 330 IND2 = IND + 1,3                                                   
            RAP2 = REPS(IND2)                                                      
            IF (BALRAPC(RAP2).EQ.0) GOTO 330                                       
            DO 326 GAM = 1, MAXGAM                                                  
                IF (BALGSUMS(RAP1, GAM, IX, 1) .NE.                                     
     *              BALGSUMS(RAP2, GAM, I,  1) .OR.                                      
     *              BALGSUMS(RAP1, GAM, IX, 2) .NE.                                     
     *              BALGSUMS(RAP2, GAM, I,  2)) THEN                                    
                    TASMAYS(GAM) = 1                                                
                    EITASM = .TRUE.                                                 
                ENDIF                                                               
326         CONTINUE                                                               

            IF (BALTSUMS(RAP1,JX)   .NE. BALTSUMS(RAP2,J)    .OR.                          
     *          BALTSUMS(RAP1,JX+1) .NE. BALTSUMS(RAP2,J+1)) THEN                    
                TASMAYS(MAXGAM+1) = 1                                                     
                 EITASM = .TRUE.                                                    
            ENDIF                                                                  
330     CONTINUE                                                                  

331     CONTINUE    
                                                              
        IF (I.GT.1) GOTO 351                                                      
        IF (RAP1.EQ.0 .AND. BALRAPC(5).NE.0) THEN                                   
            RAP1 = 5                                                              
            GOTO 351                                                              
        ENDIF                                                                     

        IF (RAP1.EQ.0) GOTO 500   !!! added by SMH !!!                                                
        DO 350 GAM = 1, MAXGAM  

            IF (BALGSUMS(RAP1,GAM,IX,2).NE.                                        
     *          BALGSUMS(5,GAM,I,2)) THEN      !GAMTOT                             
                TASMAYS(GAM) = 1                                                   
                EITASM = .TRUE.                                                    
            ENDIF                                                                  

            IF (BALTSUMS(RAP1,J+1).NE.BALTSUMS(5,J+1)) THEN                        
                TASMAYS(GAM) = 1                                                    
                EITASM = .TRUE.                                                   
            ENDIF                                                                  
350     CONTINUE                                                                  


351     CONTINUE                                                                  

        IF (RAP1.EQ.0) GOTO 500                                                   

        DO 352 GAM = 1, MAXGAM              !SALSNP                                
            IF (RAP1.NE.5) THEN                                                       
                IF (SALES(1,IX,GAM) .NE. BALGSUMS(RAP1,GAM,IX,1).OR.                     
     *              SALES(2,IX,GAM) .NE. BALGSUMS(RAP1,GAM,IX,2)) THEN                   
                    TASMAYS(GAM) = 1                                                   
                    EITASM = .TRUE.                                                    
                ENDIF                                                                  
            ELSEIF (SALES(2,IX,GAM) .NE. BALGSUMS(RAP1,GAM,IX,2)) THEN                  
                TASMAYS(GAM) = 1                                                   
                EITASM = .TRUE.                                                    
            ENDIF  
                                                                   
            IF (RAP1.NE.5) THEN                                                       
                IF (TOTALS(1,IX) .NE. BALTSUMS(RAP1,JX).OR.                              
     *              TOTALS(2,IX) .NE. BALTSUMS(RAP1,JX+1)) THEN                          
                    TASMAYS(GAM) = 1                                                     
                    EITASM = .TRUE.                                                    
                ENDIF                                                                  
            ELSEIF (TOTALS(2,IX).NE.BALTSUMS(RAP1,JX+1)) THEN                         
                TASMAYS(GAM) = 1                                                     
                EITASM = .TRUE.                                                    
            ENDIF                                                                     

352     CONTINUE                                                                  
C                                  !SYSSNP                                      
        IF (I.EQ.1 .AND. BALTSUMS(RAP1,JX+1) .NE. WAGERS .OR.                          
     *      I.EQ.2 .AND. VOIDS  .NE. BALTSUMS(RAP1,JX+1) .OR.                           
     *      I.EQ.3 .AND. VALIDS .NE. BALTSUMS(RAP1,JX+1)) THEN                        
            TASMAYS(MAXGAM+1) = 1                                                     
            EITASM = .TRUE.                                                    
        ENDIF                                                                     
C                                            !CANREP                            
        IF (I.EQ.2 .AND. VOIDS .NE. BALTSUMS(7,J+1)) THEN                             
            TASMAYS(MAXGAM+1) = 1                                                     
            EITASM = .TRUE.                                                    
        ENDIF                                                                     
C                                            !CLKREP                            
        IF (BALRAPC(4).NE.0) THEN                                                
            IF (BALTSUMS(RAP1,JX)   .NE. BALTSUMS(4,J).OR.                            
     *          BALTSUMS(RAP1,JX+1) .NE. BALTSUMS(4,J+1)) THEN                      
                TASMAYS(MAXGAM+1) = 1                                                    
                EITASM = .TRUE.                                                   
            ENDIF                                                                 
                                                                                
        ENDIF                                                                    
C                                     
500     CONTINUE                                                                  
C                                                                               

        RETURN                                                                    

        END                                                                       
