C FNGETCLERK.FTN                                                                  
C                                                                               
C                                                                               
C V01 20-SEP-89 MGM INITIAL RELEASE FOR FINLAND                                 
C                                                                               
C SUBROUTINE TO READ AGENTS RECORD FROM CLERK FILE AND EXTRACT                  
C SALES FOR A GIVEN CLERK OR ALL CLERKS.                                        
C                                                                               
C CALLING SEQUENCE:                                                             
C      CALL FNGETCLERK(TERMINAL,CLERK,FUNC,INDVTAB,                               
C                    INDVSPE,INDVMIS)                                           
C INPUT                                                                         
C     TERMINAL - TERMINAL NUMBER.                                               
C     CLERK    - CLERK NUMBER TO EXTRACT.                                       
C     FUNC     - 0=INDIVIDUAL , 1 = ALL                                         
C OUTPUT                                                                        
C     INDVTAB  - TABLE OF SALES (AS IN PRMAGT.DEF)                              
C     INDVSPE  - TABLE OF SPECIAL SALES                                         
C     INDVMIS  - TABLE OF MISCELLANEOUS SALES                                   
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT   
        SUBROUTINE FNGETCLERK(TERMINAL,CLERK,FUNC,INDVTAB,                          
     *                        INDVSPE,INDVMIS)                                      
        IMPLICIT NONE
C                    
        INCLUDE 'INCLIB:SYSPARAM.DEF'                                                           
        INCLUDE 'INCLIB:SYSEXTRN.DEF'                                                           

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:CLERK.DEF'
C                       
        ! functions
        INTEGER*4  TERMINAL
        INTEGER*4  CLERK
        INTEGER*4  FUNC
        INTEGER*4  INDVTAB(AGAMLEN,MAXGAM) !SALES TRANSACTIONS, GAME               
        INTEGER*4  INDVSPE(ASPELEN,MAXGAM) !SPECIAL SALES, GAME                    
        INTEGER*4  INDVMIS(AMISLEN,NUMTOT) !MISCELLANEOUS SALES,#OF TOTALS          

        ! variables
        INTEGER*4  CLRKBEG
        INTEGER*4  CLRKEND
        INTEGER*4  ST
        INTEGER*4  I
        INTEGER*4  J
        INTEGER*4  K

        INTEGER*4 FDB(7)                                                          
        LOGICAL EXCLUDE                   !EXCLUDE CLERK IN MEMORY                
C                                                                               
        CALL FASTSET(0, INDVTAB, AGAMLEN*MAXGAM)                                    
        CALL FASTSET(0, INDVSPE, ASPELEN*MAXGAM)                                    
        CALL FASTSET(0, INDVMIS, AMISLEN*NUMTOT)                                    

        EXCLUDE=.FALSE.                                                           
C                                                                               
        IF(TERMINAL .LE.0 .OR. TERMINAL.GT.NUMAGT) RETURN                            
        IF(CLERK.LT.0 .OR. CLERK.GT.8) RETURN      !OUT OF RANGE                    
C                                                                               
C OPEN AND READ CLERK FILE                                                      
C                                                                               
        CALL OPENW(8,SFNAMES(1,CLK),4,0,0,ST)                                     
        CALL IOINIT(FDB,8,CLRKSEC*256)                                                
        IF(ST.NE.0) THEN                                                          
            CALL FILERR(SFNAMES(1,CLK),1,ST,0)                                     
            CALL CLOSEFIL(FDB)                                                     
            RETURN                                                                 
        ENDIF                                                                     
C                                                                               
        CALL READW(FDB,TERMINAL,CLRKREC,ST)                                       
        IF(ST.NE.0) THEN                                                          
            CALL FILERR(SFNAMES(1,CLK),2,ST,TERMINAL)                              
            CALL CLOSEFIL(FDB)                                                     
            RETURN                                                                 
        ENDIF                                                                     
        CALL CLOSEFIL(FDB)                                                        
C                                                                               
C FILL IN TABLE FROM RECORD                                                     
C                                                                               
        CLRKBEG=CLERK                                                             
        CLRKEND=CLERK                                                             
C                                                                               
        IF(FUNC.EQ.1) THEN                                                        
            CLRKBEG = 1                                                               
            CLRKEND = 8                                                               
            EXCLUDE=.TRUE.                                                          
        ENDIF                                                                     
        IF(CLRKBEG.EQ.0) RETURN                                                   
C                                                                               
        DO I = 1, AGAMLEN                     ! AS IN PRMAGT.DEF                         
            DO K = 1, MAXGAM                  ! FOR EVERY GAME                           
                DO 50 J = CLRKBEG, CLRKEND    ! FOR EACH CLERK                           
                    IF(EXCLUDE) THEN                                                       
                        IF(J.EQ.CLERK) GOTO 50    !EXCLUDE CLERK IN MEMORY                  
                    ENDIF                                                                  

                    INDVTAB(I,K) = INDVTAB(I,K) + CLRKDAY(I,K,J)                               
                    IF (I.GT.ASPELEN) GOTO 50                                              
                    INDVSPE(I,K)=INDVSPE(I,K)+CLRKSPE(I,K,J)                               
50              CONTINUE                                                               
            END DO
        END DO
C                                                                               
        DO I = 1, AMISLEN                   ! MISCELLANEOUS SALES                      
            DO K = 1, NUMTOT                ! FOR EACH TOTAL                           
                DO 80 J=CLRKBEG,CLRKEND     ! FOR EACH CLERK                           
                    IF(EXCLUDE) THEN                                                    
                        IF(J.EQ.CLERK) GOTO 80                                           
                    ENDIF                                                               
                    INDVMIS(I,K) = INDVMIS(I,K) + CLRKMIS(I,K,J)                            
80              CONTINUE                                                               
            END DO
        END DO
C                                                                               
        RETURN                                                                    

        END                                                                       
