C
C SUBROUTINE FILL_I4WORD
C $Log:   GXAFXT:[GOLS]FILL_I4WORD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:09:58   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   18 Jun 1993 20:14:32   HXK
C  Initial revision.
C
C ** Source - vsystap.fcc **
C
                                          
C-------------------------------------------------                              
      SUBROUTINE FILL_I4WORD (VSTRING,VNB_MARKS,VI4_WORD)        
      IMPLICIT NONE
                                                                                
C Input parameters
C ----------------
      CHARACTER     VSTRING(*)                                                  
      INTEGER*4     VNB_MARKS                                                   

C Output parameters
C ----------------
      INTEGER*4     VI4_WORD



      CHARACTER     C2    /'2'/  !'2' corresponds to a mark.
      INTEGER*4     I


                                                                                
      DO 100 I=1,VNB_MARKS                                                      
         IF (VSTRING(I).EQ.C2) THEN                                             
            CALL BSET (VI4_WORD,I-1)                                         
         ENDIF                                                                  
 100  CONTINUE                                                                  

       
      RETURN                                                                    
      END                                                                       
