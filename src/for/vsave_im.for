C
C SUBROUTINE VSAVE_IM
C $Log:   GXAFXT:[GOLS]VSAVE_IM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:56:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   18 Jun 1993 20:15:46   HXK
C  Initial revision.
C
C ** Source - vsystap.fcc **
C
                                                                                
C--------------------------------------------------                             
      SUBROUTINE VSAVE_IM (FDB)                                                 
      IMPLICIT NONE
                                                                                
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:LSYSCOM.DEF'
      INCLUDE 'INCLIB:RECSCF.DEF'
                                                                                
                                                                                
      INTEGER*4     VSTAT,FDB(7)
                                                                                
                                                                                

      CALL WRITEQIO (FDB,1,LSYS_ATR,LSYS_COMMON_LENGTH*4,VSTAT)                   
      IF (VSTAT.NE.0) THEN                                                      
         CALL FILERR(SCFSFN(1,LSF),2,VSTAT,1)                                   
         TYPE*,'VSYSTAP aborted, write error '                                  
         TYPE*,'LSYSCHK.FIL not updated'                                        
      ELSE                                                                      
         TYPE*,'LTOSYS.FIL updated '                          
      ENDIF                                                                     
                                                                                
                                                                                
      RETURN                                                                    
      END                            !end vsave_im                              
