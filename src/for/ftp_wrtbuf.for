C FTP_WRTBUF.FTN                                                                    
C $Log:   GXAFXT:[GOLS]FTP_WRTBUF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:15:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   02 Sep 1994 18:04:36   HXK
C  Merge of May,June RFSS batch 
C  
C     Rev 1.1   26 Jun 1994 14:15:22   HXK
C  minor fixes
C  
C     Rev 1.0   12 May 1994 15:17:48   HXK
C  Initial revision.
C  
C                                                                               
C SUBROUTINE TO WRITE A BUFFER TO FTP FILE                          
C                                                                               
C                                                                               
C=======OPTIONS/CHECK/EXT
      SUBROUTINE FTP_WRTBUF
      IMPLICIT NONE

      INCLUDE 'INCLIB:SYSPARAM.DEF'                                          
      INCLUDE 'INCLIB:SYSEXTRN.DEF'                                          
      INCLUDE 'INCLIB:GLOBAL.DEF'                                            
      INCLUDE 'INCLIB:FTP.DEF'

      INTEGER*4 TSTREC   /0/

C WRITE TO FILE                                                 

      WRITE(FLUN,9000) IBMBUF
      TSTREC=TSTREC+1                                                        

C CLEAR BUFFER AND RESET INDEX
C ----------------------------
      CALL FASTSET(0,IBUF,BYTLEN/4)
      RETURN                                                                    
9000  FORMAT(A100)
      END                                                                       
