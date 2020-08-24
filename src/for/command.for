C                                                                               
C
C $Log:   GXAFXT:[GOLS]COMMAND.FOV  
C  
C     Rev 1.0   17 Apr 1996 12:41:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   20 Jul 1993 10:29:40   GXA
C  Changed WAIT to XWAIT.
C  
C     Rev 1.0   12 Jul 1993 19:59:08   GXA
C  Initial revision.
C                
C=======OPTIONS /CHECK=NOOVERFLOW                                                               
      SUBROUTINE COMMAND(CBUF)                                                  
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'                                                 
      INCLUDE 'INCLIB:GLOBAL.DEF'                                                           
C
      INTEGER*4 CBUF(CDLEN), ST                                                     
C                                                                               
C                                                                               
10    CONTINUE                                                                  
      CALL QUECMD(CBUF,ST)                                                      
      IF(ST.NE.0) THEN                                                          
        TYPE*,IAM(),'Queue command error > ',ST,' continue to retry'
	CALL XWAIT(4,2,ST)
        GOTO 10                                                                 
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
