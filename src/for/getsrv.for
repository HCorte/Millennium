C GETSRV.FOR
C $Log:   GXAFXT:[GOLS]GETSRV.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:23:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   20 Aug 1993 14:17:24   HXN
C  Initial revision.
C                                                                               
C 25-APR-91  HHE  SEPARATE UNIT CREATED                                         
C                                                                               
C                                                                               
C SUBROUTINE TO GET SERVICE CHARGES FROM ASF                                    
C ------------------------------------------                                    
C                                                                               
C=======OPTIONS /CHECK/EXT
      SUBROUTINE GETSRV ( UNIT,RENT,AGENT,EOF )                                 
      IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'  !which includes PRMAGT.DEF
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'


      INTEGER*4 UNIT,AGENT,RENT
      LOGICAL EOF                                                               
      LOGICAL INIT /.FALSE./                                                    
C                                                                               
      INTEGER*4 NOCHECK0
      COMMON /NOCHECK0/ NOCHECK0                                                


      INTEGER*4 AGT,ST,CERR,AGTOT,GNUM

C BEGIN CODE ----------------------------------

      NOCHECK0=-1                                                               

C                                                                               
C     INIT AT THE FIRST CALL                                                    
C                                                                               
      IF (.NOT.INIT) THEN                                                       
         CALL OPENASF(UNIT)                                                     
         EOF  = .FALSE.                                                         
         INIT = .TRUE.                                                          
         AGT  = 0                                                               
      ENDIF                                                                     
C                                                                               
C     LOOP HERE UNTIL RENT FOUND                                                
C                                                                               
10    CONTINUE                                                                  
      AGT = AGT + 1                                                             
      IF (AGT.GT.NUMAGT) THEN                                                   
         CALL CLOSASF                                                           
         EOF = .TRUE.                                                           
         RETURN              !DONE                                            
      ENDIF                                                                     
C                                                                               
C     READ AGENT INFO                                                           
C                                                                               
      CALL READASF(AGT,ASFREC,ST)                                               
      IF (ST.NE.0) THEN                                                         
         EOF = .TRUE.                                                           
         RETURN               !RETURN ON ERROR                                 
      ENDIF                                                                     
C                                                                               
C     TEST IF RENT MUST BE TAKEN                                                
C                                                                               
      CALL ASCBIN(ASFINF,SAGNO,LAGNO,AGENT,CERR)                                
      IF (CERR.LT.0) GOTO 10              ! NO AGENT NUMBER                     
C                                                                               
      IF (ASFINV(ASFSRV,1).EQ.0) GOTO 10  ! NO RENT FOR THIS AGENT              
C                                                                               
C     TEST IF AGENT HAS BEEN ACTIVE                                             
C                                                                               
      DO 400 GNUM=1,MAXGAM                ! TOTALS FOR EACH GAME                
C                                                                               
         AGTOT = ASFBIL(GSAMT,GNUM,1)-                                          
     *           ASFBIL(GCAMT,GNUM,1)+                                          
     *           ASFBIL(GVAMT,GNUM,1)                                           
C                                                                               
         IF (AGTOT.NE.0) GOTO 500         ! HAS BEEN ACTIVE                     
C                                                                               
400   CONTINUE                                                                  
      GOTO 10                 ! FOR UNACTIVE AGENTS                             
C                                                                               
C     TAKE RENT AND RETURN                                                      
C                                                                               
500   CONTINUE                                                                  
      RENT = ASFINV(ASFSRV,1)                                                   


      RETURN                                                                    
      END       ! GETSRV.FCC
