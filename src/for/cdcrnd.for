C CDCRND.FTN                                                                    
C $Log:   GXAFXT:[GOLS]CDCRND.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:30:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   11 Nov 1994 13:35:56   HXK
C  made change for week a day in week 52 being in following year
C  
C     Rev 1.0   13 Jul 1993 16:46:30   HXN
C  Initial revision.
C                                                                               
C V01 25-APR-91  HHE  CREATED                                                   
C                                                                               

C=======OPTIONS/CHECK/EXT
      SUBROUTINE CDCRND(CDCDAY,GAME,INDEX,YEAR,WEEK)                            
      IMPLICIT NONE

      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:DATBUF.DEF'

      INTEGER*4 CDCDAY,GAME,INDEX,YEAR,WEEK

      CALL FIGWEK(CDCDAY+1,WEEK,YEAR) ! Week start from Sunday !!!!

      RETURN                                                                    
      END      ! CDCRND.FTN
