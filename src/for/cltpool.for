C CLTPOOL.FOR
C
C $Log:   GXAFXT:[GOLS]CLTPOOL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:37:34   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   01 Feb 1995 20:47:20   HXK
C  Fix for Lotto 7/39
C  
C     Rev 1.0   11 Oct 1993 13:16:34   HXK
C  Initial revision.
C                                                                      
C SUBROUTINE TO REDISTRIBUTE PARIMUTUEL PRIZE POOLS                             
C                                                                               
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE CLTPOOL(POOLS,SHRCNT,PERCNT,DIV1,DIV2,BNS)    
      IMPLICIT NONE
C
C       
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'           
      INCLUDE 'INCLIB:GLOBAL.DEF'               
C
      INTEGER*4 POOLS(LTGDIV)           !PRIZE POOLS FOR EACH DIVISION
      INTEGER*4 SHRCNT(LTGDIV,2)        !NUMBER OF SHARES PER DIVISION
      INTEGER*4 PERCNT(LTGDIV)          !PERCENTAGE OF TOTAL PRIZE PER DIV
      INTEGER*4 DIV1                    !FIRST DIVISION TO BE COMBINED
      INTEGER*4 DIV2                    !SECOND DIVISION TO BE COMBINED
      INTEGER*4 TOTPOL                  !COMBINED POOL OF DIVS 1 & 2
      INTEGER*4 TOTCNT                  !COMBINED SHARES OF DIVS 1 & 2
      INTEGER*4 TOTPER                  !COMBINED PERCENTAGES OF 1 & 2
      INTEGER*4 PRIZE                   !NEW PRIZE PER SHARE
      INTEGER*4 BNS                     !LOTTO BONUS DRAW FLAG
C                                                                               
C                                                                               
      TOTPOL=POOLS(DIV1)+POOLS(DIV2)                                            
      TOTCNT=SHRCNT(DIV1,BNS)+SHRCNT(DIV2,BNS)                                          
      TOTPER=PERCNT(DIV1)+PERCNT(DIV2)                                          
      PRIZE=TOTPOL/TOTCNT                                                       
C                                                                               
C                                                                               
      POOLS(DIV1)=PRIZE*SHRCNT(DIV1,BNS)                                            
      POOLS(DIV2)=PRIZE*SHRCNT(DIV2,BNS)
      PERCNT(DIV1)=IDINT(DFLOAT(POOLS(DIV1))/DFLOAT(TOTPOL)*                    
     *          DFLOAT(TOTPER))                                                 
      PERCNT(DIV2)=TOTPER-PERCNT(DIV1)                                          
      RETURN                                                                    
      END                                                                       
