C
C SUBROUTINE POOLINI
C $Log:   GXAFXT:[GOLS]POOLINI.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:24:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:18:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - poolbld.for **
C
C
C====================================================================
C INITIALIZE LOTTO POOL COMMON(POOLQUE)
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE POOLINI
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:POOLLTO.DEF'
C
        INTEGER*4 BNK
C
        CALL DEFLST(LTOQ1(1,1),LQUE1-QHEDSZ)
        CALL DEFLST(LTOQ1(1,2),LQUE1-QHEDSZ)
        CALL DEFLST(ADDOVR(1,1),AOVRQUE-QHEDSZ)
        CALL DEFLST(ADDOVR(1,2),AOVRQUE-QHEDSZ)
        CALL DEFLST(REMOVR(1,1),ROVRQUE-QHEDSZ)
        CALL DEFLST(REMOVR(1,2),ROVRQUE-QHEDSZ)
C
        DO 10, BNK=1,BANKSORT
          CALL DEFLST(BANK(1,BNK),BNKQ-QHEDSZ)
10      CONTINUE
        LTCURPAG=0               !INVALIDATE CURRENT PAGE IN MEMORY
C
        RETURN
        END
