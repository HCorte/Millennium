C SUBROUTINE GETEURXRF
C
C GETEURXRF.FOR
C
C V01 01-APR-2016 SCML M16 PROJECT
C
C SUBROUTINE TO GET CROSS SYSTEM REFERENCE NUMBER OF EUROMILLIONS SYSTEM
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE GETEURXRF(CROSS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:EURCOM.DEF'
C
        INTEGER*4 CROSS
C
C       ASSIGN THE INCREMENT NEXT CROSS REF NUMBER
C
        CROSS = EURS_NXTXRF
        EURS_NXTXRF = EURS_NXTXRF + 1
C
C       ADD SYSTEM OFFSET
C
        CROSS = CROSS + EURS_XRFOFF
C       
        RETURN
C
        END
C
C END GETEURXRF.FOR
C
