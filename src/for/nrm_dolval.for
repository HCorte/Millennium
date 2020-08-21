C
C FUNCTION DOLVAL
C $Log:   GXAFXT:[GOLS]DOLVAL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:59:38   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:09:12   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_cmony.for **
C
 
C
C SUBROUTINE TO CONVERT DOLLARS TO VALUNITS
C FOR VISION INPUT/DISPLAY
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        INTEGER FUNCTION DOLVAL(AMOUNT)
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        DOUBLE PRECISION BIG
        INTEGER*4 AMOUNT
C
C
        BIG=DFLOAT(AMOUNT)*DOLL_BASE/DYN_VALUNIT
        DOLVAL=IDINT(BIG)
        RETURN
        END
