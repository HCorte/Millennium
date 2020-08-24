C
C SUBROUTINE BLDSYS_MOVCHR
C $Log:   GXAFXT:[GOLS]BLDSYS_MOVCHR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:19:38   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:44:36   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - bldsys.for **
C
C
C
C
	SUBROUTINE BLDSYS_MOVCHR(CFROM,FIND,CTO,TIND,MLEN)
        IMPLICIT NONE
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        CHARACTER   CFROM (*)           !Character Array to Move From.
        CHARACTER   CTO*(*)             !Character Array to Move To.
C
        INTEGER*4   FIND                !Byte Index to Start Move From.
        INTEGER*4   TIND                !Byte Index to Start Move To.
        INTEGER*4   MLEN                !Length of Move in Bytes.
        INTEGER*4   C, L                !Loop Variable(s).
C
        DO 100 C = 0,MLEN-1
           CTO(TIND+C:TIND+C) = CFROM(FIND+C)
100     CONTINUE
C
        RETURN
        END
