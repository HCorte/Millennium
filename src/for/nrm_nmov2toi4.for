C
C SUBROUTINE NMOV2TOI4
C $Log:   GXAFXT:[GOLS]NMOV2TOI4.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:12:46   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:07:32   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_nbuffmove.for **
C
C
C++++++++++++++++++++++++++++++++++
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE NMOV2TOI4(DATA,BUFFER,BYTE_OFFSET)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INTEGER*4 BYTE_OFFSET
	BYTE BUFFER(*)
	BYTE DATA(4)
C
	DATA(1) = BUFFER(BYTE_OFFSET+1)
	DATA(2) = BUFFER(BYTE_OFFSET+2)
	DATA(3) = 0
	DATA(4) = 0
C
	RETURN
	END
