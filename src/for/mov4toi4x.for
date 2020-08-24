C
C SUBROUTINE MOV4TOI4X
C $Log:   GXAFXT:[GOLS]MOV4TOI4X.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:04:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   04 Sep 1993 14:45:08   WXS
C  Initial revision.
C  
C     Rev 1.0   21 Jan 1993 17:01:46   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_buffmove.for **
C
C
C++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MOV4TOI4X(DATA,BUFFER,BYTE_OFFSET)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	BYTE BUFFER(*)
	BYTE DATA(4)
	INTEGER*4 BYTE_OFFSET
C
	DATA(4) = BUFFER(BYTE_OFFSET+4)
	DATA(3) = BUFFER(BYTE_OFFSET+3)
	DATA(2) = BUFFER(BYTE_OFFSET+2)
	DATA(1) = BUFFER(BYTE_OFFSET+1)
C
	RETURN
	END
