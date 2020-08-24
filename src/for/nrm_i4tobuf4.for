C
C SUBROUTINE I4TOBUF4
C $Log:   GXAFXT:[GOLS]I4TOBUF4.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:34:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:36:28   DAB
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
	SUBROUTINE I4TOBUF4(DATA,BUFFER,BYTE_OFFSET)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	BYTE BUFFER(*)
	BYTE DATA(4)
	INTEGER*4 BYTE_OFFSET
C
	BUFFER(BYTE_OFFSET+1) = DATA(4)
	BUFFER(BYTE_OFFSET+2) = DATA(3)
	BUFFER(BYTE_OFFSET+3) = DATA(2)
	BUFFER(BYTE_OFFSET+4) = DATA(1)
C
	RETURN
	END
