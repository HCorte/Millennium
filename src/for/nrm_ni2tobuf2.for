C
C SUBROUTINE NI2TOBUF2
C $Log:   GXAFXT:[GOLS]NI2TOBUF2.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:12:12   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:06:56   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_nbuffmove.for **
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE NI2TOBUF2(DATA,BUFFER,BYTE_OFFSET)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INTEGER*4 BYTE_OFFSET
	BYTE BUFFER(*)
	BYTE DATA(2)
C
	BUFFER(BYTE_OFFSET+1) = DATA(1)
	BUFFER(BYTE_OFFSET+2) = DATA(2)
C
	RETURN
	END
