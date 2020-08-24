C
C FUNCTION SNIF_VALUE
C $Log:   GXAFXT:[GOLS]SNIF_VALUE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:10:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:39:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_snif.for **
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C SNIF_VALUE(ADDR)
C RETURNS THE VALUE AT THE LOCATION 'ADDR'
C
C-----------------------------------------------------------------------
C
C=======OPTIONS /CHECK=NOOVERFLOW
	INTEGER*4 FUNCTION SNIF_VALUE(ADDR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4   ADDR
C
	SNIF_VALUE = ADDR
	RETURN
	END
