C
C SUBROUTINE IINIBF
C $Log:   GXAFXT:[GOLS]IINIBF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:35:36   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:38:12   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_hshfili4.for **
C
C
C
C
C
C *** IINIBF
C
C This call is no longer supported because it is very dangerous
C Use IINIB instead.
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE IINIBF(LUN,BIGBUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 BIGBUF, LUN
C
	TYPE *,IAM(),'This program has called IINIBF which is no longer'
	TYPE *,IAM(),'supported.  Change the call to IINIB and specify'
	TYPE *,IAM(),'the size of the buffer.'
	CALL GSTOP(GEXIT_SUCCESS)
	END
