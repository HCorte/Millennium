C
C SUBROUTINE IWRITE
C $Log:   GXAFXT:[GOLS]IWRITE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:41:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:43:36   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_hshrndi4.for **
C
C
C
C
C
C *** IWRITE
C
C This will rewrite a previously read record
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE IWRITE(I4REC,LUN,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	INTEGER*4 I4REC(*)             !  INPUT: RECORD TO WRITE
	INTEGER*4 LUN                  !  INPUT: LOGICAL UNIT #
	INTEGER*4 STATUS               ! OUTPUT: 0=OK, ELSE ERROR #
C
	LOGICAL   PROTECT
C
C Declare dummy record to simplify calling sequence to IWRITR
C
	INTEGER*4 ORGREC(10)           ! (THIS IS IGNORED BY IWRITR)
C
C
C
	PROTECT=.FALSE.
	CALL IWRITR(PROTECT,I4REC,ORGREC,LUN,STATUS)
C
	RETURN
	END
