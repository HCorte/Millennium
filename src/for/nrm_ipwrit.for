C
C SUBROUTINE IPWRIT
C $Log:   GXAFXT:[GOLS]IPWRIT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:40:38   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:42:28   DAB
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
C *** IPWRIT
C
C This will re-write a previously read record, but will check to be
C sure the record has not changed in the meantime.
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE IPWRIT(I4REC,ORGREC,LUN,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	INTEGER*4 I4REC(*)             !  INPUT: RECORD TO WRITE
	INTEGER*4 ORGREC(*)            !  INPUT: COPY OF ORIGINAL RECORD
	INTEGER*4 LUN                  !  INPUT: LOGICAL UNIT #
	INTEGER*4 STATUS               ! OUTPUT: 0=OK, ELSE ERROR #
C
	LOGICAL   PROTECT
C
C
C
	PROTECT=.TRUE.
	CALL IWRITR(PROTECT,I4REC,ORGREC,LUN,STATUS)
C
C
	RETURN
	END
