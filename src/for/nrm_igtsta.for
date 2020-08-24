C
C SUBROUTINE IGTSTA
C $Log:   GXAFXT:[GOLS]IGTSTA.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:35:28   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:38:02   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_hshfili4.for **
C
C
C
C
C *** IGTSTA
C
C This will obtain the statistics recorded for a logical unit.
C Note that the unit need not be open to obtain the stats.
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE IGTSTA(LUN,STATAB)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	INTEGER*4 LUN                       !  INPUT: LOGICAL UNIT #
	INTEGER*4 STATAB(6)                 ! OUTPUT: STATISTICS
	INTEGER*4 TOTAVL
C
C
C
C
	STATAB(1)=FCB(FCBHIT,LUN)        !# OF HITS
	STATAB(2)=FCB(FCBMIS,LUN)        !# OF MISSES
	STATAB(3)=FCB(FCBOVR,LUN)        !# OF RECORDS NOT IN NORMAL BLK
	STATAB(4)=FCB(FCBTOV,LUN)        !TOTAL OF DISTANCES FROM NORMAL
	TOTAVL=(FCB(FCBMAX,LUN)+3) * FCB(FCBHSH,LUN)
	STATAB(5)=TOTAVL                 !TOTAL AVAILABLE SLOTS
	STATAB(6)=FCB(FCBUSD,LUN)        !TOTAL SLOTS USED
C
	RETURN
	END
