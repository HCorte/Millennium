C
C SUBROUTINE LOCKPAGES
C $Log:   GXAFXT:[GOLS]LOCKPAGES.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:52:26   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:53:18   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lodimg.for **
C
C
C
C *** LOCKPAGES
C
C When compiled with debug lines, this will print the size of each common
C In any case, it will call LKPMEM to lock down pages in the common.
C
C When the name of the common is '???TOTAL???', this will output the sum of
C all previous calls.
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE LOCKPAGES( NAME, FIRST, LAST)
	IMPLICIT NONE
C
	INCLUDE	'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER   NAME*(*)
	INTEGER*4   FIRST
	INTEGER*4   LAST
C
	REAL*8	    KBYTES
	INTEGER*4   BYTES, PAGES
	INTEGER*4   TOTPAGES/0/
C
C
C
	IF(NAME.NE.'???TOTAL???')THEN
	  CALL LKPMEM(FIRST, LAST)
	  BYTES    = (%LOC(LAST)+3 - %LOC(FIRST)) + 1
	  PAGES    = ( (BYTES+511)/512)
	  TOTPAGES = TOTPAGES + PAGES
	ELSE
	  PAGES    = TOTPAGES
	ENDIF
C
D	KBYTES = DFLOAT(PAGES)/2.0D0
D	TYPE 1001,IAM(), NAME, PAGES, KBYTES
D1001   FORMAT(X,A,A10,I8,' PAGES, ',F10.1,' KBYTES')
C
	RETURN
	END
