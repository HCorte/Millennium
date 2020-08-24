C
C SUBROUTINE INOCHKS
C $Log:   GXAFXT:[GOLS]INOCHKS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:37:02   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:39:28   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_hshfili4.for **
C
C
C
C
C *** INOCHKS
C
C This is called only if you are ABSOLUTELY sure that any record
C written to the file is not a duplicate of another key in the
C file.  Specifically, this inhibits checking for a duplicate
C key.  It is most useful if you also use IWRIBF.
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE INOCHKS(LUN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	INTEGER*4 LUN                    !  INPUT: LOGICAL UNIT #
C
C
	IF(FCB(FCBLUN,LUN).NE.LUN)THEN
	  TYPE *,IAM(),'INOCHKS- IOPEN NOT CALLED FOR LUN'
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
	FCB(FCBDUP,LUN)=1
C
	RETURN
	END
