C
C SUBROUTINE CHKNULL
C $Log:   GXAFXT:[GOLS]CHKNULL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:32:38   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:50:06   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - anlasf.for **
C
C
C
C
C **** CHKNULL
C
C This will check for a string of nulls and/or spaces
C
C ST= 0 -> Only nulls or spaces were present
C   =-1 -> Some other character was present
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CHKNULL(ARY,OFF,LEN,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 ARY(*)              !Input array
	INTEGER*4 OFF                 !Beginning offset (1-n)
	INTEGER*4 LEN                 !Length in bytes
	INTEGER*4 ST                  !Status
C
	INTEGER*4 K, TEMP
C
C
	DO 1100 K=OFF,LEN+OFF-1
	  CALL ILBYTE(TEMP,ARY,K-1)
	  IF(TEMP.NE.0 .AND. TEMP.NE.ICHAR(' '))THEN
	    ST=-1
	    GOTO 9000
	  ENDIF
1100	CONTINUE
	ST=0
C
9000	CONTINUE
	RETURN
	END
