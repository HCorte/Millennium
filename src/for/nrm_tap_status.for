C
C FUNCTION TAP_STATUS
C $Log:   GXAFXT:[GOLS]TAP_STATUS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:28:16   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:49:10   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_tapeio.for **
C
C
C
C*********************************************************************
C
C *** TAP_STATUS      (INTERNAL ONLY) - FUNCTION TO CONVERT TAPE STATUS
C
C*********************************************************************
C
	INTEGER*4 FUNCTION TAP_STATUS(FDB)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    'INCLIB:TAPEIO.DEF'
	INCLUDE	    '($SSDEF)'
C
	INTEGER*4   FDB(TAPFDB_LENGTH)
	INTEGER*4   STATUS
C
C
	STATUS = IAND (FDB (TAPFDB_IOSB), '0000FFFF'X)
	IF(STATUS)THEN
	  TAP_STATUS = 0
	ELSE
C
	  IF (STATUS.EQ.SS$_ENDOFFILE) THEN
	    TAP_STATUS = '88'X		    !END OF FILE FOR CONCURRENT
C
	  ELSE IF (STATUS.EQ.SS$_ENDOFTAPE) THEN
	    TAP_STATUS = '90'X		    !EOT ENCOUNTERED ON CONCURRENT
C
	  ELSE
	    TAP_STATUS = STATUS		    !RETURN VAX STATUS
	  ENDIF
	ENDIF
C
	RETURN
	END
