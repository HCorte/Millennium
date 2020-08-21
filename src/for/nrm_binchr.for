C
C SUBROUTINE BINCHR
C $Log:   GXAFXT:[GOLS]BINCHR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:16:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:41:58   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_chrbin.for **
C
C
C
C BINCHR converts an ascii string into a numeric quantity.  It
C works like ASCBIN, except you pass it a character string.
C
C Calling sequence:
C
C     CALL BINCHR(STRING,NUM)
C
C INPUT:
C     NUM    - NUMBER TO CONVERT
C
C OUTPUT:
C     STRING - CHARACTER STRING (***NOT AN ARRAY)
C
C *** Note:  Only as many low order digits as will fit in the
C            string are converted.  No error status is returned.
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE BINCHR(STRING,NUM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER STRING*(*)
	INTEGER*4 NUM
C
C
	INTEGER*4 ACCUM
	INTEGER*4 XLEN
	INTEGER*4 K,X
C
	XLEN=LEN(STRING)
	ACCUM=NUM
C
	DO 1200 K=XLEN,1,-1
	  X=MOD(ACCUM,10)
	  STRING(K:K)=CHAR(X + ICHAR('0'))
	  ACCUM=ACCUM/10
1200	CONTINUE
C
	RETURN
	END
