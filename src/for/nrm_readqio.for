C
C SUBROUTINE READQIO
C $Log:   GXAFXT:[GOLS]READQIO.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:38:48   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:27:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskqio.for **
C
C
C
C
C *** READIO       <<<READ and wait for completion>>>
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE READQIO(FDB, BLOCK, BUFFER, NUMBYTES, STATUS)
	IMPLICIT NONE
C
	INCLUDE	'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DISKQIO.DEF'
C
	INCLUDE '($SYSSRVNAM)'
	INCLUDE '($IODEF)'
C
	INTEGER*4   FDB(7)
	INTEGER*4   BLOCK
	INTEGER*4   BUFFER(*)
	INTEGER*4   NUMBYTES
	INTEGER*4   STATUS
C
	INTEGER*4   IOLEN
	INTEGER*4   CHAN
	INTEGER*4   BEGBLK
C
	INTEGER*4   HOLD
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	EQUIVALENCE (I4TEMP,I2TEMP)
C
C
	IOLEN = NUMBYTES
	CHAN  = FDB(FDB_CHAN)
	BEGBLK= BLOCK + 1
C
C	*** DO A QIO WITH WAIT
C
	STATUS=SYS$QIOW ( , %VAL(CHAN), %VAL(IO$_READVBLK),
     1		        FDB(FDB_IOSB),,,
     1	                BUFFER, %VAL(IOLEN), %VAL(BEGBLK),,,)
C
	IF(STATUS)THEN
	  I4TEMP = FDB(FDB_IOSB)
	  STATUS = ZEXT(I2TEMP(1))
	  IF(STATUS)THEN
	    STATUS = 0
	  ENDIF
	ENDIF
C
C	I/O COUNT IS NOT FULLWORD ALIGNED...GET IT ANYWAY
C
	I4TEMP = FDB(FDB_IOSB)
	HOLD   = I2TEMP(2)
	I4TEMP = FDB(FDB_IOSB+1)
	I2TEMP(2) = I2TEMP(1)
	I2TEMP(1) = HOLD
C
	FDB(FDB_IOLEN) = I4TEMP
C
9000	CONTINUE
	RETURN
	END
