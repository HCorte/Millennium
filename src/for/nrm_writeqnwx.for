C
C SUBROUTINE WRITEQNWX
C $Log:   GXAFXT:[GOLS]WRITEQNWX.FOV  $
C  
C     Rev 1.0   17 Apr 1996 16:04:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 18:08:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_diskqio.for **
C
C
C
C *** WRITEQNWX	  <<< general purpose write using supplied event flag >>>
C
C (**** EVENT FLAG TO USE IS IN OFFSET 6 OF FDB ****)
C
C *** YOU SHOULD NOT NORMALLY CALL THIS FROM A USER ROUTINE.  IF YOU DO,
C     BE AWARE THAT THE STATUS RETURNED IS THE VAX STATUS (1=OK)
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE WRITEQNWX(FDB, BLOCK, BUFFER, STATUS)
	IMPLICIT NONE
C
	INCLUDE	'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DISKQIO.DEF'
C
	INCLUDE	'($SYSSRVNAM)'
	INCLUDE '($IODEF)'
C
	INTEGER*4   FDB(7)
	INTEGER*4   BLOCK
	INTEGER*4   BUFFER(*)
	INTEGER*4   STATUS
C
	INTEGER*4   BEGBLK
	INTEGER*4   IOLEN
	INTEGER*4   CHAN
	INTEGER*4   FLAG
	INTEGER*4   BLKSIZ
C
C
C	Set BLKSIZ = the number of disk blocks necessary to hold the
C	transaction.  (If the # of bytes in a record is a multiple of
C	256 but not 512, it is rounded up)
C
	BLKSIZ= (FDB(FDB_BYTES)+511)/512
	BEGBLK= (BLOCK-1)*BLKSIZ + 2
	IOLEN = FDB(FDB_BYTES)
	CHAN  = FDB(FDB_CHAN)
	FLAG  = FDB(FDB_EVNT)
C
C	*** DO A QIO WITH NO WAIT
C
	STATUS=SYS$QIO (%VAL(FLAG), %VAL(CHAN), %VAL(IO$_WRITEVBLK),
     1		        FDB(FDB_IOSB),,,
     1	                BUFFER, %VAL(IOLEN), %VAL(BEGBLK),,,)
C
	  RETURN
	  END
