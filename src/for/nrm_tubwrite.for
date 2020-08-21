C
C SUBROUTINE TUBWRITE
C $Log:   GXAFXT:[GOLS]TUBWRITE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:39:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:55:34   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_hshsubi4.for **
C
C
C
C
C
C *** TUBWRITE
C
C This routine performs an actual write of a tub.  The tub number
C must be in FCBTUBBLK.  The routine is smart enough to figure out
C if we are near the end of the file and can no longer output an
C entire tub.  In this case, it will output bucket by bucket to the
C end.
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TUBWRITE(LUN,IOBUF,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	INTEGER*4 LUN                   !LOGICAL UNIT #
	INTEGER*4 IOBUF(*)              !I/O BUFFER (TUB)
	INTEGER*4 STATUS                !STATUS OF I/O
	INTEGER*4 BKT, TUBOFF, LASTBKT, FRSTBKT, SNGLCNT, TUBNUM
C
C
C
	TUBNUM=FCB(FCBTUBBLK,LUN)
	IF(TUBNUM.EQ.0)THEN
	  TYPE *,IAM(),'TUBWRITE- CALLED WITHOUT A TUB TO WRITE'
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
	IF(TUBNUM.LT.FCB(FCBTUBLST,LUN))THEN
	  CALL WRITEW(FCB(FCBFDB,LUN),TUBNUM,IOBUF(1),STATUS)
	  GO TO 9000
	ENDIF
C
C Come here only if accessing the last tub and the last tub is
C not a full size tub.
C
C If there are no bugs, I believe it is impossible to come here
C if the last bucket is a full bucket.  In any case, we will stop
C if I am mistaken.
C
	CALL IOCHGBUK(FCB(FCBFDB,LUN),BUCSEC)       !**B03
C
C
C Set number of buckets in last tub.
C
	SNGLCNT=MOD( FCB(FCBHSH,LUN),FCB(FCBTUBSIZ,LUN) )
	IF(SNGLCNT.LE.0)THEN
	  TYPE *,IAM(),'TUBWRITE- ATTEMPT TO WRITE BEYOND END OF FILE'
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
C Set 1st bucket number in last bucket (may point beyond end of file)
C
	FRSTBKT=(FCB(FCBTUBLST,LUN)-1)*FCB(FCBTUBSIZ,LUN) + 1
	LASTBKT=FRSTBKT+SNGLCNT-1
C
	TUBOFF=0
	DO 1900 BKT=FRSTBKT,LASTBKT
	  CALL WRITEW(FCB(FCBFDB,LUN),BKT,IOBUF(TUBOFF+1),STATUS)
	  IF(STATUS.NE.0)GO TO 2000
	  TUBOFF=TUBOFF+I4BUCSIZ
1900	CONTINUE
C
2000	CONTINUE
C
C  Set the I/O size back to what it was before
C
	CALL IOCHGBUK(FCB(FCBFDB,LUN),BUCSEC*FCB(FCBTUBSIZ,LUN)) !**B03
C
9000	CONTINUE
	RETURN
	END
