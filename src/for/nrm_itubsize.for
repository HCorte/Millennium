C
C SUBROUTINE ITUBSIZE
C $Log:   GXAFXT:[GOLS]ITUBSIZE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:41:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:43:26   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_hshfili4.for **
C
C
C
C
C
C *** ITUBSIZE
C
C This is called only if you intend to use sequential access to
C a file and you can provide a normal I/O buffer that is bigger
C than I4BUCSIZ.  If so, make sure the buffer is a multiple of
C I4BUCSIZ and tell this routine how big it is.  All sequential
C I/O to the file will be done on a TUB basis.  If you call this
C routine and later call a random access routine, you have a problem
C and the random access routine will stop.  All random access assumes
C tub size is 1.
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE ITUBSIZE(LUN,TUBLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	INTEGER*4 LUN              !  INPUT: LOGICAL UNIT #
	INTEGER*4 TUBLEN           !  INPUT: LENGTH OF IOBUF (WORDS)
C
	INTEGER*4 BKTSPERTUB
C
C
C
	IF(FCB(FCBLUN,LUN).NE.LUN)THEN
	  TYPE *,IAM(),'ITUBSIZE- IOPEN NOT CALLED FOR LUN'
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
	IF(FCB(FCBMOD,LUN).NE.0)THEN
	  TYPE *,IAM(),'ITUBSIZE- MUST BE CALLED BEFORE FILE ACCESS'
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
	BKTSPERTUB=TUBLEN/I4BUCSIZ
	IF(BKTSPERTUB.LE.1)THEN
	  TYPE *,IAM(),'ITUBSIZE- LENGTH OF TUB TOO SHORT'
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
	FCB(FCBTUBSIZ,LUN)=BKTSPERTUB       !# OF BUCKETS PER TUB
	FCB(FCBTUBBLK,LUN)=0                !CURRENT TUB #
	FCB(FCBTUBOFF,LUN)=0                !OFFSET OF IOBUF IN TUB
	FCB(FCBTUBCHG,LUN)=0                !TUB HAS NOT CHANGED
C
C Now we set the last tub number.  If file is an even number of tubs
C long, last tub is out of file range.  This should not be a problem
C since we should never reference the last tub in this case.
C
	FCB(FCBTUBLST,LUN)=FCB(FCBHSH,LUN)/BKTSPERTUB + 1
C
C Set up disk I/O size
C
	CALL IOINIT(FCB(FCBFDB,LUN),LUN,BUCSEC*FCB(FCBTUBSIZ,LUN)*256)
C
	RETURN
	END
