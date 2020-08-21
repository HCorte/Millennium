C
C SUBROUTINE IINIB
C $Log:   GXAFXT:[GOLS]IINIB.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:35:32   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:38:08   DAB
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
C *** IINIB
C
C This is called only if you are going to use buffered writes
C Note that there used to be a call for IINIBF which assumed
C a size of BUFSIZ bytes for the big buffer, but that seems
C quite dangerous to me.  I think it's better to let the
C caller say how big his buffer is.
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE IINIB(LUN,BIGBUF,I4SIZE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	INTEGER*4 LUN                    !  INPUT: LOGICAL UNIT #
	INTEGER*4 I4SIZE                 !  INPUT: I*4 SIZE OF BIGBUF
	INTEGER*4 BIGBUF(I4SIZE)         !SCRATCH: BIG BUFFER ARRAY
	INTEGER*4 K
C
C
C
	IF(FCB(FCBLUN,LUN).NE.LUN)THEN
	  TYPE *,IAM(),'IINIB- IOPEN NOT CALLED FOR LUN'
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
C Must have enough room for at least 2 records
C
	IF(I4SIZE.LT.FCB(FCBHSH,LUN)+BUFCNT+
     *	            (FCB(FCBLN4,LUN)+1)*2)THEN
	  TYPE *,IAM(),'IINIB- BIG BUFFER NOT BIG ENOUGH'
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
	DO 1100 K=1,I4SIZE
	  BIGBUF(K)=0
1100	CONTINUE
C
	FCB(FCBBIG,LUN)=I4SIZE
C
	BIGBUF(BUFRSZ)=FCB(FCBLN4,LUN)            !I*4 SIZE OF A RECORD
	BIGBUF(BUFBKS)=FCB(FCBHSH,LUN)            !# OF BUCKETS
	BIGBUF(BUFLUN)=LUN                        !UNIT #
	BIGBUF(BUFFRE)=BUFCNT+BIGBUF(BUFBKS)+1    !1ST FREE OFFSET
C
	RETURN
	END
