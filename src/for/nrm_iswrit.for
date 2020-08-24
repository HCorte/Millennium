C
C SUBROUTINE ISWRIT
C $Log:   GXAFXT:[GOLS]ISWRIT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:41:34   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:43:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_hshseqi4.for **
C
C
C
C
C *** ISWRIT
C
C This will output a record to a hashed file.
C Note that it will not actually output on the call, but will
C output the previously accessed block if we are accessing a new
C block.  Of course, we read a block before appending to it.
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE ISWRIT(I4REC,LUN,IOBUF,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	INTEGER*4 I4REC(*)               !  INPUT: RECORD TO WRITE
	INTEGER*4 LUN                    !  INPUT: LOGICAL UNIT #
	INTEGER*4 IOBUF(*)               !SCRATCH: I/O BUFFER
	INTEGER*4 STATUS                 ! OUTPUT: 0=OK, ELSE ERROR #
C
	INTEGER*4 DIFF, K, TUBOFF, OLDLEN, OLDOFF, NEWBLK
	INTEGER*4 NEWI2KEY, LENREC, NUMREC, NEWI4KEY
C
	INTEGER*4 I4TEMP
	INTEGER*2 I2TEMP(2)
	EQUIVALENCE (I4TEMP,I2TEMP)
C
C
C
C Be sure that this file is open and that for sequential access
C we either always read or always write.
C
	IF(FCB(FCBLUN,LUN).NE.LUN)THEN
	  STATUS=ERRUNT
	  GO TO 9000
	ENDIF
C
	NEWI4KEY=I4REC(FCB(FCBI4K,LUN))
	NUMREC=ISHFT(NEWI4KEY,-30) + 1
	LENREC=NUMREC*FCB(FCBLN4,LUN)
	NEWI4KEY=IAND(NEWI4KEY,INDMSK)
C
	I4TEMP=I4REC(FCB(FCBI2K,LUN))
	NEWI2KEY=I2TEMP(FCB(FCBI2H,LUN))
C
	IF(NEWI4KEY.EQ.0)THEN
	  STATUS=ERRSER
	  GO TO 9000
	ENDIF
C
	NEWBLK=MOD(NEWI4KEY,FCB(FCBHSH,LUN)) + 1
C
	IF(FCB(FCBMOD,LUN).NE.SEQWRI)THEN
	  IF(FCB(FCBMOD,LUN).EQ.0)THEN
	    FCB(FCBMOD,LUN)=SEQWRI
	    FCB(FCBBLK,LUN)=0
	  ELSE
	    STATUS=ERRMOD
	    GO TO 9000
	  ENDIF
	ENDIF
C
C If the block # has changed, write out the old block
C and get the new block into IOBUF
C
	IF(FCB(FCBBLK,LUN).NE.NEWBLK)THEN
	  IF(FCB(FCBBLK,LUN).NE.0 .AND. FCB(FCBCHG,LUN).NE.0)THEN
	    CALL WRITHASH(LUN,FCB(FCBBLK,LUN),IOBUF,STATUS)
	    IF(STATUS.NE.0)GO TO 9000
	    FCB(FCBCHG,LUN)=0
	  ENDIF
	  FCB(FCBBLK,LUN)=0
	  CALL READHASH(LUN,NEWBLK,IOBUF,STATUS)
	  IF(STATUS.NE.0)GO TO 9000
	  FCB(FCBBLK,LUN)=NEWBLK
	  FCB(FCBOFF,LUN)=0
	ENDIF
C
C Now search the block & see if this key already exists.
C If not, put this record at the end of the buffer if it will fit, or
C   find a slot to put it in if it won't.
C If we find the record already in the buffer, overwrite it if the
C new record is the same length.  If a different length, make the
C old record a dummy and put the new record at the end.
C
C Note that FNDKEY is smart and will keep reading until it finds
C either the key indicated or an empty record.
C OLDLEN>0 means the record was found and OLDLEN is its length
C OLDLEN=0 means an empty record was found
C OLDLEN<0 means the file is full
C
2000	CONTINUE
	IF(FCB(FCBDUP,LUN).EQ.0)THEN
	  FCB(FCBOFF,LUN)=0
	ENDIF
	CALL FNDKEY(LUN,IOBUF,NEWI4KEY,NEWI2KEY,OLDOFF,OLDLEN)
	IF(OLDLEN.LT.0)THEN
	  STATUS=ERRFUL
	  GO TO 9000
	ENDIF
C
C If a different length, mark the old one as a dummy & search again
C
C If found and INOCHKS was issued, return an error
C
	TUBOFF=FCB(FCBTUBOFF,LUN)
	IF(OLDLEN.NE.0)THEN
	  IF(FCB(FCBDUP,LUN).NE.0)THEN
	    TYPE *,IAM(),'HSHSEQ:ISWRIT - RECORD ALREADY EXISTS/INOCHKS'
	    TYPE *,IAM(),'IN BUCKET/OFFSET ',FCB(FCBBLK,LUN),
     *			                     FCB(FCBOFF,LUN)
	    TYPE *,IAM(),'KEY = ',NEWI2KEY,'/',NEWI4KEY
	    STATUS=ERRRFN
	    GO TO 9000
	  ENDIF
	  IF(OLDLEN.NE.LENREC)THEN
	    I4TEMP=IOBUF(TUBOFF+OLDOFF+FCB(FCBI2K,LUN))
	    I2TEMP(FCB(FCBI2H,LUN))=-I2TEMP(FCB(FCBI2H,LUN))
	    IOBUF(TUBOFF+OLDOFF+FCB(FCBI2K,LUN))=I4TEMP
	    FCB(FCBCHG,LUN)=1
	    GO TO 2000
	  ENDIF
	ENDIF
C
C If same length or not found, put new record in slot
C
	DO 2100 K=1,LENREC
	  IOBUF(TUBOFF+OLDOFF+K)=I4REC(K)
2100	CONTINUE
	FCB(FCBOFF,LUN)=OLDOFF
	FCB(FCBCHG,LUN)=1
C
	STATUS=0
C
C
C Accumulate some statistics
C
	FCB(FCBHIT,LUN)=FCB(FCBHIT,LUN)+1
	FCB(FCBUSD,LUN)=FCB(FCBUSD,LUN)+NUMREC
	IF(NEWBLK.NE.FCB(FCBBLK,LUN))THEN
	  DIFF=FCB(FCBBLK,LUN)-NEWBLK
	  IF(DIFF.LT.0)DIFF=FCB(FCBHSH,LUN)+DIFF
	  FCB(FCBOVR,LUN)=FCB(FCBOVR,LUN)+1
	  FCB(FCBTOV,LUN)=FCB(FCBTOV,LUN)+DIFF
	ENDIF
C
C
9000	CONTINUE
	RETURN
	END
