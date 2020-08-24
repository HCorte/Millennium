C
C SUBROUTINE IWRITR
C $Log:   GXAFXT:[GOLS]IWRITR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:41:54   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:43:42   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_hshrndi4.for **
C
C
C
C
C *** IWRITR
C
C This will output a record to a hashed file.
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE IWRITR(PROTECT,I4REC,ORGREC,LUN,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	LOGICAL   PROTECT                !  INPUT: .TRUE. =CMPR VS ORIG
C                                      ;         .FALSE.=NO COMPARE
	INTEGER*4 I4REC(*)               !  INPUT: RECORD TO WRITE
	INTEGER*4 ORGREC(*)              !  INPUT: ORIGINAL RECORD
C                                      ;         (IF PROTECT=.TRUE.)
	INTEGER*4 LUN                    !  INPUT: LOGICAL UNIT #
	INTEGER*4 STATUS                 ! OUTPUT: 0=OK, ELSE ERROR #
	INTEGER*4 K, OLDLEN, OLDOFF, NEWBLK, NEWI2KEY
	INTEGER*4 LENREC, NUMREC, NEWI4KEY
C
	INTEGER*4 I4TEMP
	INTEGER*2 I2TEMP(2)
	EQUIVALENCE (I4TEMP,I2TEMP)
C
	LOGICAL   CHKPROTECT
C
C
C
C Be sure that this file is open for direct access.
C
	IF(FCB(FCBLUN,LUN).NE.LUN)THEN
	  STATUS=ERRUNT
	  GO TO 9000
	ENDIF
C
	IF(FCB(FCBMOD,LUN).NE.DIRMOD)THEN
	  IF(FCB(FCBMOD,LUN).EQ.0)THEN
	    FCB(FCBMOD,LUN)=DIRMOD
	    MYBLK=0                         !***B04
	    MYLUN=0                         !***B04
	  ELSE
	    STATUS=ERRMOD
	    GO TO 9000
	  ENDIF
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
C
C If the requested block is not in MYBUF already, get it.
C
	IF(MYLUN.NE.LUN .OR. MYBLK.NE.NEWBLK)THEN
	  MYBLK=0
	  CALL READW(FCB(FCBFDB,LUN),NEWBLK,MYBUFI4,STATUS)
	  IF(STATUS.NE.0)GO TO 9000
	  FCB(FCBBLK,LUN)=NEWBLK
	  MYBLK=NEWBLK
	  MYLUN=LUN
	  FCB(FCBLK4,LUN)=0                !   force a new 'findkey'
	  FCB(FCBLK2,LUN)=0                !   force a new 'findkey'
	ENDIF
C
	CHKPROTECT=PROTECT
C
C If key and length is same as previous read for this lun,
C simply copy the record & overwrite.
C
	IF(NEWI4KEY.EQ.FCB(FCBLK4,LUN) .AND.
     *	   NEWI2KEY.EQ.FCB(FCBLK2,LUN) .AND.
     *	   LENREC  .EQ.FCB(FCBLLN,LUN))THEN
	  OLDOFF=FCB(FCBOFF,LUN)
	  OLDLEN=LENREC
	  GO TO 3000
	ENDIF
C
C Invalidate old key access info.
C
	FCB(FCBLLN,LUN)=0
	FCB(FCBOFF,LUN)=0
C
C If something has changed, search for the key.  If it does not
C already exist, find an empty record and insert it.
C
C Note that FNDKEY is smart and will keep reading until it finds
C either the key indicated or an empty record.
C OLDLEN>0 means the record was found and OLDLEN is its length
C OLDLEN=0 means an empty record was found
C OLDLEN<0 means the file is full
C
2000	CONTINUE
	CALL FNDKEY(LUN,MYBUFI4,NEWI4KEY,NEWI2KEY,OLDOFF,OLDLEN)
	MYBLK=FCB(FCBBLK,LUN)
	MYLUN=LUN
	IF(OLDLEN.LT.0)THEN
	  STATUS=ERRFUL
	  GO TO 9000
	ENDIF
C
C
C If protected write, be sure record has not changed
C
3000	CONTINUE
	IF(CHKPROTECT)THEN
	  IF(OLDLEN.EQ.0)THEN
	    STATUS=ERRUPD
	    GO TO 9000
	  ENDIF
	  DO 3100 K=1,OLDLEN
	    IF(ORGREC(K).NE.MYBUFI4(OLDOFF+K))THEN
	      STATUS=ERRUPD
	      GO TO 9000
	    ENDIF
3100	  CONTINUE
	  CHKPROTECT=.FALSE.
	ENDIF
C
C
C If a different length, mark the old one as a dummy & search again
C
	IF(OLDLEN.NE.0 .AND. OLDLEN.NE.LENREC)THEN
	  I4TEMP=MYBUFI4(OLDOFF+FCB(FCBI2K,LUN))
	  I2TEMP(FCB(FCBI2H,LUN))=-I2TEMP(FCB(FCBI2H,LUN))
	  MYBUFI4(OLDOFF+FCB(FCBI2K,LUN))=I4TEMP
	  CALL WRITEW(FCB(FCBFDB,LUN),FCB(FCBBLK,LUN),MYBUFI4,STATUS)
	  IF(STATUS.NE.0)GO TO 9000
	  GO TO 2000
	ENDIF
C
C If same length or not found, put new record in slot
C
	DO 3200 K=1,LENREC
	  MYBUFI4(OLDOFF+K)=I4REC(K)
3200	CONTINUE
	CALL WRITEW(FCB(FCBFDB,LUN),FCB(FCBBLK,LUN),MYBUFI4,STATUS)
	IF(STATUS.NE.0)GO TO 9000
C
	STATUS=0
	FCB(FCBHIT,LUN)=FCB(FCBHIT,LUN)+1          !FOR STATISTICS
C
9000	CONTINUE
	RETURN
	END
