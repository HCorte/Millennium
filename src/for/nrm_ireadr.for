C
C SUBROUTINE IREADR
C $Log:   GXAFXT:[GOLS]IREADR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:40:48   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:42:44   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_hshrndi4.for **
C
C
C
C
C
C *** IREADR
C
C This will search a file for the key specified & return it.
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE IREADR(LUN,I4KEY,I2KEY,I4REC,I4LEN,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	INTEGER*4 LUN                  !  INPUT: LOGICAL UNIT #
	INTEGER*4 I4KEY                !  INPUT: I*4 KEY TO LOOK FOR
	INTEGER*4 I2KEY                !  INPUT: I*2 KEY TO LOOK FOR
	INTEGER*4 I4REC(*)             ! OUTPUT: TO HOLD RECORD
	INTEGER*4 I4LEN                ! OUTPUT: ACTUAL LENGTH (I*4)
	INTEGER*4 STATUS               ! OUTPUT: 0=OK, ELSE ERROR #
C
	INTEGER*4 K, OLDLEN, OLDOFF, NEWBLK
C
C
C
C
C Be sure that this file is open for random access and that
C ITUBSIZE was not called. (If it was called with a tub size
C of 1 bucket, it is ok but not desirable).
C
	IF(FCB(FCBLUN,LUN).NE.LUN)THEN
	  STATUS=ERRUNT
	  GO TO 9000
	ENDIF
C
	IF(FCB(FCBMOD,LUN).EQ.DIRMOD)THEN
	  GO TO 1000
	ELSE IF(FCB(FCBMOD,LUN).EQ.0)THEN
	  FCB(FCBMOD,LUN)=DIRMOD
	  MYBLK=0                          !***B04
	  MYLUN=0                          !***B04
	ELSE
	  STATUS=ERRMOD
	  GO TO 9000
	ENDIF
C
	IF(FCB(FCBTUBSIZ,LUN).NE.1)THEN
	  STATUS=ERRERR
	  GO TO 9000
	ENDIF
C
1000	CONTINUE
	IF(I4KEY.EQ.0)THEN
	  STATUS=ERRSER
	  GO TO 9000
	ENDIF
C
	NEWBLK=MOD(I4KEY,FCB(FCBHSH,LUN)) + 1
C
C
C See if the block currently in memory is what we are looking
C for, if not, go get it.
C
	IF(MYLUN.NE.LUN .OR.
     *	   MYBLK.NE.NEWBLK)THEN
	  MYBLK=0
	  CALL READW(FCB(FCBFDB,LUN),NEWBLK,MYBUFI4,STATUS)
	  IF(STATUS.NE.0)GO TO 9000
	  FCB(FCBBLK,LUN)=NEWBLK
	ENDIF
	FCB(FCBOFF,LUN)=0
C
C Now search the block & see if this key is there.
C
C Note that FNDKEY is smart and will keep reading until it finds
C either the key indicated or an empty record.
C OLDLEN>0 means the record was found and OLDLEN is its length
C OLDLEN=0 means an empty record was found
C OLDLEN<0 means the file is full
C
	CALL FNDKEY(LUN,MYBUFI4,I4KEY,I2KEY,OLDOFF,OLDLEN)
	MYBLK=FCB(FCBBLK,LUN)
	MYLUN=LUN
	IF(OLDLEN.LE.0)THEN
	  STATUS=ERRRNF
	  FCB(FCBMIS,LUN)=FCB(FCBMIS,LUN)+1          !FOR STATISTICS
	  GO TO 9000
	ENDIF
C
C Found the record...return it.
C
	I4LEN=OLDLEN
	DO 2100 K=1,I4LEN
	  I4REC(K)=MYBUFI4(OLDOFF+K)
2100	CONTINUE
	FCB(FCBLK4,LUN)=I4KEY
	FCB(FCBLK2,LUN)=I2KEY                   !SAVE THIS KEY, POSITION
	FCB(FCBLLN,LUN)=I4LEN                   !AND RECORD LENGTH
	FCB(FCBOFF,LUN)=OLDOFF                  !FOR IWRITR
C
	STATUS=0
	FCB(FCBHIT,LUN)=FCB(FCBHIT,LUN)+1       !FOR STATISTICS
C
9000	CONTINUE
	RETURN
	END
