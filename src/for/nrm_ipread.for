C
C SUBROUTINE IPREAD
C $Log:   GXAFXT:[GOLS]IPREAD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:40:34   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:42:22   DAB
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
C *** IPREAD
C
C This will get the record corresponding to the indicated keys and
C will save a copy of the record in ORGREC (for later protected write)
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE IPREAD(XKEYS,I4REC,ORGREC,LUN,I2SIZE,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	INTEGER*4 XKEYS(2)             !  INPUT: (1)=I2KEY (AS I*4)
C                                    ;         (2),I4KEY
	INTEGER*4 I4REC(*)             ! OUTPUT: TO HOLD RECORD
	INTEGER*4 ORGREC(*)            ! OUTPUT: TO HOLD COPY OF RECORD
	INTEGER*4 LUN                  !  INPUT: LOGICAL UNIT #
	INTEGER*4 I2SIZE               ! OUTPUT: I*2 LENGTH OF RECORD
	INTEGER*4 STATUS               ! OUTPUT: 0=OK, ELSE ERROR #
C
	INTEGER*4 K, RECI4LEN, I4KEY, I2KEY
C
C
C
C
	I2KEY=XKEYS(1)
	I4KEY=XKEYS(2)
C
	CALL IREADR(LUN,I4KEY,I2KEY,I4REC,RECI4LEN,STATUS)
C
C If read ok, copy to ORGREC
C
	IF(STATUS.EQ.0)THEN
	  DO 1100 K=1,RECI4LEN
	    ORGREC(K)=I4REC(K)
1100	  CONTINUE
	  I2SIZE=RECI4LEN*2
	ENDIF
C
	RETURN
	END
