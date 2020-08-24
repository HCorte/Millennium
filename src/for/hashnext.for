C SUBROUTINE HASHNEXT
C  
C     Rev 1.0   21 Jan 1993 16:34:30   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE HASHNEXT(INDEX1,INDEX2,RECORD,RETURN_STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HASHMEM.DEF'
C
	INTEGER*4   INDEX1,INDEX2,RECORD(*),RETURN_STATUS
C
C POINTER FOR SEQUENTIAL RETRIEVAL
C
	INTEGER*4  HASH_NEXT_RECORD, HASH_NEXT_BLOCK


C
	RETURN_STATUS = HASH_RETURN_OK  !ASSUME IT IS GOOD
C
C IF FIRST RETRIEVAL, INITIALIZE POINTERS
C
	IF(INDEX1 .EQ. 0)  THEN
	   HASH_NEXT_RECORD = 0
	   HASH_NEXT_BLOCK  = 1
	ELSE
	   IF(HASH_NEXT_BLOCK .LE.0 .OR.
     *	      HASH_NEXT_BLOCK .GT. HASH_NUM_BLOCKS)THEN
	      HASH_NEXT_RECORD = 0
	      HASH_NEXT_BLOCK = 1
	   ENDIF
	ENDIF
C
10	CONTINUE
	HASH_NEXT_RECORD = HASH_NEXT_RECORD+1
	IF(HASH_NEXT_RECORD .GT. HASH_RECORDS) THEN
	   HASH_NEXT_RECORD = 1
	   HASH_NEXT_BLOCK  = HASH_NEXT_BLOCK+1
	   IF (HASH_NEXT_BLOCK .LE. HASH_NUM_BLOCKS) GOTO 20
C
C  NO MORE RECORDS
C
	   RETURN_STATUS = HASH_RETURN_NOT_FOUND
	   INDEX1 = 0
	   INDEX2 = 0
	   CALL FASTSET(0,RECORD,HASH_RECORD_SIZE)
	   GOTO 9000
	ENDIF
C
20	CONTINUE
C
C SKIP IF EMPTY SPOT
C
	IF(HASH_TAB(1,HASH_NEXT_RECORD,HASH_NEXT_BLOCK).EQ.0)THEN
	   HASH_NEXT_RECORD = HASH_RECORDS !POINT TO END OF BLOCK
	   GOTO 10
	ENDIF
C
C COPY RECORD (FOUND)
C
	IF(HASH_RECORD_SIZE .GT. 1)
     *	 CALL FASTMOV(HASH_TAB(2,HASH_NEXT_RECORD,HASH_NEXT_BLOCK),
     *	              RECORD, HASH_RECORD_SIZE)
	RECORD(1)=HASH_TAB(2,HASH_NEXT_RECORD,HASH_NEXT_BLOCK)/16
	INDEX1=HASH_TAB(1,HASH_NEXT_RECORD,HASH_NEXT_BLOCK)
	INDEX2=IAND(HASH_TAB(2,HASH_NEXT_RECORD,HASH_NEXT_BLOCK),15)
C
9000	CONTINUE
	RETURN
	END
