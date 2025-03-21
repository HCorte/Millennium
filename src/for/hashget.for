C
C SUBROUTINE HASHGET
C $Log:   GXAFXT:[GOLS]HASHGET.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:31:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:34:24   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - hashmem.for **
C
C HASHMEM.FOR
C
C V01 29-JUN-92 GCAN INITIAL RELEASE FOR THE NETHERLANDS (FROM SWEDEN)
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C
C SUBROUTINES USED TO ACCESS ODDSET POOLS (TOTO SELECT)
C
C HASH_TAB - IS A HASH TABLE USED TO STORE TOTO SELECT COMBINATIONS
C            OF 4,5 AND 6 PLAYED ONLY
C HASH_DIR - IS A DIRECT ACCESS TABLE CAPABLE OF STORING ALL
C            POSSIBLE TOTO SELECT COMBINATIONS OF 3
C
C HASHGET(INDEX1,INDEX2,RECORD,NEXT_RECORD,
C         NEXT_BLOCK,RETURN_STATUS)  ; GET A RECORD
C
C INPUT PARAMETERS:
C   INDEX1 -   I*4 INDEX USED
C   INDEX2 -   I*4 INDEX, WITH VALUE [0..15]  - # OF ROWS
C
C OUTPUT PARAMETERS:
C   RECORD  -  RECORD RETRIEVED FROM HASH TABLES
C   RETURN_STATUS = (-1) COULD NOT GET THE RECORD (TOO MANY SEARCHES)
C   NEXT_RECORD = RECORD NUMBER OF RECORD FOUND
C   NEXT_BLOCK = BLOCK NUMBER IN WHICH RECORD WAS FOUND
C
C HASHPUT(INDEX1,INDEX2,RECORD,RECSTAT,NEXT_RECORD,
C         NEXT_BLOCK,RETURN_STATUS)  ; PUT A RECORD
C
C INPUT PARAMETERS:
C   INDEX1 -   I*4 INDEX USED
C   INDEX2 -   I*4 INDEX, WITH VALUE [0..15]  - #OF ROWS
C   RECORD  -  RECORD TO STORE IN HASH TABLES
C   RECSTAT -  IF.NE.0 MEANS SAME RECORD JUST FOUND
C              BY HASHGET DO NOT RE-SEARCH FOR THE RECORD
C              USE RECORD NUMBER AND BLOCK NUMBER FOUND
C              BY HASHGET
C   NEXT_RECORD - RECORD NUMBER WHERE RECORD WAS FOUND IN BY
C                 HASHGET
C   NEXT_BLOCK - BLOCK NUMBER WHERE RECORD WAS FOUND IN BY
C                HASHGET
C
C OUTPUT PARAMETERS:
C
C   RETURN_STATUS = (-1) COULD NOT PUT THE DATA (TOO MANY SEARCHES)
C
C HASHNEXT(INDEX1,INDEX2,RECORD,RETURN_STATUS) ; GET NEXT RECORD
C
C INPUT PARAMETERS:
C   INDEX1 -   IF ZERO IT WILL INITIALIZE SEARCH TABLES
C
C OUTPUT PARAMETERS:
C   INDEX1 -   NEXT INDEX FOUND
C   INDEX2 -   NEXT INDEX2 FOUND [0..15] - # OF ROWS
C   RECORD -   RECORD RETRIEVED
C   RETURN_STATUS
C
C EACH ELEMENT IS ADDRESSED BY 2 INDEXES:
C     INDEX1  - INTEGER*4 (ANY NON 0 I*4 VALUE)
C     INDEX2  - INTEGER*4, HAS TO TAKE VALUE [0..15]   - #OF ROWS
C     RECORD  - FIRST ELEMENT OF THE RECORD HAS THE LIMIT OF
C   OTHER ELEMENTS ARE I*4
C
C
C
C COPYRITF.DEF+++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C COPYRIGHT 1991 GTECH CORPORATION.  ALL RIGHTS RESERVED.
C
C CONFIDENTIAL PROPRIETARY INFORMATION
C This item is the property of GTECH Corporation, W. Greenwich, Rhode
C Island, and contains confidential and trade secret information.  It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH.  Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published or disclosed, in whold or in part, directly
C or indirectly, except as expressly authorized by an officer of
C GTECH pursuant to written agreement.
C COPYRITF.DEF-------------------------------------------------------
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE HASHGET(INDEX1, INDEX2, RECORD, NEXT_REC,
     *	                   NEXT_BLOCK,RETURN_STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HASHMEM.DEF'
C
	INTEGER*4   INDEX1, INDEX2, RECORD(*), RETURN_STATUS
	INTEGER*4   NEXT_REC, NEXT_BLOCK, TIMES, BLOCK
C
C CHECK FOR VALID KEYS
C
	IF (INDEX1.EQ.0 .OR. INDEX2.GT.15 .OR. INDEX2.LT.0) THEN
	    RETURN_STATUS = HASH_RETURN_INVALID_KEY
	    GOTO 9000
	ENDIF
	RETURN_STATUS = HASH_RETURN_OK
C
C IF COMBINATION OF 3 RETRIEVE RECORD FROM DIRECT ACCESS TABLE
C
	IF(INDEX2.LE.3.AND.INDEX2.GT.0)THEN
	   RECORD(1)=HASH_DIR(INDEX1)    !CHANGED TO (1)
	   RETURN
	ENDIF
C
C GET MEMORY BLOCK TO START SEARCH WITH
C
	TIMES   = 1
	CALL HASHBLOCK(INDEX1,BLOCK,TIMES)
	NEXT_BLOCK=BLOCK
C
10	CONTINUE
	DO 100, NEXT_REC = 1,HASH_RECORDS
	   IF (HASH_TAB(1,NEXT_REC,NEXT_BLOCK) .EQ. 0) THEN
C
C RECORD NOT FOUND
C
	   RECORD(1) = 0
	   IF (HASH_RECORD_SIZE .GT. 1)
     *	       CALL FASTSET(0,RECORD,HASH_RECORD_SIZE)
	       RETURN_STATUS = HASH_RETURN_NOT_FOUND
	       GOTO 9000
	   ELSEIF(HASH_TAB(1,NEXT_REC,NEXT_BLOCK) .EQ. INDEX1) THEN
	       IF(IAND(HASH_TAB(2,NEXT_REC,NEXT_BLOCK),15) .EQ.
     *	          INDEX2) THEN
C
C RECORD FOUND
C
	          IF(HASH_RECORD_SIZE .GT. 1)
     *	             CALL FASTMOV(HASH_TAB(2,NEXT_REC,NEXT_BLOCK),
     *	             RECORD, HASH_RECORD_SIZE)
	             RECORD(1) = HASH_TAB(2,NEXT_REC,NEXT_BLOCK)/16
	             GOTO 9000
	       ENDIF
	   ENDIF
100	CONTINUE
C
	TIMES = TIMES+1
	IF (TIMES .GT. HASH_MAX_SEARCH) THEN
C
C SEARCH TOO LONG, RETURN WITH ERROR
C
	    RETURN_STATUS = HASH_RETURN_TOO_MANY
	    GOTO 9000
	ENDIF
C
C GET NEXT BLOCK FOR SEARCH
C
	CALL HASHBLOCK(INDEX1,NEXT_BLOCK,TIMES)
C
	GOTO 10
C
9000	CONTINUE
	RETURN
	END
