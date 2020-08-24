C
C SUBROUTINE ASCBIN
C $Log:   GXAFXT:[GOLS]ASCBIN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:13:04   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:39:54   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_ascbin.for **
C
C VAX_ASCBIN.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C ASCBIN.FOR
C
C V01 16-JUL-90 TKO  RELEASED FOR VAX
C
C This emulates the ASCBIN.MAC routine on Concurrent
C
C CALL ASCBIN(BYTARY,OFF,LEN,NUM,ST)
C
C	***CONVERT FROM ASCII TO UNSIGNED BINARY
C
C 	BYTARY  = ARRAY OF CHARACTERS (***NOT A CHARACTER STRING***)
C	OFF     = BEGINNING OFFSET IN THE ARRAY
C	LEN	= # OF BYTES TO CONVERT
C
C	NUM	= I4 NUMBER CONVERTED
C	ST	= 0 -> OK, ELSE ERROR
C
C CALL BINASC(BYTARY,OFF,LEN,NUM)
C
C	***CONVERT FROM UNSIGNED BINARY TO ASCII***
C
C	(CONVERT NUM INTO I4ARRAY STARTING AT OFF FOR LEN BYTES)
C
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE ASCBIN(BYTARY,OFF,LEN,NUM,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	BYTE		BYTARY(*)
	INTEGER*4	OFF
	INTEGER*4	LEN
	INTEGER*4	NUM
	INTEGER*4	ST
C
	INTEGER*4       TEMP
	INTEGER*4	X
	INTEGER*4       K
C
C
	TEMP = 0
	DO 1100 K = OFF, OFF+LEN-1
	  X = BYTARY(K) - ICHAR('0')
	  IF(X.LT.0 .OR. X.GT.9)GOTO 7000
	  IF(TEMP.GT.214748364)GOTO 7000    !214748364 = '7FFFFFFF'X/10
	  TEMP = TEMP*10
	  IF(TEMP.GT.'7FFFFFFF'X - X)GOTO 7000
	  TEMP = TEMP+X
1100	CONTINUE
C
	NUM = TEMP
	ST = 0
	GOTO 9000
C
7000	CONTINUE
	ST = -1
	GOTO 9000
C
9000	CONTINUE
	RETURN
	END
