C
C SUBROUTINE FASTMOV
C
C V02 06-JUL-2000 UXN OTS$MOVE3 added.
C V01 16-JUL-1990 TKO RELEASED FOR VAX
C
C This emulates the FASTMOV.MAC routine on Concurrent
C
C CALL FASTMOV(INARY,OUARY,LEN)
C
C	INARY:	I*4 ARRAY FOR INPUT
C	OUARY:	I*4 ARRAY FOR OUTPUT
C	LEN:	# OF I*4 WORDS TO MOVE
C
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
	SUBROUTINE FASTMOV(INARY,OUARY,LEN)
	IMPLICIT NONE
C
	BYTE        INARY(*)
	BYTE        OUARY(*)
	INTEGER*4   LEN
C
	INTEGER*4   XLEN
C	INTEGER*4   K
C
C
	XLEN = LEN*4
	CALL OTS$MOVE3(%VAL(XLEN), INARY, OUARY)
	RETURN
C
C	K = 1
C100	CONTINUE
C	IF(XLEN.GT.65535)THEN
C	  CALL LIB$MOVC3(65535,INARY(K),OUARY(K))
C	  XLEN = XLEN-65535
C	  K    = K + 65535
C	  GOTO 100
C	ENDIF
C
C	IF(XLEN.GT.0)THEN
C	  CALL LIB$MOVC3(XLEN, INARY(K), OUARY(K))
C	ENDIF
C
C	RETURN
	END
