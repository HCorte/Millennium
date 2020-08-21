C
C SUBROUTINE MOVTAB
C
C V03 04-JUL-2000 UXN Code optimized (OTS$MOVE3 added)
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 16-JUL-1990 TKO RELEASED FOR VAX
C
C This emulates the MOVTAB.MAC routine on Concurrent (in FAST1.MAC)
C
C  I don't know why walter wrote it, it seems to do the same as FASTMOV
C
C CALL MOVTAB(INARY,OUARY,LEN)
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
	SUBROUTINE MOVTAB(INARY,OUARY,LEN)
	IMPLICIT NONE
C
	INTEGER*4   INARY(*)
	INTEGER*4   OUARY(*)
	INTEGER*4   LEN
	INTEGER*4   XLEN
C
	XLEN = LEN*4
	CALL OTS$MOVE3(%VAL(XLEN),INARY,OUARY)
C
	RETURN
	END
