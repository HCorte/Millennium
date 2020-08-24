C
C SUBROUTINE MOVBYT
C
C V03 04-JUL-2000 UXN LIB$MOVC3 replaced with OTS$MOVE3 to be able to move
C                     more than 64k
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 16-JUL-1990 TKO RELEASED FOR VAX
C
C This emulates the MOVBYT.MAC routine on Concurrent
C
C CALL MOVBYT(INARY,INOFF,OUARY,OUOFF,LEN)
C
C	INARY:	BYTE ARRAY
C	INOFF:	STARTING OFFSET
C	OUARY:	BYTE ARRAY FOR OUTPUT
C	OUOFF:	STARTING OFFSET
C	LEN:	# OF BYTES TO MOVE
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
	SUBROUTINE MOVBYT(INARY,INOFF,OUARY,OUOFF,LEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	BYTE	    INARY(*)
	INTEGER*4   INOFF
	BYTE	    OUARY(*)
	INTEGER*4   OUOFF
	INTEGER*4   LEN
C
	CALL OTS$MOVE3(%VAL(LEN), INARY(INOFF), OUARY(OUOFF))
C
	RETURN
	END
