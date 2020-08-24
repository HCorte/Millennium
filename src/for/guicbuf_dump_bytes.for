C GUICBUF_DUMP_BYTES
C
C V02 31-OCT-2000 UXN GUI prefix added.
C V01 23-JUN-1993 MP  INITIAL RELEASE FOR VAX
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C INPUT:
C	DBG_UNIT - debugging logical unit nr
C	TEXT	 - text to be outputed
C	BUF	 - byte buffer array
C	BOFF	 - buffer offset
C	LEN	 - length to output
C OUTPUT:
C	none
C RESULT:
C	data dumped to the device (file) identified by DBG_UNIT
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUICBUF_DUMP_BYTES(DBG_UNIT, TEXT, BUF, BOFF, LEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4	DBG_UNIT
	CHARACTER*(*)	TEXT
	BYTE		BUF(*)
	INTEGER*4	BOFF, LEN
	INTEGER*4	J
C
	DO 100 J=1,LEN
	    WRITE(DBG_UNIT, 9010) IAM(),TEXT,' at offset ',J-1,': ',
     *	       ZEXT(BUF(BOFF-1+J))
9010	    FORMAT(1X,A,A30,A,I3,A,I3.3)
100	CONTINUE
C
	WRITE(DBG_UNIT,*)
	BOFF=BOFF+LEN
C
	RETURN
	END
