C
C V05 24-JAN-2011 RXK Statement RETURN removed. 
C V01 21-MAR-2001 UXN Initial release.
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
	PROGRAM SAY
	IMPLICIT NONE
	INCLUDE '(LIB$ROUTINES)'

	INTEGER*4 ST
	CHARACTER CMDLIN*80
        INTEGER*4 CMDLEN
C
	ST = LIB$GET_FOREIGN(CMDLIN,,CMDLEN,)
        IF(.NOT.ST .OR. CMDLEN.LT.1) GOTO 100
C
	CALL OPSTXT( CMDLIN(1:CMDLEN) )

100     CONTINUE
	END
