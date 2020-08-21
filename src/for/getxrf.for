C GETXRF.FOR
C
C V01 01-FEB-94 PXN INITIAL RELEASE FOR NETHERLANDS
C V01 01-JUN-93 PXN INITIAL RELEASE FOR IRELAND
C V01 13-NOV-91 JPJ RELEASED FOR VAX (INSTANTS)
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
C SUBROUTINE TO GET CROSS SYSTEM NUMBER
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETXRF(CROSS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 CROSS
C
C ASSIGN THE INCREMENT NEXT CROSS REF NUMBER
C
	CROSS=NXTIXRF
	NXTIXRF=NXTIXRF+1
C
C ADD SYSTEM OFFSET
C
	CROSS=CROSS+SYSSER
C
	RETURN
	END
