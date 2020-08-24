C
C V01 07-FEB-2000 UXN Initial revision.
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
        SUBROUTINE ADD_PENNY(PENNY, AMT, NFRAC)
        IMPLICIT NONE
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INTEGER*4 PENNY, AMT, NFRAC
	INTEGER*4 M
	
	AMT = AMT*DYN_BETUNIT
	AMT = AMT / NFRAC
	M = MOD(AMT,DYN_BETUNIT)
	IF(M.GT.0) PENNY = PENNY + M
	AMT = AMT / DYN_BETUNIT
	IF(PENNY.GE.DYN_BETUNIT) THEN
	    AMT = AMT + PENNY/DYN_BETUNIT
	    PENNY = MOD(PENNY,DYN_BETUNIT)
	ENDIF

	END
