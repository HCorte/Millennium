C
C V01 03-MAR-2000 UXN Separated from MULSUBS
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
	SUBROUTINE VLTCSET(VOL,K,PRF,NAME)
	IMPLICIT NONE
C
	INTEGER * 4 VOL,K,NAME(*)
	CHARACTER * 3 PRF
	CHARACTER * 20 CTEMP
	INTEGER * 4 TEMP(5),I
	EQUIVALENCE(TEMP,CTEMP)
C
	CTEMP(1:20)=' '
	WRITE(CTEMP,1000) VOL,PRF,K
C
	DO I=1,5
	   NAME(I)=TEMP(I)
	ENDDO
C
	RETURN
1000	FORMAT(A4,':',A3,I2.2,'.FIL')
	END
