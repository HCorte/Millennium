C  GXSRC:CROACT.FOR
C  
C V01 13-JUN-FRP FRP IPS Distribution release.
C
C SUBROUTINE TO BUILD CROSS SYSTEM ORDER-PACK ACTIVATION MESSAGE.
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
	SUBROUTINE CROACT(OUTTAB,TRABUF,BUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
        INTEGER*4 IND, BUF, CROSS
	BYTE      OUTTAB(*)
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
C SET LENGTH
C
	IND=2
	I4TEMP=24
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C SET GTECH SLOT ID
C
	I4TEMP=BUF
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C SET GTECH CROSS REFERENCE NUMBER
C
	CALL GETXRF(CROSS)
	TRABUF(TIXRF)=CROSS
	I4TEMP=CROSS
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	OUTTAB(IND+2) = I1TEMP(3)
	OUTTAB(IND+3) = I1TEMP(4)
	IND=IND+4
C
C SET TRANSACTION TYPE CODE
C
	I4TEMP=37
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C EMPTY SPACE
C
	I4TEMP=0
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C SET RETAILER NUMBER
C
	I4TEMP=TRABUF(TAGT)
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	OUTTAB(IND+2) = I1TEMP(3)
	OUTTAB(IND+3) = I1TEMP(4)
	IND=IND+4
C
C EMPTY SPACE
C
	I4TEMP=0
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C Invoice number (first 4 digits)
C
	I4TEMP=TRABUF(TOINV2)
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C Invoice number (last 9 digits)
C
	I4TEMP=TRABUF(TOINV3)
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	OUTTAB(IND+2) = I1TEMP(3)
	OUTTAB(IND+3) = I1TEMP(4)
	IND=IND+4
C
	RETURN
	END
