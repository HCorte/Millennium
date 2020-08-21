C  GXSRC:DCLOT.FOR
C
C V04 05-OCT-2000 UXN AlphaIPS release.
C V03 05-DEC-1996 HXK Updated for Finland IPS
C V02 11-FEB-1992 JPJ ADDED (GVT)
C V01 13-NOV-1991 JPJ RELEASED FOR VAX (INSTANTS)
C
C SUBROUTINE TO DECODE CROSS SYSTEM LOT MESSAGE.
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
	SUBROUTINE DCLOT(TRABUF,OUTTAB)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
        INTEGER*4 IND, LENGTH, LOTTYP
	BYTE      OUTTAB(*)
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
C GET LENGTH
C
	IND=2
	I4TEMP=0
	I1TEMP(1)=OUTTAB(IND+0)
	I1TEMP(2)=OUTTAB(IND+1)
	LENGTH=I4TEMP
C
C GET H->IPS->H TRANSACTION TYPE
C
	IND=10
	I4TEMP=0
	I1TEMP(1)=OUTTAB(IND+0)
	I1TEMP(2)=OUTTAB(IND+1)
	LOTTYP=I4TEMP
C
C CHECK ERROR CODE
C
        IND=14
	I4TEMP=0
	I1TEMP(1)=OUTTAB(IND+0)
	I1TEMP(2)=OUTTAB(IND+1)
	TRABUF(TIERR)=I4TEMP
        IND=IND+2
	IF(TRABUF(TIERR).GT.INOER) GOTO 8000
	IF(LENGTH.NE.22) THEN
	   TRABUF(TIERR)=INLTH
	   GOTO 8000
	ENDIF
C
C GET AMOUNT SOLD
C
	I4TEMP=0
	I1TEMP(1)=OUTTAB(IND+0)
	I1TEMP(2)=OUTTAB(IND+1)
	I1TEMP(3)=OUTTAB(IND+2)
	I1TEMP(4)=OUTTAB(IND+3)
	TRABUF(TLAMT)=I4TEMP
	IND=IND+4
C
C GET COMMISION AMOUNT
C
	I4TEMP=0
	I1TEMP(1)=OUTTAB(IND+0)
	I1TEMP(2)=OUTTAB(IND+1)
	I1TEMP(3)=OUTTAB(IND+2)
	I1TEMP(4)=OUTTAB(IND+3)
	TRABUF(TLCOM)=I4TEMP
	IND=IND+4
C
8000	CONTINUE
	RETURN
	END
