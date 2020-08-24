C  GXSRC:DCCNF.FOR
C  
C V05 05-OCT-2000 UXN AlphaIPS release.
C V04 22-SEP-1994 MCM START INDEX AT 2 (BYTE 1 IS RESERVED)
C V03 08-SEP-1994 MCM SWAPPING BYTES IS NO LONGER NECESSARY ON THE DEC LMS
C V02 11-FEB-1992 JPJ ADDED (GVT)
C V01 13-NOV-1991 JPJ RELEASED FOR VAX (INSTANTS)
C
C SUBROUTINE TO DECODE CROSS SYSTEM CONFIRMATION MESSAGE.
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
	SUBROUTINE DCCNF(TRABUF,OUTTAB)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
        INTEGER*4 IND, LENGTH
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
C CHECK ERROR CODE
C
	IND=14
	I4TEMP=0
	I1TEMP(1)=OUTTAB(IND+0)
	I1TEMP(2)=OUTTAB(IND+1)
	TRABUF(TIERR)=I4TEMP
	IF(TRABUF(TIERR).GT.INOER) GOTO 8000
	IF(LENGTH.NE.14) THEN
	   TRABUF(TIERR)=INLTH
	   GOTO 8000
	ENDIF
	IND=IND+2
C
C
C
8000	CONTINUE
	RETURN
	END
