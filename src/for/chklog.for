C  GXSRC:SPE_CHKLOG.FOR
C  
C V03 05-OCT-2000 UXN AlphaIPS release.
C V02 14-OCT-1992 GAP INITIAL RELEASE FOR TEXAS
C V01 04-MAY-1992 GAP RELEASED FOR WEST VIRGINIA 
C
C SUBROUTINE TO DECODE CROSS SYSTEM CHECKWRITER MESSAGE.
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
	SUBROUTINE CHKLOG(TRABUF,OUTTAB)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
        INTEGER*4 IND
	BYTE      OUTTAB(*)
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
C GET CROSS REFERENCE NUMBER OF VALIDATION
C
	IND=6
	I4TEMP=0
	I1TEMP(1)=OUTTAB(IND+0)
	I1TEMP(2)=OUTTAB(IND+1)
	I1TEMP(3)=OUTTAB(IND+2)
	I1TEMP(4)=OUTTAB(IND+3)
	TRABUF(TSOLD)= I4TEMP-CHKWRTOFF
C
C CHECK STATUS
C
	IND=14
	I4TEMP=0
	I1TEMP(1)=OUTTAB(IND+0)
	I1TEMP(2)=OUTTAB(IND+1)
	IF(I4TEMP.NE.99) THEN 
	    TRABUF(TERR)=BCRS
	    TRABUF(TSTAT)=REJT
	    TRABUF(TSNEW)=I4TEMP
	ENDIF
C
8000	CONTINUE
	RETURN
	END
