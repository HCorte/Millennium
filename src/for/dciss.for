C  GXSRC:DCISS.FOR
C  
C V01 10-JUN-2005 FRP IPS Distribution release.
C
C SUBROUTINE TO DECODE CROSS SYSTEM PACK ISSUES MESSAGE.
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DCISS(TRABUF,OUTTAB)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
C
        INTEGER*4 IND, LENGTH, I
	BYTE      OUTTAB(*)
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
C
C GET LENGTH
C
        IND=2
        I4TEMP=0
        I1TEMP(1)=OUTTAB(IND+0)
        I1TEMP(2)=OUTTAB(IND+1)
        LENGTH=I4TEMP
C
        IF(LENGTH.NE.12+TRABUF(TINUM)*2) THEN
           TRABUF(TIERR)=INLTH
           GOTO 8000
        ENDIF
C
C GET RESULT CODE
C
        IND=14
        DO 100 I=0,TRABUF(TINUM)-1
C
           I4TEMP=0
           I1TEMP(1)=OUTTAB(IND+0)
           I1TEMP(2)=OUTTAB(IND+1)
           TRABUF(TIRES+I)=I4TEMP
           IND=IND+2
C
100     CONTINUE
C
C
8000	CONTINUE
	RETURN
	END
