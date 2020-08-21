C
C SUBROUTINE DSPEI
C V01 06-Jun-97 UXN  Initial release for Finland
C
C SUBROUTINE TO DECODE TEBE INQUIRY INPUT MESSAGE
C
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DSPEI(MESTAB,TRABUF,MESLEN,TCODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
	BYTE	    MESTAB(*)
	INTEGER*4   TEMP, TEMP1, TEMP2
	INTEGER*4   MESS(EDLEN)
	INTEGER*4   ENCMES, ENCACT, MYCHKSUM, CHKLEN, TYP, SUBTYP
	INTEGER*2   MESLEN,TCODE
C
	INTEGER*4   JUL			!Julian date for Unfraction transaction.
	INTEGER*4   SER			!Serial# for Unfraction transaction.
	INTEGER*4   CDIG		!Check digit for Unfraction transaction.
	INTEGER*4   DUMMY 		!Dummy variable.
	INTEGER*4   CDC			!CDC date for Unfraction transaction.
	INTEGER*4   CERR		!Serial# conversion error.
	INTEGER*4   WAGSER		!Wager Serial# returned for Unfrac trans
	INTEGER*4   INPVER		!INPVER Function.
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I1TEMP,I2TEMP,I4TEMP)
C
C GET TRANSACTION NUMBER
C
	TEMP = ZEXT(MESTAB(1))
	TRABUF(TTRN) = IAND(TEMP,15)
C
C GET TYPE / SUBTYPE
C
	TEMP = ZEXT(MESTAB(2))
	SUBTYP = IAND(TEMP,15)
	TYP = ISHFT(TEMP,-4)
C
C GET CHECKSUM
C
	TEMP1 = ZEXT(MESTAB(3))
	TEMP2 = ZEXT(MESTAB(4))
	TRABUF(TCHK) = ISHFT(TEMP1,8) + TEMP2
C
C
	IF(TYP.EQ.8.AND.SUBTYP.EQ.7)	THEN
	    TRABUF(TSFUN) = TREPR
	ELSE
	    TRABUF(TERR)  = SYNT
	    GOTO 100
	ENDIF
C
C bank id
C
	I1TEMP(4) = ZEXT(MESTAB(5))
	I1TEMP(3) = ZEXT(MESTAB(6))
	I1TEMP(2) = ZEXT(MESTAB(7))
	I1TEMP(1) = ZEXT(MESTAB(8))
	TRABUF(TSDT1) = I4TEMP
C
C bank account
C 
	I1TEMP(4) = ZEXT(MESTAB(9))
	I1TEMP(3) = ZEXT(MESTAB(10))
	I1TEMP(2) = ZEXT(MESTAB(11))
	I1TEMP(1) = ZEXT(MESTAB(12))
	TRABUF(TSDT2) = I4TEMP

C
C search start time
C
	I1TEMP(4) = ZEXT(MESTAB(13))
	I1TEMP(3) = ZEXT(MESTAB(14))
	I1TEMP(2) = ZEXT(MESTAB(15))
	I1TEMP(1) = ZEXT(MESTAB(16))
	TRABUF(TSDT3) = I4TEMP

C
C search end time
C
	I1TEMP(4) = ZEXT(MESTAB(17))
	I1TEMP(3) = ZEXT(MESTAB(18))
	I1TEMP(2) = ZEXT(MESTAB(19))
	I1TEMP(1) = ZEXT(MESTAB(20))
	TRABUF(TSDT4) = I4TEMP
C
100	CONTINUE
	IF(TRABUF(TSFUN).EQ.0) TRABUF(TERR)=SYNT
	IF(TRABUF(TERR).NE.NOER) TRABUF(TSTAT)=REJT
	RETURN
C
	END
