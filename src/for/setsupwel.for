C
C PROGRAM SETSUPWEL
C
C V01 06-FEB-2014 FRPO INITIAL RELEASE
C
C This will set P(SUPWEL) on/off to any Terminal or Line number
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM SETSUPWEL
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 ST, ANS, DEBVAL
C
	CHARACTER*3 ONOFF(0:1)/'OFF','ON '/
C
	CALL COPYRITE
C
	DEBVAL=P(SUPWEL)
C
	TYPE*,IAM()
	TYPE*,IAM(),'Debug is set       ', ONOFF(DEBVAL)
C
	TYPE*,IAM()
	CALL INPYESNO('Do you want to turn debugging '//ONOFF(1-DEBVAL)//'     [Y/N]',ANS)
	IF(ANS.EQ.1) DEBVAL=1-DEBVAL
	P(SUPWEL)=DEBVAL
C
	TYPE*,IAM()
	TYPE*,IAM(),'Debug set to       ', ONOFF(DEBVAL)
	TYPE*,IAM()
C
	END
