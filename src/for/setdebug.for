C
C PROGRAM SETDEBUG
C
C V03 03-OCT-2000 UXN USER INTERFACE IMROVED.
C V02 01-AUG-1990 XXX RELEASED FOR VAX
C V01 10-APR-1989 TKO INITIAL RELEASE
C
C This will set P(XXDEBUG) on/off and set P(XXDTRLN) to any
C Terminal or Line number
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
	PROGRAM SETDEBUG
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 ST, ANS, DTLVAL, DEBVAL
C
	CHARACTER*3 ONOFF(0:1)/'ON ','OFF'/
C
	CALL COPYRITE
C
	DEBVAL=P(XXDEBUG)
	DTLVAL=P(XXDTRLN)
C
	TYPE*,IAM()
	TYPE*,IAM(),'Debug is set       ', ONOFF(DEBVAL)

	IF(DEBVAL.EQ.0) THEN
	   IF(DTLVAL.EQ.0) TYPE*,IAM(),'Debug all terminals '
	   IF(DTLVAL.LT.0) TYPE*,IAM(),'Debug line ',ABS(DTLVAL)
	   IF(DTLVAL.GT.0) TYPE*,IAM(),'Debug ter ',DTLVAL
	ENDIF
	TYPE*,IAM()
	CALL INPYESNO('Do you want to turn debugging '//ONOFF(1-DEBVAL)//
     *                '     [Y/N]',ANS)
	IF(ANS.EQ.1) DEBVAL = 1 - DEBVAL
	IF(DEBVAL.EQ.0) THEN
	      CALL INPNUM('Enter ter/line value (-line : +ter : 0=all)',
     *	                  DTLVAL,-9999,9999,ST)
	      IF(ST.NE.0) DTLVAL = P(XXDTRLN)
	ENDIF
	P(XXDEBUG)=DEBVAL
	P(XXDTRLN)=DTLVAL
	END
