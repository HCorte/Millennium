C
C QUEBUG.FOR
C
C V01 01-JAN-2010 FJG  ePASSIVE
C
C QUE DEBUG MESSAGES TO ERRLOG
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
C Copyright 2010 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE QUEBUG(TASK,STEP,VALUE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INTEGER*4 TASK, STEP, VALUE
	INTEGER*4 MESS(EDLEN)
C
	MESS(1)=TASK
	MESS(2)=TEGEN
	MESS(3)=99
	MESS(4)=STEP
	MESS(5)=VALUE	
	CALL QUEMES(MESS)
	RETURN
	END
