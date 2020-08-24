C
C SUBROUTINE LOGMES
C
C V02 25-JUL-2000 UXN MODE 5 ADDED.
C V01 XX-XXX-XXXX XXX INITIAL RELEASE
C
C SUBROUTINE TO PRINT/LOG MESSAGES
C
C MODE - 1  LOG TO CONSOLE ONLY
C MODE - 2  LOG TO DISK ONLY
C MODE - 3  LOG TO DISK AND CONSOLE
C MODE - 4  LOG TO DISK AND CONSOLE
C MODE - 5  SEND MESSAGE TO NOTPRO
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
	SUBROUTINE LOGMES(EBUF,ALARM)
	IMPLICIT NONE
C
	INTEGER*4 EBUF(40)	!ERRLOG MESSAGE BUFFER
	LOGICAL ALARM		!ALARM LOGICAL
C
	CHARACTER*1  BELL/'07'X/
C
	CHARACTER*80 OUTLIN
	INTEGER*4    I4OUTLIN(20)
	EQUIVALENCE (OUTLIN,I4OUTLIN)
	CHARACTER*132 MSG
	INTEGER*4     ST
C
C SEND TO NOTPRO
C
	CALL FASTMOV(EBUF, I4OUTLIN, 20)

	IF(ALARM) THEN    
	     MSG = OUTLIN//BELL//BELL//BELL//BELL
	ELSE
	     MSG = OUTLIN
	ENDIF

	CALL MLOG(MSG,ST)
	RETURN
	END
