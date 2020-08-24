C *** SUBROUTINE CHKREPLY ***
C
C V02 19-JUN-2000 OXK Cleanup w/ WARNINGS=ALL
C V01 15-FEB-1991 RRB VAX INITIAL RELEASE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 2000 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	DECODE MESSAGES RECEIVED FROM THE TELENEX MATRIX SWITCH CONTROLLER.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CHKREPLY(MESS,MSGNUM,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:MSCCOM.DEF'
	INCLUDE 'INCLIB:MSCMSGS.DEF'
C
	CHARACTER*(*) MESS
	INTEGER*4     STATUS, MESSAGE, IND
        INTEGER*4     MSGNUM

C
C SCAN MESSAGE LIST FOR MATCH
C
	IND=0
	MSGNUM = 0
	DO 100 MESSAGE = 1,MSC_NUM_MSGS
	   IND=INDEX(MESS,MSC_MSGS(MESSAGE)(1:MSC_MSG_LEN(MESSAGE)))
	   IF(IND.GT.0) THEN                !FOUND MESSAGE
              MSGNUM = MESSAGE
	      GOTO 200
	   ENDIF
100     CONTINUE
C
200	CONTINUE
	RETURN
	END
