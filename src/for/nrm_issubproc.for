C
C ISSUBPROC - THIS FUNCTION RETURNS -
C                    .TRUE.  - IF THE CALLING PROCESS IS SUBPROCESS, 
C		     .FALSE. - OTHERWISE.
C
C V01 10-NOV-97 UXN INITIAL RELEASE.
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C=======OPTIONS/CHECK=NOOVERFLOW
	LOGICAL*4 FUNCTION ISSUBPROC
	IMPLICIT NONE
C
	INCLUDE	    '($SYSSRVNAM)'
	INCLUDE	    '($JPIDEF)'
C
	INTEGER*4   OWNERID
C
	STRUCTURE   /JPISTRUC/
	    INTEGER*2	BUFLEN
	    INTEGER*2	ITMCOD
	    INTEGER*4	BUFADR
	    INTEGER*4	LENADR
	END STRUCTURE
C
	RECORD	    /JPISTRUC/ ITEMLIST(2)
C
	INTEGER*4   RETURN_LEN
C
	INTEGER*4   ST
C
	ISSUBPROC = .FALSE.
C
C Get the PID of my MASTER
C
	ITEMLIST(1).BUFLEN = 4
	ITEMLIST(1).ITMCOD = JPI$_OWNER
	ITEMLIST(1).BUFADR = %LOC(OWNERID)
	ITEMLIST(1).LENADR = %LOC(RETURN_LEN)
C
	ITEMLIST(2).BUFLEN = 0			!TO TERMINATE LIST
	ITEMLIST(2).ITMCOD = 0			!TO TERMINATE LIST
C
	ST = SYS$GETJPIW( ,%VAL(0),,ITEMLIST(1),,,)
	IF(.NOT.ST)THEN
	  CALL LIB$SIGNAL(%VAL(ST))
	  GOTO 10000				!NORMAL PROMPT
	ENDIF
C
	ISSUBPROC = OWNERID .NE. 0
C
10000	CONTINUE
C
	END
