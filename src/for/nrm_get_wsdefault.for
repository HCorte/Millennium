C
C SUBROUTINE GET_WSDEFAULT
C
C V01 30-JUL-1999 UXN INITIAL RELEASE.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C GET_WSDEFAULT(WSDEFAULT) - RETURNS DEFAULT WORKING SET 
C
C-----------------------------------------------------------------------
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS/CHECK=NOOVERFLOW
	SUBROUTINE GET_WSDEFAULT(WSDEFAULT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE	    '($SYSSRVNAM)'
	INCLUDE	    '($JPIDEF)'
C
	INTEGER*4   WSDEFAULT
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
C
C
C Get the PID of my MASTER
C
	ITEMLIST(1).BUFLEN = 4
	ITEMLIST(1).ITMCOD = JPI$_DFWSCNT
	ITEMLIST(1).BUFADR = %LOC(WSDEFAULT)
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
D	TYPE *,IAM(),'DEFAULT WORKING SET= ',WSDEFAULT
10000	CONTINUE
C
	RETURN
	END
