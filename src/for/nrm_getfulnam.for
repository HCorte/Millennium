C
C SUBROUTINE GETFULNAM
C
C V02 08-FEB-2000 UXN Alpha changes.
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C
C **** GETFULNAM
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETFULNAM (I4FULNAM)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    '($SYSSRVNAM)'
	INCLUDE	    '($JPIDEF)'
C
	INTEGER*4	I4FULNAM(4)
C
	INTEGER*4	I4MYNAME(4)
	CHARACTER*15	CXMYNAME
	EQUIVALENCE	(I4MYNAME,CXMYNAME)
	VOLATILE        CXMYNAME,I4MYNAME
C
	INTEGER*4	NAMLEN
C
	INTEGER*4	I4ITEM(4)
	INTEGER*2	I2ITEM(6)
	EQUIVALENCE	(I4ITEM,I2ITEM)
C
	INTEGER*4	ST
C
C
C
	I2ITEM(1) = 15
	I2ITEM(2) = JPI$_PRCNAM
	I4ITEM(2) = %LOC(CXMYNAME)
	I4ITEM(3) = %LOC(NAMLEN)
	I4ITEM(4) = 0		    !TO TERMINATE LIST
C
C	CLEAR PROCESS NAME IN 'SYSPARAMS'
C
C
	CXMYNAME = ' '
	ST = SYS$GETJPIW( ,%VAL(0),,I4ITEM,,,)
	IF(.NOT.ST)THEN
	  CALL LIB$SIGNAL(%VAL(ST))
	  GOTO 9000
	ENDIF
C
C FILL OUT THE NAME WITH BLANKS
C
	CXMYNAME = CXMYNAME(1: NAMLEN)
C
C
9000	CONTINUE
	I4FULNAM(1) = I4MYNAME(1)
	I4FULNAM(2) = I4MYNAME(2)
	I4FULNAM(3) = I4MYNAME(3)
	I4FULNAM(4) = I4MYNAME(4)
C
	RETURN
	END
