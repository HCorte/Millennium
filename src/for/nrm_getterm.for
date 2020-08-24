C
C SUBROUTINE GETTERM
C
C V04 08-FEB-2000 UXN Alpha changes.
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 26-MAR-1991 TKO TERMINAL NAME RETURNED IS 8 CHARACTERS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C GETTERM.FOR
C
C
C RETURNS TERMINAL DEVICE NAME OF THE LOGIN (!!!) PROCESS
C BY FINDING OWNER(S) OF THE CURRENT PROCESS UNTIL IT FINDS
C THE ONE THAT HAS A TERMINAL
C
C ****NOTE**** GETJPI returns a 7 byte value indicating terminal name.
C If the actual name is less than 7 characters, it puts a colon on the
C end.  If equal to 7 charactes, it does not put the colon at the end which
C means we have to add it ourselves.
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
	SUBROUTINE GETTERM (TNAME, STATUS)
	IMPLICIT  NONE
C
	CHARACTER*8 TNAME	! OUTPUT
	INTEGER*4 STATUS	! OUTPUT
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    '($SYSSRVNAM)'
	INCLUDE	    '($JPIDEF)'
C
	CHARACTER*7 TN		! LOCAL SPACE FOR TERMINAL NAME
	BYTE	    CTN(7)
	EQUIVALENCE (CTN(1), TN)
	VOLATILE CTN
C
	INTEGER*4   TNL		! LENGTH OF THE 'TN'
C
	INTEGER*4	I4ITEM(4)
	INTEGER*2	I2ITEM(6)
	EQUIVALENCE	(I4ITEM,I2ITEM)
C
	INTEGER*4	NDX
	INTEGER*4	ST, SONPID, DADPID
C
C	SET PARAMETERS FOR 'GETJPI' CALL
C
	I2ITEM(1) = 7
	I2ITEM(2) = JPI$_TERMINAL
	I4ITEM(2) = %LOC(TN)
	I4ITEM(3) = %LOC(TNL)
	I4ITEM(4) = 0		    !TO TERMINATE LIST
C
C	CLEAR THE NAME
C
	CTN(1) = 0
C
C	START WITH PID 0 - SELF
C
	SONPID = 0
	ST = SYS$GETJPIW( ,%VAL(0),,I4ITEM,,,)
C
C	DO A LOOP WHILE TERMINAL DEVICE IS FOUND
C
10	CONTINUE
	IF(.NOT.ST .OR. CTN(1) .EQ. 0 .OR. TN .EQ. '       ')THEN
	    CALL GETOWNR(SONPID, DADPID, ST)
	    IF (ST .NE. 0) THEN
		TYPE *,'FAILED TO GET DAD''S PID'
		STATUS = 1
		RETURN
	    ENDIF
	    SONPID=DADPID
	    ST = SYS$GETJPIW( ,SONPID,,I4ITEM,,,)
	    GOTO 10
	ENDIF
 
C
D	TYPE *, 'TERMINAL NAME IS ', TN
D	TYPE *, '.................'
C
	TNAME = TN
	NDX=INDEX(TNAME,':')
	IF(NDX.EQ.0)THEN
	  TNAME(8:8)=':'
	ENDIF
	STATUS = 0
C
	RETURN
	END
