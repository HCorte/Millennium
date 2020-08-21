C
C SUBROUTINE GETOWNR
C $Log:   GXAFXT:[GOLS]GETOWNR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:21:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:27:18   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_getownr.for **
C
C VAX_GETOWNR.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
	SUBROUTINE GETOWNR (SONPID, DADPID, STATUS)
	IMPLICIT  NONE
C
	INTEGER*4 SONPID
	INTEGER*4 DADPID
	INTEGER*4 STATUS
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    '($SYSSRVNAM)'
	INCLUDE	    '($JPIDEF)'
C
	INTEGER*4	PIDBUF, BUFLEN
C
	INTEGER*4	I4ITEM(4)
	INTEGER*2	I2ITEM(6)
	EQUIVALENCE	(I4ITEM,I2ITEM)
C
	INTEGER*4	ST
C
	I2ITEM(1) = 4
	I2ITEM(2) = JPI$_OWNER
	I4ITEM(2) = %LOC(PIDBUF)
	I4ITEM(3) = %LOC(BUFLEN)
	I4ITEM(4) = 0		    !TO TERMINATE LIST
C
	IF (SONPID .EQ. 0) THEN
		ST = SYS$GETJPIW( ,%VAL(0),,I4ITEM,,,)
	ELSE
		ST = SYS$GETJPIW( ,SONPID,,I4ITEM,,,)
	ENDIF
	IF(.NOT.ST)THEN
	    TYPE *, 'FAILED TO GET JPI$_OWNER'
	    CALL LIB$SIGNAL(%VAL(ST))
	    STATUS = 1
	    RETURN
	ENDIF
C
	DADPID = PIDBUF
	STATUS = 0
	RETURN
	END
