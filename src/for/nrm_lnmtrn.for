C
C SUBROUTINE LNMTRN
C
C V02 11-Nov-1999 RXK Changed for ALPHA (UXN)
C V01 21-Jan-1993 DAB Initial Release Based on Netherlands Bible, 12/92, and
C                     Comm 1/93 update DEC Baseline
C
C ** Source - nrm_lnmsubs.for **
C
C******************************************************************
C
C TRANSLATE LOGICAL NAME
C
C*****************************************************************
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
	SUBROUTINE LNMTRN (LOGICAL_NAME, TRANSLATE_NAME,
     *			  TABLE_NAME, STATUS)
	IMPLICIT  NONE
C
	CHARACTER*(*) LOGICAL_NAME	! INPUT
	CHARACTER*(*) TRANSLATE_NAME	! INPUT
	CHARACTER*(*) TABLE_NAME	! INPUT
	INTEGER*4 STATUS		! OUTPUT
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    '($SYSSRVNAM)'
	INCLUDE	    '($LNMDEF)'
C
	INTEGER*4   TNL		! LENGTH OF THE 'TRANSLATE_NAME'
C
	INTEGER*4	I4ITEM(7)
	INTEGER*2	I2ITEM(6)
	EQUIVALENCE	(I4ITEM,I2ITEM)
	INTEGER*4	ST
	CHARACTER*80	LNAM, TRNAM, TABNAM
	INTEGER*4	TABLEN,LNAMLEN
C
C	SET PARAMETERS FOR 'TRNLNM' CALL
C
	I2ITEM(1) = 80
	I2ITEM(2) = LNM$_STRING
	I4ITEM(2) = %LOC(TRNAM)
	I4ITEM(3) = %LOC(TNL)
	I4ITEM(4) = 0		    !TO TERMINATE LIST

 	CALL STR$TRIM(TABNAM,TABLE_NAME,TABLEN)
 	CALL STR$TRIM(LNAM,LOGICAL_NAME,LNAMLEN)
	TRNAM = ' '

	ST = SYS$TRNLNM (0,TABNAM(1:TABLEN), LNAM(1:LNAMLEN),, I4ITEM)
	IF (.NOT. ST) THEN
D	    TYPE *, 'VAX_LNMTRN: FAILED FOR ', LNAME,' TAB: ', TABNAM
D	    CALL LIB$SIGNAL(%VAL(ST))
	    STATUS = 1
	    RETURN
	ENDIF
	TRANSLATE_NAME = TRNAM
	STATUS = 0
	RETURN
	END
