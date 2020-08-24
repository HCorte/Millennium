C
C SUBROUTINE LNMCRE
C
C V03 11-Nov-1999 RXK Changed for ALPHA (UXN)
C V02 21-Jan-1993 DAB Initial Release Based on Netherlands Bible, 12/92, and
C                     Comm 1/93 update DEC Baseline
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C ** Source - nrm_lnmsubs.for **
C
C LNMSUBS.FOR
C
C
C VAX LOGICAL NAMES MAINTANANCE ROUTINES:
C LNMCRE - CREATED LOGICAL NAME
C LNMTRN - TRANSLATE LOGICAL NAME
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
C******************************************************************
C
C CREATE LOGICAL NAME
C
C*****************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE LNMCRE (LOGICAL_NAME, TRANSLATE_NAME,
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
	INTEGER*4	IANYL
	EXTERNAL	IANYL
	INTEGER*4   TNL		! LENGTH OF THE 'TRANSLATE_NAME'
C
	INTEGER*4	I4ITEM(7)
	INTEGER*2	I2ITEM(14)
	EQUIVALENCE	(I4ITEM,I2ITEM)
	INTEGER*4	ATTRIBUTES/0/, DUMMY, ST
C
	CHARACTER*80	LNAM, TRNAM, TABNAM
	INTEGER*4	TABNAMLEN,LNAMLEN,TRNAMLEN
C
C	SET LENGTH OF ARGUMENTS
C
	CALL STR$TRIM(TRNAM, TRANSLATE_NAME, TRNAMLEN)
	CALL STR$TRIM(TABNAM, TABLE_NAME, TABNAMLEN)
	CALL STR$TRIM(LNAM, LOGICAL_NAME, LNAMLEN)
C
C	SET PARAMETERS FOR 'CRELNM' CALL
C
C***	ATTRIBUTES = IOR(LNM$M_CONCEALED, LNM$M_TERMINAL)
C
	I2ITEM(1) = 4
	I2ITEM(2) = LNM$_ATTRIBUTES
	I4ITEM(2) = %LOC(ATTRIBUTES)
	I4ITEM(3) = %LOC(DUMMY)
C
	I2ITEM(7) = 80
	I2ITEM(8) = LNM$_STRING
	I4ITEM(5) = %LOC(TRNAM)
	I4ITEM(6) = %LOC(TNL)
	I4ITEM(7) = 0		    !TO TERMINATE LIST
 
	ST = SYS$CRELNM (0, TABNAM(1:TABNAMLEN),
     *			  LNAM(1:LNAMLEN),, I4ITEM)
	IF (.NOT. ST) THEN
D	    TYPE *, 'VAX_LNMCRE: FAILED FOR ', LNAM
D	    CALL LIB$SIGNAL(%VAL(ST))
	    STATUS = 1
	    RETURN
	ENDIF
	STATUS = 0
	RETURN
	END
