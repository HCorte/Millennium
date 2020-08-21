C 
C NRM_LNMTRNI4.FOR
C
C This subroutine translates the logical name to integer value
C and returns 0, if successed.
C
C V01 28-FEB-98 UXN Initial release.
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
	SUBROUTINE LNMTRNI4(LOGICAL_NAME, VALUE, STATUS)
	IMPLICIT NONE
	INCLUDE '($LNMDEF)'
	INCLUDE '($SYSSRVNAM)'
C
	CHARACTER*(*)	LOGICAL_NAME
	INTEGER*4	VALUE,STATUS
	INTEGER*4	MAX_LEN,LENGTH
	PARAMETER	(MAX_LEN=80)
	CHARACTER*80	STRING
C
	STRUCTURE /ITEMS/
	    INTEGER*2	LENGTH
	    INTEGER*2	ITEMCODE
	    INTEGER*4	BUFFER
	    INTEGER*4	ACTUAL
	END STRUCTURE
C
	RECORD /ITEMS/ ITEM(2)
C
	ITEM(1).ITEMCODE = LNM$_STRING
	ITEM(1).LENGTH   = MAX_LEN
	ITEM(1).BUFFER   = %LOC(STRING)
	ITEM(1).ACTUAL   = %LOC(LENGTH)
	ITEM(2).ITEMCODE = 0
	ITEM(2).LENGTH   = 0
C
	STATUS = SYS$TRNLNM(LNM$M_CASE_BLIND,
     *                      'LNM$FILE_DEV', 
     *                      LOGICAL_NAME,,
     *                      ITEM)
	IF(.NOT.STATUS) RETURN	
C
	STATUS = -1
	IF(LENGTH.LE.0) RETURN
C
	STRING = STRING(:LENGTH)
	STATUS = 0
	CALL OTS$CVT_TI_L(STRING,VALUE,%VAL(4),%VAL(1))
	END
