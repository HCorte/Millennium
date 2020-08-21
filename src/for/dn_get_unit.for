C
C SUBROUTINE DN_GET_UNIT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_GET_UNIT.FOV                              $
C  $Date::   17 Apr 1996 12:58:08                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - dn_init.for;1 **
C
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	SUBROUTINE DN_GET_UNIT (CHAN, UNIT)
	IMPLICIT NONE
C  DN_GET_UNIT
C  John A Harney                                             17-Apr-1991
C
C  Subroutine to return the unit number associated with a channel
C
C NOTE: If you change this change the AST version at the follows.
C
C  Calling sequence:
C	CALL DN_GET_UNIT (CHAN, UNIT)
C		CHAN - word by reference - read only
C		UNIT - longword by reference - write only


	INCLUDE '($SYSSRVNAM)'
	INCLUDE '($DVIDEF)'

	INTEGER*2 CHAN
	INTEGER*4 UNIT, DUMMY, STAT

	STRUCTURE /ITEM_LIST_3/
		INTEGER*2 BUFFER_LENGTH
		INTEGER*2 ITEM_CODE
		INTEGER*4 BUFFER_ADDRESS
		INTEGER*4 RETURN_ADDRESS
	END STRUCTURE

	RECORD /ITEM_LIST_3/ ITMLST(2)

	ITMLST(1).ITEM_CODE = DVI$_UNIT
		ITMLST(1).BUFFER_LENGTH = 4
		ITMLST(1).BUFFER_ADDRESS = %LOC(UNIT)
		ITMLST(1).RETURN_ADDRESS = %LOC(DUMMY)
	ITMLST(2).ITEM_CODE = 0
	ITMLST(2).BUFFER_LENGTH = 0

	STAT = SYS$GETDVI (,%VAL(CHAN),,ITMLST,,,,)
	IF (.NOT.STAT) CALL EXIT(STAT)

	RETURN
	END
