C
C SUBROUTINE WRITEBRK
C
C V02 25-JUL-2000 UXN ISDETACHED() added.
C V01 15-NOV-1991 TKO Initial release
C
C This routine will write a user supplied string of characters to SYS$OUTPUT
C using QIO with a write breakthrough.
C
C Calling sequence:
C
C	CALL WRITEBRK ( STRING )
C
C Input:
C	STRING	 A character string to be written (as many characters as the
C                length of the string).
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
	SUBROUTINE WRITEBRK( STRING )
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE     'INCLIB:SYSEXTRN.DEF'
	INCLUDE	    '($SYSSRVNAM)'
	INCLUDE	    '($IODEF)'
	INCLUDE	    '($SSDEF)'
C
	CHARACTER   STRING*(*)
C
	INTEGER*4    WRITEFUNCOD
	PARAMETER   (WRITEFUNCOD = IO$_WRITEVBLK + IO$M_BREAKTHRU
     *                                           + IO$M_REFRESH)
C
	INTEGER*4   CRTCHAN
C
	INTEGER*4   I4IOSB(4)
	INTEGER*2   I2IOSB(8)
	EQUIVALENCE (I4IOSB,I2IOSB)
C
	INTEGER*4   ST
C
	IF(ISDETACHED()) THEN
	    CALL OPSTXT(STRING)
	    RETURN  
	ENDIF
C
C
C Assign input device (do not use lun on vax)
C
	ST = SYS$ASSIGN('SYS$OUTPUT', CRTCHAN,,)
	IF(.NOT.ST) THEN
	  CALL LIB$SIGNAL(%VAL(ST))
	  GOTO 9000
	ENDIF
C
	ST = SYS$QIOW ( , %VAL(CRTCHAN), %VAL(WRITEFUNCOD),
     *                   I4IOSB,,,
     *                   %REF(STRING), %VAL(LEN(STRING)),,
     *                   %VAL('00000020'X),,)
C
	ST = SYS$DASSGN( %VAL(CRTCHAN) )
C
9000	CONTINUE
	RETURN
	END
