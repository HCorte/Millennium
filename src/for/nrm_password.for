C
C SUBROUTINE PASSWORD
C
C V03 04-JUL-2000 UXN SYS$OUTPUT IS LUN 6 NOT 5
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C PASSWORD.FOR
C
C V01 01-AUG-90 TKO  RELEASED FOR VAX
C
C******************************************************************************
C*
C*        ROUTINE TO READ PASSWORD WITHOUT ECHOING THE PASSWORD BACK TO
C*        THE USER
C*
C*       CALLING SEQUENCE  CALL PASSWORD(LU,PASSWRD)
C*
C*       WHERE LU IS AN INTEGER * 4 VARIABLE AND PASSWRD IS A
C*       CHARACTER * 20 VARIABLE
C*
C*      THIS ROUTINE WILL PROMPT THE USER "PLEASE ENTER PASSWORD"
C*
C*
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
	SUBROUTINE PASSWORD(LUN, PASSWRD)
	IMPLICIT NONE
C
	INCLUDE	    'INCLIB:SYSPARAM.DEF'
	INCLUDE	    '($SYSSRVNAM)'
	INCLUDE	    '($IODEF)'
	INCLUDE	    '($SSDEF)'
C
	INTEGER*4   LUN
	CHARACTER   PASSWRD*(*)
C
	INTEGER*4    READFUNCOD
	PARAMETER   (READFUNCOD = IO$_READVBLK + IO$M_NOECHO)
C
	INTEGER*4   CRTCHAN
C
	INTEGER*4   I4IOSB(4)
	INTEGER*2   I2IOSB(8)
	EQUIVALENCE (I4IOSB,I2IOSB)
C
	INTEGER*4   ST
C
C
C Assign input device (do not use lun on vax)
C
	ST = SYS$ASSIGN('SYS$INPUT', CRTCHAN,,)
	IF(.NOT.ST) CALL LIB$STOP(%VAL(ST))
C
	CALL WIMG(6,'Please enter password ')
	ST = SYS$QIOW ( , %VAL(CRTCHAN), %VAL(READFUNCOD),
     *                   I4IOSB,,,
     *                   %REF(PASSWRD), %VAL(LEN(PASSWRD)),,,,)
	ST = SYS$DASSGN( %VAL(CRTCHAN) )
C
	RETURN
	END
