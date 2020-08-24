C
C PROGRAM ELOG
C
C V05 06-AUG-2000 UXN NOTPRO renamed to ELOG
C V04 07-MAR-1994 JWE Change to temporary mailboxs
C V03 25-NOV-1991 TKO DO WRITE BREAKTHRU
C V02 09-MAY-1991 MP  ADDED CALL TO SNIF_AND_WRKSET
C V01 25-APR-1991 MRM INITIAL RELEASE.
C
C THIS PROGRAM WILL CREATE A MAILBOX AND WILL
C RECEIVE INTERRUPTS WHEN A MESSAGE IS PLACED
C INTO IT.
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
	PROGRAM ELOG
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:NOTEVN.DEF'
	INCLUDE '($SYSSRVNAM)'
	INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
C
        CHARACTER   GXEVNNAM*4              !NAME FUNCTION
C
	INTEGER*4   STATUS
	INTEGER*4   NT_EVNMASK /0/
C
	CALL COPYRITE
C
	CALL SNIF_AND_WRKSET
C
C SETUP THE MAILBOX FOR INTERTASK MESSAGES TO NOTPRO.
C IF MAILBOX DOES NOT EXIST, CREATE IT.
C
        STATUS=SYS$ASSIGN(GXEVNNAM()//NT_MESNAME,NT_MESCHANNEL,,)
        IF(.NOT.STATUS) THEN
          STATUS=SYS$CREMBX(%VAL(0),NT_MESCHANNEL,%VAL(132),,
     *                      %VAL('FD00'X),,GXEVNNAM()//NT_MESNAME)
	  IF(STATUS.NE.0) CALL LIB$SIGNAL(%VAL(STATUS))
        ENDIF
C
C START A READ OUTSTANDING.
C
	CALL NT_START_MESS
C
C WAIT FOREVER ON EVENT FLAGS.
C
	STATUS=SYS$WFLOR(%VAL(1),%VAL(NT_EVNMASK))
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
	END
