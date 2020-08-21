C
C SUBROUTINE NT_START_MESS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NT_START_MESS.FOV                            $
C  $Date::   17 Apr 1996 14:15:22                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - notpro.for;1 **
C
C
C
C *************************************************************
C NT_START_MESS
C 
C THIS ROUTINE WILL START ANOTHER READ OUTSTANDING ON THE
C MAILBOX.
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE NT_START_MESS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:NOTEVN.DEF'
	INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4 STATUS, FUNCTION
	INTEGER*4 LOCAL_IOSB(2)
C
	EXTERNAL NT_MESTRAP
C
        FUNCTION= IO$_SETMODE .OR. IO$M_WRTATTN
        STATUS=SYS$QIOW(,%VAL(NT_MESCHANNEL),%VAL(FUNCTION),
     *                  LOCAL_IOSB,,,%REF(NT_MESTRAP),
     *			%VAL(NT_MESCHANNEL),
     *                  ,,,,)
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
	RETURN
	END
