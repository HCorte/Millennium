C
C SUBROUTINE PROXGTP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]PROXGTP.FOV                                  $
C  $Date::   17 Apr 1996 14:32:50                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2exe.for;1 **
C
C X2EXE.FOR
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C PROXGTP.FTN
C
C V01 24-OCT-89 MBK-MRM ORIGINAL RELEASE
C
C ENCODE GTP LAYER MESSAGE TO GO OUT
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
	SUBROUTINE PROXGTP(TRABUF,MESSAGE,ORIGINAL,RESPOND,
     *	                   LENGTH,BUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2TDBH.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
C
	INTEGER*4 MESSAGE(*)
	INTEGER*4 ORIGINAL(*), BUF
	INTEGER*2 LENGTH
	LOGICAL RESPOND
C
	RESPOND=.FALSE.
C
C PROCESS FIRST THOSE THAT YOU WOULDN'T KNOW HOW TO PROCESS.
C ONLY X2XMGR KNOWS THAT YOU HAVE TO DO SOMETHING DIFFERENT.
C
	IF(TRABUF(TXPTL).EQ.X2ERR_RECONNECT) THEN
C
	ELSEIF(TRABUF(TXPTL).EQ.X2ERR_TDBH_CMD_SYNC) THEN
C
	ELSEIF(TRABUF(TXPTL).EQ.X2ERR_TDBH_CMD_XOFF) THEN
C
	ELSEIF(TRABUF(TXPTL).EQ.X2ERR_TDBH_CMD_XON) THEN
C
	ELSEIF(TRABUF(TXPTL).EQ.X2ERR_TDBH_CMD_RESET) THEN
C
	ELSEIF(TRABUF(TXPTL).EQ.X2ERR_TDBH_CMD_BEGIN) THEN
C
	ELSEIF(TRABUF(TXPTL).EQ.X2ERR_TDBH_CMD_MAINTENANCE) THEN
C
	ELSEIF(TRABUF(TXPTL).EQ.X2ERR_TDBH_CMD_LOOPBACK) THEN
C
	ELSE
C
C NOW PROCESS AS YOU KNOW BEST (DON'T CONSIDER THOSE CONDITIONS WHICH
C HAPPENED TO BE DESCRIBED ABOVE)
C
C DON'T DO ANYTHING. X2XMGR DOES THE PROCESSING.
C
	ENDIF
C
C X2OUTGTP SHOULD BE USED IN THE ABOVE IF STATEMENT IF NECESSARY
C AND ALSO IN STANDARD PROCESSING IF NECESSARY.
C RIGHT NOW WE DON'T DO ANYTHING.
C
C***  CALL X2OUTGTP(TRABUF,MESSAGE,ORIGINAL,LENGTH)
C
	RETURN
	END
