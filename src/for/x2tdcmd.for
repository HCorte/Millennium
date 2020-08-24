C
C SUBROUTINE X2TDCMD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2TDCMD.FOV                                  $
C  $Date::   17 Apr 1996 16:37:54                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2rcvbuf.for;1 **
C
C
C++++++++++++++++++++++++++++++++++++++++++++++
C     X2TDCMD(TYPE,SSAP,STATUS)
C
C     IN:
C        TYPE  - BLOCK TYPE
C        SSAP  - SOURCE SAP
C     OUT:
C        STATUS- OPERATION STATUS
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
	SUBROUTINE X2TDCMD(COMMAND_TYPE,SSAP,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2TDBH.DEF'
C
	INTEGER*4 COMMAND_TYPE,SSAP
	INTEGER*4 STATUS
C
	IF (COMMAND_TYPE.EQ.X2TDBHT_SYNC_CONF) THEN
	   STATUS=X2ERR_TDBH_CMD_SYNC   !!!!!
	ELSEIF (COMMAND_TYPE.EQ.X2TDBHT_XOFF) THEN
	   STATUS=X2ERR_TDBH_CMD_XOFF    !FOR NOW !!!!
	ELSEIF (COMMAND_TYPE.EQ.X2TDBHT_XON) THEN
	   STATUS=X2ERR_TDBH_CMD_XON    !FOR NOW !!!!
	ELSEIF (COMMAND_TYPE.EQ.X2TDBHT_RESET_CONF) THEN
	   STATUS=X2ERR_TDBH_CMD_RESET  !!!!!
	ELSEIF (COMMAND_TYPE.EQ.X2TDBHT_BEGIN_CONF)  THEN
	   STATUS=X2ERR_TDBH_CMD_BEGIN
	ELSEIF (COMMAND_TYPE.EQ.X2TDBHT_MAINTENANCE_RESP)  THEN
	   STATUS=X2ERR_TDBH_CMD_MAINTENANCE
	ELSEIF (COMMAND_TYPE.EQ.X2TDBHT_LOOPBACK)  THEN
	   STATUS=X2ERR_TDBH_CMD_LOOPBACK
	ELSE
	   STATUS=X2ERR_FATAL+X2ERR_TDBH_INV_CMD
	ENDIF
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	    TYPE *,'RETURN X2TDCMD ',STATUS,' SSAP ',SSAP
	RETURN
	END
