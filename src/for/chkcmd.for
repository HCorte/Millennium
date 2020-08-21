C
C SUBROUTINE CHKCMD
C $Log:   GXAFXT:[GOLS]CHKCMD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:31:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:48:54   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - msc_chkcmd.for;1 **
C
C CHKCMD.FOR
C
C V01 01-FEB-91 RRB VAX INITIAL RELEASE
C
C VALIDATE COMMANDS FOR MSCMGR.
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
C 	INPUT
C
C	    CMD    - COMMAND NUMBER
C	    PARAMS - COMMAND PARAMETERS
C
C	OUTPUT
C
C	    CMDOK  - GOOD OR BAD COMMAND
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CHKCMD(CMD,PARAMS,CMDOK)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:MSCCOM.DEF'
	INCLUDE 'INCLIB:MSCCMDS.DEF'
C
	INTEGER*4  CMD
	INTEGER*4  PARAMS(MSC_MAX_PARMS)
C
	LOGICAL*4  CMDOK
C
	INTEGER*4  LOCAL_PORT,NETWORK_PORT
C
	CMDOK = .TRUE.
C
C CONNECT PORT COMMAND
C
	IF(CMD.EQ.CONNECT_PORT) THEN
	   NETWORK_PORT = PARAMS(1)
	   LOCAL_PORT = PARAMS(2)
	   IF(MSCSTS.NE.MSC_ACTIVE) THEN
	      TYPE*,'MSCMGR: SWITCH NOT ACTIVE'
	      CMDOK = .FALSE.
	   ELSE IF(X2XPL_STATE(LOCAL_PORT).NE.X2XPS_IDLE) THEN
	      TYPE*,'MSCMGR: INVALID LOCAL PORT STATUS - PORT ',
     *               LOCAL_PORT,' STATUS ',X2XPL_STATE(LOCAL_PORT)
	      CMDOK = .FALSE.
	   ELSE IF(X2XPN_STATE(NETWORK_PORT).NE.X2XPS_IDLE) THEN
	      TYPE*,'MSCMGR: INVALID NETWORK PORT STATUS - PORT ',
     *               NETWORK_PORT,' STATUS ',X2XPN_STATE(NETWORK_PORT)
	      CMDOK = .FALSE.
	   ENDIF
C
C CONNECT GROUP COMMAND
C
	ELSE IF(CMD.EQ.CONNECT_GROUP) THEN
	   TYPE*,'MSCMGR: NO GROUPS CURRENTLY DEFINED'
	   CMDOK = .FALSE.
C
C CONNECT SUPERGROUP COMMAND
C
	ELSE IF(CMD.EQ.CONNECT_SUPER) THEN
	   TYPE*,'MSCMGR: NO SUPER GROUPS CURRENTLY DEFINED'
	   CMDOK = .FALSE.
C
C INVALID COMMAND NUMBER
C
	ELSE
	   TYPE*,'MSCMGR: INVALID COMMAND NUMBER ',CMD
	   CMDOK = .FALSE.
	ENDIF
C
C COMMON EXIT
C
	RETURN
	END
