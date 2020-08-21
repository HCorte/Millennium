C SUBROUTINE TAKEFIL
C
C V02 15-JUN-2000 OXK CLEANUP W/ WARNINGS=ALL
C V01 17 Apr 1996 HXK INITIAL RELEASE
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C
C     SUBROUTINE TO DISPATCH BUFFER FROM NETFINISH QUEUE IN FILMD MODE
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
	SUBROUTINE TAKEFIL(BUF,NODE,WAY)
C
C     INP - BUF - NETWORK BUFFER
C         - NODE - THATS ME
C         - CURRENT WAY
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
C
	INTEGER*4 ST, RESULT, BTYPE, WAY, NODE, BUF
C
C
	BTYPE=NETBUF(BUFTYP,BUF)
C
	IF(BTYPE.EQ.INP)THEN
	   CALL FILSRV(BUF,NODE)
	ELSEIF(BTYPE.EQ.RTR) THEN
D	   TYPE*,'NO RECOVERY IN REM TRA MODE'
	ELSEIF(BTYPE.EQ.RLG) THEN
	   CALL TSTCHG2(BUF,+1,RESULT)
	   CALL ABL(BUF,REMFINISH,ST)
	ENDIF
C
	RETURN
	END
