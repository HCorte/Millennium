C
C SUBROUTINE X2TRCSAP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2TRCSAP.FOV                                 $
C  $Date::   17 Apr 1996 16:38:54                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2xmgr.for;1 **
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     X2TRCSAP(BUF,SAP)      ;TRACE SAP ACTIVITY
C     IN:
C     BUF   -  LAN BUFFER NO
C     SAP   -  SAP # TRACED
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
	SUBROUTINE X2TRCSAP(BUF,SAP)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INTEGER*4 FILE_INIT/0/
	INTEGER*4 ST, BLOCK_SIZE, SAP, BUF
C
C     THIS FILE SHOULD BE ADDED TO BLDSYS !!!!!
C
	INTEGER*4 X2X_TRACE_FILE(4) /'X2XT','RACE','.FIL',0/
	PARAMETER (BLOCK_SIZE=(LANBLEN+65)/64)
C
C     INITIALIZE TRACE FILE IF NEEDED
	IF (FILE_INIT.EQ.0) THEN
	   CALL OPENW(X2X_TRACE_UNIT,X2X_TRACE_FILE,4,0,0,ST)
	   IF (ST.NE.0) RETURN
	   X2X_TRACE_BLOCK=0               !STARTING BLOCK NO
	   CALL IOINIT(X2X_TRACE_FDB,X2X_TRACE_UNIT,BLOCK_SIZE*256)
	   CALL GETSIZ_USED(X2X_TRACE_UNIT,X2X_TRACE_SIZE)  !FILE SIZE
	   X2X_TRACE_SIZE=X2X_TRACE_SIZE/BLOCK_SIZE    !IN BLOCKS
	   FILE_INIT=-1                    !INITIALIZED NOW
	ENDIF
C
C     ADD TO BUFFER SOME SAP STATUSES AND TIME
C
	LANBUF(LANTIME,BUF)=X2X_SYSTIM
C
	IF (SAP.GT.0) THEN
	   LANBUF(LAN_SAP_DEF_STATE,BUF)=X2XE_DEF_STATE(SAP)
	   LANBUF(LAN_SAP_ACT_STATE,BUF)=X2XE_ACT_STATE(SAP)
	   LANBUF(LAN_SAP_ACT_STATUS,BUF)=X2XE_ACT_STATUS(SAP)
	   LANBUF(LAN_SAP_DEF_STATUS,BUF)=X2XE_DEF_STATUS(SAP)
	ELSE
	   LANBUF(LAN_SAP_DEF_STATE,BUF)=0
	   LANBUF(LAN_SAP_ACT_STATE,BUF)=0
	   LANBUF(LAN_SAP_ACT_STATUS,BUF)=0
	   LANBUF(LAN_SAP_DEF_STATUS,BUF)=0
	ENDIF
C
	X2X_TRACE_BLOCK=MOD(X2X_TRACE_BLOCK+1,X2X_TRACE_SIZE)
	IF (X2X_TRACE_BLOCK.EQ.0) X2X_TRACE_BLOCK=1
	CALL WRITEW(X2X_TRACE_FDB,X2X_TRACE_BLOCK,LANBUF(-1,BUF),ST)
	RETURN
	END
