C
C SUBROUTINE X2GETAPP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2GETAPP.FOV                                 $
C  $Date::   17 Apr 1996 16:19:02                                         $
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
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     X2GETAPP(BUF,QUE,ST)     ;GET BUFFER TO PROCESS FROM APP QUE
C     IN:
C     QUE   - APPLICATION QUEUE #
C     OUT:
C     BUF   - BUFFER #
C     ST    - STATUS AS "RTL" SUBROUTINE
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
	SUBROUTINE X2GETAPP(BUF,QUE,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4 SAP, ST, QUE, BUF
	LOGICAL DUMP
C
	CALL RTL(BUF,LANAPP(1,QUE),ST)
	      IF (ST.EQ.2) RETURN
	LANBUF(LANOWN,BUF)=OWNFAPPL
C
C     CHECK IF SHOULD TRACE
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_TRACE_IN).NE.0) THEN
	   DUMP=.FALSE.
	   SAP=LANBUF(LANDORG,BUF)
	   IF (X2X_TRACE_SAP.EQ.-1.OR.SAP.EQ.X2X_TRACE_SAP) DUMP=.TRUE.
	   IF (DUMP) CALL X2TRCSAP(BUF,SAP)
	ENDIF
	RETURN
C
	END
