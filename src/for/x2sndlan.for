C
C SUBROUTINE X2SNDLAN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2SNDLAN.FOV                                 $
C  $Date::   17 Apr 1996 16:34:46                                         $
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
	SUBROUTINE X2SNDLAN(SSAP,DSAP,BUF,POINTER)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INTEGER*4 LENGTH, POINTER, BUF, DSAP, SSAP
	LOGICAL DUMP
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	    TYPE *,IAM(),'SNDLAN ',SSAP,DSAP,BUF,POINTER
	LENGTH=POINTER-1
	CALL SNDLAN(SSAP,DSAP,BUF,LENGTH)
	POINTER=0
	IF (DSAP.GT.0) X2XE_CNT_BLK(DSAP)=X2XE_CNT_BLK(DSAP)+1
C
C     CHECK IF SHOULD TRACE
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_TRACE_OUT).NE.0) THEN
	   DUMP=.FALSE.
	   IF(X2X_TRACE_SAP.EQ.-1.OR.DSAP.EQ.X2X_TRACE_SAP) DUMP=.TRUE.
	   IF (DUMP) CALL X2TRCSAP(BUF,DSAP)
	ENDIF
	RETURN
	END
