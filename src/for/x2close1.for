C
C SUBROUTINE X2CLOSE1
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CLOSE1.FOV                                 $
C  $Date::   17 Apr 1996 16:13:34                                         $
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
C++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     X2CLOSE1(SAP,QUE)        ;CLOSE SAP
C
C     IN:
C     SAP - SAP TO BE CLOSED
C     QUE - QUEUE USED BY SAP
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
	SUBROUTINE X2CLOSE1(SAP,QUE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*4 DUMMY, ST, STATUS, BUF, QUE, SAP
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0) TYPE *,IAM(),'X2CLOSE1'
20	CONTINUE
	CALL LANGETB(BUF,STATUS)
	IF (STATUS.EQ.2) THEN
	   CALL XWAIT(1,2,ST)    !WAIT IF COULD NOT GET BUFFER
	   GOTO 20
	ENDIF
C
	LANBUF(LANBTYP,BUF)=LTYPCMD
	LANBUF(LANDATAF,BUF)=CCLOSE
	LANBUF(LANDATAF+1,BUF)=CCOMMAND
	LANBUF(LANDATAF+2,BUF)=SAP
	LANBUF(LANDATAF+3,BUF)=QUE
C
	CALL X2SNDLAN(DUMMY,DUMMY,BUF,ST)
	CALL XWAIT(10,2,ST)       !JUST WAIT (SECS)
C
	IF(IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0) TYPE*,'RETURN X2CLOSE1'
	RETURN
	END
