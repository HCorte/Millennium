C
C SUBROUTINE FLUSHFRA
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]FLUSHFRA.FOV                                 $
C  $Date::   17 Apr 1996 13:11:56                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - flushfra.for;1 **
C
C FLUSHFRA.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 MBK 31-JUL-89 ORIGINAL RELEASE
C
C WILL FLUSH THE APPLICATION QUEUE
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
	SUBROUTINE FLUSHFRA(QUE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4 STATUS, BUF, QUE
C
C VALIDATE THE QUEUE
C
	IF(QUE.LE.0.OR.QUE.GT.LANMAXTSK) THEN
	   CALL OPS('**** FLUSH ERROR (ILLEGAL QUEUE) ****',QUE,0)
	   RETURN
	ENDIF
 
100	CONTINUE
	CALL LANGETA(BUF,QUE,STATUS)
	IF(STATUS.EQ.2) RETURN
C
	CALL LANRELB(BUF)
	GOTO 100
C
	END
