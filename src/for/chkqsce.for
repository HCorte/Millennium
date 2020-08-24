C
C SUBROUTINE CHKQSCE
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CHKQSCE.FOV                                  $
C  $Date::   17 Apr 1996 12:33:16                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - chkqsce.for;1 **
C
C CHKQSCE.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 26-AUG-89 MBK ORIGINAL
C
C CHECK IF ALL CONNECTIONS CLOSED AND CLOSE THE SAP IF YES
C SAP HAS TO BE IS QUIESCED MODE
C
C  SAP    - INT*4 LOCAL SAP
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
	SUBROUTINE CHKQSCE(SAP)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4 ST, BUF, BNUM, QUE, CONN, DSAP, SAP
C
	IF(SAP.LE.0.OR.SAP.GT.MAXSAP) THEN
D	   TYPE*,'**** ILLEGAL SAP TO CLOSE QUEUE ****[',SAP,0,']'
	   CALL OPS('**** QUIESCE ILLEGAL SAP ****',SAP,0)
	   RETURN
	ENDIF
C
C RETURN IF NOT QUIESCED
C
	IF(LANSAPSTS(SAP).NE.SAPQSCED) RETURN
C
C CHECK THAT THERE ARE NOT ACTIVE CONNECTIONS
C
	DO 100 DSAP=1,MAXSAP
	CONN=CONNECTION(SAP,DSAP)
	IF(LANCONN(CONN).NE.CSAPCLO) RETURN
100	CONTINUE
C
C MARK HIS BUFFERS AS DESIGNATED TO FREE LIST
C
	QUE=QUESAP(SAP)
	IF(QUE.LE.0.OR.QUE.GT.LANMAXTSK) THEN
	   CALL OPS('**** QUIESCE ILLEGAL QUEUE ****',SAP,QUE)
	   RETURN
	ENDIF
C
	DO 200 BNUM=1,LANBNUM
	IF(LANBUF(LANLIST,BNUM).EQ.QUE) LANBUF(LANLIST,BNUM)=LISTFREE
200	CONTINUE
C
C FLUSH HIS FRAP QUE
C
	CALL FLUSHFRA(QUE)
C
	IF(LANSAPSTS(SAP).EQ.SAPUP.OR.LANSAPSTS(SAP).EQ.SAPQSCED) THEN
	   NUMSAP=NUMSAP-1
	   LANSAPSTS(SAP)=SAPDOWN
	ENDIF
C
	CALL LANGETX(BUF,ST)
	IF (ST.EQ.2) THEN
	   CALL OPS('**** CANNOT GET BUFFER FOR QSCE ****',SAP,QUE)
	   RETURN
	ENDIF
C
	LANBUF(LANBTYP,BUF)=LTYPCMD
	LANBUF(LANDATAF,BUF)=CCLOSE
	LANBUF(LANDATAF+1,BUF)=CREPLYOK
	LANBUF(LANDATAF+2,BUF)=SAP
	LANBUF(LANDATAF+3,BUF)=QUE
	LANBUF(LANOWN,BUF)=OWNAPPL
	CALL ABL(BUF,LANAPP(1,QUE),ST)
	IF(LANTRPTSK(QUE).EQ.TRAPYES) THEN
	CALL QUEUE(LANTASKS(QUE),BUF,ST)
	IF(ST.NE.0) THEN
D	   TYPE*,'**** TASK TRAP FAILURE ****[',QUE,ST,']'
	   CALL OPS('**** TRAP FAILURE ****',QUE,ST)
	ENDIF
	ENDIF
C
	RETURN
	END
