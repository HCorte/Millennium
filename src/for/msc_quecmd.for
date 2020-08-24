C
C *** SUBROUTINE MSC_QUECMD ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSC_QUECMD.FOV                               $
C  $Date::   17 Apr 1996 14:07:56                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - msc_quecmd.for ***
C
C V02 07-JAN-93 RRB QUEUE TO EXEC QUEUE
C V01 26-MAR-91 RRB RELEASED FOR VAX
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	QUEUE APPLICATION COMMAND TO MSCMGR
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MSC_QUECMD(COMMAND)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:MSCCOM.DEF'
	INCLUDE 'INCLIB:MSCEVN.DEF'
C
	INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4 COMMAND(20)
	INTEGER*4 BUF, ST, STATUS
C
 	CALL MSCGETB(BUF,ST)
	IF(ST.EQ.2) THEN
	   CALL OPS('MSCMGR:Buffer Allocation Error',0,0)
	   GOTO 8000
	ENDIF
	CALL FASTMOV(COMMAND,MSCBUF(1,BUF),20)
	MSCBUF(MSCBLEN,BUF)=MSCBTYPOUT
	CALL ABL(BUF,MSCEXEC,STATUS)
D	IF(STATUS.NE.0) PAUSE 'MSCMGR EXEC QUE OVF'
C
8000	CONTINUE
	STATUS=SYS$SETEF(%VAL(MSC_EXEC_FLAG))
	IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
	RETURN
	END
