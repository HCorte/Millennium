C
C SUBROUTINE QUEUE
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]QUEUE.FOV                                    $
C  $Date::   17 Apr 1996 14:36:58                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vax_queue.for;1 **
C
C VAX_QUEUE.FOR
C
C V01 10-SEP-90 MRM INITIAL RELEASE.
C
C THIS ROUTINE WAS DEVELOPED TO EMULATE THE CONCURRENT
C TASK TRAP QUEUE.  THIS IS ACCOMPLISHED BY SETTING
C THE APPROPRIATE EVENT FLAG FOR THE INPUT TASK.
C NOTE: THE TASKS BEING TRAP HAVE BEEN MODIFIED TO
C TRAP ON THESE EVENT FLAGS.
C
C NOTE: THE TASK WHICH CALL THIS ROUTINE MUST HAVE
C PREVIOUSLY ATTACHED ITSELF TO THE COMMON EVENT
C CLUSTER (SEE LANEVN.DEF).
C
C Input parameters:
C
C	TSKNAM	    Real*8	Name of task to be trapped
C	DUMMY	    Int*4	Dummy parameter (to be consistant)
C
C Output parameters:
C
C	ST	    Int*4	Return status (-1 = unknown task)
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
	SUBROUTINE QUEUE(TSKNAM,DUMMY,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:LANEVN.DEF'
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	REAL*8	    TSKNAM	!NAME OF TASK TO QUEUE
	INTEGER*4   DUMMY	!DUMMY PARAMETER
	INTEGER*4   ST		!RETURN STATUS
	INTEGER*4   STATUS	!SYSTEM FUNCTION CALL STATUS
	INTEGER*4   TARGET	!SLOT OF TASK TO BE TRAPPED
	INTEGER*4   TASK
C
C SEARCH FOR THE TASK NAME IN COMMON.
C
	ST=0
	TARGET=-1
	DO 100 TASK=0,LANMAXTSK
	  IF(TSKNAM.EQ.LANTASKS(TASK)) TARGET=TASK
100	CONTINUE
C
C IF TASK NAME FOUND, SET THE EVENT FLAG.
C
	IF(TARGET.NE.-1) THEN
D	  TYPE 9000,LANTASKS(TARGET)
9000	  FORMAT(1X,'SETTING FLAG FOR ',A8)
	  STATUS=SYS$SETEF(%VAL(LN_EVNTSK_FLAG(TARGET)))
	  IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
	ELSE
	  ST=-1
	ENDIF
C
	RETURN
	END
