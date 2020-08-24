C GUISTARTTIME.FOR
C
C V02 31-OCT-2000 UXN GUI prefix added.
C V01 16-JUN-1993 MP  INITIAL RELEASE FOR VAX (Produced From TCPASST).
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
C Copyright 1991-1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C		This routine starts a timer trap for a specified amount
C		of time. There are 4 different types of timer traps. Only
C		one timer trap can be outstanding for each different type.
C		If a timer trap is currently outstanding for a certain type
C		and somebody wants to start another one for the same type then
C		the 2nd one will be ignored. Once the timer trap completes the
C		program will trap to the GUITCPPTIMTRAP routine.
C
C INPUT:
C	TIMETYPE    - type of time trap (1 to 5)
C	TIMEINMS    - time delay in milliseconds
C OUTPUT:
C	none
C RESULTS:
C	activated timer
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUITCPPSTARTTIME(TIMETYPE, TIMEINMS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
	INCLUDE 'INCLIB:GUILCOM.DEF'
C
	INCLUDE '($IODEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4 TIMETYPE	    !TYPE OF TIMER TRAP
	INTEGER*4 TIMEINMS	    !TIMER TRAP LENGTH
	INTEGER*4 STATUS	    !STATUS FROM SYSTEM CALL
	CHARACTER CURTIM*30	    !ASCII TIME
C
	EXTERNAL GUITCPPTIMTRAP
C
	INTEGER*4   BLANK
	DATA	    BLANK/'    '/
C
	IF(GUI_DBG_UNIT.NE.0) THEN
	  TYPE *,IAM(),'GUILSTARTTIME:  ',TIMETYPE, TIMEINMS
	ENDIF
C
C VERIFY THAT THIS IS AN OKAY TIMER TRAP TYPE
C
	IF(TIMETYPE.LT.1 .OR. TIMETYPE.GT.GUITCP_MAX_TIME_TRAPS) THEN
	  CALL FASTSET(BLANK,GUI_MES_BUF,33)
	  WRITE(GUI_MES_CBUF,9000) IAM(),TIMETYPE
	  CALL WRITEBRK(GUI_MES_CBUF)
	  RETURN
	ENDIF
C
C IF THERE IS ALREADY A TIMER TRAP OF THIS TYPE IN PROGRESS, IGNORE
C
	IF(GUITCP_TIMEINPR(TIMETYPE).EQ.GUI_INPROG) RETURN
C
C
C IF THE DELAY TIME IS DIFFERENT THEN THE LAST TIMER TRAP OF THIS
C TYPE, THEN RECONVERT THE TIME
C
	IF(GUITCP_TIMEINMS(TIMETYPE).NE.TIMEINMS)THEN
          WRITE(CURTIM,9100) TIMEINMS/60000,
     *			     MOD(TIMEINMS/1000,60),
     *			     MOD(TIMEINMS,1000)
          STATUS=SYS$BINTIM(CURTIM,GUITCP_TIMEDELY(1,TIMETYPE))
          IF(.NOT.STATUS) THEN
	    CALL FASTSET(BLANK,GUI_MES_BUF,33)
	    WRITE(GUI_MES_CBUF,9200) IAM(),TIMEINMS,STATUS
	    CALL WRITEBRK(GUI_MES_CBUF)
	    RETURN
	  ENDIF
	  GUITCP_TIMEINMS(TIMETYPE)=TIMEINMS
	ENDIF
C
C
C ENABLE TIMER TRAP
C
        STATUS=SYS$SETIMR(,GUITCP_TIMEDELY(1,TIMETYPE),
     *			   GUITCPPTIMTRAP,TIMETYPE,0)
        IF(.NOT.STATUS) THEN
	  CALL FASTSET(BLANK,GUI_MES_BUF,33)
	  WRITE(GUI_MES_CBUF,9300) IAM(),TIMETYPE,STATUS
	  CALL WRITEBRK(GUI_MES_CBUF)
	  RETURN
	ENDIF
C
C SET THE TYPE TO HAVING A TIMER TRAP IN PROGRESS
C
	GUITCP_TIMEINPR(TIMETYPE) = GUI_INPROG
C
	RETURN
C
9000	FORMAT(A,'GUILSTARTTIME: Invalid GUI Buffer  # ',I8)
9100    FORMAT('0000 00:',I2.2,':',I2.2,'.',I3.3)
9200	FORMAT(A,'GUILSTARTTIME: Error converting Time ',I8,
     *		    ' Status ',I8)
9300	FORMAT(A,'GUILSTARTTIME: ERor starting Timer Trap ',I8,
     *		    ' Status ',I8)
C
	END
