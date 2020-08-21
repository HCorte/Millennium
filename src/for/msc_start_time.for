C
C *** SUBROUTINE MSC_START_TIME ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSC_START_TIME.FOV                           $
C  $Date::   17 Apr 1996 14:08:24                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - msc_start_trap.for ***
C
C V03 09-DEC-93 RRB NO LONGER A READ TRAP. READ/WRITES ARE SYNCHRONOUS
C                   IN NATURE. WILL PEND READ AFTER WRITE IN SNDBUF.
C V02 12-JAN-93 RRB ONLY ONE POLL TIME PARAMETER IN TIMER TRAP
C V01 22-JAN-91 RRB INITIAL VAX VERSION
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
C	Starts a Timer trap for MSCMGR
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MSC_START_TIME(TIMER_EVENT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:MSCCOM.DEF'
	INCLUDE 'INCLIB:MSCEVN.DEF'
C
	INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4 TIMER_EVENT
	INTEGER*4 TIME_INIT/-1/
	INTEGER*4 CONF_INIT/-1/
	INTEGER*4 STAT_INIT/-1/
	INTEGER*4 STATUS
C
	INTEGER*4   DELAY(2,MSC_NUM_TIMTRAPS)		!TIMER DELAY
	CHARACTER   CURTIM*30		                !ASC TIME
C
	EXTERNAL MSC_TIMTRAP
C
	SAVE TIME_INIT, CONF_INIT, STAT_INIT, DELAY
C
C CALCULATE TIME OFFSET FOR TIMER TRAP BASED UPON TIMER EVENT TYPE.
C
D	TYPE*,IAM(),'START TIMER TRAP ',TIMER_EVENT
	IF(TIMER_EVENT.EQ.MSC_POLL_EVENT) THEN
	   IF(TIME_INIT.EQ.-1) THEN
              WRITE(CURTIM,9000) 0,MSC_POLLTIME/1000,
     *                           MOD(MSC_POLLTIME,1000)
              STATUS=SYS$BINTIM(CURTIM,DELAY(1,TIMER_EVENT))
              IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
	      TIME_INIT = 0
	   ENDIF
	ELSE IF(TIMER_EVENT.EQ.MSC_CONF_EVENT) THEN
	   IF(CONF_INIT.EQ.-1) THEN
              WRITE(CURTIM,9000) MSC_CONF_INTVL/60,
     *                           MOD(MSC_CONF_INTVL,60),0
              STATUS=SYS$BINTIM(CURTIM,DELAY(1,TIMER_EVENT))
              IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
	      CONF_INIT = 0
	   ENDIF
	ELSE IF(TIMER_EVENT.EQ.MSC_STAT_EVENT) THEN
	   IF(STAT_INIT.EQ.-1) THEN
              WRITE(CURTIM,9000) MSC_STAT_INTVL/60,
     *                           MOD(MSC_STAT_INTVL,60),0
              STATUS=SYS$BINTIM(CURTIM,DELAY(1,TIMER_EVENT))
              IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
	      STAT_INIT = 0
	   ENDIF
	ELSE
	   CALL OPS('MSCMGR: Invalid timer event in MSC_START_TIME'
     *              ,TIMER_EVENT,TIMER_EVENT)
	   GOTO 8000
	ENDIF
C
C ENABLE TIMER TRAP
C
        STATUS=SYS$SETIMR(,DELAY(1,TIMER_EVENT),MSC_TIMTRAP,
     *                     MSC_TIMER_FLAGS(TIMER_EVENT),0)
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
8000    CONTINUE
	RETURN
C
9000    FORMAT('0000 00:',I2.2,':',I2.2,'.',I3.3)
C
	END
