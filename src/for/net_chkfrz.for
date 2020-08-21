C
C SUBROUTINE NET_CHKFRZ
C
C
C V02 01-MAR-2001 UXN TYPE* replaced with OPS()
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NET_CHKFRZ.FOV                               $
C  $Date::   17 Apr 1996 14:10:54                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - net_chkfrz.for ***
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
C	CHECK IF SYSTEM IS NOT FROZEN TOO LONG
C	(ONLY FREEZES CAUSED BY NETLOG/NETMGR ARE CHECKED)
C
C
C Calling Sequence:
C	CALL NET_CHKFRZ(STATUS)
C
C Output:
C	STATUS - SYSTEM WAS FROZEN TOO LONG
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE NET_CHKFRZ(STATUS)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	BUF,
     *			CHECK_BUF,
     *			CURRENT_FREEZ		/ 0/,
     *			LAST_NETCMDFRZ		/ 0/,
     *			NET_MAX_TIME_FROZEN	/26/,	! UP TO 13 SECS FROZEN
     *			NET_NETFRZ_INTERVAL	/ 4/,
     *			ST,
     *			STATUS
C
	LOGICAL*4	FROZEN_STATE		/.FALSE./
C
C COMMON DECLARATIONS
C
	COMMON /TEST_CHECK/ CHECK_BUF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PROCESS FOR THE LAST STATE .NOT. FROZEN
C
	IF (.NOT. FROZEN_STATE) THEN
	  CURRENT_FREEZ = CURRENT_FREEZ + 1
	  IF (CURRENT_FREEZ .LT. NET_NETFRZ_INTERVAL) GOTO 9999
C
	  CURRENT_FREEZ  = 0
	  LAST_NETCMDFRZ = NETCMDFRZ
	  IF (LAST_NETCMDFRZ .LT. 900) GOTO 9999
C
C FREEZE STARTED
C
	  FROZEN_STATE = .TRUE.
	  GOTO 9999
C
C IF NETCMDFRZ CHANGED GO BACK TO UNFROZEN STATE
C
	ELSE
	  IF (LAST_NETCMDFRZ .NE. NETCMDFRZ) THEN
	    CURRENT_FREEZ = 0
	    FROZEN_STATE  = .FALSE.
	    GOTO 9999
	  ENDIF
C	
	  CURRENT_FREEZ = CURRENT_FREEZ + 1
C
C IF FROZEN TOO LONG:
C - RELEASE BUFFERS FROM RECOVEY QUEUE
C - UNFREEZE
C - PRINT THE MESSAGE:
C - COMMUNICATION TO REMOTE SYSTEM IS NOT CLOSED,
C   SHOULD BE CLOSED AND RESTARTED MANUALLY.
C
	  IF (CURRENT_FREEZ .GT. NET_MAX_TIME_FROZEN) THEN
	    CURRENT_FREEZ = 0
C
30	    CONTINUE					! FREE RECOVERY BUFFERS
	    CALL RTL(BUF, RECOVQUE, ST)
	    IF (ST .NE. 2) THEN
	      CHECK_BUF = 501
	      CALL FREEBUF(BUF)
	      GOTO 30
	    ENDIF
C
	    P(CMDFRZ) = 0
	    NETCMDFRZ = 0
C
	    CALL OPS('System frozen too long = ', LAST_NETCMDFRZ, 0)
	    CALL OPSTXT('Take a dump of the secondary commons')
	    CALL OPSTXT('Let software know about the problem')
	    CALL OPSTXT('Remove and add the links to secondary system')
	  ENDIF
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
