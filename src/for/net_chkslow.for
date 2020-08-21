C
C SUBROUTINE NET_CHKSLOW
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NET_CHKSLOW.FOV                              $
C  $Date::   17 Apr 1996 14:11:02                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source -  net_chkslow.for ***
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
C	CHECK IF ANY OF THE SYSTEMS (NODES) IS TOO SLOW IN PROCESSING DATA.
C
C Calling Sequence:
C	CALL NET_CHKSLOW(STATUS)
C
C Output:
C	STATUS - 0 = OK
C		!0 = SLOW SYSTEM NUMBER
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE NET_CHKSLOW(STATUS)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:DESNET.DEF'
C
C PARAMETER DECLARATIONS
C
	INTEGER*4	LAST_LEN
C
	PARAMETER	(LAST_LEN = (NET_XFER_TIME_MAX + 1) * NETSYS)
C
C LOCAL DECLARATIONS
C
	INTEGER*4	CURRENT_INTERVAL	/0/,
     *			DELAY_OFFSET,
     *			LAST_DELAYS(0:NET_XFER_TIME_MAX, NETSYS)
     *						/LAST_LEN * 0/,
     *			NODE,
     *			STATUS
C
	LOGICAL*4	FIRST_RUN_AFTER_RECOVERY(NETSYS)
     *						/NETSYS * 2/
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C THE FOLLOWING STATEMENT IS NOT ABSOULTELY NECESSARY UNDER VAX FORTRAN ...
C HOWEVER, IT WOULD BE REQUIRED TO COMPLY WITH THE ANSI F77 SPECIFICATION.
C
	SAVE		CURRENT_INTERVAL
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	STATUS = 0
C
	CURRENT_INTERVAL = CURRENT_INTERVAL + 1
	IF (CURRENT_INTERVAL .LT. NET_SLOW_REPORT_INTERVAL) GOTO 9999
C
	CURRENT_INTERVAL = 0
C
C CHECK ALL ACTIVE SYSTEM, NOT IN RECOVERY MODE.
C
	DO 200 NODE = 1, NETSYS
	  IF (NETMODE(NODE, 1) .NE. ROUACT) GOTO 200
C
	  IF (NETMODE(NODE, 1) .EQ. FILMD) THEN
	    FIRST_RUN_AFTER_RECOVERY(NODE) = 2		! MAKE IT A FULL 1st RUN
	    GOTO 200
	  ENDIF
C
	  DO 100 DELAY_OFFSET = NET_XFER_TIME_MAX,
     *                         NET_FIRST_REPORT_DELAY, -1
C
	    IF (NET_XFER_TIME(DELAY_OFFSET, NODE) .GT.
     *          LAST_DELAYS(DELAY_OFFSET, NODE)) THEN
C
	      IF (FIRST_RUN_AFTER_RECOVERY(NODE) .GT. 0) THEN
		FIRST_RUN_AFTER_RECOVERY(NODE) =
     *          FIRST_RUN_AFTER_RECOVERY(NODE) - 1
		GOTO 200
	      ENDIF
C
	      CALL OPS('*** NET_CHKSLOW - SYSTEM IS SLOW ***',
     *                 NODE, DELAY_OFFSET)
	      STATUS = NODE
	      GOTO 200
	    ENDIF
C
100	  CONTINUE
200	CONTINUE
C
	CALL FASTMOV(NET_XFER_TIME(0, 1), LAST_DELAYS(0, 1),
     *               (NET_XFER_TIME_MAX + 1) * NETSYS)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
