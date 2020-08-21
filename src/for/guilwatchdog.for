C GUIWATCHDOG.FOR
C 
C V02 31-OCT-2000 UXN GUI prefix added.
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C		This routine attempts checks watch dogs for all 'active'
C		connections. If watch dog is not .TRUE. it means that there
C		where no messages from the Client for the time specified in
C		GUI_WATCH_TIME. In such case an automatic disconnect is 
C		performed.
C
C INPUT:	none
C OUTPUT:	none
C RESULT:	disconnected Clients
C		
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUITCPPWATCHDOG
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
	INCLUDE 'INCLIB:GUILCOM.DEF'
	INCLUDE 'INCLIB:INETDEF.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4 CONN_INX	
	LOGICAL   RED_IGN_ONLY  ! used in the call to GUILDODISC
C
	IF(GUI_DBG_UNIT.NE.0) THEN
D	  TYPE *,IAM(),'GUILWATCHDOG: started.........'
	ENDIF
C
	IF(GUI_WATCH_TIME .LE. 0) THEN
	  CALL GUITCPPSTARTTIME(GUITCP_TIME_WATCH, GUI_WATCH_TIME_DEFAULT)
	  IF(GUI_DBG_UNIT.NE.0) THEN
	    TYPE *,IAM(),'GUILWATCHDOG: GUI_WATCH_TIME is not set'
	  ENDIF
	  GOTO 10000
	ENDIF
C
	DO 1000 CONN_INX=1, GUI_MAX_CONN
C
	  IF(GUI_DBG_UNIT.NE.0) THEN
D	    TYPE *,IAM(),'GUILWATCHDOG: conn,stat,watch: ', CONN_INX,
D     *			 GUI_CONN_STS(CONN_INX), GUITCP_WATCH_DOG(CONN_INX)
	  ENDIF
C
	  IF(GUI_CONN_STS(CONN_INX).NE.GUI_CONN_STS_CONNECTED) GOTO 1000
C
	  IF(GUITCP_WATCH_DOG(CONN_INX)) THEN
	    GUITCP_WATCH_DOG(CONN_INX) = .FALSE.	! RDIOCOMP changes it to TRUE
	    GOTO 1000
	  ENDIF
C
	  RED_IGN_ONLY = .FALSE.
	  CALL GUITCPPDODISC(CONN_INX, RED_IGN_ONLY)
C
1000	CONTINUE
C
C come to this routine again later
C
	CALL GUITCPPSTARTTIME(GUITCP_TIME_WATCH, GUI_WATCH_TIME)
C
10000	CONTINUE
C
	RETURN
C
	END
