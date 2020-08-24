C
C SUBROUTINE X2LODBRO
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2LODBRO.FOV                                 $
C  $Date::   17 Apr 1996 16:21:14                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2lodbro.for;1 **
C
C X2LODBRO.FOR
C
C V04 22-Aug-95 das Modified to handle background loads
C V03 29-DEC-94 GPR Modified to handle multiple MCPs per subnetwork
C V02 20-JUL-94 WS MULTINETWORK CHANGES - Integrate UK changes 
C		   into X2X Baseline
C V01 01-DEC-91 XXX RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will load the GTECH Distributed Network
C common from the Network broadcast relay configuration.
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
	SUBROUTINE X2LODBRO
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XBRO.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
C
	INTEGER*4   REL, ST
	CHARACTER   X2FILNAM*20         !File name function
C

C OPEN THE BROADCAST RELAY DEFINITION FILE.
C
	CALL OPENX(1,X2FILNAM(XBRO),4,0,0,ST)
	CALL IOINIT(X2XBRO_FDB,1,X2XBRO_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XBRO),'OPENX',ST,0)
	  CALL GPAUSE
	ENDIF
C
C ATTEMPT TO LOAD ALL RELAY APPLICATIONS.
C
	DO 100 REL=1,X2X_RELAY_APPS
C
C READ RECORD ONE AS ALL DATA IS CONTAINED THERE.
C
	  CALL READW(X2XBRO_FDB,REL,X2XBRO_REC,ST)
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XBRO),'READW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XBRO_REC(1).LE.0) GOTO 100
C
C STORE THE INFORMATION INTO COMMON.
C
	  X2XR_SEND_TIMOUT(REL)  = X2XBRO_SNDTIM       !Send timeout
	  X2XR_MAX_RETRY(REL)    = X2XBRO_MAXRET       !Maximum retries
	  X2XR_WAIT_TIMOUT(REL)  = X2XBRO_WAITTIM      !Wait timeout
	  X2XR_APP_STATUS(REL)   = X2XBRO_RELSTAT      !Relay status
	  X2XR_MAX_ACTIVE(REL)   = X2XBRO_MAXACT       !Maximum active
	  X2XR_APP_ATRIBUTE(REL) = X2XBRO_ATTRIB       !Attribute
	  IF(X2XBRO_APPFLG.EQ.1) THEN
	    X2XR_APP_FLAGS(REL) = X2STMES_RELAYF_IC +
     *	                          X2STMES_RELAYF_AR +
     *	                          X2STMES_RELAYF_AC
          ELSEIF (X2XBRO_APPFLG.EQ.2) THEN
            X2XR_APP_FLAGS(REL) = X2STMES_RELAYF_AR +
     *                            X2STMES_RELAYF_AC
	  ELSE
	    X2XR_APP_FLAGS(REL) = X2STMES_RELAYF_IC +
     *	                          X2STMES_RELAYF_PE
	  ENDIF
	  X2XR_SUBNETWORK(REL) = X2XBRO_SUBNETWORK		!V02
	  X2XR_BCST_ENABLE(REL) = X2XBRO_BCST_ENABLE		!V02
	  X2XR_BCST_ROUTE(REL) = X2XBRO_BCST_ROUTE		!V02
	  X2XR_BCST_STATION1(REL) = X2XBRO_BCST_STATION1	!V02
	  X2XR_BCST_STATION2(REL) = X2XBRO_BCST_STATION2	!V02
	  X2XR_WAIT_MAX(REL) = X2XBRO_WAIT_MAX 			!V02
	  X2XR_START_IN_A_SEC(REL) = X2XBRO_START_IN_A_SEC	!V02
	  X2XR_CALL_IN_A_SEC(REL) = X2XBRO_CALL_IN_A_SEC	!V02
	  X2XR_ENABLE_STATION_BROADCAST(REL) = 			!V02
     *			      X2XBRO_ENABLE_STATION_BROADCAST	!V02
	  X2XR_DLL_RESET_MESSAGE(REL)= X2XBRO_RESMES		!V02
	  X2XR_DLL_RESET_DELAY(REL) = X2XBRO_RESDEL		!V02
	  X2XR_DLL_BETWEEN_LOADS_DELAY(REL) = X2XBRO_LODDEL	!V02
	  X2XR_DLL_BETWEEN_SEGS_DELAY(REL) = X2XBRO_SEGDEL	!V02
	  X2XR_SNDREST(REL)=X2XBRO_SNDREST			!V02
C.........X2XR_FIRST_MCP(REL)=X2XBRO_FIRST_MCP			!V03
C........ X2XR_LAST_MCP(REL)=X2XBRO_LAST_MCP			!V03
	  X2XR_AUTOBAUD(REL)=X2XBRO_AUTOBAUD			!V02
	  X2XR_TIMES_SEND_MCP(REL)=X2XBRO_TIMES_SEND_MCP	!V02
	  X2XR_TIMES_SEND_APP(REL)=X2XBRO_TIMES_SEND_APP	!V02
	  X2XR_POLL_LOGIC_ENABLE(REL)=X2XBRO_POLL_LOGIC_ENABLE	!V02
C     
          X2XR_APP_TO_SEND(REL) = X2XBRO_APP_TO_SEND            !V04
C
100	CONTINUE
C
C CLOSE THE FILE AND RETURN.
C
	CALL CLOSEFIL(X2XBRO_FDB)
	RETURN
C
	END
