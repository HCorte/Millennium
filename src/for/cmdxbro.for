C
C SUBROUTINE CMDXBRO
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CMDXBRO.FOV                                  $
C  $Date::   17 Apr 1996 12:39:30                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - cmdxsub.for;1 **
C
C V04 22-aug-95 das Modified to handle background downloads
C V03 29-DEC-94 GPR Modified to handle multiple MCPs per subnetwork
C V02 20-JUL-94 WS MULTINETWORK CHANGES - Integrate UK changes into 
C		   X2X Baseline
C
C
C =============================================================
C CMDXBRO
C
C This subroutine loads the relay broadcast information into
C common.
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CMDXBRO(FIELD,ALLREC,ADDFLG)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XBRO.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
C
	INTEGER*4   FIELD           !Modified field
	INTEGER*4   ALLREC(128)      !Record buffer
	INTEGER*4   ADDFLG          !New network port
	INTEGER*4   BRO, REL
        INTEGER*4   START_LOC   !%Loc OF FIRST field in Global record   !V03
        INTEGER*4   BYTES_PER_FIELD     !Bytes in a field               !V03
        PARAMETER   (BYTES_PER_FIELD=4)					!V03
C
C UPDATE COMMON WITH THE RELAY BROADCAST CONFIGURATION.
C
	CALL FASTMOV(ALLREC,X2XBRO_REC,128)
	REL=X2XBRO_APPNUM
        IF(REL.LE.0 .OR. REL.GT.X2X_RELAY_APPS) GOTO 8000
C
C       ***** Start V03 changes *****
C
        START_LOC = %LOC(X2XBRO_APPNUM)
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_SNDTIM)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_SEND_TIMOUT(REL)  = X2XBRO_SNDTIM
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_MAXRET)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_MAX_RETRY(REL)    = X2XBRO_MAXRET
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_WAITTIM)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_WAIT_TIMOUT(REL)  = X2XBRO_WAITTIM
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_RELSTAT)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_APP_STATUS(REL)   = X2XBRO_RELSTAT
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_MAXACT)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_MAX_ACTIVE(REL)   = X2XBRO_MAXACT
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_ATTRIB)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_APP_ATRIBUTE(REL) = X2XBRO_ATTRIB
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_APPFLG)-START_LOC)/BYTES_PER_FIELD)+1) THEN
	  IF(X2XBRO_APPFLG.EQ.1) THEN
	    X2XR_APP_FLAGS(REL) = X2STMES_RELAYF_IC +
     *	                          X2STMES_RELAYF_AR +
     *	                          X2STMES_RELAYF_AC
          ELSEIF(X2XBRO_APPFLG.EQ.2) THEN          
            X2XR_APP_FLAGS(REL) = X2STMES_RELAYF_AR +
     *                            X2STMES_RELAYF_AC
	  ELSE
	    X2XR_APP_FLAGS(REL) = X2STMES_RELAYF_IC +
     *	                          X2STMES_RELAYF_PE
	  ENDIF
	ENDIF
C
C 	***** Start V02 changes *****
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_BCST_ENABLE)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_BCST_ENABLE(REL) = X2XBRO_BCST_ENABLE
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_BCST_ROUTE)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_BCST_ROUTE(REL) = X2XBRO_BCST_ROUTE
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_BCST_STATION1)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_BCST_STATION1(REL) = X2XBRO_BCST_STATION1
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_BCST_STATION2)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_BCST_STATION2(REL) = X2XBRO_BCST_STATION2
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_SUBNETWORK)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_SUBNETWORK(REL) = X2XBRO_SUBNETWORK
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_WAIT_MAX)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_WAIT_MAX(REL) = X2XBRO_WAIT_MAX
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_START_IN_A_SEC)-
     *	    START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_START_IN_A_SEC(REL) = X2XBRO_START_IN_A_SEC
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_CALL_IN_A_SEC)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_CALL_IN_A_SEC(REL) = X2XBRO_CALL_IN_A_SEC
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_ENABLE_STATION_BROADCAST)-
     *	    START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_ENABLE_STATION_BROADCAST(REL) =
     *      X2XBRO_ENABLE_STATION_BROADCAST

	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_RESMES)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_DLL_RESET_MESSAGE(REL) = X2XBRO_RESMES
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_RESDEL)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_DLL_RESET_DELAY(REL) = X2XBRO_RESDEL
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_LODDEL)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_DLL_BETWEEN_LOADS_DELAY(REL) = X2XBRO_LODDEL
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_SEGDEL)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_DLL_BETWEEN_SEGS_DELAY(REL) = X2XBRO_SEGDEL
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_SNDREST)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_SNDREST(REL) = X2XBRO_SNDREST
C
C.V04	IF(FIELD.EQ.
C.V04*    ((%LOC(X2XBRO_FIRST_MCP)-START_LOC)/BYTES_PER_FIELD)+1)
C.V04*	  X2XR_FIRST_MCP(REL) = X2XBRO_FIRST_MCP
C
C.V04	IF(FIELD.EQ.
C.V04*    ((%LOC(X2XBRO_LAST_MCP)-START_LOC)/BYTES_PER_FIELD)+1)
C.V04*	  X2XR_LAST_MCP(REL) = X2XBRO_LAST_MCP
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_AUTOBAUD)-START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_AUTOBAUD(REL) = X2XBRO_AUTOBAUD
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_TIMES_SEND_MCP)-
     *	    START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_TIMES_SEND_MCP(REL) = X2XBRO_TIMES_SEND_MCP
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_TIMES_SEND_APP)-
     *	    START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_TIMES_SEND_APP(REL)=X2XBRO_TIMES_SEND_APP
C
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_POLL_LOGIC_ENABLE)-
     *	    START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_POLL_LOGIC_ENABLE(REL)=
     *	    X2XBRO_POLL_LOGIC_ENABLE
C
C 	***** End V02 changes *****
C
C 	***** End V03 changes *****
C
C 	***** Start V04 changes *****
	IF(FIELD.EQ.
     *    ((%LOC(X2XBRO_APP_TO_SEND)-
     *	    START_LOC)/BYTES_PER_FIELD)+1)
     *	  X2XR_APP_TO_SEND(REL) = X2XBRO_APP_TO_SEND
C 	***** End V04 changes *****
C
8000    CONTINUE
	RETURN
	END
