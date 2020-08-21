C
C SUBROUTINE X2LODLPC
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2LODLPC.FOV                                 $
C  $Date::   17 Apr 1996 16:21:34                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2lodlpc.for;1 **
C
C X2LODLPC.FOR
C
C V02 06-NOV-95 SCD ADD CODE TO LOAD SAP NUMBERS INTO CONFIGURED_SAP
C		    ARRAY FOR USE BY SUBNETWORK GUI
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will load the GTECH Distributed Network
C common from the Local Port Configuration file.
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
	SUBROUTINE X2LODLPC
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XLPC.DEF'
C
	INTEGER*4   PORT            !Port number
	INTEGER*4   ST              !Status
	INTEGER*4   PRTCNT          !Count of ports loaded
	CHARACTER   X2FILNAM*20     !File name function
C
C OPEN THE LOCAL PORT CONFIGURATION FILE.
C
	PRTCNT=0
	CALL OPENLOD(X2FILNAM(XLPC),1)
C
C LOOP THROUGH ALL LOCAL PORTS IN THE FILE.
C
	PORT=0
C***  WRITE(5,9000)
100	CONTINUE
	  PORT=PORT+1
	  IF(PORT.GT.X2X_LOCAL_PORTS) GOTO 8000
	  CALL READLOD(1,PORT,X2XLPC_REC,ST)
	  IF(ST.EQ.144) GOTO 8000
C
C SKIP UNUSED SLOTS.
C
	  IF(X2XLPC_REC(1).LE.0) THEN
	    IF(PORT.LT.X2XLPC_RANGE(2,1)) THEN
	      GOTO 100
	    ELSE
	      GOTO 8000
	    ENDIF
	  ENDIF
C
C STORE THE INFORMATION INTO COMMON.
C
	  X2XPL_SAP(PORT)=X2XLPC_SAP
	  X2XPL_SAP_PORT(PORT)=X2XLPC_SAP_PORT
	  X2XPL_STATE(PORT)=X2XLPC_STATE
	  X2XPL_LOCAL_TO_NETWORK(PORT)=X2XLPC_NETPORT
	  X2XPL_SITE(PORT)=X2XLPC_SITE
	  IF(X2XLPC_NETPORT.GE.1.AND.X2XLPC_NETPORT.LE.X2X_NETWORK_PORTS)
     *       X2XPN_NETWORK_TO_LOCAL(X2XLPC_NETPORT)=PORT
	  X2XE_LOCAL_PORT(X2XLPC_SAP_PORT,X2XLPC_SAP)=PORT
	  PRTCNT=PRTCNT+1
          CONFIGURED_SAP(X2XLPC_SAP) = .TRUE.		!V02 CHANGE
	  IF(PORT.LT.X2XLPC_RANGE(2,1)) GOTO 100
C
C PROGRAM TERMINAL.
C
8000	CONTINUE
C***  WRITE(5,9010) PRTCNT
	CALL CLOSLOD(1)
	RETURN
C
C     ==================== Format Statements ==================
C
C***9000  FORMAT(1X,'Loading Local Port Configuration')
C***9010  FORMAT(1X,I6,' local ports loaded')
	END
