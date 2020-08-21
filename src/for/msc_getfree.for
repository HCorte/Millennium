C
C *** SUBROUTINE GETFREE ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSC_GETFREE.FOV                              $
C  $Date::   17 Apr 1996 14:07:36                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - msc_getfree.for ***
C
C V01 14-JAN-91 RRB FIND UNUSED LOCAL PORT FOR TELENEX MATRIX SWITCH MANAGEMENT.
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
C Calling Sequence:
C	CALL GETFREE(BADPORT,FREEPORT)
C
C Input:	BADPORT  - PORT BEING CHANGED
C
C Output:	FREEPORT - PORT TO CHANGE TO (0 - NO PORTS AVAILABLE)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETFREE(BADPORT,FREEPORT)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*4 BADPORT, FREEPORT
	INTEGER*4 LOCAL_PORT,NET_PORT,PVC_CIRCUIT,STN,EXT,ST,SAP
	INTEGER*4 FREE_PORT, TESTPORT, PORT_STATE
	LOGICAL*4 ASSIGNED
C
	FREEPORT = 0
C
C SCAN FRONT ENDS FOR AVAILABLE PORTS
C   CHECK SAP FOR VALID STATUS, IF NOT SATURATED THEN CHECK EACH
C   PORTS STATUS AND AVAILABILITY.
C
       DO 200 SAP = X2X_ACTUAL_SAP,X2X_SAP
	  IF(X2XE_ACT_STATUS(SAP).EQ.X2XES_IDLE .OR.           !IF GOOD SAP
     *	       X2XE_ACT_STATUS(SAP).EQ.X2XES_ONLINE) THEN
D           TYPE*,IAM(),'FOUND GOOD SAP'
	    IF(X2XE_CAPACITY(SAP).NE.X2XE_MAX_CAPACITY(SAP)) THEN
D              TYPE*,IAM(),'FOUND SAP WITH FREE PORTS'
	       DO 100 FREE_PORT = 1,X2X_SAP_PORTS
	         ASSIGNED = .FALSE.
                 TESTPORT = X2XE_LOCAL_PORT(FREE_PORT,SAP)
	         IF(TESTPORT.LE.0.OR.TESTPORT.GT.X2X_LOCAL_PORTS)
     *              GOTO 100
D		 TYPE*,IAM(),'TESTING LOCAL PORT ',TESTPORT
	         IF(X2XPL_LOCAL_TO_NETWORK(TESTPORT).GT.0)
     *                ASSIGNED = .TRUE.
	         IF(.NOT.ASSIGNED) THEN
D                  TYPE*,IAM(),'FOUND UNASSIGNED PORT'
	           PORT_STATE = X2XPL_STATE(TESTPORT)
	           IF(PORT_STATE.EQ.X2XPS_ON_LINE.OR.    !GOOD PORT STATUS?
     *                  PORT_STATE.EQ.X2XPS_IDLE) THEN
	              FREEPORT = TESTPORT
		      GOTO 8000
	           ENDIF
	         ENDIF
100	       CONTINUE
	    ENDIF
          ENDIF
200    CONTINUE
C
8000   CONTINUE
       RETURN
       END
