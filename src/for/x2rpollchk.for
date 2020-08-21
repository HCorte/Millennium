C
C SUBROUTINE X2RPOLLCHK
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RPOLLCHK.FOV                               $
C  $Date::   17 Apr 1996 16:32:04                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2xrel.for;2 **
C
C V04 12-DEC-94 DAS Integrate UK changes into X2X Baseline
C V02 19-JUL-94 WS MULTINETWORK CHANGES
C
C ************************************************************
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
	SUBROUTINE X2RPOLLCHK(STATION,PROCESS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
C
	INTEGER*4   STATION
	INTEGER*4   PROCESS
	INTEGER*4   TEMP
	INTEGER*4   CONF
	INTEGER*4   DSAP, ISAP
        INTEGER*4   CONTYPE
	INTEGER*4   CLASS				!V02
C
C IF POLLING HAD BEEN DISABLED FOR THIS STATION DURING
C THIS RELAY PROCESS THEN REENABLE NOW
C
	IF(X2XR_SAVE_STATION_POLL_FLAG(STATION,PROCESS).NE.0) THEN
	  CALL BCLR(BX2XS_PARAM(STATION),7)       !ENABLE POLLING
	  X2XR_SAVE_STATION_POLL_FLAG(STATION,PROCESS)=0
C
C UPDATE STATION CONFIGURATION
C
          TEMP=ZEXT (BX2XS_CONF(STATION))
          CONF=ISHFT(IAND(TEMP,'E0'X),-5)
          CONF=CONF+1
          IF(MOD(CONF,8).EQ.0) CONF=0
          CONF=ISHFT(CONF,5)
          TEMP=IAND(TEMP,'1F'X)+CONF
	  IF (X2XS_TYPE(STATION).EQ.X2XST_BCST) 		!V02
     *		     TEMP=0					!V02
          BX2XS_CONF(STATION)=TEMP
C
C SEND SOFT RESET TO STATION 
C
	  DSAP = ZEXT(BX2XS_SAP(STATION))			!V02
C
C         WE NEED TO CHECK HERE FOR CONNECTION TYPE
C         SINCE X25 HAS NO SAP NUMBER WE WILL FORCE
C         A SAP NUMBER. USE THE FACT THAT ASYNC STATION
C         WILL ALWAYS REPORT A CAPACITY OF ZERO. THE SAP
C         NUMBER IS NEEDED TO FORCE CODE EXECUTION AT THIS
C         TIME. MULTI_NETWORK CODE SHOULD REPLACE THIS
C         KLUDGE.
C
          CONTYPE=ZEXT(BX2XS_CONN_TYPE(STATION))
	  CLASS=X2XS_STNCLS(STATION)				!V02
          IF(CONTYPE.EQ.X2XSCT_X25SVC) THEN
            DO 201 ISAP = X2X_ACTUAL_SAP, X2X_SAP
	       IF (X2XC_SUBNETWORK(CLASS).NE.X2XE_SUBNETWORK(ISAP)) !V02
     *			  GOTO 201				!V02
               IF(X2XE_CAPACITY(ISAP).NE.0) DSAP =ISAP
201         CONTINUE
          ENDIF
C..
	  IF(DSAP.GE.1.AND.DSAP.LE.X2X_SAP) THEN
	    IF(X2XE_ACT_STATUS(DSAP).EQ.X2XES_ONLINE) THEN
		CALL X2STNRES(STATION)
	    ENDIF
	  ENDIF
	ENDIF
C
	RETURN
        END
