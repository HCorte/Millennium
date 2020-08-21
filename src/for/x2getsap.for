C
C SUBROUTINE X2GETSAP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2GETSAP.FOV                                 $
C  $Date::   17 Apr 1996 16:19:26                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2sndbuf.for;2 **
C
C V04  29-NOV-95 WJK Save NEXTSAP across subroutine calls and initialize 
C		     correctly.
C V03  6-JUN-94 WS CHANGE TO SUBNETWORK TYPE - Integrate UK changes 
C		   into X2X Baseline
C V02  8-MAR-94 JWE Add connection type check...
C
C+++++++++++++++++++++++++++++++++++++++++++++++++
C
C     X2GETSAP(SAP,CONN_TYPE,SUBNETWORK,STATUS)      GET SAP TO SEND DATA TO-V03
C
C     IN:						!V03
C	 CONN_TYPE  - STATION CONNECTION TYPE		!V03
C	 SUBNETWORK - SUBNETWORK NO			!V03
C     OUT:
C        SAP   - DESTINATION SAP
C        STATUS- .NE.0 IF COULD NOT GET ANY
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
	SUBROUTINE X2GETSAP(SAP, CONN_TYPE, SUBNETWORK, STATUS)	!V03
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2TDBH.DEF'
C
	INTEGER*4   CONN_TYPE
	INTEGER*4   X2MAP_CONN_TYPE
	INTEGER*4   SUBNETWORK					!V03
C
	INTEGER*4  MAXLOOP
	PARAMETER (MAXLOOP=4)
	INTEGER*4  STARTING_SAP					!V04
	PARAMETER (STARTING_SAP=X2X_ACTUAL_SAP-1)		!V04
C
	INTEGER*4 CAPACITY, LOOP_CNT, TOT_CHECK, STATUS, SAP

	INTEGER*4 NEXTSAP/STARTING_SAP/				!V04
	SAVE NEXTSAP						!V04
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	   TYPE *,'X2GETSAP ',SAP
	TOT_CHECK=0
	LOOP_CNT=1
10	CONTINUE
	NEXTSAP=NEXTSAP+1
	IF (NEXTSAP.GT.X2X_SAP) NEXTSAP=X2X_ACTUAL_SAP		!V03
	TOT_CHECK=TOT_CHECK+1
	IF (TOT_CHECK.GT.X2X_SAP-X2X_ACTUAL_SAP+1) THEN		!V03
	   TOT_CHECK=0
	   IF (LOOP_CNT.LT.X2X_MAX_THRESHOLD) THEN
	      LOOP_CNT=LOOP_CNT+1
	      GOTO 10
	   ENDIF
	   STATUS=-1
	   IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	      TYPE *,'RET X2GETSAP ',SAP,STATUS
	   RETURN                !COULD NOT GET ANY
	ENDIF
C
C Ignore SAPs with the worng network type
C
	IF(X2MAP_CONN_TYPE(CONN_TYPE) .NE. X2XE_NETWORK_TYPE(NEXTSAP))
	1   GOTO 10
C
C IGNORE SAP WITH WRONG SUBNETWORK TYPE				!V03
C
	
	IF (X2XE_SUBNETWORK(NEXTSAP).NE.SUBNETWORK .AND.	!V03
     *	    X2XE_SUBNETWORK(NEXTSAP).NE.255) GOTO 10		!V03
	IF (X2XE_ACT_STATUS(NEXTSAP).NE.X2XES_ONLINE) GOTO 10
	IF (X2XE_FE_TYPE(NEXTSAP).EQ.X2TDBH_FE_TYPE_DIAL_UP) GOTO 10
	CAPACITY=X2XE_CAPACITY(NEXTSAP)
	IF (CAPACITY.GT.X2X_THRESHOLD(LOOP_CNT)) THEN
	   SAP=NEXTSAP
	   X2XE_CAPACITY(NEXTSAP)=X2XE_CAPACITY(NEXTSAP)-1
	   IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	      TYPE *,'RET X2GETSAP ',SAP,STATUS
	   STATUS=0
	   RETURN
	ENDIF
	GOTO 10
	END
