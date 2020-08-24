C
C SUBROUTINE PORTCNT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]PORTCNT.FOV                                  $
C  $Date::   17 Apr 1996 14:27:02                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2chkstn.for;1 **
C
 
C ==================================================
C PORTCNT
C
C V02 19-AUG-94 GPR SPEED UP STATION CHECKS - Integrate UK changes into 
C		    X2X Baseline
C
C Calling sequence:
C
C     CALL PORTCNT(PORTS)
C
C Output parameters:
C
C     PORTS   Int*2(X2X_STATIONS)     Count of ports for each station
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
	SUBROUTINE PORTCNT(PORTS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XSPC.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INTEGER*2   PORTS(X2X_STATIONS)
	INTEGER*4   REC
	INTEGER*4   ST
	CHARACTER   X2FILNAM*20
C
C
C OPEN THE STATION PORT CONFIGURATION FILE.
C
        CALL OPENX2X(X2FILNAM(XSPC),4)				  !V02

	REC=0
	CALL FASTSET(0,PORTS,X2X_STATIONS/2)
C
C READ ALL RECORDS IN THE PORT CONFIGURATION FILE.
C
100	CONTINUE
	REC=REC+1
CV02	CALL READW(X2XSPC_FDB,REC,X2XSPC_REC,ST)
	CALL READX2X(4,REC,X2XSPC_REC,ST)			  !V02
	IF(ST.EQ.144) GOTO 8000
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XSPC),'READW',ST,REC)
	  CALL GPAUSE
	ENDIF
	IF(X2XSPC_REC(1).LE.0) GOTO 100
C
	IF(X2XSPC_STN.GT.0 .AND. X2XSPC_STN.LE.X2X_STATIONS) THEN
	  PORTS(X2XSPC_STN)=PORTS(X2XSPC_STN)+1
	ELSE
	  TYPE *,'PORTS ARRAY OVER/UNDER FLOW:  ',X2XSPC_STN
	  CALL GPAUSE
	ENDIF
	GOTO 100
C
C PROGRAM EXIT
C
8000	CONTINUE
	CALL CLOSX2X(4)						!V02
	RETURN
	END
