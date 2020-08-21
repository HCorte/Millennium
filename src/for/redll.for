C  REDLL.FOR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]REDLL.FOV                                    $
C  $Date::   17 Apr 1996 14:41:20                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C V02 13-SEP-95 DAS TAKEN FROM LEIPZIG FOR BACKGROUND LOADS
C V01 XX-XXX-XX XXX INITIAL RELEASE
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
C
C++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE REREQDLL(NUM_LOAD,SUBNETWORK)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:REQCOM.DEF'
C
	INTEGER*4 NUM_LOAD
	INTEGER*4 OFF, SUBNETWORK, OFF_APP
C
	NUM_LOAD=0
C
C	RETURN IF NOTHING LOADING DURING 2 INTRVAL TIMES

	IF (P(ACTTIM)-RE_LAST_REFRESH_TIME(SUBNETWORK)-
     *	     2* RE_TIME_INTERVAL(SUBNETWORK) .GT.0) RETURN

	DO 100, OFF_APP=1,MAXAPP
	DO 100, OFF=1,MAXLOADS
	    NUM_LOAD=NUM_LOAD+RE_TERMINALS_PER_INTERVAL(OFF,OFF_APP,
     *					      SUBNETWORK,1)
100	CONTINUE
	RETURN

	END
C
C
C++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE RESTNDLL(NUM_LOAD,SUBNETWORK)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:REQCOM.DEF'
C
	INTEGER*4 NUM_LOAD
	INTEGER*4 OFF, SUBNETWORK
	INTEGER*4 OFF_APP
C
	NUM_LOAD=0
C
C	RETURN IF NOTHING LOADING DURING 2 INTRVAL TIMES

	IF (P(ACTTIM)-RE_LAST_REFRESH_TIME(SUBNETWORK)-
     *	     2* RE_TIME_INTERVAL(SUBNETWORK) .GT.0) RETURN

	DO 100, OFF_APP=1,MAXAPP
	DO 100, OFF=1,MAXLOADS
	    NUM_LOAD=NUM_LOAD+RE_STATIONS_PER_INTERVAL(OFF,OFF_APP,
     *		      SUBNETWORK,1)
100	CONTINUE
	RETURN
	END

C++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE REMSGDLL(NUM_LOAD,SUBNETWORK)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:REQCOM.DEF'
C
	INTEGER*4 NUM_LOAD
	INTEGER*4 OFF, SUBNETWORK
	INTEGER*4 OFF_APP
C
	NUM_LOAD=0
C
C	RETURN IF NOTHING LOADING DURING 2 INTRVAL TIMES

	IF (P(ACTTIM)-RE_LAST_REFRESH_TIME(SUBNETWORK)-
     *	     2* RE_TIME_INTERVAL(SUBNETWORK) .GT.0) RETURN

	DO 100, OFF_APP=1,MAXAPP
	DO 100, OFF=1,MAXLOADS
	    NUM_LOAD=NUM_LOAD+RE_LOADS_PER_INTERVAL(OFF,OFF_APP,SUBNETWORK,1)
100	CONTINUE
	RETURN
	END

