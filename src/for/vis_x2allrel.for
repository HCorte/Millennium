C
C SUBROUTINE X2ALLREL
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2ALLREL.FOV                                 $
C  $Date::   17 Apr 1996 16:07:32                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_x2allrel.for;1 **
C
C X2ALLREL.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF, AGTINF.DEF for Finland.
C
C V04 12-DEC-94 GPR Integrate UK changes into X2X Baseline
C V03 20-OCT-94 GPR Change format to I5 for Act, Req, Max
C V02  7-MAR-94 JWE Use available capacity instead of max capacity
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This snapshot will display all of the current relay information
C for a given relay application task.
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
	SUBROUTINE X2ALLREL
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2VIS.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
	INCLUDE 'INCLIB:X2XGRP.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4   ACTCNT                      !Active group count
	INTEGER*4   MAXCAP / 0 /                !Max frontend capacity
	INTEGER*4   QUECNT, REL, I, LINIDX
	CHARACTER   STATUS(0:2)*6               !Relay status
	CHARACTER   ATTRIB(0:4)*6               !Type of relay application
C
	DATA        STATUS  /'notdef','  idle','active'/
      DATA        ATTRIB  /'notdef',' chain','allrel','allbro','stnbro'/
C
	WRITE(CLIN1,9000)
	WRITE(CLIN2,9010)
	WRITE(CLIN3,9020)
	WRITE(CLIN4,9030)
	LINIDX=5
C
C
C DISPLAY ALL RELAY APPLICATION TASKS.
C
	DO 50 REL=1,X2X_RELAY_APPS
C
C COUNT THE NUMBER OF ACTIVE RELAY GROUPS FOR EACH
C RELAY APPLICATION TASK.
C
	  ACTCNT=0
	  DO 100 I=1,X2X_NUM_GROUPS
	    IF(X2XR_GROUP_ACTIVE(I,REL).NE.0) THEN
	      ACTCNT=ACTCNT+1
	    ENDIF
100	  CONTINUE
C
C DISPLAY RELAY APPLICATION IDENTIFICATION.
C
C ***** Start V04 changes *****
C
C DETERMINE THE TOTAL CAPACITY OF ALL THE FRONT ENDS.
C
	  MAXCAP=0
	  DO 20 I=1,X2X_SAP
	    IF (X2XR_SUBNETWORK(REL).EQ.X2XE_SUBNETWORK(I))
     *		     	  MAXCAP=MAXCAP+X2XE_MAX_CAPACITY(I)
20	  CONTINUE
C
C ***** End V04 changes *****
C
	  WRITE(XNEW(  LINIDX),9040) REL, STATUS(X2XR_APP_STATUS(REL)),
     *	                         ATTRIB(X2XR_APP_ATRIBUTE(REL)),
     *	                         X2XR_NO_ACTIVE(REL),
     *	                         X2XR_NO_REQUEST(REL),
     *	                        (MAXCAP*X2XR_MAX_ACTIVE(REL))/100+1,
     *	                         X2XR_SEND_TIMOUT(REL)/1000,
     *	                         X2XR_SUBNETWORK(REL),			  !V04
     *	                         X2XR_ACTIVITY_CNT(REL),
     *	                         X2XR_NON_ACTIVE_CNT(REL),
     *	                         X2XR_TIMOUT_CNT(REL),
     *	                         QUECNT(X2XR_APP_QUEUE(1,REL)),
     *	                         ACTCNT
	  LINIDX=LINIDX+1
50	CONTINUE
C
C PROGRAM EXIT.
C
	RETURN
C
C     ================== Format Statements ===================
C
9000	FORMAT('Relay Broadcast snapshot')
9010	FORMAT(T21,'==== Relay ====',T38,'Send',T43,'Snet',T58,'Non',	!V04
     *	       T62,'Timeout',T70,'Queue',T77,'Act')			!V04
9020	FORMAT(T4,'Status',T12,'Attrib',T20,' Act   Req   Max',		!V03
     *	       T38,'Time',T43,' no ',T51,'Acty',T56,'Active',
     *	       T64,'Cnt',T71,'Cnt',T75,'Group')
9030	FORMAT(79('='))
9040	FORMAT(I2,T4,A6,T12,A6,T19,I5,T25,I5,T31,I5,T37,I5,T43,I3,	!V03
     *	       T47,I8,T56,I5,T62,I5,T69,I5,T76,I4)			!V03
	END
