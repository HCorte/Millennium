C
C PROGRAM DISNET
C $Log:   GXAFXT:[GOLS]DISNET.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:54:10   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:05:20   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - disnet.for **
C
C DISNET.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C
C     PROGRAM TO DISABLE ACTIVE CONNECTIONS
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM DISNET
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
C
	INTEGER*4 ST, OFF, OFF1
C
	CALL COPYRITE
C
C
	DO 20, OFF1=1,NUMWAY
	DO 10 OFF=1,NETSYS
	   IF (NETROUT(OFF,OFF1).EQ.ROUACT) THEN
	     NETROUT(OFF,OFF1)=ROUIDLE
	     NETSTAT(OFF,OFF1)=NSTAIDLE
	   ENDIF
10	CONTINUE
	NETBACKUP(OFF1)=0
20	CONTINUE
C
C
	CALL XWAIT(5,2,ST)
C
C
	TYPE *,IAM(),'Connections to other systems disabled'
	CALL GSTOP(GEXIT_SUCCESS)
C
	END
