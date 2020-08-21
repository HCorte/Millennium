C
C SUBROUTINE QUEINP
C $Log:   GXAFXT:[GOLS]QUEINP.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:36:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:25:18   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_quemgr.for **
C
C QUEMGR.FOR
C
C V03 12-MAR-91 TKO CHANGED FOR NEW LOCKING
C V02 30-JAN-91 KWP REMOVED QUEOUT( NOT NEEDED FOR X2X)
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C QUEMGR
C
C V01 28-JUN-90 MP  INITIAL RELEASE
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
C
C	THIS MODULE CONTAINS THE FOLLOWING SUBROUTINES
C		QUEINP, DQUINP, GETBUF, RELBUF
C	THEY WORK AS CORRESPONDING ROUTINES ON CONCURRENT
C	OS/32 MACHINES.
C
C	THESE ROUTINES AS WELL AS OTHER QUING ROUTINES UTILIZE
C	LISTS WITH EXTENDED HEADERS AND USE APPROPRIATE
C	DEFLST, ABL, RTL ROUTINES (CONVERTED)
C
C
C************************************************************
C
C	QUEINP - QUEUES TRANSACTION TO INPUT QUEUE
C
C***********************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE QUEINP (BUFNUM, RETSTAT)
	IMPLICIT NONE
C
	INTEGER*4	RETSTAT
	INTEGER*4	BUFNUM
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:GLIST.DEF'
C
	INTEGER*4	STAT
C
C
	RETSTAT = 0
C
	CALL ABL (BUFNUM, INQUE, STAT)
C
	IF (STAT .NE. GLIST_STAT_GOOD) THEN
		RETSTAT = -1
	ENDIF
C
	END
