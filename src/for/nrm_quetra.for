C
C SUBROUTINE QUETRA
C $Log:   GXAFXT:[GOLS]QUETRA.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:36:48   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:25:50   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_appque.for **
C
C APPQUE.FOR
C
C V02 08-MAR-91 TKO CHANGE TO USE LISTSIZE,LIB$ADAWI
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
C
C	THIS MODULE CONTAINS THE FOLLOWING SUBROUTINES
C		QUETRA, DQUTRA, TOPQUE
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
C	QUETRA - QUEUES TRANSACTION TO A SPECIFIED TASK
C
C***********************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE QUETRA (TASK, BUFNUM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:QUECOM.DEF'
	INCLUDE 'INCLIB:GLIST.DEF'
C
	INTEGER*4	TASK
	INTEGER*4	BUFNUM
C
	INTEGER*4	STAT, RESULT
C
	CALL ABL (BUFNUM, QUETAB(1, TASK), STAT)
C
	IF (STAT .EQ. GLIST_STAT_GOOD) THEN
	  CALL LIB$ADAWI(1, ACTTSK(TASK), RESULT)
	ENDIF
C
	RETURN
	END
