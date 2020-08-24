C
C PROGRAM LODIMGSS
C $Log:   GXAFXT:[GOLS]LODIMGSS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:53:44   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   15 Dec 1995 12:52:42   HXK
C  Removed AGTCOM
C  
C     Rev 1.1   25 Aug 1993 12:12:00   SXH
C  SPLIT INTO TWO (LODIMGSS2
C  
C     Rev 1.0   21 Jan 1993 16:54:02   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lodimgss.for **
C
C LODIMGSS.FOR
C
C V03 09-MAY-91 MP  ADDED CALL TO GET_WSEXTENT
C V02 01-MAY-91 MP  INITIAL RELEASE FOR MARYLAND
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This program exists solely to lock global pages in memory during
C STOPSYS processing.
C It is run by STOPSYS
C
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM LODIMGSS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:NBRCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:LTOPOL.DEF'
C**	INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:LOGCOM.DEF'
C
	INTEGER*4   ST, WORKSET_SIZE
C
	CALL COPYRITE
C
	CALL GET_WSEXTENT(WORKSET_SIZE)
	CALL WRKSET(WORKSET_SIZE)
C
	CALL LKPMEM(FRST_CONCOM, LAST_CONCOM)
	CALL LKPMEM(FRST_NETCOM, LAST_NETCOM)
	CALL LKPMEM(FRST_KIKCOM, LAST_KIKCOM)
	CALL LKPMEM(FRST_LOGCOM, LAST_LOGCOM)
	CALL LKPMEM(FRST_LTOCOM, LAST_LTOCOM)
C
	CALL LKPMEM(FRST_NBRCOM, LAST_NBRCOM)
	CALL LKPMEM(FRST_POOLLTO, LAST_POOLLTO)
	CALL LKPMEM(FRST_LTOPOL, LAST_LTOPOL)

C
C	FIRST WAIT FOR THE STATUS TO BECOME 'DSCLOS'
C
1000	CONTINUE
	CALL XWAIT(10,2,ST)		!WAIT 10 SECONDS
	IF(DAYSTS.NE.DSCLOS) GOTO 1000
C
C	AFTER THAT WAIT FOR THE STATUS TO BECOME SOMETHING ELSE...
C
2000	CONTINUE
	CALL XWAIT(5,2,ST)		!WAIT 5 SECONDS
	IF(DAYSTS.EQ.DSCLOS) GOTO 2000
C
	CALL GSTOP(GEXIT_SUCCESS)
	END
