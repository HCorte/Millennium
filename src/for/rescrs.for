C  GXSRC:RESCRS.FOR
C  
C  $Log:   GXAFXT:[GOLS]RESCRS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:43:36   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   03 Jan 1994 22:35:32   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 18:27:18   SYSTEM
C  Initial revision.
C
C
C
C V01 18-NOV-91 KWP INITIAL RELEASE FOR VAX
C
C This is called by RESET to clear CRSCOM and to initialize the timer
C lists.
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
	SUBROUTINE RESCRS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:CRSCOM.DEF'
C
C
C ZERO OUT THE TIMER LIST
C
	CALL FASTSET(0,TIMERLIST,4*(NUMPRO+2))
	TIMERTOP=-1                    !TIMER LIST PROCESSING AREA
	TIMERBOT=-1
C
C ZERO OUT THE TC BUFFERS
C
	CALL FASTSET(0,TCBUF,TCBUFSIZ*TCBUFMAX)
C
	RETURN
	END
