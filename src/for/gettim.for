C
C SUBROUTINE GETTIM
C $Log:   GXAFXT:[GOLS]GETTIM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:23:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:29:18   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - gettim.for;1 **
C
C GETTIM.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C SUBROUTINE TO TIME STAMP TRANSACTIONS
C CALLING SEQUENCE
C     CALL GETTIM(TIME)
C INPUT
C     NONE
C OUTPUT
C     TIME - ELAPSED TIME IN SECONDS SINCE MIDNIGHT
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
	SUBROUTINE GETTIM(TIME)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 TIME
C
	CALL ICLOCK(2,TIME)
	RETURN
	END
