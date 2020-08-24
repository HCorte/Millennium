C
C SUBROUTINE HOLD
C $Log:   GXAFXT:[GOLS]HOLD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:32:30   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:35:26   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_hold.for **
C
C HOLD.FOR
C
C V02 12-APR-91 MP  TOSIMPLIFY THE MATTER, ELIMINTED THE OPTION
C		    OF WAIT INSTEAD OF 'SYS$HIBER'
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE HOLD (STR, STAT)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE	'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4 STR
	INTEGER*4 STAT
C
	STAT = SYS$HIBER()
	IF (.NOT. STAT) THEN
	    TYPE *,IAM(), 'BAD STATUS FROM ''SYS$HIBER'' '
	    CALL LIB$SIGNAL(%VAL(STAT))
	ENDIF
C
	RETURN
	END
