C
C SUBROUTINE XTIM
C $Log:   GXAFXT:[GOLS]XTIM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 16:47:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 18:38:54   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_xtim.for **
C
C VAX_XTIM.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C XTIM.FOR
C
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE XTIM( TIMARY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4   TIMARY(3)
	REAL*4	    SECNDS
	INTEGER*4   TEMP
C
	TEMP = SECNDS(0.0)
	TIMARY(1) = TEMP/3600
	TIMARY(2) = (TEMP - (TEMP/3600)*3600)/60
	TIMARY(3) =  TEMP - (TEMP/60)*60
C
	RETURN
	END
