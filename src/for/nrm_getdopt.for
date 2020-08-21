C
C SUBROUTINE GETDOPT
C $Log:   GXAFXT:[GOLS]GETDOPT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:19:32   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:25:06   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_getdopt.for **
C
C VAX_GETDOPT.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C GETDOPT.FOR
C
C V01 01-AUG-90 TKO  INITIAL RELEASE
C
C This is a dummy routine to replace the Concurrent GETDOPT routine
C (which was used to indicate US or Foreign date format).
C
C It always returns 'US' format
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
	SUBROUTINE GETDOPT(PARAM)
	IMPLICIT NONE
C
	INTEGER*4 PARAM
C
C
	PARAM = 0
	RETURN
	END
