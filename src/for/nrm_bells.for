C
C SUBROUTINE BELLS
C $Log:   GXAFXT:[GOLS]BELLS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:15:34   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:41:30   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_bells.for **
C
C VAX_BELLS.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C	ROUTINE TO OUTPUT BELLS
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
	SUBROUTINE BELLS(NR)
	IMPLICIT NONE
C
	INTEGER*4 NR
C
	CHARACTER*1 BELL/'07'X/
	INTEGER*4   I
C
	WRITE(5, 9999) (BELL,I=1,NR)
	RETURN
C
9999	FORMAT(' ', 100(A),$)
	END
