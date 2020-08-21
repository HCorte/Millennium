C
C SUBROUTINE BITOR
C $Log:   GXAFXT:[GOLS]BITOR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:17:12   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:42:34   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_bitor.for **
C
C VAX_BITOR.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C BITOR.FOR
C
C V01 01-AUG-90 TKO  RELEASED FOR VAX
C
C This will 'or' together the bits in 2 byte arrays
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE BITOR(BARY1, BARY2, NUMBYT, DEST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	BYTE		BARY1(*)
	BYTE		BARY2(*)
	INTEGER*4	NUMBYT
	BYTE		DEST(*)
C
	INTEGER*4	TEMP1
	INTEGER*4	TEMP2
	INTEGER*4	K
C
C
	DO 1100 K = 1, NUMBYT
	  TEMP1 = BARY1(K)
	  TEMP2 = BARY2(K)
	  DEST(K) = IOR(TEMP1, TEMP2)
1100	CONTINUE
C
	RETURN
	END
