C
C SUBROUTINE CHECKSUM
C $Log:   GXAFXT:[GOLS]CHECKSUM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:30:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:47:52   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_checksum.for **
C
C VAX_CHECKSUM.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C CHECKSUM.FOR
C
C V01 01-AUG-90 TKO  RELEASED FOR VAX
C
C This is the fortran version of the SZREK checksum algorithm
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
	SUBROUTINE CHECKSUM(BARY, BEGOFF, NUMBYT, RESULT)
	IMPLICIT NONE
C
	BYTE		BARY(0:*)
	INTEGER*4	BEGOFF
	INTEGER*4	NUMBYT
	INTEGER*4	RESULT
C
	INTEGER*4	NDX
	INTEGER*4	BVAL
C
	RESULT = 0
	DO 7777 NDX = 0, NUMBYT-1
	  BVAL = BARY(BEGOFF+NDX)
	  BVAL = IAND(BVAL, '000000FF'X)
	  RESULT = NDX + (RESULT+BVAL)*37
	  RESULT = IAND(RESULT,'000000FF'X)
7777	CONTINUE
C
	RETURN
	END
