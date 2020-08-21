C
C SUBROUTINE CHKTAB
C $Log:   GXAFXT:[GOLS]CHKTAB.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:34:06   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:51:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_chktab.for **
C
C VAX_CHKTAB.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C CHKTAB.FOR
C
C V01 16-JUL-90 TKO  RELEASED FOR VAX
C
C This emulates the CHKTAB.MAC routine on Concurrent (in FAST1.MAC)
C
C This routine will perform a checksum of a table by rotating the result
C 1 bit to the right before acding the next element.
C
C CALL CHKTAB(RESULT,INARY,LEN)
C
C	RESULT:	I*4 CHECKSUM RESULT
C	INARY:	I*4 ARRAY FOR INPUT
C	LEN:	# OF I*4 WORDS IN TABLE
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
	SUBROUTINE CHKTAB(RESULT, INARY,LEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INTEGER*4   RESULT
	INTEGER*4   INARY(*)
	INTEGER*4   LEN
C
	INTEGER*4   K
C
C
	RESULT = 0
	DO 1100 K = 1, LEN
	  RESULT = ISHFTC(RESULT, 1, 32) + INARY(K)
1100	CONTINUE
C
	RETURN
	END
