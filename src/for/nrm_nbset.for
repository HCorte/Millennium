C
C SUBROUTINE NBSET
C $Log:   GXAFXT:[GOLS]NBSET.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:09:30   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:05:28   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_bitsubs.for **
C
C VAX_BITSUBS.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C BITSUBS.FOR
C
C V02 14-MAY-92 GCAN ADDED A SET OF ROUTINES THAT WILL PERFORME BIT 
C                    OPERATIONS ON A BYTE ARRAY, COUNTING FROM LEFT.
C V01 01-AUG-90 TKO  RELEASED FOR VAX
C
C This is a set of bit manipulation routines which will set, clear, or test
C bits. There are two sets of routines. Routines in the first group count
C bits (in integer*4 words) from left to right emulating Concurrent calls.
C Bit 0 is the LEFTmost bit of the first integer*4 word, bit 31 is the
C RIGHTmost bit of the same longword, bit 32 is the LEFTmost bit of the next
C word, etc.
C Routines in the second group count bits (in integer*2 or integer*4 words)
C from right to left that is natural to VAX machines. In this case
C bit 0 is the RIGHTmost bit of the first integer*2 word, bit 15 is the
C LEFTmost bit of the same word, bit 16 is the RIGHTmost bit of the next
C word, etc. We always assume an integer*2 array is passed to the routines
C in the second group.
C Routines in the third group count bits (in byte arrays) from left to right.
C Bit 0 is the LEFTmost bit of the first byte, bit 8 is the LEFTmost bit
C in the second byte.
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
C***********************************************************************
C
C FIRST GROUP ROUTINES - COUNT BITS FROM LEFT TO RIGHT IN INTEGER*4 WORDS
C
C***********************************************************************
C
C **** NBSET - SET A BIT COUNTING FROM LEFT IN INTEGER*4 ARRAY
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE NBSET(I4ARY, BITNUM)
	IMPLICIT NONE
C
C
	INTEGER*4   I4ARY(0:*)
	INTEGER*4   BITNUM
C
	INTEGER*4   WRD
	INTEGER*4   NDX
C
C
	WRD = ISHFT(BITNUM, -8)
	NDX = IAND (BITNUM, '0000001F'X)
	I4ARY(WRD) = IBSET(I4ARY(WRD), 31-NDX)
	RETURN
	END
