C
C FUNCTION QUECNT
C $Log:   GXAFXT:[GOLS]QUECNT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:36:12   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:25:06   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vax_quecnt.for;1 **
C
C VAX_QUECNT.FOR
C
C V02 08-MAR-91 TKO USE LISTSIZE
C V01 11-SEP-90 MRM RELEASED FOR VAX
C
C COUNT=QUECNT(QUEUE)
C
C COUNT   - INT*4
C QUEUE   - PE LIST
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
	INTEGER*4 FUNCTION QUECNT(QUEUE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INTEGER*4 QUEUE(*)
C
	INTEGER*4 SIZE
C
	CALL LISTSIZE(QUEUE, SIZE)
	QUECNT = SIZE
C
	RETURN
	END
