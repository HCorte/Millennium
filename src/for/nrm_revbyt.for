C
C SUBROUTINE REVBYT
C $Log:   GXAFXT:[GOLS]REVBYT.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:44:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:31:08   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_revbyt.for **
C
C VAX_MOVBYT.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C REVBYT.FOR
C
C
C This emulates the REVBYT.MAC routine on Concurrent
C
C CALL REVBYT(INARY,INOFF,OUARY,OUOFF,LEN)
C
C	INARY:	BYTE ARRAY
C	INOFF:	STARTING OFFSET
C	OUARY:	BYTE ARRAY FOR OUTPUT
C	OUOFF:	STARTING OFFSET
C	LEN:	# OF BYTES TO MOVE
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
	SUBROUTINE REVBYT(INARY,INOFF,OUARY,OUOFF,LEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	BYTE	    INARY(*)
	INTEGER*4   INOFF
	BYTE	    OUARY(*)
	INTEGER*4   OUOFF
	INTEGER*4   LEN
	INTEGER*4   I
C
	DO 10 I=1,LEN
	OUARY(OUOFF+I-1)=INARY(INOFF-I+1)
10	CONTINUE
	RETURN
	END
