C
C SUBROUTINE NOTIFY
C $Log:   GXAFXT:[GOLS]NOTIFY.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:13:26   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:08:06   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_notify.for **
C
C NRM_NOTIFY.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This routine simply provides an interface between the caller and notify1.
C Its real purpose is to determine the address of the call and pass it to
C notify1, but I don't do it here.
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
	SUBROUTINE NOTIFY (CALLADR, PARAM1, PARAM2, WAY)
	IMPLICIT NONE
C
	INTEGER*4 CALLADR
	INTEGER*4 PARAM1
	INTEGER*4 PARAM2
	INTEGER*4 WAY
C
	CALLADR = 0
	CALL NOTIFY1(CALLADR, PARAM1, PARAM2, WAY)
C
	RETURN
	END
