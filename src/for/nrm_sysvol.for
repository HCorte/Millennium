C
C SUBROUTINE SYSVOL
C $Log:   GXAFXT:[GOLS]SYSVOL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:26:36   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:47:50   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_getvol.for **
C
C GETVOL.FOR
C
C V02 03-MAY-91 TKO Default to SYSX
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C DUMMY GETVOL/SYSVOL ROUTINES
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
C	THE FOLLOWING ROUTINE IS NOT CALLED...
C	SUBROUTINE GETVOL(DUMMY)
C	IMPLICIT NONE
C	INTEGER*4   DUMMY
C	RETURN
C	END
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SYSVOL(VOLUME)
	IMPLICIT NONE
C
	INTEGER*4   VOLUME(*)
C
	CHARACTER   CXVOL*4
	INTEGER*4   I4VOL
	EQUIVALENCE (CXVOL,I4VOL)
	DATA	    CXVOL/'SYSX'/
C
	VOLUME(1) = I4VOL
	RETURN
	END
