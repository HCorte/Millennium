C
C SUBROUTINE CLRSCR
C $Log:   GXAFXT:[GOLS]CLRSCR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:37:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:54:44   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_clrscr.for **
C
C VAX_CLRSCR.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C CLRSCR.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 16-JAN-89 LMF INITIAL RELEASE FOR INDIANA (FROM ILL)
C V01 01-JUN-88 XXX RELEASED FOR MICHIGAN
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
	SUBROUTINE CLRSCR (LUN)
	IMPLICIT NONE
C
	INTEGER*4	LUN
C
	CHARACTER*1	ESC	    /Z1B/	    ! ESCAPE CHARACTER
	CHARACTER*3     CLRSCRN     /'[2J'/         ! CLEAR SCREEN
	CHARACTER*2     HOMECUR     /'[H'/          ! HOME CURSOR
C
	WRITE (LUN, 101)ESC//CLRSCRN, ESC//HOMECUR
101	FORMAT('+',A,A,$)
	RETURN
	END
