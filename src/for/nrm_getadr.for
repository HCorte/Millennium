C
C SUBROUTINE GETADR
C $Log:   GXAFXT:[GOLS]GETADR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:18:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:23:58   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_getadr.for **
C
C GETADR.FOR
C
C V01 12-MAR-91 TKO  INITIAL RELEASE
C
C This routine returns the address of a parameter.  The caller should really
C just do X = %LOC(Y), but this routine is provided for compatibility with
C concurrent.
C
C CALL GETADR ( X, PNTX)
C
C INPUT:      X -> address of a variable passed by reference
C
C OUTPUT:  PNTX -> address of X returned as value
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
	SUBROUTINE GETADR( X, PNTX )
	IMPLICIT NONE
C
	BYTE	    X(*)
	INTEGER*4   PNTX
C
	PNTX = %LOC(X)
C
	RETURN
	END
