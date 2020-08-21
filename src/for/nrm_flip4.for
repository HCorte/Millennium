C
C FUNCTION FLIP4
C $Log:   GXAFXT:[GOLS]FLIP4.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:11:36   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:19:04   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_flip4.for **
C
C FLIP4.FOR
C
C V01 31-MAY-91 TKO  Initial release
C
C This is an INTEGER*4 function to flip 4 bytes around in a longword.
C
C CALLING SEQUENCE:
C
C	I4X = FLIP4(I4Y)
C
C Input:
C	I4Y	    4 bytes in the form A-B-C-D
C
C Output:
C	FLIP4(I4Y)  Same 4 bytes as D-C-B-A
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	INTEGER*4 FUNCTION FLIP4(I4Y)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	BYTE	    I4Y(*)
C
	INTEGER*4   I4X
	BYTE	    I1X(4)
	EQUIVALENCE (I4X,I1X)
C
C
	I1X(1) = I4Y(4)
	I1X(2) = I4Y(3)
	I1X(3) = I4Y(2)
	I1X(4) = I4Y(1)
C
	FLIP4  = I4X
C
	RETURN
	END
