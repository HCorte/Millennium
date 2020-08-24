C
C FUNCTION FLIP2
C $Log:   GXAFXT:[GOLS]FLIP2.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:11:32   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:18:58   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_flip2.for **
C
C FLIP2.FOR
C
C V01 31-MAY-91 TKO  Initial release
C
C This is an INTEGER*2 function to flip 2 bytes around in a word.
C
C CALLING SEQUENCE:
C
C	I2X = FLIP2(I2Y)
C
C Input:
C	I2Y	    2 bytes in the form A-B
C
C Output:
C	FLIP2(I2Y)  Same 2 bytes as B-A
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
	INTEGER*2 FUNCTION FLIP2(I2Y)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	BYTE	    I2Y(*)
C
	INTEGER*2   I2X
	BYTE	    I1X(2)
	EQUIVALENCE (I2X,I1X)
C
C
	I1X(1) = I2Y(2)
	I1X(2) = I2Y(1)
C
	FLIP2  = I2X
C
	RETURN
	END
