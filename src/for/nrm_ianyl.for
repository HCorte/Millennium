C
C FUNCTION IANYL
C $Log:   GXAFXT:[GOLS]IANYL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:34:26   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:36:52   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_strings.for **
C
C VAX_STRINGS.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This is a set of routines which will emulate the intrinsic string
C functions of Concurrent.
C
C	FUNCTION IANYL(A,B)   - Returns offset in A of any character in B
C                               (searches from left to right)
C	FUNCTION INANYL(A,B)  - Returns offset in A of first char not in B
C                               (searches from left to right)
C	FUNCTION IANYR(A,B)   - Returns offset in A of last char in B
C			        (searches from right to left)
C	FUNCTION INANYR(A,B)  - Returns offset in A of last char not in B
C				(searches from right to left)
C
C
C
C **** IANYL
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
	INTEGER*4 FUNCTION IANYL(STRING,LOOK)
	IMPLICIT NONE
C
	CHARACTER	STRING*(*)
	CHARACTER	LOOK*(*)
C
	INTEGER*4	STR$FIND_FIRST_IN_SET
	EXTERNAL	STR$FIND_FIRST_IN_SET
C
	IANYL = STR$FIND_FIRST_IN_SET( STRING, LOOK)
	RETURN
	END
