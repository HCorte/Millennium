C
C BLOCK DATA X2XDEBUG
C $Log:   GXAFXT:[GOLS]X2XDEBUG.FOV  $
C  
C     Rev 1.0   17 Apr 1996 16:42:28   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   29 Sep 1993 16:42:34   JWE
C  Initial revision.
C  
C     Rev 1.0   21 Jan 1993 18:37:36   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - X2XCOM.for;1 **
C
C X2XDEBUG.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	PROGRAM X2XDEBUG
	IMPLICIT NONE
C
	INCLUDE	'INCLIB:SYSPARAM.DEF'
	INCLUDE	'INCLIB:SYSEXTRN.DEF'
C
C NAME: X2XDEBUG.FTN
C
	INCLUDE	'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XQUE.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
C
	CALL COPYRITE
C
	CALL GSTOP(GEXIT_SUCCESS)
	END
