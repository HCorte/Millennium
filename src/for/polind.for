C
C SUBROUTINE POLIND
C
C V03 07-JUL-1999 UXN MAXSCR CHANGED TO MAX_SCORE
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C SCORE POOL ROUTINES
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
	OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE POLIND(HOME,AWAY,INDEX)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INTEGER*4 HOMA,AWAY,INDEX
	IF(HOME.LT.0.OR.AWAY.LT.0) THEN
	  INDEX=0
	ELSE
	  INDEX=(HOME+1)+(AWAY*(MAX_SCORE+1))
	ENDIF
	RETURN
	END
