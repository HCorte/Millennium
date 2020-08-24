C
C SUBROUTINE INDPOL
C
C V02 07-JUL-1999 UXN MAXSCR CHANGED TO MAX_SCORE
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
	SUBROUTINE INDPOL(HOME,AWAY,INDEX)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INTEGER*4 HOME,AWAY,INDEX
C
C
	AWAY=INT(INDEX/(MAX_SCORE+1))
	HOME=INT(INDEX-(AWAY*(MAX_SCORE+1)))-1
	IF(HOME.EQ.-1) THEN
	   HOME=50
	   AWAY=ABS(AWAY-1)
	ENDIF
	RETURN
	END
