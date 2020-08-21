C
C V01 12-JUN-2000 UXN Initial release.
C
C LOGICAL*4 FUNCTION ISODDSET(GTYP)
C
C Input:
C        GTYP - game type
C Return:
C        .TRUE.  if it is an oddset game type (excluding PITKA)
C        .FALSE. otherwise.
C
C LOGICAL*4 FUNCTION ISODDSET2(GTYP)
C
C Input:
C        GTYP - game type
C Return:
C        .TRUE.  if it is an oddset game type (including PITKA)
C        .FALSE. otherwise.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	LOGICAL*4 FUNCTION ISODDSET(GTYP)
	IMPLICIT NONE
	INCLUDE 'INCLIB:GLOBAL.DEF'

	INTEGER*4  GTYP
	
	INTEGER*4 I
	LOGICAL*4 GAMES(MAXTYP)
	LOGICAL*4 FIRST/.TRUE./

	IF(FIRST) THEN
	   DO I=1, MAXTYP
	      GAMES(I) = I.EQ.TWIT.OR.I.EQ.TCPL.OR.I.EQ.TDBL.OR.
     *                   I.EQ.TSTR.OR.I.EQ.TTRP.OR.I.EQ.TSCR.OR.
     *                   I.EQ.TSSC
	   ENDDO  
	   FIRST = .FALSE.
        ENDIF
	
	IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) THEN
           ISODDSET = .FALSE.
	ELSE
	   ISODDSET = GAMES(GTYP)
	ENDIF

	END  	
C****************************************************************************
C****************************************************************************
	LOGICAL*4 FUNCTION ISODDSET2(GTYP)
	IMPLICIT NONE
	INCLUDE 'INCLIB:GLOBAL.DEF'
	
	INTEGER*4 GTYP
	
	LOGICAL*4 ISODDSET
	EXTERNAL  ISODDSET

	ISODDSET2 = ISODDSET(GTYP) .OR. GTYP.EQ.TTSL

	END  	
