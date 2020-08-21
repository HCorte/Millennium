C PROGRAM SETLOTSHDR
C
C V01 25-JAN-2011 HXK  INITIAL RELEASE FOR PORTUGAL - LOTO2 CHANGES
C
C SET HI DRAW FOR LOTS (TOTOLOTO SABADO) ONLY (GAME 6)
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
C Copyright 2011 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C======	OPTIONS /CHECK=NOOVERFLOW
	PROGRAM SETLOTSHDR
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
	COMMON SCFREC
	INTEGER*4 ST
C
C
	CALL COPYRITE
C
C READ SCF RECORD
C
	CALL GETSCONF(SCFREC,ST)
C
	CALL LOTSHIDRAW
C
	CALL GSTOP(GEXIT_SUCCESS)
C
C
	END
