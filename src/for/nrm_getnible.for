C
C SUBROUTINE GETNIBLE
C $Log:   GXAFXT:[GOLS]GETNIBLE.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:21:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:26:52   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_nibble.for **
C
C NIBLE.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C     ++++++++++++++++++++++++++++++++++++++
C
C     GET NIBBLE
C     GETNIBLE(VALUE,TAB,OFF)
C     IN:
C     TAB - TABLES OF NIBLES
C     OFF - NIBBLE OFFSET (STARTING FROM 1)
C     OUT:
C     VALUE - NIBBLE
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETNIBLE(VALUE,TAB,OFF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INTEGER*2 TAB(*)
	INTEGER*4 DATA, OFFSET, OFF, VALUE
C
	IF (OFF.LE.0) RETURN
	OFFSET=(OFF+1)/2-1
	CALL ILBYTE(DATA,TAB,OFFSET)
	IF (MOD(OFF,2).NE.0) THEN
	   VALUE=DATA/16
	ELSE
	   VALUE=IAND(DATA,15)
	ENDIF
	RETURN
	END
