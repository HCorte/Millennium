C
C SUBROUTINE ASFOPEN
C $Log:   GXAFXT:[GOLS]ASFOPEN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:13:22   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:40:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - asfopen.for **
C
C ASFOPEN.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE ASFOPEN(XLUN)
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
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:ASFSUBS.DEF'
C
	INTEGER*4 XLUN, ST
C
	LUN=XLUN
	CALL OPENFILE(LUN,ASF,ST)
	IF(ST.NE.0)THEN
	  CALL FILERR('ASF.FIL',1,ST,0)
	ENDIF
C
	CALL IOINIT(FDB,LUN,ASFSEC*RECSPERBKT*256)
C
	BKTNUM=-1                   !FORCE A READ
	BKTCHG=0                    !NOTHING HAS BEEN CHANGED
C
	RETURN
	END
