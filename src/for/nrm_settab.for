C
C SUBROUTINE SETTAB
C $Log:   GXAFXT:[GOLS]SETTAB.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:02:42   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:36:44   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_settab.for **
C
C VAX_SETTAB.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SETTAB.FOR
C
C V01 16-JUL-90 TKO  RELEASED FOR VAX
C
C This emulates the SETTAB.MAC routine on Concurrent (in FAST1.MAC)
C
C I don't know why walter wrote it.  It seems to be the same as FASTSET
C
C CALL SETTAB(VALUE,OUARY,LEN)
C
C	VALUE:	I*4 VALUE TO SET
C	OUARY:	I*4 ARRAY FOR OUTPUT
C	LEN:	# OF I*4 WORDS TO SET
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
	SUBROUTINE SETTAB(VALUE,OUARY,LEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INTEGER*4   VALUE
	INTEGER*4   OUARY(*)
	INTEGER*4   LEN
C
	INTEGER*4   K
C
C
	DO 1100 K = 1, LEN
	  OUARY(K) = VALUE
1100	CONTINUE
C
	RETURN
	END
