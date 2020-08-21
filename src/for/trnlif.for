C
C SUBROUTINE TRNLIF
C $Log:   GXAFXT:[GOLS]TRNLIF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:36:56   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:53:04   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_trnlif.for **
C
C TRNLIF.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C
C     TRNLIF.FTN
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
	SUBROUTINE TRNLIF(MIN,MAX,AVRAGE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
C
	INTEGER*4  MAXS
	PARAMETER (MAXS=9)
C
	INTEGER*4 STATUS, TIME, STAOFF, AVRAGE, MAX, MIN
	INTEGER*4 STAB(0:MAXS)
	INTEGER*4 MAXTIM
	INTEGER*4 MINTIM
	DATA MAXTIM/0/,MINTIM/123456789/
	DATA STAB/10*0/
	INTEGER*4 TOT
	DATA TOT/0/
	REAL*8 AVG
	SAVE STAOFF
C
10	CONTINUE
	CALL RTL(TIME,PRFQUE,STATUS)
	IF (STATUS.EQ.1) STATUS=0
	IF (STATUS.NE.0) RETURN           !IF CANNOT GET 1
	IF (TIME.LT.0) GOTO 10            !INVALID TIME
	STAOFF=MOD(STAOFF+1,MAXS+1)
	TOT=TOT-STAB(STAOFF)+TIME          ! TOTAL FOR LAS N TRANSACTIONS
	STAB(STAOFF)=TIME
	IF (TIME.EQ.0) GOTO 10
	IF (TIME.GT.MAXTIM) MAXTIM=TIME
	IF (TIME.LT.MINTIM) MINTIM=TIME
	IF (STAOFF.EQ.0) THEN
	MIN=MINTIM
	MAX=MAXTIM
	AVG=TOT/(MAXS+1)
	AVRAGE=AVG
C
	MINTIM=123456789
	MAXTIM=0
	RETURN
	ENDIF
	GOTO 10
	END
