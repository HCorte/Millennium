C SUBROUTINE GETENC
C  
C     Rev 1.0   17 Apr 1996 13:19:36   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C     Rev 1.0   21 Jan 1993 16:25:12   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C V01 01-AUG-90 XXX RELEASED FOR VAX
C V01 22-JUN-89   LOU 2.   INITIAL RELEASE FOR SWEDEN.
C
C SUBROUTINE TO PROCESS ENCRYPTION KEY REQUESTS.
C
C CALLING SEQUENCE:
C     CALL GETENC(BUF)
C INPUT
C     BUF    - PROCOM BUFFER NUMBER
C OUTPUT
C     NONE
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
	SUBROUTINE GETENC(BUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
C
	INTEGER*4 ONFLAG, TEMP, KEY, BUF
C
C***	TER=HPRO(TERNUM,BUF)
	KEY=0
C***	KEY=TERTAB(ENCKEY,TER)
	TEMP=IEOR(KEY,SYSKEY)
	ONFLAG=1
	CALL MOVBYT(ONFLAG,4,HPRO(INPTAB,BUF),3,1)
	CALL MOVBYT(TEMP,3,HPRO(INPTAB,BUF),4,2)
	HPRO(OUTLEN,BUF)=5
	HPRO(ENCOVR,BUF)=-1
	HPRO(SPCFUN,BUF)=1
	RETURN
	END
