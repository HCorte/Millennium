C
C SUBROUTINE QUEMES
C $Log:   GXAFXT:[GOLS]QUEMES.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:36:36   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:25:34   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_quemes.for **
C
C QUEMES.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C SUBROUTINE TO QUEUE SYSTEM ERROR MESSAGES
C TO BE LOGGED BY ERROR LOGGING TASK.
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
	SUBROUTINE QUEMES(MESS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
C
	INTEGER*4 MESS(EDLEN)
C
	INTEGER*4 I,STATUS,BUF
C
C
C
C IF MESSAGE QUEUE FULL OR NO MESSAGE LOGGING RETURN
C
	!PARAMETER (MESLOG=22) !MESSAGE LOGGING DEVICES
	!PARAMETER (MESFUL=24)   !MESSAGE QUEUE FULL FLAG
	!PARAMETER (MAXMES=23)   !MAXIMUM ERRLOG QUE SIZE
	IF(P(MESLOG).EQ.0.OR.P(MESFUL).EQ.1) RETURN 
	IF(ACTTSK(ERR).GE.P(MAXMES)) THEN !ACTTSK(MAXTSK)           ACTIVE TASK LIST                     32       34
	  P(MESFUL)=1
	  CALL RELSE(TSKNAM(ERR),STATUS)
	  RETURN
	ENDIF
C
C ALLOCATE PROCOM PROCESSING BUFFER
C IF NO BUFFERS AVAILABLE THEN RETURN
C
	CALL GETBUF(BUF)
	IF(BUF.EQ.0) RETURN
C
C TRANSFER MESSAGE TO PROCOM BUFFER
C
	DO 10 I=1,EDLEN !PARAMETER (EDLEN=20)        !MESSAGE DATA LENGTH
	PRO(ETASK+I-1,BUF)=MESS(I) !procom.def <-----> PARAMETER (ETASK=49) !TASK ID NUMBER
	!starts at 49 and can reach to 49+20=69
10	CONTINUE
C
C QUEUE MESSAGE TO ERRLOG QUEUE AND RELEASE ERRLOG TASK
C
	HPRO(TRCODE,BUF)=TYPERR
	CALL QUETRA(ERR,BUF)
	CALL RELSE(TSKNAM(ERR),STATUS)
	RETURN
	END
