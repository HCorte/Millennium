C
C SUBROUTINE CHKSER
C $Log:   GXAFXT:[GOLS]CHKSER.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:33:38   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:51:14   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - chkser.for **
C
C CHKSER.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C SUBROUTINE TO CHECK SERIAL NUMBERS IN SECONDARY SYSTEMS
C CALLING SEQUENCE
C     CALL CHKSER(SERIAL,SIZE)
C INPUT
C     SIZE   - RECORD SIZE (IN LOG RECORDS)
C OUTPUT
C     SERIAL - TRANSACTION SERIAL NUMBER  (FIRST FOR MULTI-TRANS)
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
	SUBROUTINE CHKSER(SERIAL,SIZE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
C
	INTEGER*4 MESS(EDLEN), INDEX, MYSER, SIZE, SERIAL
C
C ASSIGN THEN INCREMENT SERIAL NUMBER
C AND INCREMENT TRANSACTION COUNT
C
10	CONTINUE
	MYSER=NXTSER
	NXTSER=NXTSER+1
C
C INCREMENT SERIAL NUMBER FOR MULTI-RECORD TRANSACTIONS
C
	IF(SIZE.EQ.3) THEN
	  INDEX=MOD(MYSER-1,LBLK)+1
	  IF(INDEX.EQ.LBLK-1)THEN
	    NXTSER=NXTSER+1
	  ELSE IF(INDEX.LT.LBLK-1)THEN
	    NXTSER=NXTSER+2
	  ENDIF
	  GOTO 20
	ENDIF
C
	IF(SIZE.EQ.2) THEN
	  INDEX=MOD(MYSER-1,LBLK)+1
	  IF(INDEX.EQ.LBLK) GOTO 20
	  NXTSER=NXTSER+1
	ENDIF
C
20	CONTINUE
	IF(MYSER.NE.MOD(SERIAL,SYSOFF)) THEN
	  MESS(1)=DIS
	  MESS(2)=TENET
	  MESS(3)=6
	  MESS(4)=SERIAL
	  MESS(5)=MYSER
	  MESS(6)=TRNMD
	  CALL QUEMES(MESS)
	  NXTSER=MOD(SERIAL,SYSOFF)
	  GOTO 10
	ENDIF
	P(NXTTRA)=P(NXTTRA)+1
	PERFRM(1,PERTRA)=PERFRM(1,PERTRA)+1
	RETURN
	END
