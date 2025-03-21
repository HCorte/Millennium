C
C PROGRAM CMDPRO
C $Log:   GXAFXT:[GOLS]CMDPRO.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:38:48   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:56:38   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - cmdpro.for **
C
C CMDPRO.FOR
C
C V03 09-MAY-91 MP  ADDED CALL TO SNIF_AND_WRKSET
C V02 22-JAN-91 KWP REMOVED LINCOM/TERCOM
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C COMMAND PROCESSING TASK
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
	PROGRAM CMDPRO
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:QUECOM.DEF'
	INTEGER*4 MESS(EDLEN), LINE, BUF, STATUS, TASK
C
C
	CALL COPYRITE
C V03
	CALL SNIF_AND_WRKSET
C
C
	TASK=CMD
	MESS(1)=TASK
C
C WAIT FOR SOMETHING TO DO
C IF END OF DAY THEN CALL GSTOP(GEXIT_SUCCESS)
C
	P(CMDFRZ)=0
	GOTO 20
10	CONTINUE
	IF(DAYSTS.EQ.DSCLOS) CALL GSTOP(GEXIT_SUCCESS)
	CALL HOLD(0,STATUS)
	IF(DAYSTS.EQ.DSSUSP) GOTO 10
C
C GET BUFFER NUMBER FROM TOP OF COMMAND QUEUE.
C IF NO COMMANDS QUEUED, GO BACK TO WAIT STATE.
C
20	CONTINUE
	CALL TOPQUE(TASK,BUF)
	IF(BUF.EQ.0) GOTO 10
C
C FILL IN TRANSACTION BUFFER FOR COMMAND
C
	TRABUF(TSTAT)=GOOD
	TRABUF(TERR)=NOER
	TRABUF(TGAM)=TIN
	TRABUF(TSER) =PRO(SERIAL,BUF)
	TRABUF(TTIM) =PRO(TSTAMP,BUF)
	TRABUF(TCDC) =DAYCDC
	TRABUF(TTER) =0
	TRABUF(TTYP)=TCMD
	TRABUF(TTRN) =0
	TRABUF(TINTRA)=1
	TRABUF(TAGT)=PRO(CAGT,BUF)
	TRABUF(TCMNUM)=PRO(CNUM,BUF)
	TRABUF(TCMNEW)=PRO(CVAL,BUF)
	TRABUF(TCMTYP)=PRO(CTYP,BUF)
	TRABUF(TCMLIN)=PRO(CLIN,BUF)
	TRABUF(TCMTER)=PRO(CTER,BUF)
	TRABUF(TCMSRC)=PRO(CSRC,BUF)
	TRABUF(TCMDT1)=PRO(CDT1,BUF)
	TRABUF(TCMDT2)=PRO(CDT2,BUF)
	TRABUF(TCMDT3)=PRO(CDT3,BUF)
	TRABUF(TCMDT4)=PRO(CDT4,BUF)
	TRABUF(TCMDT5)=PRO(CDT5,BUF)
	TRABUF(TSIZE)=HPRO(NUMLRC,BUF)
C
C PROCESS AND LOG COMMAND
C
C      CALL OPSTXT('CALLED CMDPRO!!!')
C      CALL OPS('TRABUF(TCMTYP)=',TRABUF(TCMTYP),TRABUF(TCMTYP))
C      CALL OPS('TRABUF(TCMNEW)=',TRABUF(TCMNEW),TRABUF(TCMNEW))
	CALL CMDROU(TRABUF,MESS)
	IF(TRABUF(TERR).NE.NOER) TRABUF(TSTAT)=REJT
	CALL TRALOG(TRABUF,PRO(WRKTAB,BUF))
	CALL WLOG(TRABUF(TSER),PRO(WRKTAB,BUF),TASK)
	IF(MESS(2).NE.0) CALL QUEMES(MESS)
C
C PROCESS MESSAGE BROADCAST COMMANDS
C
	IF(TRABUF(TCMTYP).EQ.TCSPE.AND.TRABUF(TCMNUM).EQ.5) THEN
	  HPRO(LINENO,BUF)=TRABUF(TCMLIN)	!SAP #
	  PRO(CNUM,BUF)=0
	  HPRO(OUTLEN,BUF)=0
	  HPRO(TRCODE,BUF)=TYPUNS
	  HPRO(MSGNUM,BUF)=TRABUF(TCMNEW)
	  HPRO(TERNUM,BUF)=TRABUF(TCMTER)
	ENDIF
C
C
	CALL QUETRA(LOG,BUF)
	CALL DQUTRA(TASK,BUF)
C
C IF SYSTEM FROZEN THEN CLEAR CMDFRZ,
C RELEASE DISPATCHER AND GO BACK TO
C WAIT STATE, ELSE CONTINUE DEQUEING
C COMMANDS FROM COMMAND QUEUE.
C
	IF(P(CMDFRZ).EQ.1) THEN
	  P(CMDFRZ)=0
	  CALL RELSE(TSKNAM(DIS),STATUS)
	  GOTO 10
	ENDIF
	GOTO 20
	END
