C
C PROGRAM DAYEND
C
C V08 13-NOV-97 UXN SNIF_AND_WRKSET added.
C
C $Log:   GXAFXT:[GOLS]DAYEND.FOV  $
C  
C     Rev 1.3   18 Dec 1996 11:57:58   HXK
C  Update from TEBE project (MXP,WXW,PXN,MJF)
C  
C     Rev 1.4   09 Dec 1996 15:22:32   WXW
C  Change x2x maint options value to nite mode.
C  
C     Rev 1.3   28 Nov 1996 18:11:00   WXW
C  Telebetting startup, changes MP/PXN/WXW.
C  Pstteb added.
C  
C     Rev 1.2   17 May 1996 11:42:24   HXK
C  Update from Wojtek, Siew Mun
C  
C     Rev 1.1   13 Jul 1993 14:14:26   SXH
C  Released for Finland
C  
C     Rev 1.0   21 Jan 1993 16:02:20   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - dayend.for **
C
C DAYEND.FOR
C
C V02 17-APR-91 MP  CHECK THE COMPLETEION OF CHECKPOINTS
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
	PROGRAM DAYEND
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
C
	INTEGER*4  CBUF(EDLEN)                !
	INTEGER*4  YNFLG                      !
	INTEGER*4  I                          !
	INTEGER*4  ST                         !
	INTEGER*4  CMDVALUE(4)  /4*-1/        !
        INTEGER*4  STATUS
        INTEGER*4  TSKSTS

C
C V02	DEFINE THE COUNT FOR THE WAITING LOOP
C
	INTEGER*4  COUNT                      !
	INTEGER*4  MAXCOUNT                   !
	INTEGER*4  DELAY                      !
C
	CALL COPYRITE
	CALL SNIF_AND_WRKSET
C
C
	CBUF(6)='SYS '            !COMMAND SOURCE (SYSTEM)
C
C CHANGE X2X MAINT OPTIONS VALUE TO NITE MODE.
C
5       CONTINUE
        CMDVALUE(1)=1
        CALL X2BLDCMD(1,XGBL,1,14,CMDVALUE,2,1,CBUF)
        CALL QUECMD(CBUF,ST)
        IF(ST.NE.0) THEN
          TYPE*,IAM(),'Queue command error for x2x nite game  '
          CALL XWAIT(1,2,ST)
          GOTO 5
        ENDIF
C
        TYPE*,IAM(),'Wait 15 sec to send night game command...  '
        CALL XWAIT(15,2,ST)
C
C CHANGE X2X GAME STATE CHANGE TO SHUTDOWN.
C
7	CONTINUE
	CMDVALUE(1)=4
C***	CALL X2BLDCMD(1,XGBL,1,25,CMDVALUE,2,1,CBUF)
	CALL X2BLDCMD(1,XGBL,1,22,CMDVALUE,2,1,CBUF)
	CALL QUECMD(CBUF,ST)
	IF(ST.NE.0) THEN
	  TYPE*,IAM(),'Queue command error for x2x network shutdown  '
	  CALL XWAIT(1,2,ST)
	  GOTO 7
	ENDIF
C
C SET SYSTEM DORMANT
C
10	CONTINUE
	CBUF(1)=SYSSTS
	CBUF(2)=SYSDOR
	CBUF(3)=TCPAR
	CALL QUECMD(CBUF,ST)
	IF(ST.NE.0) THEN
	  TYPE*,IAM(),'Queue command error for system dormant  '
	  CALL XWAIT(1,2,ST)
	  GOTO 10
	ENDIF
C
C WAIT 15 SECS - FORCE CHECKPOINT
C WAIT 15 SECS AND FORCE ANOTHER CHECKPOINT
C
C
	CALL XWAIT(15,2,ST)
	CBUF(1)=1
	CBUF(3)=TCGEN
	CALL QUECMD(CBUF,ST)
	CALL XWAIT(15,2,ST)
	CALL QUECMD(CBUF,ST)
C
C WAIT 'MAXCOUNT' TIMES 'DELAY' SECS EACH FOR COMPLETION OF CHECKPOINTS
C
	MAXCOUNT = 10
	DELAY    = 15
C
	TYPE *,IAM(),' WAITING FOR CHECKPOINTS TO COMPLETE'
	TYPE *,IAM(),' WILL WAIT ',MAXCOUNT,' TIMES, ',
     1	  DELAY,' SECONDS EACH TIME'
	DO 30 COUNT = 1, 10
	    CALL XWAIT(15,2,ST)
	    IF(LCHKPNT .EQ. 0 .AND. P(CHKFLG) .EQ. 0) GOTO 40
	    TYPE *,IAM(),' STILL WAITING ...'
30	CONTINUE
	TYPE *,IAM(),'CHECKPOINTS DID NOT COMPLETE... CONTINUING DAYEND'
C
40	CONTINUE
C
C V04	GIVE MORE TIME FOR THE MESSAGE TO GO OUT
C
	CALL XWAIT(15,2,ST)
C
C   AND CLOSE THE DAY
C
60	CONTINUE
	CBUF(1)=2
	CBUF(2)=DSCLOS
	CBUF(3)=TCGEN
	CALL QUECMD(CBUF,ST)
	IF(ST.NE.0) THEN
	  TYPE*,IAM(),'Queue command error for daysts   '
	  CALL XWAIT(1,2,ST)
	  GOTO 60
	ENDIF
C
C KICK ALL ONLINE TASKS
C AND DISABLE COMMUNICATIONS.
C
	CALL XWAIT(2,2,ST)
	DO 80 I=1,NUMTSK
	  IF(TSKNAM(I).NE.'        ') CALL RELSE(TSKNAM(I),ST)
80	CONTINUE
	CALL XWAIT(15,2,ST)
C
        TYPE*,IAM(),'All tasks are stopped'
D	TYPE*,IAM(),'..............Verify the following:..................'
D	TYPE*,IAM(),'1.Both checkpoints are completed.'
D	TYPE*,IAM(),'2.All tasks (except POOLPRO and OVERPRO) are stopped.'
100	CONTINUE
	CALL PRMYESNO('Are you ready to update files? ',YNFLG)
	IF(YNFLG.NE.1)THEN
	  TYPE*,IAM(),' Continue DAYEND when ready'
	  CALL GPAUSE
	  GO TO 100
	ENDIF
C
C UPDATE AGENT, GAME, LOTTO POOL AND TELEBETTING FILES
C
720     CONTINUE
        CALL XWAIT(2,2,STATUS)
        CALL STTSK(8HGETOFSAL,TSKSTS,STATUS)
        IF (STATUS.NE.4) THEN
           GOTO 720
        ENDIF

	CALL PSTGDF
	CALL PSTASF
	CALL PSTSTF
	CALL POOLSAV
C	CALL PSTTEB
	TYPE*,IAM(),' DAYEND processing complete'
	CALL GSTOP(GEXIT_SUCCESS)
C
C
	END
