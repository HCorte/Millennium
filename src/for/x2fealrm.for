C
C SUBROUTINE X2FEALRM
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2FEALRM.FOV                                 $
C  $Date::   17 Apr 1996 16:16:46                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2fealrm.for;2 **
C
C X2FEALRM.FOR
C
C V03 03-MAR-96 wsm Changed MNUMBER=24 for Finland.
C V02 22-NOV-95 WJK INITIALIZE UNINITIALIZED DATA FOR ALPHA
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This subroutine will extract the message from the upline alarm
C Front End message, and if the alarm flag is set, will send the
C message to ERRLOG to be displayed.
C When in night mode ERRlog is not active, therefore we will use
C NOTPRO and MLOG. MLOG requires buffer lengths of 132.
C
C Calling Sequence:
C
C     CALL X2FEALRM(TRABUF,MESS,ORGMESS,LEN)
C
C Input parameters:
C
C     TRABUF      Int*4(TRALEN)   Transaction buffer
C     ORGMESS     Int*4(*)        Message from Front End
C
C Output parameters:
C
C     MESS        Int*4(*)        Message to be sent to station.
C     MESLEN      Int*2           Length of output message (bytes)
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2FEALRM(TRABUF,DUMMY,ORGMESS,MESLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2FEMES.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
C
        INTEGER*4   MNUMBER         !THIS IS THE MESSAGE NUMBER/ VARIES FROM
          PARAMETER(MNUMBER=24)     !SITE TO SITE / LOOK AT MESGEN.FOR
	INTEGER*2   MESLEN          !Output message length
	INTEGER*4   ORGMESS(*)      !Station input message
	INTEGER*4   DUMMY(*)        !Dummy parameter
	INTEGER*4   TXHDRLEN        !Length of FE header
	INTEGER*4   MESS(EDLEN)     !Buffer for QUEMES
	INTEGER*4   I4TEMP          !Temp buffer
        INTEGER*4   TASK, I, BYTCNT, FEHDRLEN
        INTEGER*4   ALRM_FORM, TEMP, ALRM_FLAG
        INTEGER*4   ALRM_CODE, ALRM_LEN, TMPCNT, J
        INTEGER*4   ST              !Status variable for mlog
C
	CHARACTER   CTEMP(4)        !Temp buffer
	CHARACTER   CMESS(132)       !ASCII message
	CHARACTER   C2MESS*132       !ASCII message
	CHARACTER   TMPMESS(132)     !Work buffer
	CHARACTER   CNULL           !Null character
	PARAMETER   (CNULL = CHAR(0))				! V02
	CHARACTER   BELL /Z07/      !Ring bell
	EQUIVALENCE (MESS(4),CMESS,C2MESS)
	EQUIVALENCE (I4TEMP,CTEMP)
C
	TASK=X2P
	MESS(1)=TASK
	MESS(2)=TEGEN
	MESS(3)=MNUMBER
	DO 10 I=1,132                !initialize 132 mainly for MLOG
	  CMESS(I)=' '
10	CONTINUE
	BYTCNT=0
C
C GET THE TRANSPORT LAYER HEADER LENGTH.
C
	CALL ILBYTE(TXHDRLEN,ORGMESS,X2PRO_OFFSET-1)
	TXHDRLEN=TXHDRLEN-1
C
C GET THE FE HEADER LENGTH.
C
	FEHDRLEN=X2FEMES_DATALEN+1
	CALL ILBYTE(ALRM_FORM,ORGMESS,X2FEMES_FORMAT-1+TXHDRLEN)
	CALL MOV2TOI4(TEMP,ORGMESS,X2FEMES_MESCOD-1+TXHDRLEN)
	ALRM_FLAG=IAND(TEMP,X2FEMES_ALM)
	ALRM_CODE=IAND(TEMP,X2FEMES_MESCOD_TEXT)
	CALL MOV2TOI4(ALRM_LEN,ORGMESS,X2FEMES_DATALEN-1+TXHDRLEN)
C
C IF THE ALARM FLAG IS SET SEND THE MESSAGE TO ERROR LOGGER.
C
	IF(ALRM_FORM.EQ.X2FEMES_FORM_ASCII .AND.
     *	   ALRM_FLAG.NE.0) THEN
C
C DISPLAY WHICH SAP IT CAME FROM.
C
	  WRITE (C2MESS,9000) TRABUF(TXSAP),BELL,BELL,BELL
9000	  FORMAT('GTX ',I2,' ALARM: ',1A,1A,1A)
	  BYTCNT=17
C
C LOOP FOR ENTIRE LENGTH OF ALARM MESSAGE.
C
	  DO 100 I=1,ALRM_LEN
C
C EXTRACT ASCII CHARACTER.
C
	    BYTCNT=BYTCNT+1
	    CALL ILBYTE(I4TEMP,ORGMESS,FEHDRLEN+TXHDRLEN+I-1)
	    CMESS(BYTCNT)=CTEMP(1)
C
C IF MESSAGE LENGTH FILLED THEN SEARCH BACKWARDS UNTIL FIRST
C SPACE IF FOUND.  STORE WORD TO BE WRAPPED INTO TMPMESS, AND
C USE THIS IN THE BEGINNING OF THE NEXT MESSAGE TO ERRLOG.
C
	    IF(BYTCNT.GE.65) THEN
	      TMPCNT=0
	      DO 105 J=68,1,-1
	        IF(CMESS(J).NE.' ' .AND. CMESS(J).NE.CNULL) THEN
	          TMPCNT=TMPCNT+1
	          TMPMESS(TMPCNT)=CMESS(J)
	          CMESS(J)=' '
	        ELSE IF(CMESS(J).EQ.' ' .AND. TMPCNT.NE.0) THEN
	          GOTO 106
	        ENDIF
105	      CONTINUE
C
C QUEUE MESSAGE TO ERRLOG AND CLEAR ASCII MESSAGE.
C IF IN NIGHT GAME MODE ERRLOG IS NOT PRESENT, SO
C CALL MLOG DIRECTLY
C
106	      CONTINUE
              IF(X2X_GAME_MODE.EQ.X2X_GAMEM_DAY) THEN
	         CALL QUEMES(MESS)
              ELSE
                CALL MLOG(CMESS,ST)
	      ENDIF
C
	      BYTCNT=0
	      DO 110 J=1,132
	        CMESS(J)=' '
110	      CONTINUE
C
C MESSAGE QUEUED, NOW STICK THE WRAPPED WORD INTO THE NEW
C BUFFER.
C
	      DO 120 J=TMPCNT,1,-1
	        BYTCNT=BYTCNT+1
	        CMESS(BYTCNT)=TMPMESS(J)
120	      CONTINUE
	    ENDIF
100	  CONTINUE
          IF(X2X_GAME_MODE.EQ.X2X_GAMEM_DAY) THEN
	    CALL QUEMES(MESS)
          ELSE
            CALL MLOG(CMESS,ST)
          ENDIF
	ENDIF
C
	RETURN
	END
