C X2XUNS.FOR
C
C V02 16-JUN-2000 OXK Cleanup w/ WARNINGS=ALL
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C	SEND TO THE X2X NETWORK.
C
C     CALL X2XUNS(BUF,MESSAGES_PER_SEC,SUBNETWORK)
C     IN:
C     BUF   -  BUFFER NO (DATA IS KEPT IN THIS PROCOM BUFFER
C     MESSAGES_PER_SEC - QUEUE UP TO MESSAGES_PER_SEC IN 1 SEC
C     SUBNETWORK - SUBNETWORK TO SEND TO
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
	SUBROUTINE X2XUNS(ORIGBUF,MESSAGES_PER_SEC,SUBNETWORK)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
C
	INTEGER*4 OUT_LEN, MES_NUM, FORMAT, MES_DEST, X2XBUF
	INTEGER*4 TER_STATUS, CURRENT_MESSAGE, TER_NUM
	INTEGER*4 NUM_MESSAGES, ST
	INTEGER*4 MESSAGES_PER_SEC, ORIGBUF
	INTEGER*4 NEXT_TERM
	INTEGER*4 STATION, CLASS, SUBNETWORK
	INTEGER*4 TO_START
C
C
	IF (MESSAGES_PER_SEC.LE.0) GOTO 500
C
C     STAGGER SENDING DATA LOGIC
C
	
	NUM_MESSAGES=MESSAGES_PER_SEC
	TER_NUM=0
	NEXT_TERM=0
	CURRENT_MESSAGE=0
100	CONTINUE
	CALL INPNUM('How many terminals to start ',TO_START,1,X2X_TERMS,ST)
150	CONTINUE
	TER_NUM=TER_NUM+1
	NEXT_TERM=NEXT_TERM+37
	IF (NEXT_TERM.GT.X2X_TERMS) NEXT_TERM=NEXT_TERM-X2X_TERMS
	IF (TER_NUM.GT.X2X_TERMS) GOTO 9000
	CALL ILBYTE(TER_STATUS,IX2XT_STATE,NEXT_TERM-1)
	IF(TER_STATUS.LE.X2XTS_NOT_DEF.OR.TER_STATUS.GE.X2XTS_DISABLED)
     *	    GOTO 150
	STATION=X2XT_STATION_NO(NEXT_TERM)
	IF (STATION.LE.0) GOTO 150
	IF (X2XS_TYPE(STATION).EQ.X2XST_BCST) GOTO 150    !SKIP BROADCAST
	CLASS=X2XS_STNCLS(STATION)
	IF (CLASS.LE.0) GOTO 150
	IF (SUBNETWORK.NE.X2XC_SUBNETWORK(CLASS)) GOTO 150
200	CONTINUE
	IF(X2X_GAME_STATE.NE.X2X_GAMES_UP) GOTO 9000
C
	CALL GETBUF(X2XBUF)
	IF(X2XBUF.LE.0) THEN
	  CALL XWAIT(5,2,ST)     ! WAIT 5 SECONDS AND TRY AGAIN
	  GOTO 200
	ENDIF
	CURRENT_MESSAGE=CURRENT_MESSAGE+1
	IF (MOD(CURRENT_MESSAGE,NUM_MESSAGES).EQ.0)
     *	        CALL XWAIT(1,2,ST)   !wait a sec
	IF (MOD(CURRENT_MESSAGE,500).EQ.0) 
     *		  TYPE *,'Sending ',CURRENT_MESSAGE
	TO_START=TO_START-1
	IF (TO_START.LE.0) GOTO 100
C
C COPY OLD BUFFER TO X2X BUFFER
C
	CALL FASTMOV(PRO(1,ORIGBUF),PRO(1,X2XBUF),PROLEN)
C
C START RELAY PROCESS.
C
	HPRO(TRCODE,X2XBUF)=TYPUNS
C***	HPRO(MSGNUM,X2XBUF)=MES_NUM
C***	HPRO(OUTLEN,X2XBUF)=LEN
C***	PRO(INPTAB,X2XBUF) =BUFFER(1)

	IF (X2X_I4_STATION) THEN
		PRO(TERNUM,X2XBUF)=NEXT_TERM
		IF (PRO(TERNUM,X2XBUF).EQ.0)  PRO(TERNUM,X2XBUF)=1
		PRO(LINENO,X2XBUF)=0
	ELSE
		HPRO(TERNUM,X2XBUF)=NEXT_TERM
		IF (HPRO(TERNUM,X2XBUF).EQ.0)  HPRO(TERNUM,X2XBUF)=1
		HPRO(LINENO,X2XBUF)=0
	ENDIF
C****	TYPE 9010,X2XBUF,NEXT_TERM,(PRO(II,X2XBUF),II=1,PROLEN)
9010	FORMAT(' BUF, TERNUM ',I5,1X,I5,' BUF ',/,8(1x,z8.8))
	CALL X2ADDPRO(X2XBUF)
	GOTO 150
C
C     BROADCAST NOT STAGGERRED
C
500	CONTINUE
	IF(X2X_GAME_STATE.NE.X2X_GAMES_UP) GOTO 9000
C
	CALL GETBUF(X2XBUF)
	IF(X2XBUF.LE.0) THEN
	  CALL XWAIT(5,2,ST)     ! WAIT 5 SECONDS AND TRY AGAIN
	  GOTO 500
	ENDIF
C
C COPY OLD BUFFER TO X2X BUFFER
C
	CALL FASTMOV(PRO(1,ORIGBUF),PRO(1,X2XBUF),PROLEN)
C
C START RELAY PROCESS.
C
	MES_DEST=X2STMES_RELAYF_DS_TERM
	FORMAT=X2XR_APPA_ALL_NO_FORMAT
	MES_NUM=HPRO(MSGNUM,X2XBUF)
	OUT_LEN=HPRO(OUTLEN,X2XBUF)
	CALL X2RSTART(PRO(1,X2XBUF),0,0,-1,MES_NUM,
     *	    OUT_LEN,PRO(OUTTAB,X2XBUF),MES_DEST,FORMAT,SUBNETWORK)
	CALL X2RADDBF(X2XBUF)
C
C RELEASE THE ORIGINAL BUFFER
C
9000	CONTINUE
	CALL RELBUF(ORIGBUF)
	RETURN
	END
