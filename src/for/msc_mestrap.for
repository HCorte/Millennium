C
C *** SUBROUTINE MSC_MESTRAP ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSC_MESTRAP.FOV                              $
C  $Date::   17 Apr 1996 14:07:48                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - msc_mestrap.for ***
C
C V01 22-JAN-91 RRB RELEASED FOR VAX
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	THIS SUBROUTINE WILL INTERCEPT A COMMAND FROM A CONSOLE,
C	DECODE AND EXECUTE IT.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MSC_MESTRAP
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:MSCCOM.DEF'
	INCLUDE 'INCLIB:MSCEVN.DEF'
	INCLUDE 'INCLIB:MSCCMDS.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4    FUNCOD, ST, STATUS
C
	INTEGER*4    IMESS(20)
	CHARACTER*80 MESS                        !Message from mailbox
	EQUIVALENCE (IMESS,MESS)
C
	INTEGER*4    CBUF(CDLEN)                 !Command buffer
	INTEGER*4    CMD                         !Command Number
	LOGICAL*4    CMDOK                       !Command valid
C
	INTEGER*4     LOCAL_PORT, NET_PORT, PREV_LOCAL_PORT
C
	INTEGER*4     FIRST, I, J, COMMAND, WORD, WORD_IND
	INTEGER*4     NUM,DIG
	INTEGER*4     MESLEN                     !Input message length
C
	CHARACTER*8              SENTENCE(MSC_CMD_LEN)
	CHARACTER*(MSC_CMD_LENB) CSENTENCE
	BYTE                     BSENTENCE(MSC_CMD_LENB)
	EQUIVALENCE   (SENTENCE, CSENTENCE, BSENTENCE)
	CHARACTER*1   SPACE/Z20/
	CHARACTER*1   CR/Z0D/
C
        RECORD /MSC_IOSSTRUCT/ LOCAL_IOSB
C
C  CLEAR COMMAND PARAMETER TABLE
C
	CMD = 0
	DO 150 I = 1,MSC_CMD_LEN
	   SENTENCE(I) = '        '
150     CONTINUE
C
C  CLEAR COMMAND BUFFER
C
	DO 175 I=1,CDLEN
	   CBUF(I)=0
175	CONTINUE
C
	CBUF(2)=0
	CBUF(3)=TCMSC
	CBUF(4)=0
	CBUF(5)=0
        CBUF(6)='MSCM'
C
C READ THE MESSAGE FROM THE MAILBOX.
C
        FUNCOD=IO$_READVBLK
        STATUS=SYS$QIOW(,%VAL(MSC_MESCHANNEL),%VAL(FUNCOD),
     *                  LOCAL_IOSB,,,IMESS,%VAL(64),,,,)
        IF(.NOT.STATUS) THEN
D         TYPE *,'ERROR READING MESSAGE '
          CALL LIB$SIGNAL(%VAL(STATUS))
        ENDIF
C
D	TYPE*,'***** MESSAGE ACCEPTED *****'
	MSC_MESCNT=MSC_MESCNT+1	    !KEEP STATISTICS
C
C CHECK COMMAND SYNTAX
C
C  READ MESSAGE A "WORD" AT THE TIME AND BUILD "SENTENCE"
C
        FIRST = 0
	WORD_IND = 1
	MESLEN = INDEX(MESS,CR)
	IF(MESLEN.LE.0) MESLEN = 64
	DO 200 I = 1,MESLEN
              IF(MESS(I:I) .GE. 'a' .AND. MESS(I:I) .LE. 'z')
     *           MESS(I:I)=CHAR(ICHAR('A')+ICHAR(MESS(I:I))-ICHAR('a'))
	      IF(MESS(I:I).NE.' ') THEN
	         IF(FIRST.LE.0) FIRST = I    ! FIRST CHARACTER IN WORD
	      ELSE IF(FIRST.GT.0) THEN
		 IF(WORD_IND.GT.MSC_CMD_LEN) GOTO 250
                 SENTENCE(WORD_IND) = MESS(FIRST:I-1) ! LAST CHARACTER
	         WORD_IND = WORD_IND + 1
                 FIRST = 0
	      ENDIF
200     CONTINUE
C
250     CONTINUE
D	TYPE*,'INPUT MESSAGE LENGTH = ',MESLEN
D       WRITE(5,9000) (SENTENCE(I),I=1,MSC_CMD_LEN)
9000    FORMAT(8(1X,A8))
C
C  SEE IF THIS A REQUEST TO ACTIVATE MATRIX SWITCH.
C  IF SO, AND CONNECTION STATUS IS "DOWN" THEN CHANGE STATUS TO
C  CONNECT PENDING.
C
	IF(SENTENCE(1)(1:3).EQ.'ACT'.AND.SENTENCE(2)(1:3).EQ.'SWI') THEN
	   IF(MSCSTS.EQ.MSC_DOWN) THEN
	       MSCSTS = MSC_REQ_ONLINE
	       CALL OPS('MSCMGR: MSC CONNECT PENDING',0,0)
	   ELSE
	       CALL OPS('MSCMGR: CONNECTION PENDING OR ALREADY ACTIVE',
     *                   0,0)
	   ENDIF
	   GOTO 8000
	ENDIF
C
C  IF OTHER THAN ACTIVATE COMMAND THEN SCAN COMMAND LIST FOR MATCH
C
	DO 400 COMMAND = 1,MSC_NUM_CMDS    !SEARCH EACH COMMAND STRING
	   DO 300 WORD = 1,MSC_CMD_LEN     !COMPARE EACH "WORD"
C
C  SKIP WORD IF IT'S A DYNAMIC VALUE
C
	      IF(CMSC_COMMAND(WORD,COMMAND)(1:1).EQ.'?') GOTO 300
	      IF(SENTENCE(WORD).NE.CMSC_COMMAND(WORD,COMMAND)) GOTO 400
300        CONTINUE
	   CMD = COMMAND
	   GOTO 500
400     CONTINUE
C
C  IF MATCH IS FOUND, GET ASSOCIATED PARAMETERS
C
500	CONTINUE
	IF(CMD.EQ.CONNECT_PORT) THEN
	   CALL GETPARM(CSENTENCE,CMSC_CONF_INFO(NET_PORT_ID),
     *                  NET_PORT)
	   IF(NET_PORT.LE.0.OR.NET_PORT.GT.X2X_NETWORK_PORTS) THEN
	      CALL OPS('MSCMGR: Invalid Network Port',NET_PORT,NET_PORT)
	      GOTO 8000
	   ENDIF
C
	   CALL GETPARM(CSENTENCE,CMSC_CONF_INFO(LOC_PORT_ID),
     *                  LOCAL_PORT)
	   IF(LOCAL_PORT.LE.0.OR.LOCAL_PORT.GT.X2X_LOCAL_PORTS) THEN
	      CALL OPS('MSCMGR: Invalid Local Port',NET_PORT,NET_PORT)
	      GOTO 8000
	   ENDIF
C
C If there is currently no connection to the specified network port then
C assign it now before issuing the command to switch.
C
	   PREV_LOCAL_PORT = X2XPN_NETWORK_TO_LOCAL(NET_PORT)
	   IF(PREV_LOCAL_PORT.LE.0) THEN
	      X2XPL_LOCAL_TO_NETWORK(LOCAL_PORT) = NET_PORT
	   ENDIF
	   CBUF(8) = PREV_LOCAL_PORT
	   CBUF(9) = LOCAL_PORT
	ELSE IF(CMD.EQ.CONNECT_GROUP) THEN
           CALL OPS('MSCMGR: Connect groups not yet suppported',
     *               CMD,CMD)
	ELSE IF(CMD.EQ.CONNECT_SUPER) THEN
           CALL OPS('MSCMGR: Connect Super Groups not yet suppported',
     *               CMD,CMD)
	ELSE
	   CALL OPS('MSCMGR: Command not found!',CMD,CMD)
	ENDIF
C
C  QUE COMMAND
C
	IF(CBUF(8).EQ.0) GOTO 8000
	CBUF(1)=CMD
1000 	CONTINUE
	CALL QUECMD(CBUF,ST)
	IF(ST.NE.0) THEN
	   TYPE*,'MSCMGR: MSC_MESTRAP Queue command error '
	   CALL XWAIT(1,2,ST)
	   GOTO 1000
	ENDIF
C
C SINGLE EXIT POINT
C
8000	CONTINUE
	CALL MSC_START_MESS  !START ANOTHER MESSAGE TRAP
	RETURN
	END
