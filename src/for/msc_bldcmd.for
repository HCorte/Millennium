C SUBROUTINE BLDCMD
C  
C V04 13-JUN-2000 OXK Cleanup w/ WARNINGS=ALL
C V03 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			Instant Pass Thru Phase 1
C V02 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C V01 04-JAN-91 RRB RELEASED FOR VAX
C
C SUBROUTINE TO BUILD ASCII SWITCH COMMANDS FOR THE MSC.
C
C CALLING SEQUENCE
C	CALL BLDCMD(CMDNUM,MSCPARMS,SETALRM)
C	INPUT
C 	    CMDNUM      - MSC COMMAND NUMBER
C	    MSCPARMS    - PARAMETER VALUES
C	    SETALRM     - ARM ALARM FLAG (1 - ARM ALARM)
C	OUTPUT
C	    NONE
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE BLDCMD(CMDNUM,MSCPARMS,SETALRM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:MSCCOM.DEF'
	INCLUDE 'INCLIB:MSCCMDS.DEF'
	INCLUDE 'INCLIB:MSCEVN.DEF'
C
	INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4   CMDNUM                           !Command number
	INTEGER*4   MSCPARMS(MSC_MAX_PARMS)          !Parameter values
	INTEGER*4   SETALRM                          !Alarm Flag
	INTEGER*4   I
	REAL*8      COMMAND(MSC_CMD_LEN+1)           !Command to MSC
	REAL*8      FORCE
	CHARACTER*8 CFORCE
	EQUIVALENCE (FORCE,CFORCE)
	CHARACTER*80 MSCCMD                          !Command to MSC
	INTEGER*4    IMSCCMD(20)
	EQUIVALENCE  (MSCCMD,IMSCCMD)
	CHARACTER*1 CR/Z0D/
C
	CHARACTER*4 ASCII_NUMBER(MSC_MAX_PARMS)
        INTEGER*4   NUMBER(MSC_MAX_PARMS), I4NUM
	EQUIVALENCE (ASCII_NUMBER,NUMBER)
C
	CHARACTER*8 ASCII_PARM(MSC_MAX_PARMS)
	REAL*8      PARM(MSC_MAX_PARMS)
	EQUIVALENCE (ASCII_PARM,PARM)
C	
	CFORCE = 'FORCE'//CR                         !Switch commands require
C						      !Force Connect	
C CLEAR COMMAND BUFFER
C
	CALL FASTSET(0,COMMAND,(MSC_CMD_LEN+1)/2)
	CALL FASTMOV(MSC_COMMAND(1,CMDNUM),COMMAND,MSC_CMD_LEN*2)
	COMMAND(MSC_CMD_LEN+1) = FORCE
C
C CONVERT BINARY VALUES TO THERE ASCII DECIMAL EQUIVALENT
C
	DO 100 I = 1,MSC_MAX_PARMS
D          TYPE*,IAM(),'PARAMETER ',I,' = ',MSCPARMS(I)
	   IF(MSCPARMS(I).NE.0) THEN
	      CALL BINASC(I4NUM,1,4,MSCPARMS(I))
	      NUMBER(I)=I4NUM
D             WRITE(5,9000) ASCII_NUMBER(I)
9000          FORMAT(1X,'BLDCMD: ASCII NUMBER = ',4A)
	   ENDIF
100	CONTINUE
C
C BUILD ASCII COMMAND PARAMETER
C
	IF(CMDNUM.EQ.CONNECT_PORT) THEN
	   IF(MSCPARMS(1).EQ.0) THEN
	      ASCII_PARM(1) = 'NULL'
	   ELSE
	      I = INDEX(CMSC_CONF_INFO(NET_PORT_ID),' ')
	      IF(I.EQ.0) I = LEN(CMSC_CONF_INFO(NET_PORT_ID))+1
	      ASCII_PARM(1) = CMSC_CONF_INFO(NET_PORT_ID)(1:I-1)//
     *                        ASCII_NUMBER(1)(2:4)
	   ENDIF
	   COMMAND(MSC_PARM_INDEX(1,CMDNUM)) = PARM(1)
	   IF(MSCPARMS(2).EQ.0) THEN
	      ASCII_PARM(2) = 'NULL'
	   ELSE
	      I = INDEX(CMSC_CONF_INFO(LOC_PORT_ID),' ')
	      IF(I.EQ.0) I = LEN(CMSC_CONF_INFO(LOC_PORT_ID))+1
	      ASCII_PARM(2) = CMSC_CONF_INFO(LOC_PORT_ID)(1:I-1)//
     *                        ASCII_NUMBER(2)(2:4)
	   ENDIF
	   COMMAND(MSC_PARM_INDEX(2,CMDNUM)) = PARM(2)
	   CALL MSC_QUECMD(COMMAND)
C
C CHECH IF WE SHOULD ALSO ENABLE ALARMS FOR THIS PORT
C
	   IF(MSCPARMS(1).GT.0.AND.SETALRM.EQ.1) THEN
	      MSCCMD = ENABLE_ALARM//ASCII_PARM(1)//CR
	      CALL MSC_QUECMD(IMSCCMD)
	   ENDIF
	ELSE
	   CALL OPS('MSCMGR: INVALID COMMAND NUMBER IN BLDCMD',
     *               CMDNUM,CMDNUM)
	ENDIF
	RETURN
	END
