C
C SUBROUTINE MLOG
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MLOG.FOV                                     $
C  $Date::   17 Apr 1996 14:03:06                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - mlog.for;1 **
C
C MLOG.FOR
C
C V03 07-FEB-2001 UXN Wait 3 times 10 ms, if mailbox is full, before giving up.
C V02  7-MAR-1994 JWE Change to temporary mailbox
C V01 21-SEP-1990 MRM RELEASED FOR VAX
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
	SUBROUTINE MLOG (STRING, ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:NOTEVN.DEF'
	INCLUDE '($SYSSRVNAM)'
	INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
C
        CHARACTER*4 GXEVNNAM              !NAME FUNCTION
C
	CHARACTER   STRING*132
	CHARACTER   LINE*132
C
	LOGICAL	    INIT
C
	INTEGER*4   ST
	INTEGER*4   WRITE_CODE
	INTEGER*4   STATUS
	INTEGER*4   IOSTAT(2)	
	INTEGER*4   WAIT_CNT
C
	IF(.NOT.INIT) THEN
	  INIT=.TRUE.
C
C SETUP THE MAILBOX FOR INTERTASK MESSAGES TO NOTPRO.
C IF MAILBOX DOES NOT EXIST, CREATE IT.
C
          STATUS=SYS$ASSIGN(GXEVNNAM()//NT_MESNAME,NT_MESCHANNEL,,)
          IF(.NOT.STATUS) THEN
            STATUS=SYS$CREMBX(%VAL(0),NT_MESCHANNEL,%VAL(132),,
     *                        %VAL('FD00'X),,GXEVNNAM()//NT_MESNAME)
	    IF(STATUS.NE.0) GOTO 8000
          ENDIF
	ENDIF
C
C WRITE THE STRING INTO THE OUTPUT BUFFER.
C
	ST=-1
	WRITE(LINE,9000) IAM(),STRING(1:114) !132 - 18 FOR IAM()
9000	FORMAT(A18,A114)
C
	WAIT_CNT = 0
7000	CONTINUE
C
C SEND MESSAGE TO NOTPRO'S MAILBOX.  NOTE: WE DO NOT WAIT
C FOR THE WRITE TO COMPLETE AS WE DO NOT REALLY CARE IF IT
C DOES NOT GET THERE - AS OPPOSED TO HANGING THE CALLING TASK
C WAITING FOR NOTPRO TO READ THE MAILBOX.
C
        WRITE_CODE = IO$_WRITEVBLK .OR. IO$M_NOW .OR. IO$M_NORSWAIT
        STATUS=SYS$QIO(,
     *                  %VAL(NT_MESCHANNEL),      !CHANNEL
     *                  %VAL(WRITE_CODE),         !FUNCTION CODE
     *                  IOSTAT,                   !STATUS BLOCK
     *                  ,,
     *                  %REF(LINE),       !P1
     *                  %VAL(132),,,,)            !P2
	IF(STATUS.EQ.SS$_MBFULL) THEN
	   WAIT_CNT = WAIT_CNT + 1
	   IF(WAIT_CNT.GT.3) GOTO 8000
	   CALL XWAIT(10,1,ST)
	   GOTO 7000
	ENDIF
	IF(STATUS.EQ.0) ST = 0
C
C PROGRAM EXIT.
C
8000	CONTINUE
	RETURN
	END
