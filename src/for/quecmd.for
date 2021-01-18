C
C SUBROUTINE QUECMD
C $Log:   GXAFXT:[GOLS]QUECMD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:36:06   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:25:02   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - quecmd.for **
C
C QUECMD.FOR
C
 
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO QUEUE COMMANDS FROM ALL APPLICATIONS
C TASKS TO DISPATCHER INPUT QUEUE.
C
C CALLING SEQUENCE:
C     CALL QUECMD(CBUF,STATUS)
C INPUT
C     CBUF   - COMMAND BUFFER
C OUTPUT
C     STATUS - QUEUEING STATUS (0 = GOOD, -1 = ERROR)
C
C
C
C COMMAND BUFFER FORMAT
C
C WORD                     CONTENTS
C   1                 COMMAND NUMBER
C   2                 COMMAND NEW VALUE
C   3                 COMMAND TYPE
C   4                 COMMAND LINE
C   5                 COMMAND TERMINAL
C   6                 COMMAND SOURCE
C   7                 COMMAND AGENT
C   8                 COMMAND DATA 1
C   9                 COMMAND DATA 2
C  10                 COMMAND DATA 3
C  11                 COMMAND DATA 4
C  12                 COMMAND DATA 5
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
	SUBROUTINE QUECMD(CBUF,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INTEGER*4 CBUF(CDLEN), BUF, STATUS
C
C
C
C ALLOCATE PROCOM PROCESSING BUFFER.  IF ALLOCATION
C ERROR, THEN SET STATUS TO -1 AND RETURN.
C
	STATUS=0
	CALL GETBUF(BUF)
	IF(BUF.EQ.0) THEN
	  STATUS=-1
	  RETURN
	ENDIF
C
C TRANSFER COMMAND INFORMATION TO PROCESSING BUFFER.
C
	HPRO(TRCODE,BUF)=TYPCMD
	HPRO(INPLEN,BUF)=52
	PRO(CNUM,BUF)=CBUF(1)
	PRO(CVAL,BUF)=CBUF(2)
	PRO(CTYP,BUF)=CBUF(3)
	PRO(CLIN,BUF)=CBUF(4)
	PRO(CTER,BUF)=CBUF(5)
	PRO(CSRC,BUF)=CBUF(6)
	PRO(CAGT,BUF)=CBUF(7)
	PRO(CDT1,BUF)=CBUF(8)
	PRO(CDT2,BUF)=CBUF(9)
	PRO(CDT3,BUF)=CBUF(10)
	PRO(CDT4,BUF)=CBUF(11)
	PRO(CDT5,BUF)=CBUF(12)
C
C QUEUE COMMAND BUFFER TO DISPATCHER INPUT QUEUE
C
	  CALL OPSTXT('**********************CALL quecmd***************************')
C      TYPE *,IAM(),'CALLING quecmd'
C      CALL PRTOUT(BUF)
	CALL QUEINP(BUF,STATUS)
	RETURN
	END
