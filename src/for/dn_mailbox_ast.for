C
C SUBROUTINE DN_MAILBOX_AST
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_MAILBOX_AST.FOV                           $
C  $Date::   17 Apr 1996 12:58:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - dn_mailbox_ast.for ***
C
C V03 21-NOV-95 PJS MODIFIED CALL TO RTL() TO BE RTL_AST().
C V02 20-APR-92 JWE Add queue interlock retry
C V01 21-APR-91 Steve Sullivan, DEC
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C This routine will execute when a mailbox read has completed. The parameter 
C passed to us is the mailbox MBXBUF which contains the maibox message.
C context is determined through the use of the unit field of the mailbox
C message.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE DN_MAILBOX_AST(MBXBUF)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
        INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:DN_LINK.DEF'			! DECNET STRUCTURES
        INCLUDE 'INCLIB:DN_BLOCK.DEF'			! DECNET DATA BLOCKS
        INCLUDE 'INCLIB:TASKID.DEF'
C
	INCLUDE '($IODEF)'
        INCLUDE '(LIB$ROUTINES)'
        INCLUDE '($MSGDEF)'
        INCLUDE '($SYSSRVNAM)'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	BUF_NO,				! DCN BUFFER # TO USE
     *			CHANNEL,			! DCN CHAN FOR QIOs
     *			I,				! GENERAL PURPOSE
     *			IOSB_ADR,			! ADDR OF IOSB IN "BUF"
     *			LNK,				! LINK BLOCK INDEX
     *			NCB_DESCRIP(2),			! DESCRIPTOR FOR NCB
     *			NCBIDX,				! INDEX INTO NCB COPY
     *			NCBLEN,				! # OF BYTES IN NCB
     *			NODELEN,			! # OF CHARS IN NODE
     *			STATUS				! STATUS VARIABLE
C
	INTEGER*2	FUNCTION			! FUNCTION CODE FOR QIOs
C
	CHARACTER*100	NCBCOPY				! COPY OF NCB fr. MBXBUF
C
	CHARACTER*32	NODENAM				! REMOTE SYSTEM NAME
C
	RECORD /DN_MBX_STRUCT/ MBXBUF			! MAILBOX MBXBUF ARG.
C
C EXTERNAL DECLARATIONS
C
	EXTERNAL	DN_CONFIRM_AST
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C BEGIN CODE
C
D	CALL OPS('MESSAGE IN THE MAILBOX ', 0, MBXBUF.MSGTYP)
C
C GET THE LINK BLOCK INDEX
C
   	LNK = 0						! INITIALIZE TO NONE
C
	DO 100 I = 1, NETSYS
	  IF (MBXBUF.UNIT .EQ. DN_LINK(I).UNIT) LNK = I
100	CONTINUE
C
C CHECK FOR I/O ERRORS
C
	IF (.NOT. DN_SYS.IOSB.STAT) GOTO 700		! QUEUE ANOTHER MBX READ
C
C PERFORM ACTIONS DEPENDING UPON THE MESSAGE TYPE FIELD OF MBXBUF
C
C WHEN WE GET A CONNECT HERE IT IS AN INCOMING CONNECTION AND
C WE NEED TO ALLOCATE A LINK BLOCK AND ACCEPT IT.
C IF THERE ARE NO LINK BLOCKS, WE REJECT THE CONNECTION.
C
	IF (MBXBUF.MSGTYP .EQ. MSG$_CONNECT) THEN
C
C COPY AND BUILD A DESCRIPTOR FOR THE NCB DATA
C
	  NCBLEN = MBXBUF.INFO(MBXBUF.INFO(1) + 1) 
	  NCBIDX = MBXBUF.INFO(1) + 2			! NCB IN MESSAGE
C
	  DO 200 I = 1, NCBLEN
	    NCBCOPY(I:I) = CHAR(MBXBUF.INFO(I + MBXBUF.INFO(1) + 2))
200	  CONTINUE
C
	  NCB_DESCRIP(1) = NCBLEN
	  NCB_DESCRIP(2) = %LOC(NCBCOPY)
C
C ALLOCATE A BUFFER HEADER...
C
C LOOK FOR THE LINK BLOCK SET UP FOR THIS NODE.
C WE HAVE A STATIC RELATIONSHIP BETWEEN LINK BLOCKS AND
C THE NODES THAT ARE ASSIGNED TO THEM. ON INCOMING LINKS WE LOOK
C FOR A LINK BLOCK THAT HAS A MATCH IN THE ASSIGNED NODE AND
C TASK NAME WITH THE CORRESPONDING FIELDS IN THE INCOMING NCB.
C THESE ARE VARIABLE FIELDS WHICH MUST BE PARSED OUT OF THE NCB BY HAND.
C THIS IS WHAT WE ARE DOING HERE.
C
	  DO 300 I = 1, 100				! GET THE NODE NAME
	    IF (NCBCOPY(I:I) .EQ. ':') THEN		! DELIMITED BY COLON
	      NODELEN = I - 1				! NODE LENGTH
	      NODENAM = NCBCOPY(1:NODELEN)		! COPY STRING
	      GOTO 400					! EXIT LOOP
	    ENDIF
300	  CONTINUE
C
400	  CONTINUE
	  LNK = 0					! INIT = NO LINK BLOCK
C
	  DO 500 I = 1, NETSYS				! ALLOCATE LINK BLOCK
	    IF (NODENAM .EQ. DN_LINK(I).NODE(:DN_LINK(I).NODELEN) .AND.
     *          DN_LINK(I).STATE .EQ. STATE_DOWN) THEN
	      LNK = I					! NAMES MATCH, ASSIGN
	      DN_LINK(LNK).STATE_COUNT(STATE_CONFIRM) =	! COUNT STATE CHANGES
     *        DN_LINK(LNK).STATE_COUNT(STATE_CONFIRM) + 1
C
C SAVE TIME
C
	      STATUS=SYS$GETTIM(DN_LINK(LNK).STATE_TIME(STATE_CONFIRM))
	      DN_LINK(LNK).CHANNEL = CHANNEL		! SAVE CHANNEL
	      DN_LINK(LNK).OWNER   = NTL		! OWNER OF LINK
	      GOTO 600					! EXIT LOOP
	    ENDIF
500	  CONTINUE
C
C IF WE FAILED TO GET A LINK BLOCK THEN WE MUST REJECT THE
C CONNECTION REQUEST. ELSE WE ACCEPT IT. WE USE THE SAME QIO
C TO DECNET WITH A DIFFERENT FUNCTION CODE OR CHANNEL.
C THE SAME AST IS USED AND DETERMINES ITS ACTIONS UPON THE INFORMATION
C IN THE BUFFER HEADER THAT IS SUPPLIED.
C
C GET BUFFER HEADER FROM FREE LIST (IF EMPTY, RETURN)
C
600	  CONTINUE
	  CALL RTL_AST(BUF_NO, DCN_FREE, STATUS)	! FOR CONSISTENCY (V03)
	  IF (STATUS .EQ. 2) GOTO 9999
C
	  IF (LNK .EQ. 0) THEN				! IF TRUE, NO LINK BLOCK
	    FUNCTION = (IO$_ACCESS .OR. IO$M_ABORT)	! MUST REJECT LINK
	    CHANNEL = DN_SYS.NETCHAN			! USE DCN CHAN TO REJECT
C
	    CALL BUFINI(DN_BUFFER(BUF_NO),		! BUFFER TO INITIALIZE
     *                  DN_SYS.NETCHAN,			! CHANNEL TO USE
     *                  0,				! NO SOURCE FOR REJECT
     *                  0,				! NO LINK ON REJECT
     *                  0,				! NO DATA BUFFER USED
     *                  IOSB_ADR)			! IOSB to use (address)
C
	  ELSEIF (NETMASTER(WAYINP) .EQ. NODEID .OR.
     *            NETBACKUP(WAYINP) .EQ. NODEID) THEN
	    FUNCTION = (IO$_ACCESS .OR. IO$M_ABORT)	! MUST REJECT LINK
	    CHANNEL = DN_SYS.NETCHAN			! USE DCN CHAN TO REJECT
C
	    CALL BUFINI(DN_BUFFER(BUF_NO),		! BUFFER TO INITIALIZE
     *                  DN_SYS.NETCHAN,			! CHANNEL TO USE
     *                  0,				! NO SOURCE FOR REJECT
     *                  0,				! NO LINK ON REJECT
     *                  0,				! NO DATA BUFFER USED
     *                  IOSB_ADR)			! IOSB to use (address)
	    CALL OPS('REJECTING CONNECTION REQUEST', LNK, 10 + LNK) 
C
	  ELSE
	    FUNCTION = IO$_ACCESS			! OK TO ACCEPT
	    STATUS = SYS$ASSIGN('_NET:',		! LOGICAL FOR CHANNEL
     *                          CHANNEL,		! CHANNEL TO ASSIGN
     *                          ,			! ACCESS MODE
     *                          DN_SYS.MBXNAME)		! MAILBOX LOGICAL NAME
C
	    IF (.NOT. STATUS) THEN
	      DN_LINK(LNK).STATE = STATE_DOWN		! DOWN
	      CALL LIB$SIGNAL(%VAL(STATUS))
	      GOTO 700					! QUEUE ANOTHER MBX READ
	    ENDIF
C
	    DN_LINK(LNK).CHANNEL = CHANNEL
	    CALL DN_GET_UNIT_AST(CHANNEL,		! GET THE LINK'S UNIT
     *                           DN_LINK(LNK).UNIT)
C
	    CALL BUFINI(DN_BUFFER(BUF_NO),		! BUFFER TO INITIALIZE
     *                  CHANNEL,			! CHANNEL WE ARE USING
     *                  NTL,				! SOURCE TASK
     *                  LNK,				! DN_LINK STRUCT INDEX
     *                  0,				! NO DATA BUF SUPPLIED
     *			IOSB_ADR)			! ADDRESS OF IOSB
	  ENDIF
C
C QUEUE THE CONNECT REQUEST RESPONSE
C
	  STATUS = SYS$QIO(,				! EVENT FLAG
     *                     %VAL(CHANNEL),		! CHANNEL
     *                     %VAL(FUNCTION),		! FUNCTION
     *                     %VAL(IOSB_ADR),		! STATUS BLOCK
     *                     DN_CONFIRM_AST,		! AST ADDRESS
     *                     DN_BUFFER(BUF_NO),		! AST PARAMETER
     *                     ,				! P1
     *                     %REF(NCB_DESCRIP),		! P2
     *                     ,				! P3
     *                     ,				! P4
     *                     ,				! P5
     *                     )				! P6
C
	  IF (.NOT. STATUS) THEN			! ERROR QUEUEING MSG ?
	    CALL LIB$SIGNAL(%VAL(STATUS))
	    IF (CHANNEL .NE. DN_SYS.NETCHAN) THEN
	      STATUS = SYS$DASSGN(%VAL(CHANNEL))	! FREE THE CHANNEL
	      CALL LIB$SIGNAL(%VAL(STATUS))
 	    ENDIF
	    DN_LINK(LNK).STATE = STATE_DOWN		! DOWN
          ENDIF
	ELSE IF (MBXBUF.MSGTYP .EQ. MSG$_DISCON) THEN
	ELSE IF (MBXBUF.MSGTYP .EQ. MSG$_TRMUNSOLIC) THEN
	ELSE IF (MBXBUF.MSGTYP .EQ. MSG$_CRUNSOLIC) THEN
	ELSE IF (MBXBUF.MSGTYP .EQ. MSG$_ABORT) THEN
	ELSE IF (MBXBUF.MSGTYP .EQ. MSG$_CONFIRM) THEN	! HANDLED BY CONNECT AST
	ELSE IF (MBXBUF.MSGTYP .EQ. MSG$_EXIT) THEN
	ELSE IF (MBXBUF.MSGTYP .EQ. MSG$_INTMSG) THEN
	ELSE IF (MBXBUF.MSGTYP .EQ. MSG$_PATHLOST) THEN
	ELSE IF (MBXBUF.MSGTYP .EQ. MSG$_PROTOCOL) THEN
	ELSE IF (MBXBUF.MSGTYP .EQ. MSG$_REJECT) THEN	! HANDLED BY CONNECT AST
	ELSE IF (MBXBUF.MSGTYP .EQ. MSG$_THIRDPARTY) THEN
	ELSE IF (MBXBUF.MSGTYP .EQ. MSG$_TIMEOUT) THEN	! HANDLED BY CONNECT AST
	ELSE IF (MBXBUF.MSGTYP .EQ. MSG$_NETSHUT) THEN
	  CALL DN_NETSHUT				! DISCONNECT ALL LINKS
	ENDIF
C
C QUEUE ANOTHER MAILBOX READ
C
700	CONTINUE
	CALL QUEMBX_AST(MBXBUF)				! QUEUE ANOTHER MBX READ
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
