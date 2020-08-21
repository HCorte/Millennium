C GUICBUF.FOR
C
C V02 13-NOV-2000 UXN GUI_WORKER queues added for multi-threaded GUIMGR
C V01 23-JUN-1993 MP  INITIAL RELEASE FOR VAX (Adopted from CRSRCV)
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
C Copyright 2001 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C This subroutine processes buffers which have been placed
C on the 'GUI_FROM_LINK' queue.  These include acknowledgements
C to transmission we have sent, and new Clietns' requests.
C
C Input parameters:
C
C     none
C
C Output parameters:
C
C     NR_BUFS Int*4   NR Buffers received.
C
C Results:
C     Messages assembled in GUI_ASMB_BUFs.
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUICBUF(NR_BUFS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
C
	INTEGER*4   NR_BUFS		!buffers processed
	INTEGER*4   BUF_INX             !buffer number
	INTEGER*4   CONN_INX		!connection number
	INTEGER*4   IOLEN               !Byte length of xfer
	INTEGER*4   ST                  !Work variable
	INTEGER*4   IO_BUF_OFF		!Record offset
	INTEGER*4   MSG_LEN_LEFT	!bytes to move
	INTEGER*4   IO_LEN_LEFT		!bytes left in current read buffer
C
C       Buffers to assemble Clients' messages
C
        INTEGER*4       GUI_ASMB_BUF(GUI_BUF_SIZ, GUI_MAX_CONN)
        BYTE            GUI_ASMB_BBUF(GUI_BUF_SIZ*4, GUI_MAX_CONN)
        EQUIVALENCE     (GUI_ASMB_BBUF,GUI_ASMB_BUF)
C
C       The following variables are used by GUIMGR to keep
C       track of messages that were split by TCPP/IP. The READOFF
C       variable keeps track of what byte the current message ended with.
C       The READLEN variable keeps track of remaining bytes left for current
C       message.
C
C       ie. Client sends 2 messages with length of 30 each.
C           Gtech receives 47 byte message and 13 byte message.
C           After receiving the 1st message, BUFFOFF will be 17( 47-30)
C           and BUFFLEN will be 13( 30-BUFFOFF).
C
        INTEGER*4 GUI_ASMB_BUF_OFF(GUI_MAX_CONN) !offset in current ASSM buf
        INTEGER*4 GUI_ASMB_MSG_LEN(GUI_MAX_CONN) !msg length in bytes
        INTEGER*4 GUI_ASMB_BUF_STS(GUI_MAX_CONN)
        INTEGER*4 GUI_DEASMB_BUF_OFF(GUI_MAX_CONN) !offset in input buffer
C
        INTEGER*4       GUI_ASMB_BUF_STS_INPROG
        INTEGER*4       GUI_ASMB_BUF_STS_COMPLETED
        PARAMETER       (GUI_ASMB_BUF_STS_INPROG=1)
        PARAMETER       (GUI_ASMB_BUF_STS_COMPLETED=2)
C
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
	INTEGER*4  WORKER_BUF, MOVE_SIZE
	LOGICAL*4  FIRST/.TRUE./
C
	INTEGER*4   BLANK
	DATA	    BLANK/'    '/
C
	IF(FIRST) THEN
	    FIRST = .FALSE.
            DO CONN_INX=1, GUI_MAX_CONN
               GUI_ASMB_BUF_OFF(CONN_INX)=0    ! offset for assembling messages
               GUI_ASMB_MSG_LEN(CONN_INX)=0    ! length of assembled message
               GUI_ASMB_BUF_STS(CONN_INX)=GUI_ASMB_BUF_STS_INPROG
               GUI_DEASMB_BUF_OFF(CONN_INX)=0  ! offset of input messages
            ENDDO
	ENDIF
	NR_BUFS = 0
C
	DO 8000 CONN_INX=1, GUI_MAX_CONN
100	  CONTINUE
	  CALL LISTTOP(BUF_INX,GUI_FROM_LINK_QUES(1,CONN_INX),ST)
	  IF(ST.EQ.2) GOTO 8000				! next connection
C
	  NR_BUFS = NR_BUFS + 1
C
C Check buffer condition
C
	  CALL GUIMGR_CHECK_BUF(BUF_INX,'GUICBUF')
C
C IF DEBUG MODE ENABLED print connection, buffer, status
C
	  IF(GUI_DBG_UNIT.GT.0) THEN
	    WRITE(GUI_DBG_UNIT,*)IAM(),
     *	      'GUICBUF:  CONN,BUFF,STS=',
     *	      CONN_INX, BUF_INX, 
     *	      GUI_LINK_BUF(GUI_BUF_IO_STS_OFF,BUF_INX)
	  ENDIF
C
C CHECK THE BUFFER STATUS TO DETERMINE WHAT PROCESSING IS
C REQUIRED BY GUICBUF.
C
	  IF(GUI_LINK_BUF(GUI_BUF_IO_STS_OFF,BUF_INX).EQ.GUI_GODRED)
     *	    GOTO 1000
	  IF(GUI_LINK_BUF(GUI_BUF_IO_STS_OFF,BUF_INX).EQ.GUI_BADRED) 
     *	    GOTO 2000
	  IF(GUI_LINK_BUF(GUI_BUF_IO_STS_OFF,BUF_INX).EQ.GUI_DISRED) 
     *	    GOTO 3000
	  IF(GUI_LINK_BUF(GUI_BUF_IO_STS_OFF,BUF_INX).EQ.GUI_GODWRT) 
     *	    GOTO 4000
	  IF(GUI_LINK_BUF(GUI_BUF_IO_STS_OFF,BUF_INX).EQ.GUI_BADWRT) 
     *	    GOTO 5000
	  IF(GUI_LINK_BUF(GUI_BUF_IO_STS_OFF,BUF_INX).EQ.GUI_DISWRT) 
     *	    GOTO 6000
	  IF(GUI_LINK_BUF(GUI_BUF_IO_STS_OFF,BUF_INX).EQ.GUI_NOCWRT) 
     *	    GOTO 5000
C
C  ERROR HERE, BAD BUFFER STATUS FROM GUILINK, RELEASE BUFFER
C
	  WRITE(*) IAM(), 'Bad client buf_sts: ', BUF_INX,
     *	      GUI_LINK_BUF(GUI_BUF_IO_STS_OFF,BUF_INX)
	  GOTO 7000
C
C	BUFFER STATUS = GODRED( GOOD READ, NO ERROR)
C
1000	  CONTINUE
C
C IF DEBUG MODE ENABLED DUMP TRANSACTION TO CONSOLE.
C
	  IF(GUI_DBG_UNIT.GT.0) THEN
	    CALL GUICBUF_DUMP_BUF(GUI_DBG_UNIT,
     *		'GUICBUF: ',GUI_LINK_BUF(1,BUF_INX),BUF_INX)
	  ENDIF
C
	  IF(GUI_RED_IGNORE(CONN_INX).EQ.1) THEN
	    IF(GUI_DBG_UNIT.GT.0) THEN
	      WRITE(GUI_DBG_UNIT,*)IAM(),
     *	      'GUICBUF:  BUFFER RED_IGNORED ', BUF_INX
	    ENDIF
	    GOTO 7000   !IGNORE BUFFER
	  ENDIF
C
	  IF(GUI_ASMB_BUF_STS(CONN_INX).EQ.GUI_ASMB_BUF_STS_COMPLETED) THEN
	    IF(GUI_DBG_UNIT.GT.0) THEN
	      WRITE(GUI_DBG_UNIT,*)IAM(),
     *	      'GUICBUF:  ASMB_BUF_STS is COMPLETED for conn ', CONN_INX
	    ENDIF
	    GOTO 8000					     ! wait
	  ENDIF
C
	  IOLEN   = GUI_LINK_BUF(GUI_BUF_LEN_OFF,BUF_INX)    !Length of I/O
C
	  IO_BUF_OFF = GUI_DEASMB_BUF_OFF(CONN_INX) !CUR OFFSET INTO GUI_LINK_BUF
C
1100	  CONTINUE
	  IO_LEN_LEFT = IOLEN - IO_BUF_OFF	!BYTES LEFT IN GUI_LINK_BUF
	  IF(IO_LEN_LEFT.EQ.0) GOTO 7000
C
C If GUI_ASMB_BUF_OFF(CONN_INX) is 2 or greater then the length of the 
c current transaction is known. go and do a move from GUI_LINK_BUF to
C GUI_ASMB_BUF
C
	  IF(GUI_ASMB_BUF_OFF(CONN_INX).GE.2) THEN
	    MSG_LEN_LEFT = GUI_ASMB_MSG_LEN(CONN_INX) - 
     *			 GUI_ASMB_BUF_OFF(CONN_INX)
					!BYTES LEFT FOR CURRENT TRANS
	    IF(MSG_LEN_LEFT.GT.IO_LEN_LEFT) THEN
C
C not enough bytes left in GUI_link_buf
C
	      CALL LIB$MOVC3(IO_LEN_LEFT, 
     *	        GUI_LINK_BBUF(GUI_BUF_DAT_BOFF+IO_BUF_OFF,BUF_INX),
     *	        GUI_ASMB_BBUF(GUI_ASMB_BUF_OFF(CONN_INX)+1,CONN_INX))
	      GUI_ASMB_BUF_OFF(CONN_INX) = 
     *			GUI_ASMB_BUF_OFF(CONN_INX) + IO_LEN_LEFT
	      GOTO 7000		    !RELEASE CURRENT BUFFER
	    ELSE			    !GUI_ASMB_BUF IS FULL
	      CALL LIB$MOVC3(MSG_LEN_LEFT, 
     *	        GUI_LINK_BBUF(GUI_BUF_DAT_BOFF+IO_BUF_OFF,BUF_INX),
     *	        GUI_ASMB_BBUF(GUI_ASMB_BUF_OFF(CONN_INX)+1,CONN_INX))
	      IO_BUF_OFF = IO_BUF_OFF + MSG_LEN_LEFT
	      GUI_ASMB_BUF_OFF(CONN_INX) = 0
	      GOTO 1200			! this is the only way to get TO 1200
	    ENDIF
	  ELSE
	    IF(GUI_ASMB_BUF_OFF(CONN_INX).EQ.0) THEN
	      GUI_ASMB_BBUF(1,CONN_INX) = 
     *	      GUI_LINK_BBUF(GUI_BUF_DAT_BOFF+IO_BUF_OFF+0,BUF_INX)
	      IF(IO_LEN_LEFT.GT.1) THEN
	        GUI_ASMB_BBUF(2,CONN_INX) = 
     *	        GUI_LINK_BBUF(GUI_BUF_DAT_BOFF+IO_BUF_OFF+1,BUF_INX)
	        IO_BUF_OFF=IO_BUF_OFF+2
	      ELSE
	        GUI_ASMB_BUF_OFF(CONN_INX)=1
	        GOTO 7000		!release current buffer, get next
	      ENDIF
	    ELSE
	      GUI_ASMB_BBUF(2,CONN_INX) = 
     *	      GUI_LINK_BBUF(GUI_BUF_DAT_BOFF+IO_BUF_OFF+0,BUF_INX)
	      IO_BUF_OFF=IO_BUF_OFF+1
	    ENDIF
C
C come here if assemble buf has EXACTLY 2 characters that specify length
C
	    I4TEMP=0
	    I1TEMP(2)=GUI_ASMB_BBUF(1,CONN_INX)
	    I1TEMP(1)=GUI_ASMB_BBUF(2,CONN_INX)
	    GUI_ASMB_MSG_LEN(CONN_INX)=I4TEMP	!Length of transaction in Bytes
	    GUI_ASMB_BUF_OFF(CONN_INX)=2	!Reset buffer offset to byte 2
C
C  if the other system sends a bad length in the buffer then force
c  a disconnect and ignore all buffers until reconnection has been
c  established
c
	    IF(GUI_ASMB_MSG_LEN(CONN_INX).LT.GUI_MIN_MSG_LEN
     *	   .OR.GUI_ASMB_MSG_LEN(CONN_INX).GT.GUI_MAX_MSG_LEN) THEN
	      CALL FASTSET(BLANK,GUI_MES_BUF,33)
	      WRITE(GUI_MES_CBUF,9000) IAM(), GUI_ASMB_MSG_LEN(CONN_INX)
9000	      FORMAT(A,'GUICBUF: Bad MSG len:',I)
	      CALL WRITEBRK(GUI_MES_CBUF)
	      GUI_RED_IGNORE(CONN_INX)=1 !IGNORE ALL GOOD READS UNTIL DISCON
	      CALL GUIQUE(GUI_DISC_RED_IGN,ST)
	      IF(ST.NE.0) THEN		    ! GUIQUE SHOULD REPORT... ???
		TYPE *, IAM(),'ERROR QUEUEING GUILINK:  ',ST
	      ENDIF
	      GOTO 7000			!IGNORE BUFFER
	    ENDIF
	    GOTO 1100
	  ENDIF
C
1200	  CONTINUE    ! There is only one way to get here - see GOTO above
C
C full buffer !!!
C
	  GUI_ASMB_BUF_STS(CONN_INX) = GUI_ASMB_BUF_STS_COMPLETED
C
C Get free GUIWORKER queue
C
1210	  CONTINUE
	  CALL RTL(WORKER_BUF, GUI_WORKER_FREE_QUEUE, ST)
	  IF(ST.EQ.2) THEN
	     CALL OPSTXT('GUICBUF: No free GUIWORKER queue')
	     CALL XWAIT(100,1,ST)
	     GOTO  1210
	  ENDIF
	  MOVE_SIZE = MIN(GUI_ASMB_MSG_LEN(CONN_INX),GUI_BUF_SIZ*4)
	  CALL MOVBYT(GUI_ASMB_BBUF(1,CONN_INX),1,
     *                GUI_WORKER_BBUF(1,WORKER_BUF),1,
     *                MOVE_SIZE)
C
	  GUI_WORKER_SOURCE_ID(WORKER_BUF) = CONN_INX
	  GUI_WORKER_MSG_LEN(WORKER_BUF)   = MOVE_SIZE
C
	  GUI_ASMB_BUF_STS(CONN_INX) = GUI_ASMB_BUF_STS_INPROG ! CAN BE USED
	  GUI_ASMB_MSG_LEN(CONN_INX) = 0
C
C Queue this message to GUIWORKER
C
          CALL ABL(WORKER_BUF, GUI_TO_WORKER_QUEUE, ST)
C
	  IF(MSG_LEN_LEFT.EQ.IO_LEN_LEFT) GOTO 7000
C
	  GUI_DEASMB_BUF_OFF(CONN_INX) = IO_BUF_OFF !OFFSET INTO GUI_LINK_BUF
	  GOTO 8000
C
C  buffer status = BADRED( read with error)
C
2000	  CONTINUE
	  GOTO 7000
C
C  buffer status = disred( read with error, forced disconnect)
c  ignore current buffer
c
3000	  CONTINUE
	  GUI_ASMB_BUF_OFF(CONN_INX)   = 0
	  GUI_RED_IGNORE(CONN_INX) = 0	!ALL GOOD READS AFTER THIS WILL BE OKAY
	  GUI_CONN_SIGNED_ON(CONN_INX) = .FALSE.
	  GOTO 7000
C
C buffer status = godwrt( write with no errors)
C
4000	  CONTINUE
	  GOTO 7000	    !RELEASE BUFFER
C
C buffer status
C   BADWRT( write with error)
C   DISWRT( write with error, forced disconnect)
C   NOCWRT( couldn't send at all)
C
5000	  CONTINUE
C
	  CALL FASTSET(BLANK,GUI_MES_BUF,33)
	  WRITE(GUI_MES_CBUF, 9001) IAM(), 'GUICBUF: Bad write, STS= ',
     *	      GUI_LINK_BUF(GUI_BUF_IO_STS_OFF,BUF_INX)
9001	  FORMAT(A,A,I)
	  CALL WRITEBRK(GUI_MES_CBUF)
	  GOTO 7000
C
C buffer status = diswrt( write with error, forced disconnect)
C ignore current buffer 
C kill read .....
C
6000	  CONTINUE
	  GUI_ASMB_BUF_OFF(CONN_INX)   = 0
	  GUI_RED_IGNORE(CONN_INX) = 0	!all good reads after this will be okay
	  GUI_CONN_SIGNED_ON(CONN_INX) = .FALSE.
	  GOTO 5000
C
C put the buffer back onto the free list.
C
7000	  CONTINUE
	  GUI_DEASMB_BUF_OFF(CONN_INX) = 0 !OFFSET INTO GUI_LINK_BUF
	  CALL RTL(BUF_INX, GUI_FROM_LINK_QUES(1,CONN_INX), ST)
C
	  CALL ABL(BUF_INX, GUI_LINK_FRE_QUE, ST)
	  GOTO 100			    ! continue with same connection
C
8000	CONTINUE			    ! continue with next connection
C
	RETURN
	END
