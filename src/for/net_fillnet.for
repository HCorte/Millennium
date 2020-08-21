C
C SUBROUTINE NET_FILLNET
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NET_FILLNET.FOV                              $
C  $Date::   17 Apr 1996 14:11:32                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - net_fillnet.for ***
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
C	FILL THE BUFFER WITH TIMED OUT TRANSACTION HEADER.
C
C Calling Sequence:
C	CALL NET_FILLNET(BUFFER, NODE, NETBUF_NO, DCNBUF_NO, RETURN_STATUS)
C
C Input:
C	BUFFER		- DCN CONTROL BUFFER
C	NODE		- NODE THE DATA IS COMING FROM
C
C Output:
C	NETBUF_NO	- NETLOG BUFFER WITH DATA FILLED AS ORIGINAL SEND BUF
C	DCNBUF_NO	- DCN BUFFER NO
C	RETURN_STATUS	- 0 IF OK
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE NET_FILLNET(BUFFER, NODE, NETBUF_NO,
     *                         DCNBUF_NO, RETURN_STATUS)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DN_LINK.DEF' 	!DECnet Structures
	INCLUDE 'INCLIB:DN_BLOCK.DEF'  	!DECnet Data Blocks
C
C LOCAL DECLARATIONS
C
	INTEGER*4	DCNBUF_NO,
     *			IDX,
     *			NETBUF_NO,
     *			NODE,
     *			ORIGINAL_BUFFER,
     *			RETURN_STATUS,
     *			STATUS
C
	RECORD /DN_BUFFER_STRUCT/ BUFFER		! BUFFER ARGUMENT
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c FIND THE LINK BLOCK & CHECK ITS INDEX.
C
	IDX = BUFFER.LINK				! LINK BLOCK INDEX
	IF (IDX .LE. 0) THEN				! THIS IS CLEARLY WRONG!
	  RETURN_STATUS = 1	   
	  GOTO 9999
	ENDIF
C
	IF (IDX .GT. NETSYS) THEN			! THIS IS CLEARLY WRONG!
	  RETURN_STATUS = 13
	  GOTO 9999
	ENDIF
C
C WE HAVE A VALID LINK BLOCK ID, CAN WE WRITE TO IT?
C
	IF (DN_LINK(IDX).STATE .NE. STATE_RUNNING) THEN
	  RETURN_STATUS = 2
	  GOTO 9999
	ENDIF
C
C MAKE SURE WE HAVE A VALID BUFFER POINTER ...
C
	IF (BUFFER.DBUFFER .EQ. 0) THEN
	  RETURN_STATUS = 3
	  GOTO 9999
	ENDIF
C
C MAKE SURE WE HAVE A VALID CHANNEL ...
C
	IF (DN_LINK(IDX).CHANNEL .EQ. 0) THEN
	  RETURN_STATUS = 4
	  GOTO 9999
	ENDIF
C
C GET NETWORK BUFFER ...
C
	CALL EXTRABUF(NETBUF_NO, 1, STATUS)
	IF (STATUS .EQ. 2) THEN
	  RETURN_STATUS = 5
	  GOTO 9999
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C GET DCN BUFFER NUMBER FROM FREE QUEUE
C
100	CONTINUE
	CALL RTL(DCNBUF_NO, DCN_FREE, STATUS)
	IF (STATUS .EQ. 2) THEN
	  CALL OPS('*** NET_FILLNET - COULD NOT GET A DCNBUF_NO ***',
     *             NODE, 0)
	  CALL XWAIT(1, 2, STATUS)			! WAIT A SEC & TRY AGAIN
	  GOTO 100
	ENDIF
C
C GOT A DCN BUFFER ... SET IT UP.
C
	CALL DN_BUILD_TIMEOUT(DN_BUFFER(DCNBUF_NO), NETBUF_NO, NODE)
C
C FILL UP NETLOG BUFFER WITH ORIGINAL DATA
C (SO THAT NETLOG WILL DO APROPRIATE ACTION).
C
	ORIGINAL_BUFFER = BUFFER.DBUFFER_NO
C
	CALL FASTMOV(NETBUF(1, ORIGINAL_BUFFER),
     *               NETBUF(1, NETBUF_NO),
     *               HDRSIZ + 1)
C
	RETURN_STATUS = 0				! ALWAYS OK
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
