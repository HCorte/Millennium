C  GXSRC:CRSSIM.FOR
C  
C  $Log:   GXAFXT:[GOLS]CRSSIM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:44:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   24 Jan 1994 16:37:50   JPJ
C  Updated call to CRSXMT to be correct
C  
C     Rev 1.1   03 Jan 1994 20:04:38   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 17:34:20   SYSTEM
C  Initial revision.
C
C
C CRSPRO.FOR
C
C V01 18-NOV-91 KWP INITIAL RELEASE FOR VAX
C
C This program handles the interface between the game
C and the bisync programs.  This program will handle:
C requests, responses, and errors.  Requests are placed
C on the CRSPRO'S queue by INSPRO.  The requests are then formatted
C and placed on the SNDQUE.  Responses are read from the RCVQUE,
C formatted, and then sent to the appropriate processing tasks.
C Errors are read from the ERRQUE and are reported and released.
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
	PROGRAM CRSPRO
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:QUECOM.DEF'
	INCLUDE 'INCLIB:CRSCOM.DEF'
	INCLUDE 'INCLIB:TCPEVN.DEF'
C
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4   TBUF                !Transmit buffer
	INTEGER*4   PBUF                !Procom buffer
	INTEGER*4   ST,I                !Work variables
	INTEGER*4   STATUS		!Event status
C
	REAL*8      R8_TCPNAME          !Assist name for RUNTSK
	DATA        R8_TCPNAME /'TCPASST '/
C
	CALL COPYRITE
C
C
	TYPE *
	TYPE *,'<<<<< CRSSIM V01 >>>>>'
	TYPE *
C
C CREATE THE COMMON EVENT FLAG CLUSTER.
C
        STATUS=SYS$ASCEFC(%VAL(TC_EVNTIMER),TC_EVNNAME,0,0)
        IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
	CALL FASTSET(0,CONBLK,CONCSB*2048)
C
C DEFINE SYSTEM QUEUES
C
	CALL DEFLST(INQUE,NUMPRO)
	CALL DEFLST(FREEQ,NUMPRO)
	CALL DEFLST(PRFQUE,STALEN)
	CALL DEFLST(REPVQUE,REPVLEN)
	DO 300 I=1,STALEN
	  CALL ABL(I,PRFQUE,ST)
300	CONTINUE
	DO 310 I=1,NUMPRO
	  CALL RELBUF(I)
310	CONTINUE
	DO 320 I=1,NUMAPPQUE
	  CALL DEFLST(QUETAB(1,I),NUMPRO)
320	CONTINUE
	CALL RESCRS
C
C SET THE BUFFER NUMBERS IN THE HEADER.
C
	DO 110 TBUF=1,TCBUFMAX
	  TCBUF(TCBUFNUM,TBUF)=TBUF
110	CONTINUE
C
C SET UP ALL QUEUES.
C
	CALL DEFLST(FREQUE,TCBUFMAX)
	CALL DEFLST(SNDQUE,TCBUFMAX)
	CALL DEFLST(RCVQUE,TCBUFMAX)
C
	DO 120 I=1,TCBUFMAX
	  CALL ABL(I,FREQUE,ST)
120	CONTINUE
C
C
	CRSWAIT=500               !CRSPRO RUN EVERY 500 MS
	TCP_NUMBUF=0
	TCP_NUMREC=0
	TCP_BUFFOFF=0
	TCP_BUFFLEN=0
C
C WAIT FOR SOMETHING TO DO
C IF END OF DAY THEN STOP
C
1000	CONTINUE
	CALL XWAIT(CRSWAIT,1,ST)                !RUN X TIMES/SECOND
C
C CHECK FOR TIMED OUT BUFFERS.
C
	CALL CRSTIM
C
C SEE IF ANYTHING ON RECEIVE QUEUE, AND IF
C SO, FORMAT RESPONSE AND SEND TO DISPAT.
C
2000	CONTINUE
	CALL RTL(TBUF,RCVQUE,ST)
	IF(ST.NE.2) THEN
	  CALL CRSRCV(TBUF)
	  GOTO 2000
	ENDIF
C
C CALL CRSXMT TO FORMAT WRITE BUFFERS
C
4000	CONTINUE
	CALL CRSXMT(0,ST)
	IF(ST.NE.0) GOTO 4000
C
	GOTO 1000
	END
