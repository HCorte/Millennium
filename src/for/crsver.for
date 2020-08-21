C  GXSRC:CRSVER.FOR
C  
C  $Log:   GXAFIP:[GOLS]CRSVER.FOV  $
C  
C     Rev 1.1   28 Jan 1997 19:33:16   RXK
C  Message numbers for Errlog changed 
C  
C     Rev 1.0   17 Apr 1996 12:44:26   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   01 Sep 1994 17:13:24   MCM
C  SWAPPING BYTES IS NO LONGER NECESSARY ON THE DEC LMS
C  
C     Rev 1.1   03 Jan 1994 20:05:10   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 17:34:28   SYSTEM
C  Initial revision.
C
C
C
C V01 18-NOV-91 KWP INITIAL RELEASE FOR VAX
C
C THIS SUBROUTINE WILL DETERMINE HOW MANY ITEMS EXIST ON THE
C SNDQUE, AND WILL CHECK EACH ITEM TO DETERMINE IF CRSPRO
C HAS ALREADY TIMED THEM OUT.  IF THEY HAVE NOT TIMED OUT, ADD
C THEM BACK TO THE QUEUE.  IF THEY HAVE TIMED OUT, FREE UP THE
C TRANSMIT BUFFER.
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
	SUBROUTINE CRSVER
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:CRSCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
C
	INTEGER*4   XBUF            !Xmit buffer
	INTEGER*4   PBUF            !Procom buffer #
	INTEGER*4   ST              !Queue status
	INTEGER*4   STATUS          !VERTIMER status
	INTEGER*4   USEDBUF         !Number of used buffers
	INTEGER*4   I               !Work variable
	INTEGER*4   TEMP            !To extract PROCOM buffer
	INTEGER*4   XRFNUM          !Xrf #
	INTEGER*4   MESS(EDLEN)	    !ERRLOG Message Buffer
	INTEGER*4   BLEN	    !Byte Length
	INTEGER*4   ROFF	    !Record offset
	INTEGER*4   IOLEN           !Byte length of xfer
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
C DETERMINE THE NUMBER OF ITEMS ON THE SNDQUE
C
	CALL LISTSIZE(SNDQUE,USEDBUF)
C
C FOR EACH BUFFER ON THE SNDQUE, DETERMINE IF THEY HAVE ALREADY
C TIMED OUT.
C
	DO 100 I=1,USEDBUF
C
C ATTEMPT TO READ NEXT BUFFER ON THE QUEUE.
C
	  CALL RTL(XBUF,SNDQUE,ST)
	  IF(ST.NE.2)THEN
C
C EXTRACT OUT THE PROCOM BUFFER NUMBER AND THE INTERNAL
C SERIAL NUMBER  OF THE FIRST TRANSACTION IN THE BUFFER.
C
	    I4TEMP=0
	    I1TEMP(1)=B_TCBUF(B_TCBUFBEG+2,XBUF)
	    I1TEMP(2)=B_TCBUF(B_TCBUFBEG+3,XBUF)
	    PBUF=I4TEMP			!Procom Buffer #
C
C CHECK TO SEE IF THE BUFFER HAS ALREADY TIMED OUT.
C
	    CALL VERTIMER(PBUF,STATUS)
	    IF(STATUS.EQ.0) THEN
C
C IF THE FIRST TRANSACTION HAS NOT TIMED OUT
C THEN NONE OF THE TRANSACTIONS HAS TIMED OUT.
C
	      CALL ABL(XBUF,SNDQUE,ST)
	    ELSE
C
C IF THE FIRST TRANSACTION HAS TIMED OUT, ALL OF THE
C TRANSACTIONS IN THAT BUFFER HAS ALSO TIMED OUT. THE
C BUFFER CAN BE ADDED TO THE FREQUE AFTER THE XRF NUMBERS
C HAVE BEEN DISPLAYED AS ERRORS.
C
	      ROFF=0
	      IOLEN   =TCBUF(TCBUFLEN,XBUF)	    !Length of I/O
C
1000	      CONTINUE
C
	      I4TEMP=0
	      I1TEMP(1)=B_TCBUF(B_TCBUFBEG+ROFF+0,XBUF)
	      I1TEMP(2)=B_TCBUF(B_TCBUFBEG+ROFF+1,XBUF)
	      BLEN=I4TEMP			!Length of transaction in Bytes
C
	      I4TEMP=0
	      I1TEMP(1)=B_TCBUF(B_TCBUFBEG+ROFF+2,XBUF)
	      I1TEMP(2)=B_TCBUF(B_TCBUFBEG+ROFF+3,XBUF)
	      PBUF=I4TEMP			!Procom Buffer #
C
	      I4TEMP=0
	      I1TEMP(1)=B_TCBUF(B_TCBUFBEG+ROFF+4,XBUF)
	      I1TEMP(2)=B_TCBUF(B_TCBUFBEG+ROFF+5,XBUF)
	      I1TEMP(3)=B_TCBUF(B_TCBUFBEG+ROFF+6,XBUF)
	      I1TEMP(4)=B_TCBUF(B_TCBUFBEG+ROFF+7,XBUF)
	      XRFNUM=I4TEMP			!Cross Reference #
C
C DISPLAY ERROR MESSAGE TO CONSOLE.
C
	      MESS(1)=CRS
	      MESS(2)=TEGEN
	      MESS(3)=34
	      MESS(4)=XRFNUM
	      MESS(5)=PBUF
	      CALL QUEMES(MESS)
C
	      ROFF = ROFF+BLEN
C
C CHECK FOR ANOTHER RECORD IN THE TCBUF
C
	      IF(ROFF+1.LT.IOLEN) THEN
	        I4TEMP=0
	        I1TEMP(1)=B_TCBUF(B_TCBUFBEG+ROFF+0,XBUF)
	        I1TEMP(2)=B_TCBUF(B_TCBUFBEG+ROFF+1,XBUF)
	        BLEN=I4TEMP	    !Length of next transaction in Bytes
	        IF(BLEN.GT.0) GOTO 1000
	      ENDIF
C
C FREE UP BUFFER AFTER DISPLAYING ERRORS
C
	      CALL ABL(XBUF,FREQUE,ST)
C
	    ENDIF
	  ENDIF
C
100	CONTINUE
C
	RETURN
	END
