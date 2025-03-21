C
C PROGRAM TAPLOG
C $Log:   GXAFXT:[GOLS]TAPLOG.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:27:46   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   02 Sep 1994 18:13:52   HXK
C  Merge of May,June RFSS batch 
C  
C     Rev 1.1   05 Jun 1994 22:17:58   HXK
C  ADDED WEOT, XREWIND BEFORE TAPCLOS FOR SETEC OY (ICS USE).
C  
C     Rev 1.0   21 Jan 1993 17:48:36   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - taplog.for **
C
C TAPLOG.FOR
C
C V04 22-APR-92 GCAN CHANGED USRCLOS TO TAPCLOS (TKO92002)
C V03 15-MAY-91 TKO  CHANGE USRCLOS TO TAPCLOS, ESTABLISH NOFTLSIG
C V02 09-MAY-91 MP   ADDED CALL TO SNIF_AND_WRKSET
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C
C TRANSACTION TAPE LOGGING PROGRAM
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
	PROGRAM TAPLOG
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LOGCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
C
	INTEGER*4 FDB(7),MESS(EDLEN),HDRBUF(2048)
	INTEGER*4 TST, BL, ST, CURTAP
C
	INTEGER*4   NOFTLSIG
	EXTERNAL    NOFTLSIG
C
C
	CALL COPYRITE
C
C V02
	CALL SNIF_AND_WRKSET
C
C Establish NOFTLSIG as the error condition handler for TAPLOG since
C TAPLOG must always keep on running.
C
	CALL LIB$ESTABLISH (NOFTLSIG)
C
C
C
	CURTAP=0
	MESS(1)=TAP
C
C START OF TAPE LOGGER RUN LOOP
C
10	CONTINUE
	CALL HOLD(0,ST)
	IF(DAYSTS.EQ.DSSUSP) GOTO 10
C
C SCAN ALL MEMORY LOG BUFFERS FOR TAPE LOGGING REQUESTS
C IF TAPE LOGGING ACTIVE THEN WRITE BUFFER TO TAPE AND
C SET BUFFER STATUS TO USED, IF TAPE LOGGING HAS TERMINATED
C THE JUST RELEASE THE BUFFER BUF SETTING THE STATUS TO USED.
C
C
	DO 100 BL=1,NUMLOG
	IF(LOGBUF(BSTATE,BL).NE.LTAPE) GOTO 100
	IF(P(TAPESW).NE.0) THEN
	  LOGBUF(BSTATE,BL)=LTOUA
	  CALL WTAPEW(FDB,LOGBUF(DSKREC,BL),TST)
	  IF(TST.NE.0) THEN
	    MESS(2)=TELOG
	    MESS(3)=1
	    MESS(4)=TST
	    CALL QUEMES(MESS)
            CALL WEOT(FDB,ST)
            CALL XREWIND(FDB,ST)
	    CALL TAPCLOS(FDB,ST)
	    CURTAP=0
	    P(TAPESW)=0
	    TST=0
	  ENDIF
	ENDIF
	IF(LOGBUF(TSKMAP,BL).NE.0) THEN
	  CALL RESTSK(LOGBUF(TSKMAP,BL))
	  LOGBUF(TSKMAP,BL)=0
	ENDIF
	LOGBUF(BSTATE,BL)=LGUSD
100	CONTINUE
C
C IF A TAPESW CHANGE HAS OCCURED THEN NOTIFY THE
C OPERATOR AND CALL THE TAPE INITIALIZATION ROUTINE.
C
	IF(P(TAPESW).NE.CURTAP) THEN
	  MESS(2)=TELOG
	  MESS(3)=6
	  MESS(4)=CURTAP
	  MESS(5)=P(TAPESW)
	  CALL QUEMES(MESS)
	  CALL TAPCHG(FDB,CURTAP,P(TAPESW))
	  IF(P(TAPESW).NE.0) THEN
	    CALL TAPHDR(HDRBUF,0)
	    CALL WTAPEW(FDB,HDRBUF,TST)
	    IF(TST.NE.0) THEN
	      MESS(2)=TELOG
	      MESS(3)=1
	      MESS(4)=TST
	      CALL QUEMES(MESS)
              CALL WEOT(FDB,ST)
              CALL XREWIND(FDB,ST)
	      CALL TAPCLOS(FDB,ST)
	      CURTAP=0
	      P(TAPESW)=0
	      TST=0
	    ENDIF
	  ENDIF
	ENDIF
	IF(P(LOGSTP).EQ.0) GOTO 10
C
C END OF DAY, OR FATAL LOGGER ERROR. WRITE ALL MEMORY RESIDENT
C LOGGER BUFFERS TO TAPE, WRITE TAPE MARK, REWIND, AND STOP.
C
C
	DO 200 BL=1,NUMLOG
	IF(LOGBUF(BLONUM,BL).EQ.0) GOTO 200
	IF(LOGBUF(BLTAPE,BL).NE.0.AND.
     *	   LOGBUF(BSTATE,BL).NE.LTAPE) GOTO 200
	IF(P(TAPESW).NE.0) THEN
	  LOGBUF(LRCNUM,BL)=LOGBUF(BLONUM,BL)
	  CALL WTAPEW(FDB,LOGBUF(DSKREC,BL),TST)
	  IF(TST.NE.0) THEN
	    MESS(2)=TELOG
	    MESS(3)=1
	    MESS(4)=TST
	    CALL QUEMES(MESS)
            CALL WEOT(FDB,ST)
            CALL XREWIND(FDB,ST)
	    CALL TAPCLOS(FDB,ST)
	    P(TAPESW)=0
	    TST=0
	  ENDIF
	ENDIF
200	CONTINUE
C
C IF TAPE LOGGING IS ACTIVE THEN WRITE END OF TAPE MARK AND
C REWIND THE TAPE.
C
	IF(P(TAPESW).NE.0) THEN
	  P(TAPESW)=0
	  CALL TAPCHG(FDB,CURTAP,P(TAPESW))
	ENDIF
	IF(DAYSTS.EQ.DSSUSP) GOTO 10
	CALL GSTOP(GEXIT_SUCCESS)
	END
