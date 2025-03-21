C
C PROGRAM REPCAN
C
C REPCAN.FOR
C
C V05 03-OCT-2000 UXN UPDAGTINF ADDED. P(SUPFIL) REMOVED.
C                     TYPE STATEMENTS REPLACED WITH OPS()
C V04 28-JAN-1998 UXN FRACTIONS AND UNFRACTIONS ADDED.
C V03 27-JUN-1993 HXK ADDE AGTINF.DEF
C V02 12-NOV-1991 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C FOR SYSTEM RESTARTS.
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
	PROGRAM REPCAN
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
C
C
	INTEGER*4 WAGSER, BUF, STATUS, TASK
	INTEGER*4 WRKBUF(TRALEN),FDB(7)
	INTEGER*4 LOGREC(LREC*3)
	INTEGER*4 I
C
	CALL COPYRITE
	CALL SNIF_AND_WRKSET
C
C
	TASK=RPC
	CALL OPNRPC(FDB)
C
C WAIT FOR SOMETHING TO DO
C IF REPROCESSING DONE THEN STOP.
C
10	CONTINUE
	IF(P(LOGSTP).NE.0) THEN
	  CALL CLSRPC(FDB)
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
	CALL HOLD(0,STATUS)
C
20	CONTINUE
	CALL TOPQUE(TASK,BUF)
	IF(BUF.EQ.0) GOTO 10
	CALL LOGTRA(TRABUF,PRO(WRKTAB,BUF))
C
C REAPPLY CANCELLATIONS AND DELETIONS TO THE TMF
C
	IF(TRABUF(TTYP).EQ.TCAN.OR.TRABUF(TTYP).EQ.TINC) THEN
	  WAGSER=TRABUF(TWCSER)
	  CALL RLOG(WAGSER,LOGREC,TASK,STATUS)
	  IF(STATUS.NE.0) THEN
	    CALL OPS('Logger read error for wager serial',WAGSER,0)
	    GOTO 100
	  ENDIF
	  CALL LOGTRA(WRKBUF,LOGREC)
	  IF(TRABUF(TGAM).NE.WRKBUF(TGAM).OR.
     *	     WRKBUF(TTYP).NE.TWAG) THEN
             CALL OPS('Invalid record read for cancellation > ',TRABUF(TSER),0)
	    GOTO 100
	  ENDIF
	  WRKBUF(TSTAT)=VOID
	  IF(TRABUF(TTYP).EQ.TINC) WRKBUF(TSTAT)=INCA
	  CALL TRALOG(WRKBUF,LOGREC)
	  CALL WLOG(WAGSER,LOGREC,TASK)
	  GOTO 100
	ENDIF
C
C REPROCESS AGENT ADJUSTMENTS
C
	IF(TRABUF(TTYP).EQ.TSPE.AND.TRABUF(TSFUN).EQ.THASF) THEN
            CALL TRAHSF(FDB,RPC,TRABUF)
	ENDIF
C
C REPROCESS ONLINE AGENT UPDATES
C
	IF(TRABUF(TTYP).EQ.TSPE.AND.TRABUF(TSFUN).EQ.TAGTINF) THEN
           CALL UPDAGTINF(FDB,RPC,TRABUF)
	ENDIF
C
C REPROCESS FRACTIONS
C
	IF(TRABUF(TTYP).EQ.TSPE.AND.TRABUF(TSFUN).EQ.TFRC) THEN
	    WAGSER = TRABUF(TSDT1)
	    CALL RLOG(WAGSER,LOGREC,TASK,STATUS)
	    IF(STATUS.NE.0) THEN
	        CALL OPS('Logger read error for fraction > ',WAGSER,0)
	        GOTO 100
	    ENDIF
	    CALL LOGTRA(WRKBUF,LOGREC)
	    IF(WRKBUF(TSTAT).NE.GOOD) GOTO 100 ! Ticket already fractioned ?
	    WRKBUF(TSTAT)  = FRAC
	    WRKBUF(TWCSER) = TRABUF(TSER)
	    WRKBUF(TWCTER) = TRABUF(TTER)
	    CALL TRALOG(WRKBUF,LOGREC)
	    CALL WLOG(WAGSER,LOGREC,TASK)  
	    GOTO 100
	ENDIF 	
C
C REPROCESS UNFRACTIONS
C
	IF(TRABUF(TTYP).EQ.TSPE.AND.TRABUF(TSFUN).EQ.TUNFRC) THEN
	    WAGSER = TRABUF(TSDT1)
	    CALL RLOG(WAGSER,LOGREC,TASK,STATUS)	    
	    IF(STATUS.NE.0) THEN
	        CALL OPS('Logger read error for unfraction > ',WAGSER,0)
	        GOTO 100
	    ENDIF
	    CALL LOGTRA(WRKBUF,LOGREC)
	    IF(WRKBUF(TSTAT).NE.FRAC) GOTO 100 ! Ticket already unfractioned ?
C
C UNFRACTION THE ORIGINAL TICKET
C
	    WRKBUF(TSTAT)  = GOOD
	    WRKBUF(TWCSER) = TRABUF(TSER)	    
	    WRKBUF(TWCTER) = TRABUF(TTER)
	    CALL TRALOG(WRKBUF,LOGREC)
	    CALL WLOG(WAGSER,LOGREC,TASK)
C
C CHANGE THE STATUSES FOR THE FRACTIONED PARTS.
C
	    DO I = 1, TRABUF(TSDT4)
		WAGSER = TRABUF(TSDT5+I-1)
		CALL RLOG(WAGSER,LOGREC,TASK,STATUS)
		IF(STATUS.NE.0) THEN
	           CALL OPS('Logger read error for fractioned ticket > ',
     *                      WAGSER,0)
	           GOTO 100
		ENDIF
		CALL LOGTRA(WRKBUF,LOGREC)
		WRKBUF(TSTAT) = REJT
		WRKBUF(TERR)  = NFRA
		CALL TRALOG(WRKBUF,LOGREC)
		CALL WLOG(WAGSER,LOGREC,TASK)
	    ENDDO	    	    
	    GOTO 100
	ENDIF 	
C
100	CONTINUE
	CALL DQUTRA(TASK,BUF)
	CALL RELBUF(BUF)
	GOTO 20
	END
