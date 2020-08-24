C
C V01 12-MAR-2001 UXN Initial release.
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
C=======OPTIONS/CHECK=NOOVERFLOW
	SUBROUTINE CHKQCOR(QUEUE, BUF, TIMES)
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:GLIST.DEF'

	INTEGER*4 QUEUE(*)
	INTEGER*4 BUF
	INTEGER*4 TIMES
C
	INTEGER*4 I, BUFNR, LTOP, OFF
	INTEGER*4 CNT

	INTEGER*4 MAXQUESIZ
	PARAMETER(MAXQUESIZ=2048)

	INTEGER*4 BUF_TO_ADD(MAXQUESIZ)
	INTEGER*4 TAB(MAXQUESIZ)
C
	CALL FASTSET(0,TAB,MAXQUESIZ)
C
	TAB(BUF) = 1
	CNT = 0
C
C Lock the list
C
        CALL LISTLOCK_TOP(QUEUE)
	CALL MB()

        LTOP = QUEUE (GLIST_TOP)
C
C CHECK QUEUE FOR DUPLICATES
C
        DO WHILE( LTOP .NE. QUEUE(GLIST_BOT) )
C
            OFF = LTOP + 1
            IF(OFF .GT. QUEUE(GLIST_MAX_OFFSET)) OFF = GLIST_START

            BUFNR = QUEUE(OFF)

            TAB(BUFNR) = TAB(BUFNR) + 1

	    IF(TAB(BUFNR).GT.1) GOTO 500 ! QUEUE IS CORRUPTED
C	
            LTOP = OFF
        ENDDO
C
C NO DUPLICATES FOUND 
C
	CALL LISTRLSE_TOP(QUEUE)

	TIMES = 0
	RETURN
C
C QUEUE is corrupted, re-build it and remove all duplicates from the queue.
C
500	CONTINUE
C
	CALL FASTSET(0,TAB,MAXQUESIZ)
	TAB(BUF) = 1
	CNT = 0
C
C Lock queue from both ends (already locked from TOP)
C
	CALL LISTLOCK_BOT(QUEUE) 
	CALL MB()
C
        DO WHILE( QUEUE(GLIST_TOP) .NE. QUEUE(GLIST_BOT) )
C
            OFF = QUEUE(GLIST_TOP) + 1
            IF(OFF .GT. QUEUE(GLIST_MAX_OFFSET)) OFF = GLIST_START

            BUFNR = QUEUE(OFF)

	    TAB(BUFNR) = TAB(BUFNR) + 1

	    IF(TAB(BUFNR).EQ.1) THEN
	       CNT = CNT + 1
	       BUF_TO_ADD(CNT) = BUFNR
	    ENDIF
C
	    QUEUE(GLIST_TOP)     = OFF
C
	ENDDO
C
C Add non-duplicates back to the queue.
C
	DO 10 I = 1, CNT
	   BUFNR = BUF_TO_ADD(I)

	   IF(TAB(BUFNR).GT.1) THEN
	      CALL OPS('*** QUEUE CORRUPTION ***', BUFNR, TAB(BUFNR))
	   ENDIF

	   IF(BUFNR.EQ.BUF) GOTO 10

	   OFF = QUEUE(GLIST_BOT) + 1
           IF(OFF .GT. QUEUE(GLIST_MAX_OFFSET)) OFF = GLIST_START
C
C QUEUE SHOULD NEVER GET FULL HERE
C
	   IF(OFF.EQ.QUEUE(GLIST_TOP)) THEN
	      CALL OPS('*** ERROR - QUEUE FULL ***',BUFNR,TAB(BUFNR))
	      GOTO 999 
	   ENDIF
C
C ADD TO THE GLIST
C
	   QUEUE(OFF)           = BUFNR
	   QUEUE(GLIST_BOT)     = OFF
10	CONTINUE
C
C Unlock the list
C
999	CONTINUE
C
        CALL LISTRLSE_BOT(QUEUE)
        CALL LISTRLSE_TOP(QUEUE)
C
	TIMES = TAB(BUF) - 1
C
	END
