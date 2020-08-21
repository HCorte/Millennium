C
C SUBROUTINE CHKQUEUE
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CHKQUEUE.FOV                                 $
C  $Date::   17 Apr 1996 12:33:20                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - chkq.for;1 **
C
C CHKQ.FOR
C
C V02 13-DEC-94 GPR Integrate UK changes into X2X Baseline
C V01 11-SEP-90 MRM RELEASED FOR VAX
C
C     CHKQUEUE.FTN
C
C     WILL CHECK ANY QUEUE IF DATA IS ON THE LIST
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
	SUBROUTINE CHKQUEUE(BUF,QUEUE,TIMES)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4 MAX, BUFNR, ST, TIMES, BUF, OFF
C
	PARAMETER (MAX=2048)						!V02
	INTEGER*4 TAB(MAX)     !FOR QUEUES UP TO 120 ELEMENTS LONG
C
	INTEGER*4 QUEUE(*)
	DATA TAB/MAX*0/
C
C READ ALL BUFFERS FROM THE QUEUE AND ACCUMULATE TOTALS.
C
10	CONTINUE
	CALL RTL(BUFNR,QUEUE,ST)
	IF (ST.EQ.2) GOTO 20
	TAB(BUFNR)=TAB(BUFNR)+1
	GOTO 10
C
C CHECK THE INPUT BUFFER TO ENSURE THAT THE
C BUFFER DOES NOT ALREADY EXIST.
C
20	CONTINUE
	TIMES=TAB(BUF)
	IF (TIMES.NE.0)  THEN
	  TYPE *,'*** QUEUE CORRUPTION ***[',BUF,TIMES,']'
	ENDIF
	TAB(BUF)=0      !SO IT WILL NOT BE ADDED
C
C NOW ADD THE BUFFERS BACK ONTO THE QUEUE.
C
	DO 100, OFF=1,MAX
  	  IF (TAB(OFF).NE.0) THEN
	    IF (TAB(OFF).NE.1) THEN
	      TYPE*,'*** BUFFER CHECKED ***[',OFF,TAB(OFF),BUF,']'
	    ENDIF
	    CALL ABL(OFF,QUEUE,ST)
	    TAB(OFF)=0
	  ENDIF
100	CONTINUE
	RETURN
	END
