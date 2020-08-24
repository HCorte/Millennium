C
C SUBROUTINE PRINTSON
C $Log:   GXAFXT:[GOLS]PRINTSON.FOV  $
C  
C     Rev 1.1   19 May 1996 17:45:54   HXK
C  Wojtek's security stuff added
C  
C     Rev 1.0   21 Jan 1993 17:21:24   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_printson.for **
C
C PRINTSON.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C PRINTSON.FTN
C
C
C     CALLING SEQUENCE :
C                      INPUT: IND - Status index
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
	 SUBROUTINE PRINTSON(INDEX,SIND)
	 IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:RECUSE.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
C
	 INTEGER*4 COUNT
	 PARAMETER (COUNT=5)
	 INTEGER*4 REJTCNT
	 DATA REJTCNT/0/
	 INTEGER*4 SIND
	 INTEGER*4 INDEX
C
C Commented fof Finland
C
C open SIGNON.FIL ( indexed file with renewed status )
CWXW
C 3	 CONTINUE
C	 OPEN(11,FILE='SIGNON.FIL',IOSTAT=ST,ACCESS='APPEND',
C     *	   STATUS='UNKNOWN')
C	 IF(ST.NE.0) THEN
C	   TYPE *,'ERROR OPENING FILE    ST=',ST
C	   CALL GSTOP(GEXIT_SUCCESS)
C	 ENDIF
C
C
C Get terminal group number code
C
C	INQUIRE(UNIT=5,NAME=TNAM,IOSTAT=IOS)
C
C Call internal clock to get time in binary
C
C2000	CONTINUE
C	CALL ICLOCK(0,TTIM)
C	HOURS=TTIM(1)
C	MIN=TTIM(2)
C	SEC=TTIM(3)
C
C Call CDC to julian date routine
C
C	CALL XDAT(DATES)
C	MM=DATES(2)
C	DD=DATES(3)
C	YY=DATES(1)
C
C Check if five failures at every send error message to errlog
C every second failure after the first 5th will produce an error
C message
C
	REJTCNT=REJTCNT+1
	IF(SIND.EQ.1) THEN
	  IF(REJTCNT.GE.COUNT) THEN
CWXW	    MESS(2)=65
C	    CALL QUEMES(MESS)
	    CALL GSTOP(GEXIT_SUCCESS)
	  ENDIF
	ENDIF
C
C Reset rejtcnt if signon is good or change
C
	IF(SIND.EQ.2.OR.SIND.EQ.3) REJTCNT=0
C
C Write detail line of signon transaction
C
C***	WRITE(11,901,REC=LASTREC) STAT(SIND),HOURS,MIN,SEC,MM,DD,YY,
C	WRITE(11,901) STAT(SIND),HOURS,MIN,SEC,MM,DD,YY,
C     *	             FUNCNAME(INDEX),TNAM,USERID,
C     *	             USERNAM,DATEADD(1),DATEADD(2),
C     *	             DATEADD(3),DATECHA(1),DATECHA(2),DATECHA(3),
C     *	             USERADD,USERCHA
C	CLOSE(11)
C
	RETURN
C
 901	FORMAT(1X,A4,1X,I2,':',I2,':',I2,4X,I2,'/',I2,'/',I2,4X,A8,4X,
     *	       A8,1X,I4,4X,A20,7X,I2,'/',I2,'/',I2,7X,I2,'/',I2,'/',I2,
     *	       7X,I4,7X,I4)
	END
