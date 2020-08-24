C LOGSNP.FOR
C
C V03 15.FEB-11 RXK UNUSED THRULOG COMMENTED OUT
C V02 11-JAN-94 WS REMOTE LOGGING QUEUE ELEMENTS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-OCT-89 LOU R. INITIAL RELEASE FOR FINLAND
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
	SUBROUTINE LOGSNP
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LOGCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	!INCLUDE 'INCLIB:THRUCOM.DEF'
	
	EXTERNAL    QUECNT
	INTEGER*4   QUECNT

	INTEGER*4 BSTAT(9),TAPE(14),PFILE(5),BFILE(5)
	INTEGER*4 LIN, TNUM, IND, SER, BLOCK, I, BLANK, II
	CHARACTER*37 HEAD(9)
	DATA TAPE/'None','MAG1','MAG2','MAG3','MAG4',
     *	          'MAG5','MAG6','MAG7','MAG8','MAG9',
     *	          'MG10','MG11','MG12','MG13'/
	DATA HEAD/'# of buffers waiting for input.......',
     *	          '# of buffers with input in process...',
     *	          '# of buffers ready with I/O complete.',
     *	          '# of buffers ready for rewrite.......',
     *	          '# of buffers ready for output........',
     *	          '# of buffers with output in process..',
     *	          '# of buffers available for use.......',
     *	          '# of buffers waiting for tape........',
     *	          '# of buffers with tape I/O in process'/
	DATA BLANK/'    '/
C
C
C
	DO 10 I=1,9
	BSTAT(I)=0
10	CONTINUE
C
	BLOCK=HBLOCK
	SER=HSER
	DO 20 I=1,NUMLOG
	IND=LOGBUF(BSTATE,I)+1
	BSTAT(IND)=BSTAT(IND)+1
20	CONTINUE
C
C
	CALL FASTMOV(SFNAMES(1,PTMF),PFILE,5)
	CALL FASTMOV(SFNAMES(1,BTMF),BFILE,5)
	IF(P(DISKSW).EQ.0) THEN
	  CALL FASTSET(BLANK,BFILE,5)
	  BFILE(1)='None'
	ENDIF
	TNUM=P(TAPESW)+1
	WRITE(CLIN1,901)
	WRITE(CLIN4,904) PFILE
	WRITE(CLIN5,905) BFILE
	WRITE(CLIN6,906) (TAPE(TNUM))
	LIN=8
	DO 100 I=1,9
	WRITE(XNEW(  LIN),909) HEAD(I),BSTAT(I)
	LIN=LIN+1
100	CONTINUE
	LIN=LIN+1
	WRITE(XNEW(  LIN),915) BLOCK
	LIN=LIN+1
	WRITE(XNEW(  LIN),916) SER
	LIN=LIN+1
	!WRITE(XNEW(  LIN),917) (QUECNT(THRU_SENDQUE(1,I)),
     *	!		QUECNT(THRU_DELAYQUE(1,I)),I=1,2)
	!LIN=LIN+1
	!WRITE(XNEW(  LIN),918) (THRU_DELAY_CNT(I),I=1,2),
     *	!		(THRU_DELAY_FULL(II),II=1,2)
	RETURN
C
C
901	FORMAT('System logger snapshot ')
904	FORMAT('Primary transaction file ',5A4)
905	FORMAT('Backup  transaction file ',5A4)
906	FORMAT('Current tape logging     ',A4)
909	FORMAT(A37,I4)
915	FORMAT('Highest block # logged...',I8)
916	FORMAT('Highest serial # logged..',I8)
917	FORMAT('Remote log queues        ',2(I2,'/',I5,2X,'//'))
918	FORMAT('Remote tot delays        ',2I7,' blocks not sent ',2I7)
	END
