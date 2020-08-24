C  GXSRC:VIS_TCPSNP.FOR
C  
C V07 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V06 27-DEC-1994 DJO Updates as a result of shakedown testing for
C                     install system.
C V05 21-JUN-1994 MCM CHANGED STRATUS TO INSTANT SYSTEM
C V04 03-JAN-1994 SYSTEM Applying PVCS header for automatic revision history
C V03 21-DEC-1993 SYSTEM Initial revision.
C V02 02-MAR-1992 NJA CORRECTED SPELLING OF DISCONNECTED
C V01 21-NOV-1991 JPJ RELEASED FOR VAX (INSTANTS)
C
C VIS_TCPSNP.FOR
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
	SUBROUTINE TCPSNP(CLINE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:CRSCOM.DEF'
C
	INTEGER*4 ST, K, LIN, OFFSET
	INTEGER*4 BUF(CDLEN), VALUE, POS, KEYNUM, CLINE(20)
	INTEGER*2 D(LDATE_LEN)
	REAL*8    OPT(1)
	DATA OPT/'PRMSTR  '/
C
	CHARACTER*25 CONSTS(0:4),CONOVR(0:4),CONTYP(0:4)
        CHARACTER*30 CONPRM(0:2)
	DATA CONSTS/'CONNECTED                ',
     *              'CONNECTION IN PROGRESS   ',
     *              'DISCONNECTION IN PROGRESS',
     *              'DISCONNECTED             ',
     *              'UNKNOWN                  '/
C
	DATA CONTYP/'ACTIVE CONNECTION        ',
     *              'PASSIVE CONNECTION       ',
     *              'UNKNOWN                  ',
     *              'UNKNOWN                  ',
     *              'UNKNOWN                  '/
C
	DATA CONOVR/'ALLOW CONNECTIONS        ',
     *              'DISALLOW CONNECTIONS     ',
     *              'UNKNOWN                  ',
     *              'UNKNOWN                  ',
     *              'UNKNOWN                  '/
C
	DATA CONPRM/'UNKNOWN INSTANT SYSTEM        ',
     *              'PRIMARY SITE INSTANT SYSTEM   ',
     *              'BACKUP SITE INSTANT SYSTEM    '/
C
	D(VCDC)=DAYCDC
	CALL LCDATE(D)
C
C INSSNP INPUT
C
	VALUE=0
	POS=1
	CALL KEY(CLINE,OPT,1,POS,KEYNUM)
	IF(POS.GT.40) GOTO 300                      !NO INPUT
	IF(KEYNUM.EQ.0)GOTO 200                     !INPUT ERROR
	CALL NUMB(CLINE,POS,VALUE)                  !GET VALUE
	IF(VALUE.LT.0)  GOTO 205
C
C CLEAR COMMAND MESSAGE BUFFER
C
2	CONTINUE
	CALL FASTSET(0,BUF,CDLEN)
	GOTO(10) KEYNUM
C
C PRMSTR CHANGE
C
10	CONTINUE
	IF(VALUE.LT.0.OR.VALUE.GT.2) GOTO 205	!DJO - ALLOW NO CONNECTION
	BUF(1)=PRMSTR
	BUF(2)=VALUE
	BUF(3)=TCPAR
	GOTO 250
C
C INPUT ERROR
C
200	CONTINUE
	WRITE(CLIN23,800)
800	FORMAT('Input error')
	RETURN
C
C VALUE ERROR
C
205	CONTINUE
	WRITE(CLIN23,801)
801	FORMAT('Value error')
	RETURN
C
C QUEUE COMMAND BUFFER TO SYSTEM INPUT INPUT QUEUE
C
250	CONTINUE
	BUF(6)=IDNUM
	CALL VISCMD(BUF,ST)
	CALL XWAIT(2,1,ST)
C
C FORMAT TCP SNAPSHOT
C
300	CONTINUE
	WRITE(CLIN1,9001) (D(K),K=7,13)
	LIN=3
C
C STATUS PART OF SNAPSHOT
C
        WRITE(XNEW(  LIN),9002)
	LIN=LIN+1	
        WRITE(XNEW(  LIN),9003)
	LIN=LIN+1	
	LIN=LIN+1	
C
        WRITE(XNEW(  LIN),9004) 'READS       ',TCP_READS,
     *                          TCP_READERRS,0,TCP_READLERR
	LIN=LIN+1	
C
        WRITE(XNEW(  LIN),9004) 'WRITES      ',TCP_WRITES,
     *                          TCP_WRITEERRS,TCP_WRITENOCS,
     *                          TCP_WRITELERR
	LIN=LIN+1	
C
        WRITE(XNEW(  LIN),9004) 'CONNECTS    ',TCP_CONNECTS,
     *                          TCP_CONNERRS,0,TCP_CONNLERR
	LIN=LIN+1	
C
        WRITE(XNEW(  LIN),9004) 'DISCONNECTS ',TCP_DISCONNS,
     *                          TCP_DISCERRS,0,TCP_DISCLERR
	LIN=LIN+1	
C
C BUFFER PART OF SNAPSHOT
C
	LIN=LIN+1
        WRITE(XNEW(  LIN),9005)
	LIN=LIN+1	
	LIN=LIN+1	
C
        WRITE(XNEW(  LIN),9006) 'WRITE STATUS',
     *	                  (TCP_WRITESOUT(K),K=1,TCMAXWRITES)
	LIN=LIN+1	
C
        WRITE(XNEW(  LIN),9006) 'WRITE BUFFER',
     *		          (TCP_WRITETBUF(K),K=1,TCMAXWRITES)
	LIN=LIN+1	
C
        WRITE(XNEW(  LIN),9006) 'READ STATUS ',
     *			  (TCP_READSOUT(K),K=1,TCMAXREADS)
	LIN=LIN+1	
C
        WRITE(XNEW(  LIN),9006) 'READ BUFFER ',
     *	                  (TCP_READTBUF(K),K=1,TCMAXREADS)
	LIN=LIN+1	
	LIN=LIN+1	
C
C SHOW STATUS
C
	OFFSET=P(PRMSTR)
	IF(OFFSET.LT.0.OR.OFFSET.GT.2) OFFSET=0
        WRITE(XNEW(  LIN),9008) '*PRMSTR         ',
     *                          CONPRM(OFFSET),P(PRMSTR)
	LIN=LIN+1	
C
	OFFSET=TCP_CONNSTS
	IF(TCP_CONNSTS.LT.0.OR.TCP_CONNSTS.GT.4) OFFSET=4
        WRITE(XNEW(  LIN),9007) 'CONNECT STATUS  ',
     *                    CONSTS(OFFSET)
	LIN=LIN+1	
C
	OFFSET=TCP_CONNOVR
	IF(TCP_CONNOVR.LT.0.OR.TCP_CONNOVR.GT.4) OFFSET=4
        WRITE(XNEW(  LIN),9007) 'CONNECT OVERRIDE',
     *                    CONOVR(OFFSET)
	LIN=LIN+1	
C
	OFFSET=TCP_CONTYPE
	IF(TCP_CONTYPE.LT.0.OR.TCP_CONTYPE.GT.4) OFFSET=4
        WRITE(XNEW(  LIN),9007) 'CONNECT TYPE    ',
     *                    CONTYP(OFFSET)
	LIN=LIN+1	
C
        WRITE(XNEW(  LIN),9009) 'CONNECT IP      ',
     *			        ZEXT(TCP_B_REMADR(1)),
     *			        ZEXT(TCP_B_REMADR(2)),
     *			        ZEXT(TCP_B_REMADR(3)),
     *			        ZEXT(TCP_B_REMADR(4)),
     *                          TCP_REMPRT
	LIN=LIN+1	
C
C     ================== FORMAT STATEMENTS ================
C
9001	FORMAT('TCP Activity for ',7A2)
9002    FORMAT(16X,'GOOD',7X,'BAD',4X,'NO CON',6X,'LAST')
9003    FORMAT(17X,'CNT',7X,'CNT',7X,'CNT',5X,'ERROR')
9004    FORMAT(A12,2X,4(I6,4X))
9005    FORMAT(16X,'BUF1',6X,'BUF2',6X,'BUF3',
     *          6X,'BUF4',6X,'BUF5')
9006    FORMAT(A12,2X,5(I6,4X))
9007    FORMAT(A16,3X,A25)
9008    FORMAT(A16,3X,A30,I1.1)
9009    FORMAT(A16,3X,3(I3.3,'.'),I3.3,3X,'Port ', I4)
9010	FORMAT(80(' '))
C
	RETURN
	END
