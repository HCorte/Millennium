C
C PROGRAM XAGTLST
C $Log:   GXAFXT:[GOLS]XAGTLST.FOV  $
C  
C     Rev 1.0   17 Apr 1996 16:46:24   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   01 Mar 1993 12:21:26   EBD
C  Changed program name to agree with file name (xagtlst)
C  
C     Rev 1.0   01 Mar 1993 11:54:16   EBD
C  Initial revision.
C
C ** Source - xagtlst.for **
C
C XAGTLST.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C V02 12-NOV-91 MTK INITIAL RELEASE FOR NETHERLANDS
C
C AGENT DETAIL LIST REPORT
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
	PROGRAM XAGTLST
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
C
	INTEGER*4    SORT(NUMAGT)
	INTEGER*4    OPTION, SRTDTA(8,NUMAGT)
	CHARACTER    CHRDTA(32,NUMAGT)*1
	CHARACTER    CZERO*1
	CHARACTER    REPNAM*12
	INTEGER*4    J, K, I, REC, FLDLEN, FLDBEG, PAGE
	INTEGER*4    ACTTER, ACTAGT, LINCNT, COPY, EXT, ST
	INTEGER*4    REPLU /7/
	INTEGER*4    IDBEG(6), IDLEN(6)
	EQUIVALENCE (SRTDTA,CHRDTA)
C
	DATA IDBEG  /SAGNO,SNAME,SXSTN,SXPRT,SXSTN,SXADR/
C
	DATA IDLEN  /LAGNO,LNAME,LXSTN,LXPRT,LXSTN,LXADR/
C
	DATA CZERO/Z0/
C
	CALL COPYRITE
C
C
C OPEN CONSOLE, AGENT SALES FILE AND REPORT FILE
C
C
C OPEN THE AGENT SALES FILE
C
	CALL OPENASF(1)
C
500	CONTINUE
	TYPE *
	TYPE *
	TYPE *,'Sort Options: '
	TYPE *
	TYPE *,'      0 - Terminal number      1 - Agent number'
	TYPE *,'      2 - Station  number  '
	TYPE *,'                   E - Exit                    '
	TYPE *
	CALL INPNUM('Enter desired sort option     ',OPTION,0,2,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
	CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C INITIALIZE FOR EACH REPORT
C
	LINCNT = 999
	ACTAGT = 0
	ACTTER = 0
	PAGE = 0
	CALL FASTSET(0,SORT,NUMAGT)
	CALL FASTSET(0,SRTDTA,8*NUMAGT)
C
C OPEN CORRESPONDING REPORT OPTION FILE
C
	WRITE (REPNAM,8000) OPTION
	CALL ROPEN(REPNAM,7,ST)
	IF(ST.NE.0)THEN
	   TYPE *,' Error openning ',REPNAM,' > ',ST
	   CALL GPAUSE
	ENDIF
C
C SET UP SORT BASED ON OPTION
C
	IF(OPTION.EQ.0)THEN
	   ACTAGT = NUMAGT
	   GOTO 200
	ENDIF
        OPTION=OPTION+1
	TYPE*,'Sorting ...'
	FLDBEG = IDBEG(OPTION)
	FLDLEN = IDLEN(OPTION)
	IF(OPTION.EQ.3) THEN
	FLDLEN=LXSTN+LXPRT
	ENDIF
C
	DO 100 REC=1,NUMAGT
	   CALL READASF(REC,ASFREC,ST)
	   IF(ST.NE.0) CALL FILERR(GFNAMES(1,ASF),2,ST,REC)
C
C CHECK FOR VALID AGENT NUMBER
C
	   DO 110 I=SAGNO,EAGNO
	      IF(ASFBYT(I).NE.' '.AND.ASFBYT(I).NE.CZERO) GOTO 120
110	   CONTINUE
	   GOTO 100
C
C EXTRACT SORT DATA
C
120	   CONTINUE
	   ACTAGT=ACTAGT+1
	   SORT(ACTAGT) = REC
	   IF(FLDLEN.GT.32) FLDLEN=32
	   DO 600 I=1,FLDLEN
	      CHRDTA(I,ACTAGT)=ASFBYT(FLDBEG+I-1)
600	   CONTINUE
100	CONTINUE
C
C SORT DATA
C
	CALL I1SHELL(SORT,ACTAGT,SRTDTA,8)
	TYPE*,'Sorting completed ...'
C
C PRINT REPORT
C
200	CONTINUE
	DO 300 I=1,ACTAGT
	   REC=SORT(I)
	   IF(OPTION.EQ.0) REC=I
C
	   CALL READASF(REC,ASFREC,ST)
C
C CHECK FOR VALID AGENT NUMBER IF SORT BY TERMINAL OPTION
C
	   IF(OPTION.EQ.0)THEN
	      DO 310 K=SAGNO,EAGNO
	         IF(ASFBYT(K).NE.' '.AND.ASFBYT(K).NE.CZERO)THEN
	            ACTTER = ACTTER + 1
	            GOTO 320
	         ENDIF
310	      CONTINUE
	      GOTO 300
	   ENDIF
C
C SUBSTITUTE NULLS WITH BLANKS
C
320	   CONTINUE
	   DO 150 J=1,FLDLEN
	      IF(ASFBYT(J).EQ.CZERO) ASFBYT(J)=' '
150	   CONTINUE
C
C CHECK IF PAGE IS FULL IF SO GOTO NEW PAGE AND PRINT HEADER
C
	   IF(LINCNT.GT.LINSPP) THEN
	      CALL TITLE('AGENT DETAIL LISTING',REPNAM(1:8),1,7,PAGE,DAYCDC)
	      WRITE(REPLU,9000)
	      LINCNT=7
	   ENDIF
C
	   WRITE(REPLU,9001) (ASFBYT(SAGNO+K-1),K=1,LAGNO),
     *	                 REC,
     *                  (ASFBYT(SXSTN+K-1),K=1,LXSTN),
     *                  (ASFBYT(SXPRT+K-1),K=1,LXPRT),
     *                  (ASFBYT(SDROP+K-1),K=1,LDROP),
     *                  (ASFBYT(SXADR+K-1),K=1,LXADR)
C
	   LINCNT=LINCNT+1
300	CONTINUE
C
	IF(OPTION.EQ.0) ACTAGT=ACTTER
	WRITE(REPLU,9002) ACTAGT
	CALL USRCLOS1(     REPLU)
	CALL SPOOL(REPNAM,COPY,ST)
	GOTO 500
C
C     ==================== FORMAT STATEMENTS =================
C
8000	FORMAT('AGTLST',I2.2,'.REP')
9000	FORMAT(/,2X,'AGENT ', X,' TERM', 2X,' STN ',4X,'PORT',4X,
     *	       'DROP',2X,'X25 ADDRESS')
9001	FORMAT(X,<LAGNO>A,
     *         X,I5,
     *	      2X,<LXSTN>A,
     *        4X,<LXPRT>A,
     *        4X,<LDROP>A,
     *        3X,<LXADR>A)

C9001	FORMAT(X,6A,X,I5,X,5A1,4X,5A1,2X,2A1,' INS [   ]  PWR [   ]',
C     *  '  DCD[   ] RXD[   ] RTS[   ] TXD[   ] TST[   ]   REMARK')
9002	FORMAT(/,1X,'TOTAL NUMBER OF AGENTS: ',I6)
C
	END
