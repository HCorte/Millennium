C
C SUMMARY ADJUSTMENTS REPORT
C
C
C V01 13-NOV-97 UXN Initial release (produced from ADJRPT.FOR)
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE ADJRPTSUB
	IMPLICIT NONE
C
	INCLUDE '(LIB$ROUTINES)'
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:ASFREP.DEF'
C
	INTEGER*4 S, RNUM, PAGE, LINCNT
	INTEGER*4 COPY, ST, REPLU
	INTEGER*4 TOTADJ(2,3)              ! 1=CREDIT/2=DEBIT/3=NETAJUST
	INTEGER*4 NETADJ(2)                ! NET ADJUSTMENT(PAYMENT+CREDIT)
C
C ENTRY ADJRPT_BEGIN
C
	ENTRY ADJRPT_BEGIN()
C
	RNUM = 01
	TYPE *,IAM(),'**** ADJRPT Agent Adjustments Report ****'
	COPY=0
C
C CLEAR / SET VARIABLES
C
	LINCNT=70
	PAGE=0
C
C OPEN THE REPORT FILE
C
	ST = LIB$GET_LUN(REPLU)
	IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
	CALL ROPEN('ADJRPT.REP',REPLU,ST)
	IF(ST.NE.0) THEN
	   TYPE*,IAM(),'ADJRPT.REP Open error  st - ',ST
	   CALL USRCLOS1(REPLU)
	   CALL GSTOP(GEXIT_FATAL)
	ENDIF
	RETURN
C
C ENTRY ADJRPT_UPDATE
C
	ENTRY ADJRPT_UPDATE()
C
C PROCESS AGENT TOTALS
C
	   NETADJ(1)=0
	   NETADJ(2)=0
	   IF(ASFINV(ASFADJU,1).GT.0) THEN
	      CALL ADDI8I8(TOTADJ(1,1),ASFINV(ASFADJU,1),BETUNIT)
	   ELSE
	      CALL ADDI8I8(TOTADJ(1,2),ASFINV(ASFADJU,1),BETUNIT)
	   ENDIF
	   CALL ADDI8I8(NETADJ,ASFINV(ASFADJU,1),BETUNIT)
	   CALL ADDI8I8(NETADJ,ASFINV(ASFPADU,1),BETUNIT)
	   CALL ADDI8I8(TOTADJ(1,3),NETADJ,BETUNIT)
C
C ENCODE REPORT
C
	   IF(LINCNT.GT.LINSPP) THEN
	      CALL TITLE('AGENT ADJUSTMENT REPORT',
     *	                 '  ADJRPT',RNUM,REPLU,PAGE,DAYCDC)
	      WRITE(REPLU,9001)
	      LINCNT=7
	   ENDIF
	   IF(ASFINV(ASFADJU,1).GT.0) THEN
	      WRITE(REPLU,9002) (ASFBYT(S),S=SAGNO,EAGNO),
     *	                        (ASFBYT(S),S=SNAME,ENAME),
     *	                        CSMONYI8(ASFINV(ASFADJU,1),12,BETUNIT)
              LINCNT=LINCNT+1
	   ELSE IF (ASFINV(ASFADJU,1).LT.0) THEN
	      WRITE(REPLU,9003) (ASFBYT(S),S=SAGNO,EAGNO),
     *                          (ASFBYT(S),S=SNAME,ENAME),
     *                          CSMONYI8(ASFINV(ASFADJU,1),12,BETUNIT)
              LINCNT=LINCNT+1
	   ENDIF
	RETURN
C
C ENTRY ADJRPT_END	      
C
	ENTRY ADJRPT_END()
C
C REPORT ON FINAL TOTALS
C
	IF(LINCNT.GT.LINSPP) THEN
	   CALL TITLE('AGENT ADJUSTMENT REPORT',
     *	              '  ADJRPT',RNUM,REPLU,PAGE,DAYCDC)
	   WRITE(REPLU,9001)
	   LINCNT=7
	ENDIF
C
	WRITE(REPLU,9004) CSMONYI8(TOTADJ(1,1),12,BETUNIT),
     *	                  CSMONYI8(TOTADJ(1,2),12,BETUNIT)
C
	CALL USRCLOS1(REPLU)
	CALL SPOOL('ADJRPT.REP',COPY,ST)
C
	ST = LIB$FREE_LUN(REPLU)
	RETURN
C
C     ================= Format Statements ====================
C
9001	FORMAT(2X,'AGTNUM  ',12('-'),'AGENT NAME',13('-'),T54,
     *	      'CREDITS',15X,'DEBITS',12X,/,1X,131('='),/)
9002	FORMAT(2X,<LAGNO>A1,2X,<LNAME>A1,T50,A12)
9003	FORMAT(2X,<LAGNO>A1,2X,<LNAME>A1,T72,A12)
9004	FORMAT(/,2X,'TOTALS',T50,2(A12,10X))
C
	END
