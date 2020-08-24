C
C PROGRAM GENPASS
C $Log:   GXAFXT:[GOLS]GENPASS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:18:12   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   11 Nov 1993 20:44:08   HXK
C  UPDATE FOR FINLAND VAX CONVERSION.
C  
C     Rev 1.1   05 Nov 1993 15:58:50   HXN
C  INCLUDED AGTINF.DEF BEFORE RECAGT.DEF
C  
C     Rev 1.0   21 Jan 1993 16:23:52   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - genpass.for **
C
C GENPASS.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 13-DEC-89  LOU R.    INITIAL RELEASE FOR FINLAND.
C
C READ ASF AND PRODUCE REPORT WITH AGENT PASSNUMBERS.
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM GENPASS
	IMPLICIT NONE


	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'


	INTEGER*4 PASSNBR(8), Y, K, X, J, W, V, I, CERR, AGTNBR
	INTEGER*4 REC, ENDA, EXT, BEG, PAGE, LINCNT, ST
	CHARACTER CZERO/Z0/


C BEGIN CODE -----------------------------------------

	CALL COPYRITE

C CLEAR COUNTERS
C
	LINCNT=70
	PAGE=0
	CALL INPNUM('ENTER STARTING TERMINAL NUMBER ',BEG,
     *	     1,NUMAGT,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_SUCCESS)
	CALL INPNUM('ENTER ENDING TERMINAL NUMBER ',ENDA,
     *	     BEG,NUMAGT,EXT)
	IF(EXT.LT.0) CALL GSTOP(GEXIT_SUCCESS)


C OPEN THE AGENT SALES FILE
C -------------------------
	CALL OPENASF(ASF)
	CALL ROPEN('GENPASS.REP',7,ST)
	IF(ST.NE.0) THEN
	  TYPE*,' GENPASS.REP OPEN ERROR ',ST
	  CALL GSTOP(GEXIT_SUCCESS)
	ENDIF



	DO 1000 REC=BEG,ENDA
	   CALL FASTSET(0,PASSNBR,8)
	   CALL READASF(REC,ASFREC,ST)
	   CALL ASCBIN(ASFINF,SAGNO,LAGNO,AGTNBR,CERR)
	   IF(AGTNBR.EQ.0) GOTO 1000
	   CALL ASCBIN(ASFINF,SPAS1,LPAS1,PASSNBR(1),CERR)
	   CALL ASCBIN(ASFINF,SPAS2,LPAS2,PASSNBR(2),CERR)
	   CALL ASCBIN(ASFINF,SPAS3,LPAS3,PASSNBR(3),CERR)
	   CALL ASCBIN(ASFINF,SPAS4,LPAS4,PASSNBR(4),CERR)
	   CALL ASCBIN(ASFINF,SPAS5,LPAS5,PASSNBR(5),CERR)
	   CALL ASCBIN(ASFINF,SPAS6,LPAS6,PASSNBR(6),CERR)
	   CALL ASCBIN(ASFINF,SPAS7,LPAS7,PASSNBR(7),CERR)
	   CALL ASCBIN(ASFINF,SPAS8,LPAS8,PASSNBR(8),CERR)

	   DO I=1,ALENGTH         !ALENGTH=512
	      IF(ASFBYT(I).EQ.CZERO) ASFBYT(I)=' '  !fill 0 with blank char
	   END DO

C  	   GENERATE REPORT OF ALL PASSNUMBERS
C	   ----------------------------------
	   IF(LINCNT.GT.55) THEN
	     CALL TITLE('AGENT PASSNUMBER LISTING ',
     *		      ' GENPASS',1,7,PAGE,DAYCDC)
	     LINCNT=10
	     WRITE(7,10000)
	   ENDIF

	   WRITE(7,10001)(ASFBYT(V),V=SAGNO,EAGNO),
     *	              (ASFBYT(W),W=SNAME,ENAME),(PASSNBR(J),J=1,4),
     *	              (ASFBYT(X),X=SSTRT,ESTRT),(PASSNBR(K),K=5,8),
     *	              (ASFBYT(Y),Y=SCITY,ECITY)
	   LINCNT=LINCNT+4

1000	CONTINUE




	CALL GSTOP(GEXIT_SUCCESS)



10000	FORMAT(/,1X,'AGENT #',4X,'AGENT NAME',30X,'PASSNUMBERS ',/)
10001	FORMAT(1X,<LAGNO>(A1),3X,<LNAME>(A1),9X,4(2X,I4.4),/,
     *	       12X,<LSTRT>(A1),5X,4(2X,I4.4),/,
     *	       12X,<LCITY>(A1),/)



	END
