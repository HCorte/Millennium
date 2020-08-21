C GVTALL.FOR
C
C V03 08-JUN-2000 UXN ITNAMES.DEF added.
C V02 03-FEB-1997 WXW INITIAL RELEASE FOR FINLAND
C V01 01-AUG-1993 PXN INITIAL RELEASE FOR IRELAND
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM GVTALL 
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:ITNAMES.DEF'
C
C
	INTEGER*4 GVTTER(IVAL:IGTB,NUMAGT)
C
	INTEGER*4 ST,LOGREC(LMUREC),EOF,SER,I,J,K,TYPE
	INTEGER*4 CNT,TCNT(IVAL:IGTB), LINCNT/70/, PAGE
C
	CALL COPYRITE
C
	TYPE*,IAM(),'*'
	TYPE*,IAM(),'* GVTALL  V02  03-FEB-97 '
	TYPE*,IAM(),'*'
C
	CALL FASTSET(0,TCNT,16)
	CALL FASTSET(0,GVTTER,NUMAGT*16)
C
C OPEN GVTALL REPORT FILE
C
        CALL ROPEN('GVTALL.REP',7,ST)
        IF(ST.NE.0)THEN
           TYPE *,'Error opening GVTALL.REP >',ST
	   CALL USRCLOS1(7)
	   CALL GSTOP(GEXIT_SUCCESS)
       ENDIF
C
C OPEN ASF FILE
C
        CALL OPENASF(ASF)
C
C OPEN TM 
C
	CALL OPENW(PTMF,SFNAMES(1,PTMF),4,0,0,ST)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,PTMF),1,ST,0)
	CALL TOPEN(PTMF)
C
C LOOP AND READ THE TM FILE
C
100	CONTINUE
	CALL READTMF(LOGREC,SER,EOF)
	IF(EOF) GOTO 1000
C
	CALL LOGTRA(TRABUF,LOGREC)
C
C UPDATE GVT TABLE
C
	IF(TRABUF(TSTAT).EQ.GOOD) THEN
	  TYPE=TRABUF(TITYP)
   	  IF(TRABUF(TTYP).EQ.TCRS.AND.(TYPE.GE.IVAL.AND.TYPE.LE.IGTB)) THEN
            IF(BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTTOI)) GOTO 100
	    GVTTER(TYPE,TRABUF(TTER)) = GVTTER(TYPE,TRABUF(TTER)) + 1
	    TCNT(TYPE) = TCNT(TYPE) + 1
	  ENDIF
	ENDIF
	GOTO 100
C
C CLOSE TM FILE
C
1000	CONTINUE
	CALL USRCLOS1(PTMF)
C
C LIST TERMINALS THAT SIGNED ON
C
	CNT=0
	DO 1001 I=1,NUMAGT
	   IF(BTEST(AGTTAB(AGTTYP,I),AGTTOI)) GOTO 1001
           CALL READASF(I,ASFREC,ST)
           IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),1,ST,0)
C
	   DO 1100 J=IVAL,IGTB
	     IF(GVTTER(J,I).NE.0) GOTO 1110
1100	   CONTINUE
	   GOTO 1001
C
1110	   CONTINUE
           IF(LINCNT.GT.9) THEN
             CALL TITLE('GVT ALL INSTANT TRANSACTIONS REPORT','GVTALL',
     *                    1,7,PAGE,DAYCDC)
             WRITE(7,902)
             LINCNT=1
           ENDIF
	   WRITE(7,900) (ASFBYT(K),K=SAGNO,EAGNO),I,
     *                  (ASFBYT(K),K=SNAME,ENAME),
     *                  (ASFBYT(K),K=SSTRT,ESTRT),
     *                  (ASFBYT(K),K=SCITY,ECITY),
     *                  (ASFBYT(K),K=STELE,ETELE),
     *                  (ITNAMES(K),GVTTER(K,I),K=IVAL,IGTB)
           LINCNT=LINCNT+1
	   CNT=CNT+1
1001	CONTINUE
C
	WRITE(7,901)  CNT,(ITNAMES(K),TCNT(K),K=IVAL,IGTB)
C
        CALL CLOSASF
	CALL USRCLOS1(7)
	CALL GSTOP(GEXIT_SUCCESS)
C
C
C
900	FORMAT(2X,<LAGNO>(A1),5X,I5,5X,<LNAME>(A1),1X,<LSTRT>(A1),
     *         1X,<LCITY>(A1),1X,<LTELE>(A1),/,
     *       4(1X,A8,1X,I6),/,
     *       4(1X,A8,1X,I6),/,
     *       4(1X,A8,1X,I6),/,
     *       4(1X,A8,1X,I6),/,
     *       4(1X,A8,1X,I6),/)
C
901	FORMAT(1X,'TOTAL NUMBER OF AGENTS ',1X,I8,/,
     *       4(1X,A8,1X,I6),/,
     *       4(1X,A8,1X,I6),/,
     *       4(1X,A8,1X,I6),/,
     *       4(1X,A8,1X,I6),/)
C
902     FORMAT(1X,'  AGENT ',4X,'TERMINAL',3X,'NAME',24X,
     *         'ADDRESS',43X,'TELEPHONE',/,
     *         1X,' NUMBER ',4X,' NUMBER ',4X,'    ',17X,
     *         '       ',24X,'         ',/)
C
	END
