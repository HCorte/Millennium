C  GXSRC:GVTESTAB.FOR
C  
C  $Log:   GXAFIP:[GOLS]GVTESTAB.FOV  $
C  
C     Rev 1.0   26 Mar 1997 17:21:38   RXK
C  Initial revision.
C  
C V01 15-MAR-2011 GPW NUMAGT=12288
C  
C PROGRAM TO SCAN TMF AND ASF TO CREATE FILE GVTESTAB.FIL WHICH CONTAINS 
C ESTABLISHMENT MESSAGES OF THE DAY AND THE LIST OF GVT-AGENTS   
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM ALT_SIGNON
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
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
C
C
	INTEGER*4 ST,LOGREC(LMUREC),EOF,SER,K,I
	INTEGER*4 CNTGOOD, CNTBAD, TER, TABLE(NUMAGT)
	INTEGER*4 REPLUNG/7/,LINCNT, PAGE
	INTEGER*4 STN, SSTATE
	INTEGER*4 REPLUNB/8/,LINCNTB, PAGEB
	CHARACTER*14 REPGOD, REPBAD
	LOGICAL   ONLINE
C
	CALL COPYRITE
C
	TYPE*,IAM(),'*'
	TYPE*,IAM(),'* <<<<<< ALTURA sign-on/off report V01 >>>>>> '
	TYPE*,IAM(),'*'
C
	CNTGOOD = 0
	CNTBAD = 0
        CALL FASTSET(0,TABLE,NUMAGT)
C
C OPEN ASF FILE AND CREATE TABLE OF GVT' AGENTS
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
C CREATE OUTPUT RECORD
C
	IF(TRABUF(TTYP).EQ.TSPE .AND. TRABUF(TSFUN).EQ.TSON) THEN
           TER = TRABUF(TTER)
           IF(TABLE(TER).GT.0) GOTO 100
	   IF(.NOT.(TSBIT(AGTTAB(AGTTYP,TER),AGTTON))) GOTO 100
           IF(TRABUF(TSTAT).EQ.GOOD) THEN
	      CNTGOOD = CNTGOOD + 1
              TABLE(TER) = TABLE(TER) + 1 
	   ENDIF
	ENDIF
C
	GOTO 100

1000	CONTINUE
C
C WRITE END RECORD AND CLOSE FILES
C
C
C OPEN GVTESTAB REPORT FILE
C
	WRITE(REPGOD,800)
	CALL ROPEN(REPGOD,REPLUNG,ST)
	IF(ST.NE.0) THEN
	  TYPE*,'ERROR OPENING ',REPGOD,' > ',ST 
	ENDIF
C
	WRITE(REPBAD,801)
	CALL ROPEN(REPBAD,REPLUNB,ST)
	IF(ST.NE.0) THEN
	  TYPE*,'ERROR OPENING ',REPBAD,' > ',ST 
	ENDIF
C
	LINCNT = 999
	PAGE = 0
	LINCNTB = 999
	PAGEB = 0
C
	DO TER=1, NUMAGT
      	   CALL READASF(TER,ASFREC,ST)
      	   IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),2,ST,TER)
C
             IF(TABLE(TER).GT.0) THEN
	       IF(LINCNT.GT.LINSPP) THEN
	          CALL TITLE('SIGNON LISTING',REPGOD(1:8),1,7,PAGE,DAYCDC)
		  WRITE(REPLUNG,802)
		  LINCNT=7
	       ENDIF

      	       WRITE(REPLUNG,901) (ASFBYT(K),K=SAGNO,EAGNO), 
     *            (ASFBYT(K),K=SXADR,EXADR), 
     *            TER,
     *            (ASFBYT(K),K=SXSTN,EXSTN),
     *            (ASFBYT(K),K=SNAME,ENAME),
     *            (ASFBYT(K),K=STELE,ETELE)

	       LINCNT=LINCNT+1

             ELSEIF(TABLE(TER).EQ.0) THEN

	       STN = X2XT_STATION_NO(TER)
	       CALL ILBYTE(SSTATE,IX2XS_STATE,STN-1)

	       ONLINE = .FALSE.

	       DO I=SXADR,EXADR
	         IF(ASFBYT(I).GT.'0') ONLINE = .TRUE.			       
	       ENDDO

	       IF(ONLINE.AND.(SSTATE.EQ.2)) THEN 

	         CNTBAD = CNTBAD + 1

	         IF(LINCNTB.GT.LINSPP) THEN
	           CALL TITLE(' NOT SIGN LISTING',REPBAD(1:8),1,8,PAGEB,DAYCDC)
		   WRITE(REPLUNB,802)
	  	   LINCNTB=7
	         ENDIF

      	         WRITE(REPLUNB,901) (ASFBYT(K),K=SAGNO,EAGNO), 
     *             (ASFBYT(K),K=SXADR,EXADR), 
     *             TER,
     *             (ASFBYT(K),K=SXSTN,EXSTN),
     *             (ASFBYT(K),K=SNAME,ENAME),
     *             (ASFBYT(K),K=STELE,ETELE)

	         LINCNTB=LINCNTB+1
	       ENDIF
	     ENDIF

	ENDDO

        WRITE(REPLUNG,902) CNTGOOD
        WRITE(REPLUNB,903) CNTBAD

	CALL USRCLOS1(PTMF)
        CALL CLOSASF
	CALL USRCLOS1(REPLUNG)
	CALL USRCLOS1(REPLUNB)

	CALL GSTOP(GEXIT_SUCCESS)
C
C
800	FORMAT('ALT_SIGNON.REP')
801	FORMAT('ALT_NOTSIG.REP')
802	FORMAT(/,1X,' AGENT',
     *           3X,'X.25 ADDRESS',
     *           3X,' TERM',
     *           2X,'STN',
     *           3X,'NAME'
     *          52X,'PHONE')
900     FORMAT(1X,I1,1X,I2.2'.',I2.2,'.',I4.4,1X,A8)
901     FORMAT (1X,<LAGNO>A,
     *          1X,<LXADR>A,
     *          1X,I5,
     *          1X,<LXSTN>A1,
     *          1X,<LNAME>A1,
     *          1X,<LTELE>A1)
902     FORMAT (/1X,'TOTAL NUMBER OF TERMINALS SIGNED-ON',1X,I5)
903     FORMAT (/1X,'TOTAL NUMBER OF TERMINALS NOT SIGNED-ON',1X,I5)
C
	END
