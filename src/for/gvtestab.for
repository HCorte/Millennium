C  GXSRC:GVTESTAB.FOR
C  
C  $Log:   GXAFIP:[GOLS]GVTESTAB.FOV  $
C  
C     Rev 1.0   26 Mar 1997 17:21:38   RXK
C  Initial revision.
C  
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
	PROGRAM GVTESTAB 
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
C
C
	INTEGER*4 ST,LOGREC(LMUREC),EOF,SER,K
	INTEGER*4 CNTGOOD, CNTBAD, TER, ESTAB, TABLE(NUMAGT)
	INTEGER*4 REPLUN/7/,REPLEN/64/
        INTEGER*4 FILENAME(5) /'GVTE','STAB','.FIL','    ','    '/
        INTEGER*4 DD,MM,YYYY
        INTEGER*2 DATBUF(LDATE_LEN)
        CHARACTER*12 GVTID
        CHARACTER*23 DATE_TIME
        INTEGER*4    LIB$DATE_TIME
C
	CALL COPYRITE
C
	TYPE*,IAM(),'*'
	TYPE*,IAM(),'* <<<<<< GVT establishment report file  V01 >>>>>> '
	TYPE*,IAM(),'*'
C
	CNTGOOD = 0
	CNTBAD = 0
        CALL FASTSET(0,TABLE,NUMAGT)
C
C OPEN GVTESTAB REPORT FILE
C
        CALL FTP_OPEN(FILENAME,REPLUN,REPLEN,ST)
        DATBUF(VCDC)=DAYCDC     !     write  header record
        CALL LCDATE(DATBUF)
        YYYY=DATBUF(VYEAR2)
        MM=DATBUF(VMON)
        DD=DATBUF(VDAY)
        CALL LIB$DATE_TIME(DATE_TIME)
        WRITE(REPLUN,900)  0,DD,MM,YYYY, DATE_TIME(13:20)
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
	IF(TRABUF(TTYP).EQ.TCRS .AND. TRABUF(TITYP).EQ.IEST) THEN

           TER = TRABUF(TTER)
           IF(TABLE(TER).GT.0) GOTO 100
           IF(BTEST(AGTTAB(AGTTYP,TER),AGTTOI)) GOTO 100
           IF(TRABUF(TSTAT).EQ.GOOD) THEN
	      CNTGOOD = CNTGOOD + 1
              TABLE(TER) = TABLE(TER) + 1 
	   ENDIF

           CALL READASF(TER,ASFREC,ST)
           IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),2,ST,TER)

           CALL HTOA(GVTID,1,12,AGTTAB(AGTGVT1,TER),ST)

           ESTAB = 1
           WRITE(REPLUN,901) 1, (ASFBYT(K),K=SAGNO,EAGNO), 
     *           ESTAB, GVTID, TER,
     *           (ASFBYT(K),K=SXSTN,EXSTN),
     *           (ASFBYT(K),K=SNAME,ENAME)

	ENDIF
	GOTO 100

1000	CONTINUE

        DO TER=1,NUMAGT
           IF(TABLE(TER).GT.0) GOTO 1100
           CALL READASF(TER,ASFREC,ST)
           IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),2,ST,TER)

           IF(ASFBYT(ESCLS).EQ.'9') THEN 
              ESTAB = 0
              CNTBAD = CNTBAD +1
              WRITE(REPLUN,903) 1, (ASFBYT(K),K=SAGNO,EAGNO), 
     *              ESTAB,
     *              (ASFBYT(K),K=SGSER,EGSER),
     *              TER,
     *              (ASFBYT(K),K=SXSTN,EXSTN),
     *              (ASFBYT(K),K=SNAME,ENAME)
           ENDIF
1100       CONTINUE
        ENDDO
C
C WRITE END RECORD AND CLOSE FILES
C
        WRITE(REPLUN,902) 9,CNTGOOD,CNTBAD

	CALL USRCLOS1(PTMF)
        CALL CLOSASF
	CALL USRCLOS1(REPLUN)

	CALL GSTOP(GEXIT_SUCCESS)
C
C
900     FORMAT(1X,I1,1X,I2.2'.',I2.2,'.',I4.4,1X,A8)
901     FORMAT (1X,I1,1X,<LAGNO>A1,1X,I1.1,1X,A12,1X,I4.4,1X,
     *         <LXSTN>A1,1X,<LNAME>A1)
903     FORMAT (1X,I1,1X,<LAGNO>A1,1X,I1.1,1X,<LGSER>A1,1X,I4.4,1X,
     *         <LXSTN>A1,1X,<LNAME>A1)
902     FORMAT (1X,I1,1X,I8.8,1X,I8.8)
C
	END
