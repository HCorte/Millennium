C
C SUBROUTINE HLPSNP
C $Log:   GXAFXT:[GOLS]HLPSNP.FOV  $
C  
C V09 19-APR-1996 HXK Merge of updates from Finland (Rita, Wojtek, Siew Mun) 
C V08 18-APR-1996 HXK Merge of LIVE code from Finland (RXK,WXW)
C V07 16-APR-1996 RXK Different changes for HELP
C V06 11-JUN-1993 HXK ADDED AGTINF.FCC, PRMAGT.FCC
C V05 21-JAN-1993 DAB Initial Release Based on Netherlands Bible, 12/92,
C                     and Comm 1/93 update DEC Baseline
C V04 17-FEB-1992 DAS ADDED X2ERRCODE 
C V03 01-AUG-1990 XXX RELEASED FOR VAX
C V02 27-FEB-1990 MRM ADD X2X HELP SNAPSHOTS.
C V01 01-FEB-1989 XXX INITIAL RELEASE FOR SWEDEN
C
C VIS_HLPSNP.FOR
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
	SUBROUTINE HLPSNP(SLINE,PAGE_NUM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
C
	INTEGER*4 SLINE(20)
	INTEGER*4 PAGE_NUM
	INTEGER*4 BUF(2)
	INTEGER*4 J
	INTEGER*4 I
	INTEGER*4 KEYNUM
	INTEGER*4 POS
	CHARACTER CNAME(10)
	CHARACTER CBUF(8)
	CHARACTER*8  CBUF8
	CHARACTER*10 FNAME
	EQUIVALENCE(BUF,CBUF)
	EQUIVALENCE(CBUF,CBUF8)
	EQUIVALENCE(FNAME,CNAME)
	REAL*8 H(112)
	REAL*8 SHORT(112)
C
        LOGICAL FOUND
C
	DATA H  /'AGTSnp  ','DATes   ','TOTsnp  ','LINsnp  ',
     *	         'NBRsnp  ','SALes   ','QUEsnp  ','SYStem  ',
     *	         'TERsnp  ','TRNsnp  ','X2STNPrt','X2Gblsnp',
     *	         'X2STNsnp','X2ALLSAp','X2SAPsnp','X2NETsnp',
     *	         'SYNtax  ','X2ALLSTn','CONsnp  ','LANID   ',
     *	         'LANPRO  ','HOTsnp  ','X2ALLRel','X2Relsnp',
     *	         'X2ERcod ','ACTagt  ','INVsnp  ','LOTto   ',
     *	         'TSPsnp  ','STGstat ','TATSnp  ','ATTsnp  ',
     *	         'JOKer   ','SPTsnp  ','GAMsnp  ','CLOse   ',
     *	         'SLIsnp  ','SCLosnp ','RWTsnp  ','TWDsnp  ',
     *	         'PERform ','TYPsnp  ','xxxxxxxx','VALid   ',
     *	         'MESsnp  ','MISsnp  ','TCFsnp  ','TRAns   ',
     *	         'LPLsnp  ','WRFsnp  ','LOGsnp  ','COMsnp  ',
     *           'NETsnp  ','PTLsnp  ','HRSsnp  ','BUFsnp  ',
     *           'FLWsnp  ','GTNsnp  ','TSALes  ','REQSNP  ',
     *           'UNUSED  ','UNUSED  ','UNUSED  ','UNUSED  ',
     *           'OVRsnp  ','UNDsnp  ','BL1snp  ','BL2snp  ',
     *		 'TWITsnp ','TSCRsnp ','UNUSED  ','SKLsnp  ',
     *           'PPLsnp  ','TSKsnp  ','TPRsnp  ','SKOsnp  ',
     *		 'PPOsnp  ','RSCsnp  ','SELect  ','SPNsnp  ',
     *           'TSOdsnp ','SCRsnp  ','UNUSED  ','SPSTsnp ',
     *           'WITsnp  ','MENU    ','X2Delay ','SAPsnp  ',
     *           'X2AGTsnp','X2Comsnp','SPEsnp  ','STLstat ',
     *            20*'        '/
C
      DATA SHORT/'AGT     ','DAT     ','TOT     ','LIN     ',
     *	         'NBR     ','SAL     ','QUE     ','SYS     ',
     *	         'TER     ','TRN     ','X2STNP  ','X2G     ',
     *	         'X2STN   ','X2ALLSA ','X2SAP   ','X2NET   ',
     *	         'SYN     ','X2ALLST ','CON     ','LANID   ',
     *	         'LANPRO  ','HOT     ','X2ALLR  ','X2R     ',
     *	         'X2ER    ','ACT     ','INV     ','LOT     ',
     *	         'TSP     ','STG     ','TAT     ','ATT     ',
     *	         'JOK     ','SPT     ','GAM     ','CLO     ',
     *	         'SLI     ','SCL     ','RWT     ','TWD     ',
     *	         'PER     ','TYP     ','xxxxxxxx','VAL     ',
     *	         'MES     ','MIS     ','TCF     ','TRA     ',
     *	         'LPL     ','WRF     ','LOG     ','COM     ',
     *           'NET     ','PTL     ','HRS     ','BUF     ',
     *           'FLW     ','GTN     ','TSAL    ','REQ     ',
     *           '        ','        ','        ','        ',
     *           'OVR     ','UND     ','BL1     ','BL2     ',
     *		 'TWIT    ','TSCR    ','        ','SKL     ',
     *           'PPL     ','TSK     ','TPR     ','SKO     ',
     *		 'PPO     ','RSC     ','SEL     ','SPN     ',
     *           'TSO     ','SCR     ','        ','SPST    ',
     *           'WIT     ','MENU    ','X2D     ','SAP     ',
     *           'X2AGT   ','X2C     ','SPE     ','STL     ',
     *            20*'        '/
C
	SMODE=.TRUE.
C
C HLPSNP INPUT
C
	POS=1
	CALL SPACES
	CLR=1

	CALL KEY(SLINE,SHORT,86,POS,KEYNUM)
	IF(KEYNUM.NE.0) GOTO 5
C
	CALL KEY(SLINE,H,86,POS,KEYNUM)
C
CCCCC	IF(POS.GT.40.AND.KEYNUM.EQ.0) GOTO 1000
	IF(KEYNUM.EQ.0) GOTO 8000
C
5     CONTINUE
C
	GOTO(10,20,30,40,50,60,70,80,90,100,
     *	  110,120,130,140,150,160,170,180,190,200,
     *	  210,220,230,240,250,260,270,280,290,300,
     *	  310,320,330,340,350,360,370,380,390,400,
     *	  410,420,430,440,450,460,470,480,490,500,
     *	  510,520,530,540,550,560,570,580,590,600,
     *	  610,620,630,640,650,660,670,680,690,700,
     *	  710,720,730,740,750,760,770,780,790,800,
     *    810,820,830,840,850,1000),KEYNUM
C
C AGENT SNAPSHOT
C
10	CONTINUE
	CALL X2XHLP('AGTSNP.HLP',FOUND)
	GOTO 920
C
C DATE SNAPSHOT
C
20	CONTINUE
	CALL X2XHLP('DATES.HLP',FOUND)
	GOTO 920
C
30	CONTINUE
	CALL X2XHLP('TOTSNP.HLP',FOUND)
	GOTO 920
C
C LINE SNAPSHOT
C
40	CONTINUE
	CALL X2XHLP('LINSNP.HLP',FOUND)
	GOTO 920
C
C NUMBER SNAPSHOT
C
50	CONTINUE
	CALL X2XHLP('NBRSNP.HLP',FOUND)
	GOTO 920
C
C SALES SNAPSHOT
C
60	CONTINUE
	CALL X2XHLP('SALES.HLP',FOUND)
	GOTO 920
C
C QUEUE SNAPSHOT
C
70	CONTINUE
	CALL X2XHLP('QUESNP.HLP',FOUND)
	GOTO 920
C
C SYSTEM SNAPSHOT
C
80	CONTINUE
	CALL X2XHLP('SYSTEM.HLP',FOUND)
	GOTO 920
C
C TERMINAL SNAPSHOT
C
90	CONTINUE
	CALL X2XHLP('TERSNP.HLP',FOUND)
	GOTO 920
C
C TRANS SNAPSHOT
C
100	CONTINUE
	CALL X2XHLP('TRNSNP.HLP',FOUND)
	GOTO 920
C
C X2STNPRT SNAPSHOT.
C
110	CONTINUE
	CALL X2XHLP('X2STNPRT.HLP',FOUND)
	GOTO 920
C
C X2GBLSNP SNAPSHOT.
C
120	CONTINUE
	CALL X2XHLP('X2GBLSNP.HLP',FOUND)
	GOTO 920
C
C X2STNSNP SNAPSHOT.
C
130	CONTINUE
	CALL X2XHLP('X2STNSNP.HLP',FOUND)
	GOTO 920
C
C X2ALLSAP SNAPSHOT.
C
140	CONTINUE
	CALL X2XHLP('X2ALLSAP.HLP',FOUND)
	GOTO 920
C
C X2SAPSNP SNAPSHOT.
C
150	CONTINUE
	CALL X2XHLP('X2SAPSNP.HLP',FOUND)
	GOTO 920
C
C X2NETSNP SNAPSHOT.
C
160	CONTINUE
	CALL X2XHLP('X2NETSNP.HLP',FOUND)
	GOTO 920
C
C VISION SYNTAX
C
170	CONTINUE
	CALL X2XHLP('SYNTAX.HLP',FOUND)
	GOTO 920
C
C X2ALLSTN SNAPSHOT.
C
180	CONTINUE
	CALL X2XHLP('X2ALLSTN.HLP',FOUND)
	GOTO 920
C
C CONSNP SNAPSHOT.
C
190	CONTINUE
	CALL X2XHLP('CONSNP.HLP',FOUND)
	GOTO 920
C
C LANID INFORMATION
C
200	CONTINUE
	CALL X2XHLP('LANID.HLP',FOUND)
	GOTO 920
C
C LANPRO INFORMATION
C
210	CONTINUE
	CALL X2XHLP('LANPRO.HLP',FOUND)
	GOTO 920
C
C HOTSNP INFORMATION.
C
220	CONTINUE
	CALL X2XHLP('HOTSNP.HLP',FOUND)
	GOTO 920
C
C X2ALLREL SNAPSHOT.
C
230	CONTINUE
	CALL X2XHLP('X2ALLREL.HLP',FOUND)
	GOTO 920
C
C X2RELSNP SNAPSHOT.
C
240	CONTINUE
	CALL X2XHLP('X2RELSNP.HLP',FOUND)
	GOTO 920
C
C X2ERRCODES
C
250     CONTINUE
        CALL X2XHLP('X2ERRCOD.HLP',FOUND)
        GOTO 920

C
C ACTSNP
C
260	CONTINUE
	CALL X2XHLP('ACTAGT.HLP',FOUND)
	GOTO 920

C
C INVSNP
C
270	CONTINUE
	CALL X2XHLP('INVSNP.HLP',FOUND)
	GOTO 920

C
C LOTSNP
C
280	CONTINUE
	CALL X2XHLP('LOTTO.HLP',FOUND)
	GOTO 920
C
C TSPSNP
C
290	CONTINUE
	CALL X2XHLP('TSPSNP.HLP',FOUND)
	GOTO 920
C
C STGSNP
C
300	CONTINUE
	CALL X2XHLP('STGSNP.HLP',FOUND)
	GOTO 920
C
C TATSNP
C
310	CONTINUE
	CALL X2XHLP('TATSNP.HLP',FOUND)
	GOTO 920
C
C ATTSNP
C
320	CONTINUE
	CALL X2XHLP('ATTSNP.HLP',FOUND)
	GOTO 920
C
C JOKSNP
C
330	CONTINUE
	CALL X2XHLP('JOKER.HLP',FOUND)
	GOTO 920
C
C SPTSNP
C
340	CONTINUE
	CALL X2XHLP('SPTSNP.HLP',FOUND)
	GOTO 920
C
C GAMSNP
C
350	CONTINUE
	CALL X2XHLP('GAMSNP.HLP',FOUND)
	GOTO 920
C
C CLOSNP
C
360	CONTINUE
	CALL X2XHLP('CLOSE.HLP',FOUND)
	GOTO 920
C
C SLISNP
C
370	CONTINUE
	CALL X2XHLP('SLISNP.HLP',FOUND)
	GOTO 920
C
C SCLSNP
C
380	CONTINUE
	CALL X2XHLP('SCLOSNP.HLP',FOUND)
	GOTO 920
C
C RWTSNP
C
390	CONTINUE
	CALL X2XHLP('RWTSNP.HLP',FOUND)
	GOTO 920
C
C TWDSNP
C
400	CONTINUE
	CALL X2XHLP('TWDSNP.HLP',FOUND)
	GOTO 920
C
C PERSNP
C
410	CONTINUE
	CALL X2XHLP('PERFORM.HLP',FOUND)
	GOTO 920
C
C TYPSNP
C
420	CONTINUE
	CALL X2XHLP('TYPSNP.HLP',FOUND)
	GOTO 920
C
C SPESNP
C
430	CONTINUE
	CALL X2XHLP('SPDEN.HLP',FOUND)
	GOTO 920
C
C VALSNP
C
440	CONTINUE
	CALL X2XHLP('VALID.HLP',FOUND)
	GOTO 920
C
C MESSNP
C
450	CONTINUE
	CALL X2XHLP('MESSNP.HLP',FOUND)
	GOTO 920
C
C MISSNP
C
460	CONTINUE
	CALL X2XHLP('MISSNP.HLP',FOUND)
	GOTO 920
C
C TCFSNP
C
470	CONTINUE
	CALL X2XHLP('TCFSNP.HLP',FOUND)
	GOTO 920
C
C TRASNP
C
480	CONTINUE
	CALL X2XHLP('TRANS.HLP',FOUND)
	GOTO 920
C
C LPLSNP
C
490	CONTINUE
	CALL X2XHLP('LPLSNP.HLP',FOUND)
	GOTO 920
C
C WRFSNP
C
500	CONTINUE
	CALL X2XHLP('WRFSNP.HLP',FOUND)
	GOTO 920
C
C LOGSNP
C
510	CONTINUE
	CALL X2XHLP('LOGSNP.HLP',FOUND)
	GOTO 920
C
C COMSNP
C
520	CONTINUE
	CALL X2XHLP('COMSNP.HLP',FOUND)
	GOTO 920
C
C NETSNP
C
530	CONTINUE
	CALL X2XHLP('NETSNP.HLP',FOUND)
	GOTO 920
C
C QUESNP
C
540	CONTINUE
	CALL X2XHLP('QUESNP.HLP',FOUND)
	GOTO 920
C
C HRSSNP
C
550	CONTINUE
	CALL X2XHLP('HRSSNP.HLP',FOUND)
	GOTO 920
C
C BUFSNP
C
560	CONTINUE
	CALL X2XHLP('BUFSNP.HLP',FOUND)
	GOTO 920
C
C FLWSNP
C
570	CONTINUE
	CALL X2XHLP('FLWSNP.HLP',FOUND)
	GOTO 920
C
C GTNSNP
C
580	CONTINUE
	CALL X2XHLP('GTNSNP.HLP',FOUND)
	GOTO 920
C
C TSALSNP
C
590	CONTINUE
	CALL X2XHLP('TSALES.HLP',FOUND)
	GOTO 920
C
C SALSNP
C
600	CONTINUE
	CALL X2XHLP('SALSNP.HLP',FOUND)
	GOTO 920
C
610	CONTINUE
620	CONTINUE
630	CONTINUE
640	CONTINUE
	GOTO 920
C
C OVRSNP
C
650	CONTINUE
	CALL X2XHLP('OVRSNP.HLP',FOUND)
	GOTO 920
C
C UNDSNP
C
660	CONTINUE
	CALL X2XHLP('UNDSNP.HLP',FOUND)
	GOTO 920
C
C BL1SNP
C
670	CONTINUE
	CALL X2XHLP('BL1SNP.HLP',FOUND)
	GOTO 920
C
C BL2SNP
C
680	CONTINUE
	CALL X2XHLP('BL2SNP.HLP',FOUND)
	GOTO 920
C
C TWITSNP
C
690	CONTINUE
	CALL X2XHLP('TWITSNP.HLP',FOUND)
	GOTO 920
C
C TSCRSNP
C
700	CONTINUE
	CALL X2XHLP('TSCRSNP.HLP',FOUND)
	GOTO 920
C
710	CONTINUE
	GOTO 920
C
C SKLSNP
C
720	CONTINUE
	CALL X2XHLP('SKLSNP.HLP',FOUND)
	GOTO 920
C
C PPLSNP
C
730	CONTINUE
	CALL X2XHLP('PPLSNP.HLP',FOUND)
	GOTO 920
C
C TSKSNP
C
740	CONTINUE
	CALL X2XHLP('TSKSNP.HLP',FOUND)
	GOTO 920
C
C TPRSNP
C
750	CONTINUE
	CALL X2XHLP('TPRSNP.HLP',FOUND)
	GOTO 920
C
C SKOSNP
C
760	CONTINUE
	CALL X2XHLP('SKOSNP.HLP',FOUND)
	GOTO 920
C
C PpOSNP
C
770	CONTINUE
	CALL X2XHLP('PPOSNP.HLP',FOUND)
	GOTO 920
C
C RSCSNP
C
780	CONTINUE
	CALL X2XHLP('RSCSNP.HLP',FOUND)
	GOTO 920
C
C SELSNP
C
790	CONTINUE
	CALL X2XHLP('SELECT.HLP',FOUND)
	GOTO 920
C
C SPNSNP
C
800	CONTINUE
	CALL X2XHLP('SPNAME.HLP',FOUND)
	GOTO 920
C
C TSODSNP
C
810	CONTINUE
	CALL X2XHLP('TSODSNP.HLP',FOUND)
	GOTO 920
C
C TULSNP
C
820	CONTINUE
	CALL X2XHLP('TULOS.HLP',FOUND)
	GOTO 920
C
C
830	CONTINUE
	GOTO 920
C
C SPSTSNP
C
840	CONTINUE
	CALL X2XHLP('SPSTSNP.HLP',FOUND)
	GOTO 920
C
C WITSNP
C
850	CONTINUE
	CALL X2XHLP('WItSNP.HLP',FOUND)
	GOTO 920
C
C REQUEST NOT IN MENU BUT TRY ANYWAY
C
8000	CONTINUE
C
	BUF(1)=SLINE(1)
	BUF(2)=SLINE(2)
        CALL X2XHLP(CBUF8,FOUND)
        IF(FOUND) GOTO 920
C
	DO 8100 I=1,6
8100	CNAME(I)=CBUF(I)
	CNAME(7)='.'
	CNAME(8)='D'
	CNAME(9)='O'
	CNAME(10)='C'
	OPEN(UNIT=4,FILE=FNAME,STATUS='OLD',ERR=990)
C
C READ TEXT FROM FILE
C
900	CONTINUE

	DO 910 I=1,22
	READ(4,2000,ERR=990,END=920) (NEW(J,I),J=1,20)
910	CONTINUE


920	CONTINUE
        IF(.NOT.FOUND) GOTO 990
	CALL USRCLOS1(     4)
        RETURN
C
C INVALID INPUT
C
990	CONTINUE

	CALL USRCLOS1(     4)
	WRITE(CLIN23,9923) SLINE(1),SLINE(2)
9923	FORMAT('SORRY, NO HELP AVAILABLE FOR ',2A4)

	RETURN
C
C DISPLAY HELP MENU
C
1000	CONTINUE
	WRITE(CLIN1,998)
	WRITE(CLIN2,1022)
	WRITE(CLIN3,999)
	WRITE(CLIN4,1022)

	IF (PAGE_NUM .LE. 1 .OR. PAGE_NUM .GT. 5) THEN
	  WRITE(CLIN5,10000)
	  WRITE(CLIN6,1022)
	  WRITE(CLIN7,1001) H(59),H(1)
	  WRITE(CLIN8,1002) H(41),H(22)
	  WRITE(CLIN9,1003) H(55),H(42)
	  WRITE(CLIN10,1004) H(65),H(27)
	  WRITE(CLIN11,1005) H(66),H(26)
	  WRITE(CLIN12,1006) H(6),H(91)
	  WRITE(CLIN13,1007)      H(46)
	  WRITE(CLIN14,1022)
	  WRITE(CLIN15,1022)
	  WRITE(CLIN16,1022)
	  WRITE(CLIN17,1022)
	ELSE IF (PAGE_NUM .EQ. 2) THEN
	  WRITE(CLIN5,20000)
	  WRITE(CLIN6,1022)
	  WRITE(CLIN7,2001) H(8),H(7)
	  WRITE(CLIN8,2002) H(35),H(51)
	  WRITE(CLIN9,2003) H(36),H(57)
	  WRITE(CLIN10,2004) H(2),H(56)
	  WRITE(CLIN11,2005) H(45),H(30)
	  WRITE(CLIN12,2006) H(44),H(92)
	  WRITE(CLIN13,2007) H(47),H(54)
	  WRITE(CLIN14,2008) H(48)
	  WRITE(CLIN15,1022)
	  WRITE(CLIN16,1022)
	  WRITE(CLIN17,1022)
	ELSE IF (PAGE_NUM .EQ. 3) THEN
	  WRITE(CLIN5,3000)
	  WRITE(CLIN6,1022)
	  WRITE(CLIN7,3001) H(28),H(62)
	  WRITE(CLIN8,3002) H(83),H(71)
	  WRITE(CLIN9,3003) H(49),H(64)
	  WRITE(CLIN10,3004) H(34),H(63)
	  WRITE(CLIN11,3005) H(80),H(67)
	  WRITE(CLIN12,3006) H(84),H(68)
	  WRITE(CLIN13,3007) H(33),H(61)
	  WRITE(CLIN14,3008) H(50),H(5)
	  WRITE(CLIN15,1022)
	  WRITE(CLIN16,1022)
	  WRITE(CLIN17,1022)
	ELSE IF (PAGE_NUM .EQ. 4) THEN
	  WRITE(CLIN5,4000)
	  WRITE(CLIN6,1022)
	  WRITE(CLIN7,4001) H(82),H(85)
	  WRITE(CLIN8,4002) H(70),H(69)
	  WRITE(CLIN9,4003) H(78),H(39)
	  WRITE(CLIN10,4004) H(79),H(72)
	  WRITE(CLIN11,4005) H(29),H(74)
	  WRITE(CLIN12,4006) H(81),H(76)
	  WRITE(CLIN13,4007) H(37),H(73)
	  WRITE(CLIN14,4008) H(38),H(75)
	  WRITE(CLIN15,4009) H(40),H(77)
	  WRITE(CLIN16,4010) H(32)
	  WRITE(CLIN17,4011) H(31)
	ELSE IF (PAGE_NUM .EQ. 5) THEN
	  WRITE(CLIN5,5000)
	  WRITE(CLIN6,1022)
	  WRITE(CLIN7,5001) H(13),H(18)
	  WRITE(CLIN8,5002) H(11),H(23)
	  WRITE(CLIN9,5003) H(24),H(14)
	  WRITE(CLIN10,5004) H(15),H(52)
	  WRITE(CLIN11,5005) H(12),H(19)
	  WRITE(CLIN12,5006) H(89),H(58)
	  WRITE(CLIN13,5007) H(16),H(53)
	  WRITE(CLIN14,5008) H(90),H(60)
	  WRITE(CLIN15,5009) H(87),H(88)
	  WRITE(CLIN16,5010) H(25),H(21)
	  WRITE(CLIN17,5011)       H(20)
	END IF
C
CCC	WRITE(CLIN20,1020)
CCC	WRITE(CLIN21,1021)
	WRITE(CLIN18,1022)
	WRITE(CLIN19,1022)
	WRITE(CLIN20,1022)
	WRITE(CLIN21,1022)
	WRITE(CLIN22,1022)
	
	RETURN
C
C FORMAT STATEMENTS
C
998	FORMAT('**** Vision help snapshot ****')
999	FORMAT('The following help is available:')
C
10000   FORMAT(10(' '),'Page 1. SALES , AGENTS',40(' '))
1001	FORMAT('<',A7,'>',1X,'Total System Sales   ',T40,  !59
     *	       '<',A7,'>',1X,'Agent snapshot       ')      !1 
1002	FORMAT('<',A7,'>',1X,'Performance snapshot ',T40,  !41
     *	       '<',A7,'>',1X,'Hotline snapshot     ')      !22
1003	FORMAT('<',A7,'>',1X,'Hourly Game Sales    ',T40,  !55
     *         '<',A7,'>',1X,'Agent Type Snapshot  ')      !42
1004	FORMAT('<',A7,'>',1X,'Over snapshot        ',T40,  !65
     *	       '<',A7,'>',1X,'Invoice snapshot     ')      !27
1005    FORMAT('<',A7,'>',1X,'Under snapshot       ',T40,  !66
     *         '<',A7,'>',1X,'Active Agents Snap   ')      !26
1006 	FORMAT('<',A7,'>',1X,'Sales snapshot       ',T40,  !6
     *         '<',A7,'>',1X,'Terminal Report Supr.')      !91
1007    FORMAT(39(' '),
     *         '<',A7,'>',1X,'Miscellaneous        ')      !46
C
20000   FORMAT(10(' '),'Page 2. SYSTEM , GAME STATUS',40(' '))
2001 	FORMAT('<',A7,'>',1X,'System snapshot      ',T40,  !8
     *         '<',A7,'>',1X,'Queue snapshot       ')      !7
2002	FORMAT('<',A7,'>',1X,'Game snapshot        ',T40,  !35
     *	       '<',A7,'>',1X,'System Logger        ')      !51
2003    FORMAT('<',A7,'>',1X,'Close snapshot       ',T40,  !36
     *	       '<',A7,'>',1X,'Data Flow snapshot   ')      !57
2004 	FORMAT('<',A7,'>',1X,'Date snapshot        ',T40,  !2
     *         '<',A7,'>',1X,'Buffer Summary       ')      !56
2005    FORMAT('<',A7,'>',1X,'Ticket Message       ',T40,  !45
     *         '<',A7,'>',1X,'STG snapshot         ')      !30
2006    FORMAT('<',A7,'>',1X,'Validation snapshot  ',T40,  !44
     *         '<',A7,'>',1X,'STL snapshot         ')      !92
2007	FORMAT('<',A7,'>',1X,'Transaction Carryover',T40,  !47
     *         '<',A7,'>',1X,'PTL snapshot         ')      !54
2008    FORMAT('<',A7,'>',1X,'Transaction Snapshot ',T40,  !48
     *         40(' '))
C
3000    FORMAT(10(' '),'Page 3. GAMES',40(' '))
3001    FORMAT('<',A7,'>',1X,'Lotto snapshot       ',T40,  !28
     *         '<',A7,'>',1X,'Ravi 65 snapshot     ')      !62
3002	FORMAT('<',A7,'>',1X,'Viking Lotto         ',T40   !83
     *         '<',A7,'>',1X,'Ravi V5 snapshot     ')      !71
3003	FORMAT('<',A7,'>',1X,'Pool Flags           ',T40,  !49
     *         '<',A7,'>',1X,'Ravi Popularity      ')      !64
3004    FORMAT('<',A7,'>',1X,'Vakio snapshot       ',T40,  !34
     *	       '<',A7,'>',1X,'Ravi Prognosis       ')      !63
3005    FORMAT('<',A7,'>',1X,'Vakio Team Names     ',T40,  !80
     *	       '<',A7,'>',1X,'Bingo AB snapshot    ')      !67
3006    FORMAT('<',A7,'>',1X,'Vakio Statistics     ',T40   !84
     *         '<',A7,'>',1X,'Bingo Fullhouse      ')      !68
3007	FORMAT('<',A7,'>',1X,'Joker snapshot       ',T40,  !33
     *	       '<',A7,'>',1X,'Speden snapshot      ')      !61
3008    FORMAT('<',A7,'>',1X,'Win Reserve Fund     ',T40,  !50
     *	       '<',A7,'>',1X,'Numbers  snapshot    ')      !5
C
4000    FORMAT(10(' '),'Page 4. GAMES',40(' '))
4001    FORMAT('<',A7,'>',1X,'Tulos snapshot       ',T40,  !82
     *	       '<',A7,'>',1X,'Voittaja Sales       ')      !85
4002    FORMAT('<',A7,'>',1X,'Tulos snapshot       ',T40,  !70
     *	       '<',A7,'>',1X,'Voittaja snapshot    ')      !69
4003    FORMAT('<',A7,'>',1X,'Tulos Results        ',T40,  !78
     *	       '<',A7,'>',1X,'Voittaja Results     ')      !39
4004	FORMAT('<',A7,'>',1X,'Pitka Rows           ',T40,  !79
     *         '<',A7,'>',1X,'Super Double         ')      !72
4005	FORMAT('<',A7,'>',1X,'Pitka Pool snapshot  ',T40,  !29
     *         '<',A7,'>',1X,'Super Double Sales   ')      !74
4006	FORMAT('<',A7,'>',1X,'Pitka Odds           ',T40,  !81
     *         '<',A7,'>',1X,'Super Double Odds    ')      !76
4007	FORMAT('<',A7,'>',1X,'Pitka Liability      ',T40,  !37
     *	       '<',A7,'>',1X,'Todays Couple        ')      !73
4008    FORMAT('<',A7,'>',1X,'Pitka Draw Date      ',T40,  !38
     *	       '<',A7,'>',1X,'Todays Couple Sales  ')      !75
4009    FORMAT('<',A7,'>',1X,'Pitka Watchdog       ',T40,  !40                  
     *	       '<',A7,'>',1X,'Todays Couple Odds   ')      !77
4010    FORMAT('<',A7,'>',1X,'Pitka Most Played Bet',T40,  !32
     *         40(' '))
4011	FORMAT('<',A7,'>',1X,'Pitka Todays Sales   ',T40,  !31
     *         40(' '))
C
5000    FORMAT(10(' '),'Page 5. COMMUNICATION',40(' '))
5001	FORMAT('<',A8,'>',1X,'Stn Port snapshot    ',T40,  !13
     *	       '<',A8,'>',1X,'All Stations snapshot')      !18
5002	FORMAT('<',A8,'>',1X,'Station snapshot     ',T40,  !11
     *	       '<',A8,'>',1X,'All Relay snapshot   ')      !23
5003 	FORMAT('<',A8,'>',1X,'Specific Relay       ',T40   !24
     *	       '<',A8,'>',1X,'X2X All SAPs snapshot')      !14
5004	FORMAT('<',A8,'>',1X,'Service Access Point ',T40,  !15
     *         '<',A8,'>',1X,'Communications       ')      !52
5005 	FORMAT('<',A8,'>',1X,'X2X Global snapshot  ',T40   !12
     *         '<',A8,'>',1X,'LAN Con snapshot     ')      !19
5006    FORMAT('<',A8,'>',1X,'Agent Port snapshot  ',T40,  !89
     *         '<',A8,'>',1X,'Gtech Network        ')      !58
5007 	FORMAT('<',A8,'>',1X,'X2X Network Ports snp',T40,  !16
     *         '<',A8,'>',1X,'Network              ')      !53 
5008    FORMAT('<',A8,'>',1X,'Terminal Status      ',T40,  !90
     *         '<',A8,'>',1X,'Request snapshot     ')      !60
5009    FORMAT('<',A8,'>',1X,'Network Delay snp    ',T40,  !87
     *         '<',A8,'>',1X,'SAP snapshot         ')      !88
5010	FORMAT('<',A8,'>',1X,'X2X Error Codes      ',T40,  !25
     *	       '<',A8,'>',1X,'LANPRO Commands      ')      !21
5011    FORMAT( 39(' '),
     *         '<',A8,'>',1X,'Lan Definition       ')      !20
C
1020	FORMAT('Note: Information for all system parameters ',
     *	 'can be obtained by typing the name.')
1021	FORMAT('      Information for all online tasks can be '
     *	 'obtained by typing the task name.')
1022	FORMAT(80(' '))
2000	FORMAT(20A4)
C
	END
