C
C INSSNP.FOR
C 
C V18 08-JUN-2000 UXN ITNAMES.DEF added.
C V17 13-MAY-1999 UXN STRTIM CHANGED TO STARTIM
C V16 26-FEB-1997 RXK P(FLSTIM)=0 allowed
C V15 19-FEB-1997 HXK Added cash amount field
C V14 05-FEB-1997 WPW Changes to GVTFlg, GVTSup, GVTGpl, GVTRst and GVTDfl.
C V13 27-FEB-1995 SMH Added TSTTIM and TSTNUM
C V12 24-FEB-1995 SMH Let GVTSUP take a value of 0
C V11 22-FEB-1995 SMH Set limit for GVTRST
C V10 16-FEB-1995 SMH Change upper limit for FLSTIM and ENDTIM
C V09 30-JAN-1995 SMH Upper values of GVTSUP and GVTGPL = 65000
C V08 13-DEC-1994 SMH Let GVTBMP be zero, and GVTFLG and GVTDFL take value 32
C V07 08-AUG-1994 SMH Added INSPRNT - issuance print parameter
C                         ESTFLG  - establishment message flag
C V06 01-FEB-1994 PXN INITIAL RELEASE FOR NETHERLANDS
C V05 01-JUN-1993 PXN INITIAL RELEASE FOR IRELAND
C V04 18-MAY-1993 MCM RELEASED FOR GEORGIA
C V03 24-APR-1992 NJA ADDED (VALIDATION MAXIMUM PARAMETER)
C V02 10-FEB-1992 JPJ ADDED (GVT)
C V01 21-NOV-1991 JPJ RELEASED FOR VAX (INSTANTS)
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
	SUBROUTINE INSSNP(DAT,CLINE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:ITNAMES.DEF'
C
        ! arguments                        
        INTEGER*4  DAT                     !
        INTEGER*4  CLINE(20)               !

        ! variables

	INTEGER*4  ST                      !
	INTEGER*4  FDB(7)                  !
	INTEGER*4  K                       !
	INTEGER*4  REC                     !
	INTEGER*4  LIN                     !
	INTEGER*4  I                       !
	INTEGER*4  J                       !
	INTEGER*4  BUF(CDLEN)              !
	INTEGER*4  VALUE                   !
	INTEGER*4  POS                     !
	INTEGER*4  KEYNUM                  !
	INTEGER*4  BITMAP

	INTEGER*2 D(LDATE_LEN)             !

        CHARACTER*1 SPACE/' '/
        CHARACTER*1 ASTER/'*'/

	REAL*8    OPT(28)                  !

	data opt/'GVTFlg  ','GVTSup  ','GVTGpl  ','GVTRst  ',
     *		 'GVTDfl  ','gvtivl  ','MAXdwn  ','DWNtim  ',
     *           'bchsiz  ','STRtim  ','ENDtim  ','FLStim  ',
     *           'valprnt ','actprnt ','setprnt ','retprnt ',
     *           'delprnt ','fwdcnt  ','valpamt ','tierlim ',
     *           'gvtbmp  ','gvtauth ','HOTphn  ','issprnt ',
     *           'estflg  ','tsttim  ','tstnum  ','        '/
c
C INSSNP INPUT
C
	VALUE=0
	POS=1
	CALL KEY(CLINE,OPT,28,POS,KEYNUM)
	IF(POS.GT.40) GOTO 400                      !NO INPUT
	IF(KEYNUM.EQ.0)GOTO 300                    !INPUT ERROR
	CALL NUMB(CLINE,POS,VALUE)                 !GET VALUE
	IF(VALUE.LT.0)  GOTO 205
C
C CLEAR COMMAND MESSAGE BUFFER
C
2	CONTINUE
	CALL FASTSET(0,BUF,CDLEN)
	GOTO(10,20,30,40,50,60,70,80,90,100,110,120,130,
     *       140,150,160,170,180,190,200,210,220,230,240,
     *       250,260,270) KEYNUM
C
C GVTFLG CHANGE, 01 - RESET GVT NOW, 32 - CALL BACK IN X MIN
C		 00 - DO NOT RESET
C
10	CONTINUE
        IF(VALUE.EQ.0.OR.VALUE.EQ.1.OR.VALUE.EQ.32) THEN 
  	   BUF(1)=GVTFLG
	   BUF(2)=VALUE
	   BUF(3)=TCPAR
	   GOTO 2500
        ELSE 
           GOTO 205
        ENDIF
C
C GVTSUP CHANGE, CALL BACK IN X MINUTS DUE TO THE SYSTEM PROBLEMS
C
20	CONTINUE
	IF(VALUE.LT.0.OR.VALUE.GT.65000) GOTO 205
	BUF(1)=GVTSUP
	BUF(2)=VALUE
	BUF(3)=TCPAR
	GOTO 2500
C
C GVTGPL CHANGE, CALL BACK IN X MINUTS DUE TO THE IPS SYSTEM PROBLEMS
C
30	CONTINUE
	IF(VALUE.LT.1.OR.VALUE.GT.65000) GOTO 205
	BUF(1)=GVTGPL
	BUF(2)=VALUE
	BUF(3)=TCPAR
	GOTO 2500
C
C GVTRST CHANGE, CALLBACK FLAG , 0 - NO CALL BACK, 1 - CALL BACK
C
40	CONTINUE
	IF(VALUE.LT.0.OR.VALUE.GT.10) GOTO 205
	BUF(1)=GVTRST
	BUF(2)=VALUE
	BUF(3)=TCPAR
	GOTO 2500
C
C GVTDFL CHANGE, IF 04 THEN GVT WILL RESET IN ADLTIM MIN
C
50	CONTINUE
        BITMAP=0
        IF(VALUE.EQ.1.OR.VALUE.EQ.2.OR.VALUE.EQ.4.OR.VALUE.EQ.8.OR.
     *     VALUE.EQ.16.OR.VALUE.EQ.32.OR.VALUE.EQ.128.OR.VALUE.EQ.0)
     *     THEN 
	   BUF(1)=GVTDFL
	   BUF(2)=VALUE
	   BUF(3)=TCPAR
    	   GOTO 2500
        ELSE 
           GOTO 205
        ENDIF
C
C GVTIVL CHANGE
C
60	CONTINUE
        GOTO 400
C61	IF(VALUE.LT.0.OR.VALUE.GT.100) GOTO 205
C	BUF(1)=GVTIVL
C	BUF(2)=VALUE
C	BUF(3)=TCPAR
C	GOTO 2500
C
C MAXDWN CHANGE
C
70	CONTINUE
	IF(VALUE.LE.0.OR.VALUE.GT.600) GOTO 205
	BUF(1)=MAXDWN
	BUF(2)=VALUE
	BUF(3)=TCPAR
	GOTO 2500
C
C DWNTIM CHANGE
C
80	CONTINUE
	IF(VALUE.LT.0.OR.VALUE.GT.4800) GOTO 205
	BUF(1)=DWNTIM
	BUF(2)=VALUE
	BUF(3)=TCPAR
	GOTO 2500
C
C BCHSIZ CHANGE
C
90	CONTINUE
        GOTO 400
C91	IF(VALUE.LT.1.OR.VALUE.GT.28) GOTO 205
C	BUF(1)=BCHSIZ
C	BUF(2)=VALUE
C	BUF(3)=TCPAR
C	GOTO 2500
C
C STARTIM CHANGE
C
100	CONTINUE
	IF(VALUE.LT.0.OR.VALUE.GT.84600) GOTO 205
	BUF(1)=STARTIM
	BUF(2)=VALUE
	BUF(3)=TCPAR
	GOTO 2500
C
C ENDTIM CHANGE
C
110	CONTINUE
	IF(VALUE.LT.1.OR.VALUE.GT.86400) GOTO 205
	BUF(1)=ENDTIM
	BUF(2)=VALUE
	BUF(3)=TCPAR
	GOTO 2500
C
C FLSTIM CHANGE
C
120	CONTINUE
	IF(VALUE.NE.0) THEN
           IF(VALUE.LT.P(STARTIM).OR.VALUE.GT.P(ENDTIM)) GOTO 205
        ENDIF
	BUF(1)=FLSTIM
	BUF(2)=VALUE
	BUF(3)=TCPAR
	GOTO 2500
C
C VALPRNT CHANGE
C
130	CONTINUE
        GOTO 400
C131	IF(VALUE.LT.0.OR.VALUE.GT.20) GOTO 205
C	BUF(1)=VALPRNT
C	BUF(2)=VALUE
C	BUF(3)=TCPAR
C	GOTO 2500
C
C ACTPRRNT CHANGE
C
140	CONTINUE
        GOTO 400
C141	IF(VALUE.LT.0.OR.VALUE.GT.20) GOTO 205
C	BUF(1)=ACTPRNT
C	BUF(2)=VALUE
C	BUF(3)=TCPAR
C	GOTO 2500
C
C SETPRNT CHANGE
C
150	CONTINUE
        GOTO 400
C151	IF(VALUE.LT.0.OR.VALUE.GT.20) GOTO 205
C	BUF(1)=SETPRNT
C	BUF(2)=VALUE
C	BUF(3)=TCPAR
C	GOTO 2500
C
C RETPRNT CHANGE
C
160	CONTINUE
        GOTO 400
C161	IF(VALUE.LE.0.OR.VALUE.GT.20) GOTO 205
C	BUF(1)=RETPRNT
C	BUF(2)=VALUE
C	BUF(3)=TCPAR
C	GOTO 2500
C
C DELPRNT CHANGE
C
170	CONTINUE
        GOTO 400
C171	IF(VALUE.LT.0.OR.VALUE.GT.20) GOTO 205
C	BUF(1)=DELPRNT
C	BUF(2)=VALUE
C	BUF(3)=TCPAR
C	GOTO 2500
C
C FWDCNT CHANGE
C
180	CONTINUE
        GOTO 400
C181	IF(VALUE.LT.1.OR.VALUE.GT.28) GOTO 205
C	BUF(1)=FWDCNT
C	BUF(2)=VALUE
C	BUF(3)=TCPAR
C	GOTO 2500
C
C VALPAMT CHANGE
C
190	CONTINUE
        GOTO 400
C191	IF(VALUE.LT.0.OR.VALUE.GT.100000) GOTO 205
C	BUF(1)=VALPAMT
C	BUF(2)=VALUE
C	BUF(3)=TCPAR
C	GOTO 2500
C
C TIERLIM CHANGE
C
200	CONTINUE
        GOTO 400
C201	IF(VALUE.LT.0.OR.VALUE.GT.600) GOTO 205
C	BUF(1)=TIERLIM
C	BUF(2)=VALUE
C	BUF(3)=TCPAR
C	GOTO 2500
C
C GVTBMP CHANGE
C
210	CONTINUE
        GOTO 400
C211	IF(VALUE.LT.0.OR.VALUE.GT.2) GOTO 205
C	BUF(1)=GVTBMP
C	BUF(2)=VALUE
C	BUF(3)=TCPAR
C	GOTO 2500
C
C GVTAUTH CHANGE
C
220	CONTINUE
        GOTO 400
C221	IF(VALUE.LT.0.OR.VALUE.GT.99999999) GOTO 205
C	BUF(1)=GVTAUTH
C	BUF(2)=VALUE
C	BUF(3)=TCPAR
C	GOTO 2500
C
C HOTPHN CHANGE
C
230	CONTINUE
	IF(VALUE.LT.0.OR.VALUE.GT.9999999) GOTO 205
	BUF(1)=HOTPHN
	BUF(2)=VALUE
	BUF(3)=TCPAR
	GOTO 2500
C
C ISSPRNT CHANGE
C
240	CONTINUE
        GOTO 400
C241	IF(VALUE.LT.0.OR.VALUE.GT.20) GOTO 205
C	BUF(1)=ISSPRNT
C	BUF(2)=VALUE
C	BUF(3)=TCPAR
C	GOTO 2500
C
C ESTFLG CHANGE
C
250	CONTINUE
        GOTO 400
C251	IF(VALUE.LT.0.OR.VALUE.GT.32) GOTO 205
C	BUF(1)=ESTFLG
C	BUF(2)=VALUE
C	BUF(3)=TCPAR
C	GOTO 2500
C
C TSTTIM CHANGE
C
260     CONTINUE
        GOTO 400
C261	HRS=VALUE/100
C	MNS=MOD(VALUE,100)
C       IF(HRS.LT.0.OR.HRS.GT.23) GOTO 205
C	IF(MNS.LT.0.OR.MNS.GT.59) GOTO 205
C        BUF(1)=TSTTIM
C        BUF(2)=VALUE
C        BUF(3)=TCPAR
C        GOTO 2500
C
C TSTNUM CHANGE
C
270     CONTINUE
        GOTO 400
C271     IF(VALUE.LT.0.OR.VALUE.GT.999) GOTO 205
C        BUF(1)=TSTNUM
C        BUF(2)=VALUE
C        BUF(3)=TCPAR
C        GOTO 2500
C
C
C INPUT ERROR
C
300	CONTINUE
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
2500	CONTINUE
	BUF(6)=IDNUM
	CALL QUECMD(BUF,ST)
C	CALL VISCMD(BUF,ST)
	CALL XWAIT(2,1,ST)
C
C GET DATE
C
400	CONTINUE
	IF(DAT.LE.0) DAT=DAYCDC
	D(VCDC)=DAT
	CALL LCDATE(D)
	IF(DAT.EQ.DAYCDC) THEN
	  SMODE=.FALSE.
	  CALL FASTMOV(DAYSTS,DAFSTS,DAFLEN)
	  GOTO 500
	ENDIF
C
C READ DATA FROM FILE
C
	SMODE=.TRUE.
	CALL OPENW(1,SFNAMES(1,DAF),0,0,0,ST)
	CALL IOINIT(FDB,1,DAFSEC*256)
	IF(ST.NE.0) THEN
	  CALL USRCLOS1(     1)
	  WRITE(CLIN23,8000) (SFNAMES(K,DAF),K=1,5),ST
	  RETURN
	ENDIF
	REC=DAT
	CALL READW(FDB,REC,DAFREC,ST)
	IF(ST.NE.0) THEN
	  CALL USRCLOS1(     1)
	  WRITE(CLIN23,8001) (SFNAMES(K,DAF),K=1,5),ST,REC
	  RETURN
	ENDIF
	CALL USRCLOS1(     1)
	IF(DAFSTS.EQ.DUNUSD) THEN
	  WRITE(CLIN23,8002) (D(K),K=7,13)
	  RETURN
	ENDIF
	IF(DAFSTS.EQ.DNOSAL) THEN
	  WRITE(CLIN23,8003) (D(K),K=7,13)
	  RETURN
	ENDIF
C
C FORMAT SALES SNAPSHOT
C
500	CONTINUE
C
C
C
	WRITE(CLIN1,9001) (D(K),K=7,13)
	WRITE(CLIN3,9004) ASTER,OPT(1),P(GVTFLG),
     *			  ASTER,OPT(2),P(GVTSUP),
     *			  ASTER,OPT(3),P(GVTGPL),
     *			  ASTER,OPT(4),P(GVTRST),
     *			  ASTER,OPT(5),P(GVTDFL)
	WRITE(CLIN4,9004) SPACE,OPT(6),P(GVTIVL),
     *			  ASTER,OPT(7),P(MAXDWN),
     *			  ASTER,OPT(8),P(DWNTIM),
     *			  SPACE,OPT(9),P(BCHSIZ),
     *			  ASTER,OPT(10),P(STARTIM)
	WRITE(CLIN5,9004) ASTER,OPT(11),P(ENDTIM),
     *			  ASTER,OPT(12),P(FLSTIM),
     *			  SPACE,OPT(13),P(VALPRNT),
     *			  SPACE,OPT(14),P(ACTPRNT),
     *			  SPACE,OPT(15),P(SETPRNT)
	WRITE(CLIN6,9004) SPACE,OPT(16),P(RETPRNT),
     *			  SPACE,OPT(17),P(DELPRNT),
     *			  SPACE,OPT(18),P(FWDCNT),
     *			  SPACE,OPT(19),P(VALPAMT),
     *			  SPACE,OPT(20),P(TIERLIM)
	WRITE(CLIN7,9005) SPACE,OPT(21),P(GVTBMP),
     *			  SPACE,OPT(22),P(GVTAUTH),
     *			  ASTER,OPT(23),P(HOTPHN),
     *                    SPACE,OPT(24),P(ISSPRNT),
     *                    SPACE,OPT(25),P(ESTFLG)
	WRITE(CLIN8,9008) SPACE,OPT(26),P(TSTTIM),
     *			  SPACE,OPT(27),P(TSTNUM),
     *			  SPACE,'TSTCNT  ',TESTSET_CNT


C
	LIN=10

	DO 520 I=TINS,TINS
	   J=1
	   IF(ITNAMES(J-1).EQ.'????????') GOTO 511
	   WRITE(XNEW(  LIN),9012) GTNAMES(I),ITNAMES(J-1),
     *	                           DAFCRS(J),ITNAMES(J),
     *                             DAFCRS(J+1),
     *                             CMONY(DAFIVAL,12,1)
	   LIN=LIN+1
	   DO 510 J=3,NUMCRS,2
	      IF(ITNAMES(J-1).EQ.'????????') GOTO 510
	        WRITE(XNEW(  LIN),9002) GTNAMES(I),ITNAMES(J-1),
     *	                                DAFCRS(J),ITNAMES(J),
     *                                  DAFCRS(J+1)
	      LIN=LIN+1
510	   CONTINUE
511        CONTINUE
	   LIN=LIN+2
520	CONTINUE
C
	WRITE(CLIN21,9006) CRSCNT(1)-CRSCNT(2)
C
	RETURN

C
C     ================== FORMAT STATEMENTS ================
C
8000	FORMAT(5A4,' file open error ',I4)
8001	FORMAT(5A4,' file read error ',I4,' record - ',I4)
8002	FORMAT('Record not initialized for ',7A2)
8003	FORMAT(7A2,' is not a sales date')
C
9001	FORMAT('Instant Activity for ',7A2)
9002	FORMAT(2A8,1X,I8,10X,A8,1X,I8)
9003	FORMAT(80(' '))
9004	FORMAT(5(A1,A8,1X,I5,1X))
9005	FORMAT(2(A1,A8,1X,I5,1X),A1,A7,I7,1X,2(A1,A8,1X,I5,1X))
9006	FORMAT('Number of transactions in crspro ',I3)
C
9008	FORMAT(3(A1,A8,1X,I5,1X))
9010    FORMAT('GVTS INSTALLED TODAY ',I5, ' DUMMIES CONFIGURED ',I5,
     *         ' DUMMIES IN USE ',I5)
9011    FORMAT('EVS UPDATE TABLE: SIZE ',I5, ' IN USE ',I5,' FREE ',I5)
9012	FORMAT(2A8,1X,I8,10X,A8,1X,I8,'  Cash Amt:',A12)
C
	END
