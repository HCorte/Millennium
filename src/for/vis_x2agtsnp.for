C
C SUBROUTINE X2AGTSNP
C $Log:   GXAFXT:[GOLS]X2AGTSNP.FOV  $
C  
C V09 16-MAR-2011 GPW NUMAGT=12288
C V08 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V07 02-SEP-1994 HXK Initial revision.
C V06 26-JUN-1994 HXK Initial revision.
C V05 13-JUN-1993 HXK added AGTINF.DEF
C V04 21-JAN-1993 DAB Initial Release Based on Netherlands Bible, 12/92,
C                     and Comm 1/93 update DEC Baseline
C V03 09-JUN-1994 SCD CREATE NEW SNAPSHOT X2AGTSNP FOR FINLAND'S RFSS 152
C                   BASED UP X2STNPRT.
C V02 06-FEB-1992 DAS ADDED PORT COMMAND
C V01 01-DEC-1991 DAS RELEASED FOR VAX (NETHERLANDS)
C
C
C VIS_X2AGTSNP.FOR
C
C This subroutine will display all ports defined for a
C station existing on the X2X network subsystem.
C NOTE: all necessary input information has been stored
C in X2VIS by X2LINTER.
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
	SUBROUTINE X2AGTSNP(CMDLIN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2VIS.DEF'
C
	INTEGER*4   DISDIM                          !Dimension of table
	PARAMETER  (DISDIM  = 6)
	INTEGER*4   DISSIZ
	PARAMETER  (DISSIZ  = X2X_MAXPORT*X2X_MAXTERMS)
C
	INTEGER*4   CMDLIN(20)                      !Input command line
	INTEGER*4   DISCNT                          !Display counter
	INTEGER*4   DISTBL(DISDIM,DISSIZ)           !Display table
	INTEGER*4   PRTIDX,TERIDX,AGTIDX,TIMIDX     !Indices into DISTBL
	INTEGER*4   DRPIDX,STSIDX                   !Indices into DISTBL
	INTEGER*4   MAXDISP                         !Max lines displayed
	INTEGER*4   BEGIDX,CURIDX                   !Display indices
	INTEGER*4   PRT                             !Array indices
	INTEGER*4   PRTSRT,TERSRT,AGTSRT,TIMSRT     !Sort keys
	INTEGER*4   DRPSRT,STSSRT                   !Sort keys
	INTEGER*4   KEY1,KEY2,KEY3                  !Sort keys
	INTEGER*4   LINIDX                          !Print line index
	INTEGER*4   SEC,MIN,HR                      !Time display
	INTEGER*4   STATE                           !Terminal state
	INTEGER*4   DELAY                           !Refresh delay
	INTEGER*4   CMDVALUE(4)                     !Command value
	INTEGER*4   CBUF(CDLEN)                     !Command buffer
        INTEGER*4   CONNID                          !Connection identifier
	INTEGER*4   DROP, TERM, PORT, ST, VALUE, KEYNUM
	INTEGER*4   VALUE2                          !2nd # on cmd line -
                                                    !used for TERS command
	INTEGER*4   POS, I, STN, TER, LSTSTN, LSTTIM, LSTPRT,J   !V03
	INTEGER*4   LSTSRT, LSTSEC, LSTHR, LSTMIN
	INTEGER*4   FDB(7)                          !V03
	INTEGER*4   BROAD_4		    	    !Broadcast type 4 - V03
	INTEGER*4   ADR_LEN			    !Stn address length - 
                                                    !V03
	INTEGER*4   ERR                             !V03
        INTEGER*4   NAME_WIDTH                      !# of characters 
						    !displayed for the 
						    !agent's name - V03
	BYTE	    BDROP(4)
	REAL*8      CMDOPT(5)                       !Command line options - 
                                                    !V03
	REAL*8      CMDOPT1(1)                      !Additional text for
                                                    !Command line options -
                                                    !V03
	CHARACTER   C2DROP*2			    !Drop address
	CHARACTER   CHRSTR(16)*1                    !Char address - V03
	CHARACTER   BLANK(10) /10*' '/		    !Filler - V03
	CHARACTER   TERSTATE(0:5)*7                 !Terminal state
	CHARACTER   CZERO                           !V03
	DATA        CZERO/Z0/                       !V03
	LOGICAL     LOADED      /.FALSE./           !Array loaded flag
	LOGICAL     DISPLAY     /.FALSE./           !Display info flag
C
	EQUIVALENCE (DROP,BDROP,C2DROP)
C
	PARAMETER  (MAXDISP = 13)                   !V03
	PARAMETER  (PRTIDX  = 1)
	PARAMETER  (TERIDX  = 2)
	PARAMETER  (STSIDX  = 3)                    !V03
	PARAMETER  (AGTIDX  = 4)                    !V03
	PARAMETER  (TIMIDX  = 5)
	PARAMETER  (DRPIDX  = 6)
	PARAMETER  (PRTSRT  = 1)
	PARAMETER  (TERSRT  = 2)
	PARAMETER  (STSSRT  = 3)                    !V03
	PARAMETER  (AGTSRT  = 4)                    !V03
	PARAMETER  (TIMSRT  = 5)
	PARAMETER  (DRPSRT  = 6)
	PARAMETER  (DELAY   = 60)
	PARAMETER  (BROAD_4 = 4)                    !V03
	PARAMETER  (NAME_WIDTH = 15)     !# of characters displayed
                                         !for agent's name - V03

C
	DATA        TERSTATE    /'not def','defined',' active',
     *	                         'slowpol','disable','waitmsg'/
 
	DATA        CMDOPT      /'TERState','BRO4num ',
     *	         		 'PRTstats','SOFt    ','HARd    '/ !V03
	DATA        CMDOPT1     /'ALL     '/                       !V03

	DATA        LSTMIN  / 0 /
	DATA        LSTHR   / 0 /
	DATA        LSTSEC  / 0 /
	DATA        LSTSRT  / 0 /
	DATA        LSTTIM  / 0 /
	DATA        LSTSTN  / 0 /
        DATA        LSTPRT  / 0 /
	DATA        CURIDX  / 0 /
	DATA        BEGIDX  / 0 /
	DATA        DISCNT  / 0 /
	DATA        TER     / 0 /
C
C INITIALIZE VARIABLES.
C
	STN=X2FLDINF(XSTNIDX)
	IF(STN.EQ.0) STN=LSTSTN
	IF(STN.LE.0 .OR. STN.GT.X2X_STATIONS) STN=1
	DO 20 I=1,4
	  CMDVALUE(I)=-1
20	CONTINUE
C
C IF AN X2X COMMAND HAS BEEN ENTERED, DO NOT
C CHECK FOR OTHER COMMANDS.
C
        !temporarily using X2SCRN_ALLSTN for X2SCRN_ASTNPRT!
	IF(X2SCRN(X2SCRN_INPLEV1,X2SCRN_ASTNPRT).NE.0 .OR.     !V03
     *	   X2SCRN(X2SCRN_INPLEV2,X2SCRN_ASTNPRT).NE.0 .OR.     !V03
     *	   X2SCRN(X2SCRN_INPLEV3,X2SCRN_ASTNPRT).NE.0) THEN    !V03
	  X2SCRN(X2SCRN_INPLEV1,X2SCRN_ASTNPRT)=0              !V03
	  X2SCRN(X2SCRN_INPLEV2,X2SCRN_ASTNPRT)=0              !V03
	  X2SCRN(X2SCRN_INPLEV3,X2SCRN_ASTNPRT)=0              !V03
	  GOTO 100
	ENDIF
C
C CHECK FOR INPUT COMMAND LINE.
C
	POS=1
	CALL KEY(CMDLIN,CMDOPT,5,POS,KEYNUM)    !V03
CV03	IF(POS.GT.40) GOTO 100                  !NO INPUT
	IF(POS.GT.40 .AND. KEYNUM.EQ.0) GOTO 100!V03 - NO INPUT IF NO MATCH
                                                !KEY RETURNS POS=41 EVEN
                                                !WHEN IT MATCHES CMDOPT(3),
                                                !CMDOPT(4), OR CMDOPT(5),
                                                !I.E. KEYNUM=3,4 OR 5
	IF(KEYNUM.EQ.0) THEN                    !INPUT ERROR
	  WRITE(CLIN23,9923)
	  GOTO 100
	ENDIF
	IF(KEYNUM.NE.3 .AND. KEYNUM.NE.4 .AND.
     *	   KEYNUM.NE.5) THEN
	   CALL NUMB(CMDLIN,POS,VALUE)             !GET VALUE
	   IF(VALUE.LT.0) THEN                     !VALUE ERROR
	     WRITE(CLIN23,9924)
	     GOTO 100
	   ENDIF
	ENDIF
	GOTO (50,60,65,70,80) KEYNUM            !V03
	WRITE(CLIN23,9923)
	GOTO 100
C
C TERMINAL STATUS CHANGE.
C
50	CONTINUE
CV03	IF(VALUE.LT.0.OR.VALUE.GT.4.OR.X2FLDINF(XTERIDX).EQ.0) THEN
	IF(VALUE.LT.0.OR.VALUE.GT.4) THEN        !V03 - INVALID STATUS #
	  WRITE(CLIN23,9924)
	  GOTO 100
	ENDIF

C START OF V03 CHANGE BLOCK
	CMDVALUE(1)=VALUE                        !STATUS #, SET ABOVE
	CALL NUMB(CMDLIN,POS,VALUE2)             !GET 2ND VALUE = TERMINAL #

	IF(VALUE2.LT.0) THEN                     !NO NUMBER BUT THE STRING 
	  CALL KEY(CMDLIN,CMDOPT1,1,POS,KEYNUM)  !"ALL" MAY BE PRESENT
	  IF(KEYNUM.EQ.0) THEN                   !THE STRING "ALL" ISN'T 
	    WRITE(CLIN23,9924)                   !PRESENT EITHER  - ERROR
	    GOTO 100
          ELSE                                   !ALL IS PRESENT -SET ALL 
	     DO 121 PORT=1,X2X_MAXPORT           !TERMINALS ON THIS STATION
	        DO 201 TER=1,X2X_MAXTERMS
	           TERM=X2XS_TERMS(TER,PORT,STN)
	           IF(TERM.NE.0) THEN
                      X2FLDINF(XTERIDX) = TERM
	              CALL X2BLDCMD(1,XTER,X2FLDINF(XTERIDX),5,CMDVALUE,
     *                              2,1,CBUF)

	              CBUF(6)=IDNUM              !CODE COPIED FROM LABEL 90
	              CALL VISCMD(CBUF,ST)
	              CALL XWAIT(2,1,ST)
	           ENDIF
201	        CONTINUE
121	     CONTINUE
	     GOTO 100                          !ALL COMMANDS HAVE BEEN
                                               !QUEUED AND SET SO CONTINUE 
          ENDIF
	ELSE                                   !1 TERMINAL # SPECIFIED SET
          X2FLDINF(XTERIDX) = VALUE2           !TERMINAL # AND STATE
	  CALL X2BLDCMD(1,XTER,X2FLDINF(XTERIDX),5,CMDVALUE,2,
     *                  1,CBUF)
	  GOTO 90
        ENDIF
C
C END OF V03 CHANGE BLOCK

C
C BROADCAST A MESSAGE TO A TERMINAL.
C
60	CONTINUE
CV03	IF(VALUE.LT.1.OR.VALUE.GT.256.OR.X2FLDINF(XTERIDX).EQ.0) THEN
C V03 CHANGE - ALWAYS SEND BROADCAST 4 MESSAGE PER FINLAND'S RFSS 152.
C              THE NUMERICAL ARGUMENT GIVEN BY 'VALUE' IS NOW THE TERMINAL #, 
C              NOT THE MESSAGE NUMBER.
	IF(VALUE.LT.1.OR.VALUE.GT.X2X_TERMS) THEN           !V03
	  WRITE(CLIN23,9924)                                !V03
	  GOTO 100                                          !V03
	ENDIF                                               !V03
        X2FLDINF(XTERIDX) = VALUE                           !V03

	CBUF(1)=5
	CBUF(2)=BROAD_4                                     !V03
	CBUF(3)=TCSPE
	CBUF(4)=0
	CBUF(5)=X2FLDINF(XTERIDX)
CV03	WRITE(CLIN23,9070) VALUE,X2FLDINF(XTERIDX)
	WRITE(CLIN23,9070) BROAD_4,X2FLDINF(XTERIDX)        !V03
	GOTO 90
C
C SEND A PORT STATISTICS REQUEST.                           !V03
C                                                           !V03
65	CONTINUE                                            !V03
	IF(STN.LE.0.OR.STN.GT.X2X_STATIONS) THEN            !V03
	  WRITE(CLIN23,9924)                                !V03
	  GOTO 100                                          !V03
	ENDIF                                               !V03
	CBUF(1)=4                                           !V03
	CBUF(2)=STN                                         !V03
	CBUF(3)=TCX2X                                       !V03
	CBUF(8)=TER                                         !V03
	WRITE(CLIN23,9250) STN                              !V03
	GOTO 90                                             !V03
C                                                           !V03
C SEND A SOFT RESET TO THE STATION.                         !V03
C                                                           !V03
70	CONTINUE                                            !V03
	IF(STN.LE.0.OR.STN.GT.X2X_STATIONS) THEN            !V03
	  WRITE(CLIN23,9926)                                !V03
	  GOTO 100                                          !V03
	ENDIF                                               !V03
	CBUF(1)=2                                           !V03
	CBUF(2)=STN                                         !V03
	CBUF(3)=TCX2X                                       !V03
	CBUF(8)=TER                                         !V03
	WRITE(CLIN23,9230) STN                              !V03
	GOTO 90                                             !V03
C                                                           !V03
C SEND A HARD RESET TO THE STATION.                         !V03
C                                                           !V03
80	CONTINUE                                            !V03
	IF(STN.LE.0.OR.STN.GT.X2X_STATIONS) THEN            !V03
	  WRITE(CLIN23,9926)                                !V03
	  GOTO 100                                          !V03
	ENDIF                                               !V03
	CBUF(1)=5                                           !V03
	CBUF(2)=STN                                         !V03
	CBUF(3)=TCX2X                                       !V03
	CBUF(8)=TER                                         !V03
	WRITE(CLIN23,9232) STN                              !V03
	GOTO 90                                             !V03
C                                                           !V03
C QUEUE COMMAND BUFFER TO SYSTEM INPUT QUEUE
C
90	CONTINUE
	CBUF(6)=IDNUM
	CALL VISCMD(CBUF,ST)
	CALL XWAIT(2,1,ST)
C
C IF IT IS TIME TO REFRESH THE SCREEN, SET THE LOADED
C FLAG.
C
100	CONTINUE
	IF(LSTTIM+DELAY.LT.P(ACTTIM) .OR.
     *	   X2FLDINF(XUPDIDX).NE.0) THEN
	  LOADED=.FALSE.
	  X2FLDINF(XSRTIDX)=LSTSRT
	  X2FLDINF(XUPDIDX)=0
	  LSTSTN=0
	ENDIF
C
C IF A DIFFERENT STATION NUMBER FORCE A REFRESH.
C
	IF(STN.NE.LSTSTN) THEN
	  LOADED=.FALSE.
	  X2FLDINF(XSRTIDX)=LSTSRT
	  LSTSTN=0
	  DO 110 I=7,22
	    WRITE(XNEW(  I),9060)
110	  CONTINUE
	ENDIF
C
C LOAD THE DISPLAY TABLE.
C
	IF(.NOT.LOADED) THEN
	  DISCNT=0
	  DO 120 PORT=1,X2X_MAXPORT
	    DO 200 TER=1,X2X_MAXTERMS
	      TERM=X2XS_TERMS(TER,PORT,STN)
	      IF(TERM.NE.0) THEN
	        DISCNT=DISCNT+1
	        DISTBL(PRTIDX,DISCNT)=PORT
	        DISTBL(TERIDX,DISCNT)=TERM
	        CALL ILBYTE(STATE,IX2XT_STATE,TERM-1)            !V03
	        DISTBL(STSIDX,DISCNT)=STATE                      !V03
	        DISTBL(AGTIDX,DISCNT)=AGTTAB(AGTNUM,TERM)
	        DISTBL(TIMIDX,DISCNT)=X2XT_TIME(TERM)
	        C2DROP=X2XT_DROP_AD(TERM)
	        DISTBL(DRPIDX,DISCNT)=DROP
	      ENDIF
200	    CONTINUE
120	  CONTINUE
	  LOADED=.TRUE.
	  DISPLAY=.TRUE.
C
C CALCULATE TIME LAST UPDATED.
C
	  LSTTIM=P(ACTTIM)
	  LSTSEC=LSTTIM
	  LSTHR=LSTSEC/3600
	  LSTMIN=(LSTSEC-LSTHR*3600)/60
	  LSTSEC=LSTSEC-(LSTHR*3600+LSTMIN*60)
	ENDIF
C
C SORT THE DISPLAY TABLE ACCORDING TO DESIRED INFORMATION.
C DEFAULT TO SAP SORT.
C
	IF(X2FLDINF(XSRTIDX).NE.0) THEN
	  IF(X2FLDINF(XSRTIDX).EQ.TERSRT) THEN
	    KEY1=TERIDX
	    KEY2=0
	    KEY3=0
CV03	  ELSE IF(X2FLDINF(XSRTIDX).EQ.AGTSRT) THEN
CV03	    KEY1=AGTIDX
CV03	    KEY2=0
CV03	    KEY3=0
	  ELSE IF(X2FLDINF(XSRTIDX).EQ.STSSRT) THEN                 !V03
	    KEY1=STSIDX                                             !V03
	    KEY2=TERIDX                                             !V03
	    KEY3=0                                                  !V03
CV03	  ELSE IF(X2FLDINF(XSRTIDX).EQ.TIMSRT) THEN
CV03	    KEY1=TIMIDX
CV03	    KEY2=PRTIDX
CV03	    KEY3=DRPIDX
	  ELSE
	    KEY1=PRTIDX
	    KEY2=DRPIDX
	    KEY3=0
	  ENDIF
C V03 - TO AVOID SUBSCRIPT OUT OF RANGE PROBLEMS, ONLY CALL THE SORTING
C       SUBROUTINE WHEN WE HAVE MULTIPLE RECORDS TO SORT.
          IF (DISCNT .GT. 1) CALL I4XSORT(DISTBL,DISDIM,DISCNT,
     *                                    KEY1,KEY2,KEY3)
	  LSTSRT=X2FLDINF(XSRTIDX)
	  X2FLDINF(XSRTIDX)=0
	  DISPLAY=.TRUE.
	ENDIF
C
C DISPLAY STARTING AT THE BOTTOM INDEX.
C
	IF(X2FLDINF(XBOTIDX).NE.0) THEN
	  BEGIDX=MAX0(1,DISCNT-MAXDISP)
	  X2FLDINF(XBOTIDX)=0
	  DISPLAY=.TRUE.
C
C DISPLAY STARTING AT THE TOP.
C
	ELSE IF(X2FLDINF(XTOPIDX).NE.0) THEN
	  BEGIDX=1
	  X2FLDINF(XTOPIDX)=0
	  DISPLAY=.TRUE.
C
C DISPLAY FORWARD THE SPECIFIED NUMBER OF SCREENS.
C
	ELSE IF(X2FLDINF(XFORIDX).NE.0) THEN
	  BEGIDX=MIN0(DISCNT,CURIDX+(MAXDISP*X2FLDINF(XFORIDX)))
	  X2FLDINF(XFORIDX)=0
	  DISPLAY=.TRUE.
C
C DISPLAY BACKWARDS THE SPECIFIED NUMBER OF SCREENS.
C
	ELSE IF(X2FLDINF(XBAKIDX).NE.0) THEN
	  BEGIDX=MAX0(1,CURIDX-(MAXDISP*X2FLDINF(XBAKIDX)))
	  X2FLDINF(XBAKIDX)=0
	  DISPLAY=.TRUE.
C
C DISPLAY STARTING AT THE INPUT TERMINAL NUMBER.
C
	ELSE IF(X2FLDINF(XTERIDX).NE.LSTTER) THEN
	  DO 300 I=1,DISCNT
	    IF(DISTBL(TERIDX,I).EQ.X2FLDINF(XTERIDX)) THEN
	      BEGIDX=I
	      GOTO 310
	    ENDIF
300	  CONTINUE
	  BEGIDX=CURIDX
310	  CONTINUE
	  DISPLAY=.TRUE.
	  LSTTER=X2FLDINF(XTERIDX)
C
C DISPLAY STARTING AT THE INPUT PORT NUMBER.
C
	ELSE IF(X2FLDINF(XPRTIDX).NE.LSTPRT) THEN
	  DO 400 I=1,DISCNT
	    IF(DISTBL(PRTIDX,I).EQ.X2FLDINF(XPRTIDX)) THEN
	      BEGIDX=I
	      GOTO 410
	    ENDIF
400	  CONTINUE
	  BEGIDX=CURIDX
410	  CONTINUE
	  DISPLAY=.TRUE.
	  LSTPRT=X2FLDINF(XPRTIDX)
	ENDIF
C
C CONVERT BCD ADDRESS TO ASCII.                                    !V03
C                                                                  !V03
	    ADR_LEN=X2XS_ADRESS_LEN(STN)                           !V03
	    CALL HTOA(CHRSTR,1,ADR_LEN,X2XS_ADRESS(1,STN),ERR)     !V03
C                                                                  !V03
C IF THE DISPLAY FLAG HAS BEEN SET, DISPLAY THE SCREEN
C STARTING AT THE CURRENT BEGINNING POSITION.
C
	IF(DISPLAY) THEN
	  WRITE(CLIN1,9000) STN
	  WRITE(CLIN2,9060)                                        !V03
	  WRITE(CLIN3,9010) STN,                                   !V03
     *	           (CHRSTR(J),J=1,ADR_LEN),(BLANK(J),J=ADR_LEN+1,10), !V03
     *	            X2XS_ERR_CNT(STN),                             !V03
     *		    X2XS_ACK_CNT(STN),                             !V03
     *	            X2XS_LAST_ERR_CODE(STN)                        !V03
	  WRITE(CLIN4,9020)
	  WRITE(CLIN5,9030)
	  WRITE(CLIN6,9040)
	  LINIDX=7
C
C	OPEN AGENT INFO FILE TO EXTRACT NAME AND CITY              !V03
	  CALL OPENW(1,SFNAMES(1,ASF),0,0,0,ST)                    !V03
	  CALL IOINIT(FDB,1,ASFSEC*256)                            !V03
	  IF(ST.NE.0) THEN                                         !V03
	     CALL USRCLOS1(     1)                                 !V03
	     WRITE(CLIN23,1123) (SFNAMES(J,ASF),J=1,5),ST          !V03
	     RETURN                                                !V03
	  ENDIF                                                    !V03
                                                                   !V03
	  DO 1000 I=MAX0(1,BEGIDX),MIN0(DISCNT,BEGIDX+MAXDISP)
	    PRT=DISTBL(PRTIDX,I)
	    TER=DISTBL(TERIDX,I)
	    DROP=DISTBL(DRPIDX,I)
	    STATE=DISTBL(STSIDX,I)                                 !V03
C
C CALCULATE TIME AND GET TERMINAL STATE.
C
	    SEC=X2XT_TIME(TER)
	    HR=SEC/3600
	    MIN=(SEC-HR*3600)/60
	    SEC=SEC-(HR*3600+MIN*60)

C READ AGENT'S RECORD                                              !V03
	    ALLON=.FALSE.                                          !V03
	    CLERKON=AGTHTB(AGTPASOFF,TER)                          !V03
	    CALL READW(FDB,TER,ASFREC,ST)                          !V03
	    IF(ST.NE.0) THEN                                       !V03
	       CALL USRCLOS1(     1)                               !V03
	       WRITE(CLIN23,1223) (SFNAMES(J,ASF),J=1,5),ST,TER    !V03
	       RETURN                                              !V03
	    ENDIF                                                  !V03
                                                                   !V03
  	    DO J=1,512                                             !V03
	         IF(ASFBYT(J).EQ.CZERO) ASFBYT(J)=' '              !V03
            END DO                                                 !V03
CV03	    CALL ILBYTE(STATE,IX2XT_STATE,TER-1)
C
C OUTPUT INFORMATION.
C
C....TEMP SOLUTION FOR CONN ID
C
            CONNID = ISHFT(X2XT_CONN_ID(TER),-8)
	    WRITE(XNEW(  LINIDX),9050) PRT, C2DROP, TER,
     *	      TERSTATE(STATE),                                     !V03
     *	      HR,MIN,SEC,                                          !V03
     *	      (ASFBYT(J),J=SNAME,SNAME+NAME_WIDTH-1),              !V03
     *        (ASFBYT(J),J=SCITY,ECITY),                           !V03
     *         X2XT_DELAY(TER),                                    !V03
     *	       CONNID,
     *	      X2XT_NETWORK_DELAY(TER)
	    LINIDX=LINIDX+1
1000	  CONTINUE
	  CALL USRCLOS1(     1)       !CLOSE AGT FILE WHEN WE'RE ALL DONE -
                                      !V03
C
C CLEAR ANY UNUSED SLOTS.
C
	  DO 1020 I=LINIDX,22
	    WRITE(XNEW(  I),9060)
1020	  CONTINUE
	  CURIDX=BEGIDX
	  LSTSTN=STN
	ENDIF
C
C PROGRAM EXIT.
C
	RETURN
C
C     ===================== Format Statements ===================
C
9000	FORMAT('Agent Port Snapshot for Station ',I4)                 !V03
9005	FORMAT(T65,'Update:',I2.2,':',I2.2,':',I2.2)                  !V03
9010    FORMAT(T3,'Station',I5,T16,'Stn Adr ',10A1,                   !V03
     *	       T35,'ErrCnt ',T42,I5,T48,'AckCnt',T55,I5,              !V03
     *         T61,'LastErrcde',T73,Z8)                               !V03
9020	FORMAT(T2,'(1)',T13,'(2)',T20,'(3)',T27,'Last',               !V03
     *	       T34,'Agents abbr.',                                    !V03
     *	       T56,'Delay in',T66,'Connect',T74,'Net')                !V03
9030	FORMAT(T2,'Port',T7,'Drp',T12,'Term',T19,'State',             !V03
     *       T27,'Time',T34,'Name',T50,'City',                        !V03
     *	     T56,'Central',T66,'Id',T74,'Delay')                      !V03
9040	FORMAT(T2,77('='))                                            !V03
9050	FORMAT(T3,I2,T8,A2,T11,I5,T17,A7,T25,I2.2,':',I2.2,':',       !V03
     *	       I2.2,T34,<NAME_WIDTH>A1,T50,<LCITY>A1,T57,I5,T63,Z8,   !V03
     *         T73,I6)                                                !V03
9060	FORMAT(80(' '))
9070	FORMAT(' Message ',I4,' queued to terminal ',I5)
9923	FORMAT('Input error ')
9924	FORMAT('Value error  ')
1123	FORMAT(5A4,' open error ',I4)                                 !V03
1223	FORMAT(5A4,' read error ',I4,' record ',I4)                   !V03
9230	FORMAT(1X,'Soft Reset command sent to station ',I5)           !V03
9232	FORMAT(1X,'Hard Reset command sent to station ',I5)           !V03
9250	FORMAT(1X,'Statistics request send to station ',I5)           !V03
9926	FORMAT('This terminal is not on the X2X network ')            !V03
	END
