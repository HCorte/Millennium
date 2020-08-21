C
C SUBROUTINE X2STNSNP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2STNSNP.FOV                                 $
C  $Date::   17 May 1996 11:45:42                                         $
C  $Revision::   1.1                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - vis_x2stnsnp.for **
C
C X2STNSNP.FOR
C
C V15 07-SEP-95 DAS fixed problem with STNDIS
C V15 07-SEP-95 DAS fixed problem with drop display
C V14 05-aug-95 DAS Extended addressing                     
C V13 12-DEC-94 DAS Integrate UK changes into X2X Baseline
C V12 15-NOV-94 GPR DISPLAY THE STATION TYPE IN ASCII
C V11 03-NOV-94 GPR DISPLAY THE NET AND DEF PORTS FOR X25 STATIONS AND
C		    NUMBER OF MESSAGES FLUSHED
C V10 06-OCT-94 SCD ADD GVTID AND SATELLINE IDU COMMANDS
C V09 22-AUG-94 GPR USE DATA FROM COMMONS
C V08 18-AUG-94 GPR HANDLE 12 CHAR ADDRESS FOR UK
C V07 21-JUL-94 WS  MULTINETWORK CHANGES
C V06 10-MAY-94 GPR Clean up the station snapshot
C V05  8-MAR-94 JWE Add broadcast server code
C V04 13-JUL-92 NJA FIXED PROBLEM WITH OVERFLOWS.
C V03 02-DEC-91 RRB CHANGE AGENT TO RETAILER FOR LOUISIANA
C V02 27-NOV-91 RRB DESIGNATE REQUIRED COMMAND ENTRY FOR CHANGEABLE
C                   PARAMETERS WITH CAPITAL LETTERS.
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This subroutine will display the individual station
C detail for the X2X network system.  This snapshot
C has the capability of displaying a window of information
C at the bottom on the screen based on input desired
C information.  This information will have been previously
C loaded by X2LINTER.
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
	SUBROUTINE X2STNSNP(CMDLIN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'					!V08
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2VIS.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
	INCLUDE 'INCLIB:X2XSCL.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'					!V06
C
	INTEGER*4   CMDLIN(20)                  !Input command line
	INTEGER*4   CBUF(CDLEN)                 !Command buffer
	INTEGER*4   LSTSTN,LSTPRT,LSTDRP        !Previous parameters
	INTEGER*4   STN,TER,PORT,POS            !Array indices
	INTEGER*4   STATE                       !Stn/ter state
	INTEGER*4   LASTSAP                     !Last SAP USED
	INTEGER*4   CONFIG                      !Stn configuration
	INTEGER*4   HR,MIN,SEC                  !Time variables
	INTEGER*4   TERMS(X2X_MAXTERMS)         !Terminals displayed
	INTEGER*4   ACTERM                      !Function
	INTEGER*4   CMDVALUE(4)                 !Command value
	INTEGER*4   AN                          !Temporary agt #
	INTEGER*4   DWNTER, LINIDX, HISER, DIALENA, ATRIB, FDIS
	INTEGER*4   SDIS, AUTOUPD, NETSTAT, VALUE, PRTCNT, TERCNT
	INTEGER*4   FIRSTDROP, FIRSTPORT, MSTENA, DELACK, ERRREP
	INTEGER*4   STSTATE, ERR, ADR_LEN, ST, KEYNUM, I, J		  !V11
	INTEGER*4   POLL_FLAG, STATION_TYPE
	INTEGER*4   CLASS			!V07
        INTEGER*4   NETPORTS(X2X_MAXPRT_ASSIGN)	!Network ports to use	  !V11
        INTEGER*4   NUMPORTS                    !Number of network ports  !V11
        INTEGER*4   DEFPORT						  !V11
        INTEGER*4   DEFPORTCNT						  !V11
        INTEGER*4   START_DROP(5)/ 1,31,63, 95,127/
        INTEGER*4   END_DROP(5)  /30,62,94,126,157/
        INTEGER*4   DROP_GROUP/1/,FIRST_DROP/1/,LAST_DROP/30/
        INTEGER*4   TERM_ID
C
	REAL*8      CMDOPT(18)                  !Command line options	!V09
	CHARACTER   DROPS*(X2X_MAXTERMS)        !Drops displayed
        CHARACTER*2 C2DROP
        CHARACTER*1 EXT_CHAR
        CHARACTER*1 EXTENDED(5)/' ','%','#','$','!'/
	CHARACTER   CHRSTR(16)*1                !ASCII address
	CHARACTER   YESNO(0:1)*8                !Yes/no array
	CHARACTER   STNSTATE(0:3)*8             !Stn state description
	CHARACTER   TERSTATE(0:5)*8             !Ter state description
	CHARACTER   STNTYPE(0:10)*8             !Station type
	CHARACTER   DBGFLAG(0:3)*8              !Debug print flag
	CHARACTER   FEDIS(0:4)*8                !Front end disconnect
	CHARACTER   STNDIS(0:15)*8              !Station disconnect
	CHARACTER   POLLSTS(0:1)*8              !Poll status
	CHARACTER   TYPE_OF_STN(0:4)*8		!Station Type		   !V12
	CHARACTER   TMPLIN*80			!Output line
	CHARACTER   BCST1_CFG*3,BCST1_ACT*3     !BCST1 conf and bit-flag   V05
	CHARACTER   BCST2_CFG*3,BCST2_ACT*3     !BCST2 conf and bit-flag   !V06
	LOGICAL     DISPLAY /.FALSE./           !Display station flag
	LOGICAL     DISWIN  /.FALSE./           !Display window flag
C
	DATA        YESNO       /'     yes','      no'/
	DATA        STNSTATE    /'inactive','    idle',
     *	                         '    init','disabled'/
	DATA        TERSTATE    /'undefine',' defined',
     *	                         '  active','slowpoll',
     *	                         'disabled','waitresp'/
	DATA        STNTYPE     /'unknown ','    X.21','X.25 SVC',
     *                           'X.25 PVC','X.28 PAD','X.25 FSL',
     *                           'ASYN PVC','        ','        ',
     *                           'GT DIAL ','USAT PVC'/
	DATA        DBGFLAG     /'Invalid ','No print',
     *	                         'Part prt','All prnt'/
	DATA        CMDOPT      /'STATE   ','DEbug   ',
     *	                         'DELack  ','ERRrep  ',
     *	                         'TERstate','SOFtres ',
     *	                         'STNCON  ','PRTStats',
     *	                         'HARdres ','STNDis  ',
     *	                         'FEDis   ','XNETstat',
     *	                         'AUTOstat','LSTCall ',
     *				 'POlling ','X25Stats',
     *				 'BCSTstat','EXTended'/
	DATA        FEDIS  /'Response',' Outcall',' Unc Out',
     *	                    'Fast Sel','Conn Acc'/
	DATA        STNDIS /'Def Mode','Invalid ','Invalid ',
     *	                    'Invalid ','Invalid ','Invalid ',
     *	                    'Invalid ','Unc Disc','Immd Dis',
     *	                    'Time * 1','Time * 2','Time * 3',
     *	                    'Time * 4','Time * 5','Time * 6',
     *                      'Rem Cnct'/
	DATA        POLLSTS     /' Enabled','Disabled'/ 
	DATA        TYPE_OF_STN /' Regular',2*' Invalid',		!V12
     *				 '    BCST',' Invalid'/			!V12
	DATA        TER     / 0 /
	DATA        LSTDRP  / 0 /
	DATA        LSTPRT  / 0 /
	DATA        LSTSTN  / 0 /
	DATA        LSTTER  / 0 /
C
C INITIALIZE VARIABLES.
C
	DISPLAY=.TRUE.          !ALWAYS DISPLAY
	DISWIN =.TRUE.
	STN=X2FLDINF(XSTNIDX)
	FIRSTPORT=0
	FIRSTDROP=0
	TERCNT=0
	PRTCNT=0
	TERCNT=0
	VALUE=0
	POS=1
	POLL_FLAG = 0
	IF(STN.LE.0 .OR. STN.GT.X2X_STATIONS) STN=1
	DO 20 I=1,4
	  CMDVALUE(I)=-1
20	CONTINUE
	CLASS=X2XS_STNCLS(STN)					      !V09
C
C IF AN X2X COMMAND HAS BEEN ENTERED, DO NOT
C CHECK FOR OTHER COMMANDS.
C
	IF(X2SCRN(X2SCRN_INPLEV1,X2SCRN_STNSNP).NE.0 .OR.
     *	   X2SCRN(X2SCRN_INPLEV2,X2SCRN_STNSNP).NE.0 .OR.
     *	   X2SCRN(X2SCRN_INPLEV3,X2SCRN_STNSNP).NE.0) THEN
	  X2SCRN(X2SCRN_INPLEV1,X2SCRN_STNSNP)=0
	  X2SCRN(X2SCRN_INPLEV2,X2SCRN_STNSNP)=0
	  X2SCRN(X2SCRN_INPLEV3,X2SCRN_STNSNP)=0
	  GOTO 100
	ENDIF
C
C CHECK FOR INPUT COMMAND LINE.
C
	CALL KEY(CMDLIN,CMDOPT,18,POS,KEYNUM)				!V09
	IF(KEYNUM.EQ.0) GOTO 100
	IF(KEYNUM.NE.6 .AND. KEYNUM.NE.7 .AND.
     *	   KEYNUM.NE.8 .AND. KEYNUM.NE.9 .AND.
     *	   KEYNUM.NE.14 .AND. KEYNUM.NE.16 .AND.		        !V09
     *     KEYNUM.NE.17 .AND. KEYNUM.NE.18) THEN						!v09
	  CALL NUMB(CMDLIN,POS,VALUE)             !GET VALUE
	  IF(VALUE.LT.0) THEN                     !VALUE ERROR
	    WRITE(CLIN23,9924)
	    GOTO 8000
	  ENDIF
	ENDIF
	GOTO (50,52,54,56,58,60,62,64,66,68,70,72,74,78,80,
	1     82,84,85) KEYNUM						!V09
C
C STATION STATUS CHANGE.
C
50	CONTINUE
	IF(VALUE.LT.X2XS_NOT_ACTIVE.OR.VALUE.GT.X2XS_DISABLED) THEN
	  WRITE(CLIN23,9924)
	  GOTO 8000
	ENDIF
	CMDVALUE(1)=VALUE
	CALL X2BLDCMD(1,XSTN,STN,13,CMDVALUE,2,1,CBUF)			!V09
	GOTO 90
C
C DEBUG FLAG CHANGE.
C
52	CONTINUE
	IF(VALUE.LT.X2X_NONE_PRINT.OR.VALUE.GT.X2X_ALL_PRINT) THEN
	  WRITE(CLIN23,9924)
	  GOTO 8000
	ENDIF
	CMDVALUE(1)=VALUE
	CALL X2BLDCMD(1,XSTN,STN,23,CMDVALUE,2,1,CBUF)			!V09
	GOTO 90
C
C DELIVERY ACK CHANGE.
C
54	CONTINUE
	IF(VALUE.LT.0.OR.VALUE.GT.1) THEN
	  WRITE(CLIN23,9924)
	  GOTO 8000
	ENDIF
	CMDVALUE(1)=VALUE
	CALL X2BLDCMD(1,XSTN,STN,7,CMDVALUE,2,1,CBUF)			!V09
	GOTO 90
C
C ERROR REPORTING CHANGE.
C
56	CONTINUE
	IF(VALUE.LT.0.OR.VALUE.GT.1) THEN
	  WRITE(CLIN23,9924)
	  GOTO 8000
	ENDIF
	CMDVALUE(1)=VALUE
	CALL X2BLDCMD(1,XSTN,STN,8,CMDVALUE,2,1,CBUF)			!V09
	GOTO 90
C
C TERMINAL STATUS CHANGE.
C
58	CONTINUE
	IF(VALUE.LT.0.OR.VALUE.GT.4.OR.LSTTER.EQ.0) THEN
	  WRITE(CLIN23,9924)
	  GOTO 8000
	ENDIF
	CMDVALUE(1)=VALUE
	CALL X2BLDCMD(1,XTER,LSTTER,5,CMDVALUE,2,1,CBUF)
	GOTO 90
C
C SOFT RESET STATION COMMAND.
C
60	CONTINUE
	IF(STN.LE.0.OR.STN.GT.X2X_STATIONS) THEN
	  WRITE(CLIN23,9924)
	  GOTO 8000
	ENDIF
	CALL VALIDTER(STN,TER)
	IF(TER.LE.0) THEN
	  WRITE(CLIN23,9925)
	  GOTO 8000
	ENDIF
	CMDVALUE(1)=0
	CBUF(1)=2
	CBUF(2)=STN
	CBUF(3)=TCX2X
	CBUF(8)=TER
	WRITE(CLIN23,9230) STN
	GOTO 90
C
C CONNECTION INQUIRE COMMAND.
C
62	CONTINUE
	IF(STN.LE.0.OR.STN.GT.X2X_STATIONS) THEN
	  WRITE(CLIN23,9924)
	  GOTO 8000
	ENDIF
	CALL VALIDTER(STN,TER)
	IF(TER.LE.0) THEN
	  WRITE(CLIN23,9925)
	  GOTO 8000
	ENDIF
	CALL ILBYTE(LASTSAP,IX2XS_SAP,STN-1)
	IF(LASTSAP.EQ.0) THEN
	  WRITE(CLIN23,9260)
	  GOTO 8000
	ENDIF
	CBUF(1)=3
	CBUF(2)=STN
	CBUF(3)=TCX2X
	CBUF(8)=TER
	CBUF(10)=LASTSAP
	WRITE(CLIN23,9240) STN
	GOTO 90
C
C STATION STATISTICS REQUEST COMMAND.
C
64	CONTINUE
	IF(STN.LE.0.OR.STN.GT.X2X_STATIONS) THEN
	  WRITE(CLIN23,9924)
	  GOTO 8000
	ENDIF
	CALL VALIDTER(STN,TER)
	IF(TER.LE.0) THEN
	  WRITE(CLIN23,9925)
	  GOTO 8000
	ENDIF
	CMDVALUE(1)=0
	CBUF(1)=4
	CBUF(2)=STN
	CBUF(3)=TCX2X
	CBUF(8)=TER
	WRITE(CLIN23,9250) STN
	GOTO 90
C
C HARD RESET STATION COMMAND.
C
66	CONTINUE
	IF(STN.LE.0.OR.STN.GT.X2X_STATIONS) THEN
	  WRITE(CLIN23,9924)
	  GOTO 8000
	ENDIF
	CALL VALIDTER(STN,TER)
	IF(TER.LE.0) THEN
	  WRITE(CLIN23,9925)
	  GOTO 8000
	ENDIF
	CMDVALUE(1)=0
	CBUF(1)=5
	CBUF(2)=STN
	CBUF(3)=TCX2X
	CBUF(8)=TER
	WRITE(CLIN23,9232) STN
	GOTO 90
C
C STATION DISCONNECT CHANGE.
C
68	CONTINUE
	IF(VALUE.LT.0) THEN
	  WRITE(CLIN23,9924)
	  GOTO 8000
	ELSE IF(VALUE.NE.0) THEN
	  IF(VALUE.LT.7 .OR. VALUE.GT.15) THEN
	    WRITE(CLIN23,9924)
	    GOTO 8000
	  ENDIF
	ENDIF
	CMDVALUE(1)=VALUE
	CALL X2BLDCMD(1,XSTN,STN,9,CMDVALUE,2,1,CBUF)			!V09
	GOTO 90
C
C FRONT END DISCONNECT CHANGE.
C
70	CONTINUE
	IF(VALUE.LT.0 .OR. VALUE.GT.3) THEN
	  WRITE(CLIN23,9924)
	  GOTO 8000
	ENDIF
	CMDVALUE(1)=VALUE
	CALL X2BLDCMD(1,XSTN,STN,10,CMDVALUE,2,1,CBUF)			!V09
	GOTO 90
C
C NETWORK DELAY STATISTICS.
C
72	CONTINUE
	IF(VALUE.LT.0.OR.VALUE.GT.1) THEN
	  WRITE(CLIN23,9924)
	  GOTO 8000
	ENDIF
	CMDVALUE(1)=VALUE
	CALL X2BLDCMD(1,XSTN,STN,24,CMDVALUE,2,1,CBUF)			!V09
	GOTO 90
C
C AUTO UPDATE POLLING STATUS.
C
74	CONTINUE
	IF(VALUE.LT.0.OR.VALUE.GT.1) THEN
	  WRITE(CLIN23,9924)
	  GOTO 8000
	ENDIF
	CMDVALUE(1)=VALUE
	CALL X2BLDCMD(1,XSTN,STN,25,CMDVALUE,2,1,CBUF)			!V09
	GOTO 90
C
C DIAL ENABLE CHANGE.
C
C***76	CONTINUE
C***	IF(VALUE.LT.0.OR.VALUE.GT.1) THEN				!V09
C***	  WRITE(CLIN23,9924)						!V09
C***	  GOTO 8000							!V09
C***	ENDIF								!V09
C***	CMDVALUE(1)=VALUE						!V09
C***	CALL X2BLDCMD(1,XSTN,STN,37,CMDVALUE,2,1,CBUF)			!V09
C***	GOTO 90
C
C LAST 5 CALLS STATISTICS REQUEST.
C
78	CONTINUE
	IF(STN.LE.0.OR.STN.GT.X2X_STATIONS) THEN
	  WRITE(CLIN23,9924)
	  GOTO 8000
	ENDIF
	CALL VALIDTER(STN,TER)
	IF(TER.LE.0) THEN
	  WRITE(CLIN23,9925)
	  GOTO 8000
	ENDIF
	CMDVALUE(1)=0
	CBUF(1)=6
	CBUF(2)=STN
	CBUF(3)=TCX2X
	CBUF(8)=TER
	WRITE(CLIN23,9270) STN
	GOTO 90
C
C POLLING STATUS CHANGE
C
80 	CONTINUE
	IF(VALUE.LT.0.OR.VALUE.GT.1) THEN
	  WRITE(CLIN23,9924)
	  GOTO 8000
	ENDIF
	CMDVALUE(1)=VALUE
	CALL X2BLDCMD(1,XSTN,STN,33,CMDVALUE,2,1,CBUF)			!V09
	GOTO 90
C
C STATION X25 STATISTICS REQUEST COMMAND.                        V05
C
82    CONTINUE
      IF(STN.LE.0.OR.STN.GT.X2X_STATIONS) THEN
        WRITE(CLIN23,9924)
        GOTO 8000
      ENDIF
      CALL VALIDTER(STN,TER)
      IF(TER.LE.0) THEN
        WRITE(CLIN23,9925)
        GOTO 8000
      ENDIF
      CMDVALUE(1)=0
      CBUF(1)=9
      CBUF(2)=STN
      CBUF(3)=TCX2X
      CBUF(8)=TER
      WRITE(CLIN23,9280) STN
      GOTO 90
C
C STATION BCST STATISTICS REQUEST COMMAND.                            V05
C
84    CONTINUE
      IF(STN.LE.0.OR.STN.GT.X2X_STATIONS) THEN
        WRITE(CLIN23,9924)
        GOTO 8000
      ENDIF
      CALL VALIDTER(STN,TER)
      IF(TER.LE.0) THEN
        WRITE(CLIN23,9925)
        GOTO 8000
      ENDIF
      CMDVALUE(1)=0
      CBUF(1)=10
      CBUF(2)=STN
      CBUF(3)=TCX2X
      CBUF(8)=TER
      WRITE(CLIN23,9290) STN
      GOTO 90
C
C SWITCH EXTENDED ADDRESSING
C
85    CONTINUE
      DROP_GROUP = DROP_GROUP + 1 
      IF(END_DROP(DROP_GROUP).GT.X2X_MAXTERMS) DROP_GROUP=1
      FIRST_DROP = START_DROP(DROP_GROUP)
      LAST_DROP  = END_DROP(DROP_GROUP)
      GOTO 100      
C
C QUEUE COMMAND BUFFER TO SYSTEM INPUT QUEUE
C
90	CONTINUE
C***      IF( BX2XS_CONN_TYPE(STN) .NE. X2XST_BCST ) THEN
        IF( X2XS_TYPE(STN) .NE. X2XST_BCST ) THEN
          CBUF(6)=IDNUM
          CALL VISCMD(CBUF,ST)
        ELSE
          WRITE(CLIN23, 9237)
        ENDIF
	CALL XWAIT(2,1,ST)
C
C SET THE DISPLAY FLAG IF ANY OF THE INPUT PARAMETERS
C HAVE BEEN CHANGED.
C
100	CONTINUE
	IF(X2FLDINF(XSTNIDX).NE.LSTSTN) THEN
	  DISPLAY=.TRUE.
	  DO 150 I=19,22
	    WRITE(XNEW(  I),9200)
150	  CONTINUE
	  DO 152 I=1,X2X_MAXTERMS
	    TERMS(I)=0
	    DROPS(I:I)=' '
152	  CONTINUE
	  LSTDRP=0
	ENDIF
	IF(X2FLDINF(XPRTIDX).NE.LSTPRT) THEN
	  DISWIN=.TRUE.
	  LSTPRT=X2FLDINF(XPRTIDX)
	ENDIF
	IF(X2FLDINF(XDRPIDX).NE.LSTDRP) THEN
	  DISWIN=.TRUE.
	  LSTDRP=X2FLDINF(XDRPIDX)
	ENDIF
	IF (STN.EQ.0) THEN
	  DISPLAY=.TRUE.
	  STN=1
	ENDIF
C
C DISPLAY THE STATION PORTION OF THE SNAPSHOT.
C
	IF(DISPLAY) THEN
C
C LOAD INFORMATION.
C
C ***** Start V08 changes *****
C
	  IF(CLASS.LE.0) THEN						!V09
	    WRITE(CLIN23,9210) STN
	    GOTO 8000
	  ENDIF

C ***** Start V10 changes *****

	  IF (.NOT. ((BX2XS_CONN_TYPE(STN).EQ.X2XSCT_USAT_PVC) .OR. 
     *		     (BX2XS_CONN_TYPE(STN).EQ.X2XSCT_X28PAD) .OR.
     *		     (BX2XS_CONN_TYPE(STN).EQ.X2XSCT_GTECH_DIAL)) ) THEN
	     ADR_LEN=X2XS_ADRESS_LEN(STN)
	     IF(ADR_LEN.NE.0) THEN
	       CALL HTOA(CHRSTR,1,ADR_LEN,X2XS_ADRESS(1,STN),ERR)
	     ELSE
	       ADR_LEN=1
	       CHRSTR(1)=' '
	     ENDIF
	  ELSE						!DIPSLAY EVSN/GVTID
	     ADR_LEN=X2XS_EVSN_LEN(STN)
	     IF(ADR_LEN.NE.0) THEN
	       CALL HTOA(CHRSTR,1,ADR_LEN,X2XS_EVSN(1,STN),ERR)
	     ELSE
	       ADR_LEN=1
	       CHRSTR(1)=' '
	     ENDIF
	  ENDIF
C
C ***** End V10 changes *****
C
C ***** End V08 changes *****
C
	  CALL ILBYTE(STSTATE,IX2XS_STATE,STN-1)
	  IF(TSBIT(BX2XS_PARAM(STN),7)) POLL_FLAG = 1
	  SEC=X2XS_TIME(STN)
	  HR=SEC/3600
	  MIN=(SEC-HR*3600)/60
	  SEC=SEC-(HR*3600+MIN*60)
C
C GET THE DELIVERY ACK AND ERROR REPORTING FLAGS.
C
	  ERRREP=1
	  DELACK=1
	  MSTENA=1
	  NETSTAT=1
	  AUTOUPD=1
	  IF(TSBIT(IX2XS_DOWNFLAG,(STN-1)*8+1)) ERRREP=0
	  IF(TSBIT(IX2XS_DOWNFLAG,(STN-1)*8+2)) DELACK=0
	  IF(BX2XS_STATE(STN).EQ.0) MSTENA=0				  !V09
	  CALL ILBYTE(SDIS,IX2XS_STATION_DISCONNECT,STN-1)
	  CALL ILBYTE(FDIS,IX2XS_FE_DISCONNECT,STN-1)
	  CALL ILBYTE(ATRIB,IX2XS_ATRIBUTE,STN-1)
          CALL ILBYTE(DIALENA,BX2XC_DIAL_ENABLE,CLASS-1)		  !V09
C*****          CALL ILBYTE(CONN_TYPE,IX2XS_CONN_TYPE,STN-1)
	  STATION_TYPE=X2XS_TYPE(STN)
	  IF(IAND(ATRIB,X2XSA_STATS).NE.0) NETSTAT=0
	  IF(IAND(ATRIB,X2XSA_AUTO_STATS).NE.0) AUTOUPD=0
C
C DETERMINE THE LAST SERIAL NUMBER.
C
	  HISER=0
	  DO 160 PORT=1,X2X_MAXPORT
	    DO 162 I=1,X2X_MAXTERMS
	      TER=X2XS_TERMS(I,PORT,STN)
	      IF(TER.NE.0) THEN
	        CALL ILBYTE(STATE,IX2XT_STATE,TER-1)
	        IF(AGTTAB(ALSTRA,TER).GT.HISER)
     *	          HISER=AGTTAB(ALSTRA,TER)
	      ENDIF
162	    CONTINUE
160	  CONTINUE
C
C DISPLAY STATION IDENTIFICATION
C
	  WRITE(CLIN1,9000)
	  WRITE(CLIN2,9940)						  !V06
	  IF (BX2XS_CONN_TYPE(STN).EQ.X2XSCT_USAT_PVC) THEN		  !V10
     	     WRITE(CLIN2,9012) STN, (CHRSTR(I),I=MAX0(ADR_LEN-LGSER+1,1), !V10
     * 		            ADR_LEN),HISER				  !V10
	  ELSEIF ((BX2XS_CONN_TYPE(STN).EQ.X2XSCT_X28PAD).OR.
     *		  (BX2XS_CONN_TYPE(STN).EQ.X2XSCT_GTECH_DIAL)) THEN	  !V10
     	     WRITE(CLIN2,9011) STN, (CHRSTR(I),I=MAX0(ADR_LEN-LGSER+1,1), !V10
     *		            ADR_LEN),HISER				  !V10
	  ELSE
	     WRITE(CLIN2,9010) STN, (CHRSTR(I),I=MAX0(ADR_LEN-LXADR+1,1), !V08
     *		            ADR_LEN),HISER				  !V08
	  ENDIF
	  WRITE(CLIN3,9020) STNTYPE(BX2XS_CONN_TYPE(STN)),STNSTATE(STSTATE),!V09
     *	                    DBGFLAG(X2X_STN_PRINT(STN))			  !V06
	  WRITE(CLIN4,9030) YESNO(DELACK),YESNO(ERRREP),
     *                      POLLSTS(POLL_FLAG)				  !V06
	  WRITE(CLIN5,9040) X2XS_GROUP(STN), X2XS_PVC(STN),
     *	                    X2XS_PHYS(STN)				  !V06
	  WRITE(CLIN6,9050) STNDIS(SDIS), FEDIS(FDIS), YESNO(NETSTAT)	  !V06
	  WRITE(CLIN7,9052) YESNO(AUTOUPD),YESNO(DIALENA),		  !V12
     *			    TYPE_OF_STN(STATION_TYPE)			  !V12
C
C DISPLAY STATION CLASS INFORMATION.
C
	  WRITE(CLIN9,9060)  CLASS,X2XC_DESC(CLASS)			!V09
	  WRITE(CLIN10,9070) X2XC_INTIM(CLASS),X2XC_OUTTIM(CLASS),	!V09
     *			     X2XC_RESTIM(CLASS)
	  WRITE(CLIN11,9080) X2XC_INTTIM(CLASS),X2XC_STSTIM(CLASS),	!V09
     *	                     YESNO(MSTENA)
	  WRITE(CLIN12,9090) X2XC_RETRY(CLASS),X2XC_ABS_TIM(CLASS),	!V09
     *			     X2XC_SUBNETWORK(CLASS)			!V09
C
C DISPLAY STATION STATISTICS.
C
C
C DISPLAY STATION STATISTICS.
C
        BCST1_CFG = ' no'                                             !V05
        IF((X2XS_BCST_NUM(STN) .GT. 0) .AND.
     *	   (X2XC_BCST_ENABLE1(CLASS) .EQ. 0)) BCST1_CFG = 'yes'       !V07
        BCST1_ACT = ' no'
        IF(IAND(X2XS_BCST_ACTIVE_BITMAP(STN),
     *	        X2STMES_STSBCST_CONN1_MASK).NE.0) BCST1_ACT = 'yes'   !V06
C***                    Note: BCST1 uses X2STMES_STSBCST_CONN1_MASK
        BCST2_CFG = ' no'                                             !V06
        IF((X2XS_BCST_NUM(STN) .GT. 0) .AND.
     *	   (X2XC_BCST_ENABLE2(CLASS) .EQ. 0)) BCST2_CFG = 'yes'  !V07
        BCST2_ACT = ' no'					      !V06
        IF(IAND(X2XS_BCST_ACTIVE_BITMAP(STN),
     *	        X2STMES_STSBCST_CONN2_MASK).NE.0) BCST2_ACT = 'yes'   !V06
        WRITE(CLIN8,9055) BCST1_CFG,BCST2_CFG,BCST1_ACT,BCST2_ACT,    !V11
     *			  X2XS_TIMES_MESSAGE_FLUSHED(STN)	      !V11
	WRITE(CLIN13,9100)					      !V06
	  WRITE(CLIN14,9110) X2XS_STATS_INDEX(STN),
     *	                     X2XS_CNT_ACTIVE(STN), HR,MIN,SEC
C
C CALCULATE LAST INDEX TIME.
C
	  SEC=X2XS_ALARM_TIME(STN)
	  HR=SEC/3600
	  MIN=(SEC-HR*3600)/60
	  SEC=SEC-(HR*3600+MIN*60)
	  CALL ILBYTE(LASTSAP,IX2XS_SAP,STN-1)
	  CALL ILBYTE(CONFIG,IX2XS_CONF,STN-1)
C
	  WRITE(CLIN15,9120) LASTSAP, X2XS_ALARM_INDEX(STN),
     *	                     HR,MIN,SEC
	  WRITE(CLIN16,9130) X2XS_ACK_CNT(STN),
     *	                     X2XS_ERR_CNT(STN),
     *	                     X2XS_CONN_ID(STN)
	  WRITE(CLIN17,9140) X2XS_RESET_CNT(STN),
     *	                     X2XS_LAST_ERR_CODE(STN),
     *	                     X2XS_DISC_CNT(STN)	      !!!!CONFIG
C
C DISPLAY PORT INFORMATION.
C
	  LINIDX=19
	  POS=0
	  PRTCNT=0
          DO 300 PORT=1,X2X_MAXPORT
            IF(X2XS_NUM_TERMS(PORT,STN).NE.0) THEN
              POS=POS+1
              PRTCNT=PRTCNT+1
              IF(PRTCNT.EQ.1) FIRSTPORT=PORT
            ENDIF
300	  CONTINUE
          WRITE(CLIN18,9150) 
C
C	  ***** Start V11 changes *****
C
	  IF(BX2XS_CONN_TYPE(STN).EQ.X2XSCT_X25SVC) THEN
	    NUMPORTS=0
	    CALL X2NETPRT(STN,NUMPORTS,NETPORTS)
            WRITE(XNEW(LINIDX),9176) (I,I=1,X2X_MAXPORT),
     *		 (ABS(NETPORTS(J)),J=1,MAX0(1,NUMPORTS))		  !V13
            LINIDX = LINIDX + 1
            IF (CLASS.NE.0) THEN
              DEFPORT=X2XC_DEF_PORT_OVERRIDE(CLASS)
            ELSE
              DEFPORT=0
            ENDIF
            IF (DEFPORT.EQ.0) THEN
              DEFPORTCNT=0
              DO J=1,X2XS_MAXDEF
                IF (X2XS_DEF_PORT(J,STN).NE.0) THEN
                  DEFPORTCNT=DEFPORTCNT+1
                ENDIF
              ENDDO
              WRITE(XNEW(LINIDX),9178) 
     *		   (X2XS_NUM_TERMS(I,STN),I=1,X2X_MAXPORT),
     *		   (X2XS_DEF_PORT(J,STN),J=1,MAX0(1,DEFPORTCNT))	  !V13
            ELSE
              WRITE (XNEW(LINIDX),9179) 
     *		    (X2XS_NUM_TERMS(I,STN),I=1,X2X_MAXPORT),
     *		    DEFPORT
            ENDIF
	  ELSE
            WRITE(XNEW(LINIDX),9170) (I,I=1,X2X_MAXPORT)
            LINIDX = LINIDX + 1
            WRITE(XNEW(LINIDX),9172)
     *           (X2XS_NUM_TERMS(I,STN),I=1,X2X_MAXPORT)
	  ENDIF

C	  ***** Start V11 changes *****
C
          LINIDX = LINIDX + 1
          WRITE(XNEW(LINIDX),9174) (ACTERM(STN,I),I=1,X2X_MAXPORT)
          LINIDX = LINIDX + 1
C
	  IF(POS.NE.0) LINIDX=LINIDX+1
	  DO 400 I=LINIDX,22
	    WRITE(XNEW(  I),9200)
400	  CONTINUE
	ENDIF
C
C IF ONLY 1 PORT CONFIGURED DISPLAY IT IN THE WINDOW.
C
	PORT=X2FLDINF(XPRTIDX)
	IF(PRTCNT.EQ.1 .AND. PORT.EQ.0) THEN
	  PORT=FIRSTPORT
	  DISWIN=.TRUE.
	ENDIF
C
C GET THE DISPLAY PORT INPUT BY X2LINTER. IF STATION TYPE
C IS X25, ALWAYS DISPLAY THE PORT AND THE DROP DEFINED.
C
	IF(BX2XS_CONN_TYPE(STN).EQ.X2XSCT_X25SVC) THEN			!V09
	  TER=X2XS_TERMS(1,1,STN)
C         ter may be zero if x25 remote station
          IF(TER.NE.0) THEN
	    AN=AGTTAB(AGTNUM,TER)
	    WRITE(XNEW(  22),9950) TER,
     *	                         AN/10,MOD(AN,10), !break out agt #
     *	                         AGTTAB(ALSTRA,TER),
     *	                         X2XT_NETWORK_DELAY(TER)
          ENDIF  
C
C DISPLAY PORT INFORMATION IN WINDOW AT BOTTOM OF SCREEN.
C NOTE: IF TERMINAL IS NOT RESPONDING DISPLAY DROP IN LOWER CASE.
C
	ELSE IF(DISWIN.AND.PORT.NE.0) THEN
	  DWNTER=0
	  TERCNT=0
	  FIRSTDROP=0
	  DO 2000 I=FIRST_DROP,LAST_DROP
	    TERMS(I)=X2XS_TERMS(I,PORT,STN)
	    IF(TERMS(I).NE.0) THEN
              C2DROP = X2XT_DROP_AD(TERMS(I))
              CALL X2CNVDRP(C2DROP, TERM_ID)
              IF(TERM_ID.EQ. -1) THEN
                DROPS(I:I)=' '
              ELSEIF(TERM_ID.GT.0 .AND. TERM_ID.LE.30) THEN
	         DROPS(I:I)= C2DROP(1:1)
              ELSE 
                 DROPS(I:I)= C2DROP(2:2)
              ENDIF
	      TERCNT=TERCNT+1
	      IF(TERCNT.EQ.1) FIRSTDROP=I
C
              IF(DROPS(I:I) .NE. ' ') THEN
                CALL ILBYTE(STATE,IX2XT_STATE,TERMS(I)-1)
  	        IF(STATE.NE.X2XTS_ACTIVE) THEN
	           DROPS(I:I)=CHAR(ICHAR(DROPS(I:I))+32)
	           DWNTER=DWNTER+1
	        ENDIF
              ENDIF
	    ENDIF
2000	  CONTINUE
C
          EXT_CHAR = EXTENDED(DROP_GROUP)
C
	  IF(TERCNT.EQ.1) X2FLDINF(XDRPIDX)=
     *	    ICHAR(DROPS(FIRSTDROP:FIRSTDROP))-63
	  WRITE(XNEW(  22),9180) PORT,EXT_CHAR,
     *                           DROPS(FIRST_DROP:LAST_DROP), DWNTER
	  WRITE(TMPLIN,9180) PORT,EXT_CHAR,
     *                       DROPS(FIRST_DROP:LAST_DROP), DWNTER
C
C IF THE DROP IS SET IN X2VIS, DISPLAY TERMINAL STATS.
C
	  IF(X2FLDINF(XDRPIDX).NE.0) THEN
	    DO 2010 I=1,X2X_MAXTERMS
	      IF(X2FLDINF(XDRPIDX).EQ.ICHAR(DROPS(I:I))-63.OR.
     *	         X2FLDINF(XDRPIDX)+32.EQ.ICHAR(DROPS(I:I))-63) THEN
	        TER=TERMS(I)
	      ENDIF
2010	    CONTINUE
	    IF(TER.NE.0) THEN
	      CALL ILBYTE(STATE,IX2XT_STATE,TER-1)
	      WRITE(XNEW(  22),9190) TMPLIN,
     *                               TER, AGTTAB(AGTNUM,TER),
     *	                             X2XT_DROP_AD(TER),
     *	                             TERSTATE(STATE)
	    ENDIF
	    LSTTER=TER
	  ENDIF
	ENDIF
C
C PROGRAM EXIT.
C
8000	CONTINUE
	LSTSTN=STN
	RETURN
C
C     ================== Format Statements ===================
C
9000	FORMAT('Station snapshot')
9010	FORMAT(T2,'Station:',T18,I8,T29,'Addr:',			!V08
     *	       T<35+LXADR-MIN0(LXADR,ADR_LEN)>,			        !V08
     *	       <MIN0(LXADR,ADR_LEN)>A1,					!V08
     *         T53,'Last Serial:',T65,I9)
9011	FORMAT(T2,'Station:',T18,I8,T29,'GVT Id:',			!V10
     *	       T<39+LGSER-MIN0(LGSER,ADR_LEN)>,			        !V10
     *	       <MIN0(LGSER,ADR_LEN)>A1,					!V10
     *         T53,'Last Serial:',T65,I9)
9012	FORMAT(T2,'Station:',T18,I8,T29,'IDU:',				!V10
     *	       T<39+LGSER-MIN0(LGSER,ADR_LEN)>,			        !V10
     *	       <MIN0(LGSER,ADR_LEN)>A1,					!V10
     *         T53,'Last Serial:',T65,I9)
9020	FORMAT(T2,'Type: ',T18,A8,T28,'*STATE:',T43,A8,
     *	       T52,'*DEbug flag: ',T66,A8)
9030	FORMAT(T1,'*DEL ack:',T18,A8,T28,'*ERRor report',T43,A8,
     *         T52,'*POlling',T66,A8)
9040	FORMAT(T2,'Group:',T18,I8,T29,'PVC Channel: ',T43,I8,
     *	       T53,'PVC Port:',T66,I8)
9050	FORMAT(T1,'*STN Disc:',T18,A8,T28,'*FE Disc:',T43,A8,
     *	       T52,'*XNET stats',T66,A8)
9052	FORMAT(T1,'*AUTO poll upd',T18,A8,T28,' Dial enable:',T43,A8,
     *         T53,'Station Type:',T66,A8)			      !V12
9055	FORMAT(T2,'BCST Config: ',T19,A3,'/',A3,T29,'BCST Active: ',
     *	       T44,A3,'/',A3,T53,'Msgs Flushed:',T66,I8)	      !V11
9060	FORMAT(T2,19('='),' Station Class ',I3,1X,A12,1X,21('='))
9070	FORMAT(T2,'Incall:',T18,I8,T29,'Outcall:',T43,I8,
     *	       T53,'Response:',T66,I8)
9080	FORMAT(T2,'Trans:',T16,I10,T29,'Stats:',T43,I8,
     *	       T53,'Master Ena:',T66,A8)			      !V06
9090	FORMAT(T2,'Retry cnt:',T18,I8,T29,'Abs disc:',T43,I8,	      !V07
     *           T53,'Snet ',T66,I8)				      !V07
9100  FORMAT(T2,29('='),' Statistics ',31('='))			      !V06
9110	FORMAT(T2,'Last stats:',T18,I8,T29,'Activity cnt:',T43,I8,
     *	       T53,'Time active:',T66,I2.2,':',I2.2,':',I2.2)
9120	FORMAT(T2,'Last SAP:',T18,I8,T29,'Last Index:',T43,I8,
     *	       T53,'Last index:',T66,I2.2,':',I2.2,':',I2.2)
9130	FORMAT(T2,'Ack count:',T18,I8,T29,'Error cnt:',T43,I8,
     *	       T53,'Connect id:',T66,Z8)
9140	FORMAT(T2,'Reset count:',T18,I8,T29,'Last err code:',T43,Z8,
     *	       T53,'Disconnects:',T66,I8)
9150	FORMAT(T2,26('='),' Port Configuration ',26('='))
9170    FORMAT(' Port #  : ',16(I2,2X))
9172    FORMAT(' Terminal: ',16(I2,2X))
9174    FORMAT(' # Active: ',16(I2,2X))
9176    FORMAT(' Port #  : ',<X2X_MAXPORT>(I2,2X),T28,' Netports: ',	  !V11
     *	       <MAX0(1,NUMPORTS)>(I4,' '))				  !V13
9178    FORMAT(' Terminal: ',<X2X_MAXPORT>(I2,2X),T28,' Defports: ',	  !V11
     *	       <MAX0(1,DEFPORTCNT)>(I4,' '))				  !V13
9179    FORMAT(' Terminal: ',<X2X_MAXPORT>(I2,2X),T28,' Defports: ',I4)	  !V11
9180	FORMAT(1X,'Port ',I2,1X,'(',A1,')',1X,
     *         A32,1X,'Dwn= ',I4)
9190	FORMAT(A46,' ',T48,'Term ',I6,'/',I8,'/',A2,1X,A8)
9200	FORMAT(80(' '))
9210	FORMAT(1X,'Station ',I5,' does not exist')
9230	FORMAT(1X,'Soft Reset command sent to station ',I5)
9232	FORMAT(1X,'Hard Reset command sent to station ',I5)
9237  FORMAT(1X,'Can not send to Broadcast Server')
9240	FORMAT(1X,'Connection request sent to station ',I5)
9250	FORMAT(1X,'Statistics request sent to station ',I5)
9260	FORMAT(1X,'Station is not currently connected ')
9270	FORMAT(1X,'Last 5 calls request sent to station ',I5)
9280  FORMAT(1X,'X.25 Statistics request request sent to station ',I5)  ! V05
9290  FORMAT(1X,'BCST Statistics request request sent to station ',I5)  ! V05
9924	FORMAT('Value error  ')
9925	FORMAT('No valid terminals defined for this station ')
9940	FORMAT(80(' '))
9950	FORMAT(1X,'Term # ',I6,' Retailer # ',I8.8,'-',I1.1,' Lstser ',I10,
     *	          ' Net delay ',I5)
	END
