C GUI_034.FOR
C
C V02 23-MAY-2001 WPW FIX FOR DISPLAYING SEGMENT ETC. AND ACT/BCK LOAD ADDED
C V01 21-FEB-2001 HXK INITIAL RELEASE
C
C FUNCTION TO PROVIDE COMM DATA FOR A SPECIFIC TERMINAL
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
C Copyright 2001 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C This subroutine returns GUI FUNCTION.
C
C Input parameters:
C	NONE               
C
C Output parameters:
C
C	BYTE		OUTBUF(*)    OUTPUT MESSAGE
C	INTEGER*4	MES_LEN	     MESSAGE LENGTH
C	INTEGER*4	RET_CODE:
C		0		-  no error, message accepted;
C		value >= 11	-  error number to be sent to Client.
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUI_034(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:MSGCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        ! INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XSCL.DEF'
	INCLUDE 'INCLIB:X2VIS.DEF'
	INCLUDE 'INCLIB:REQCOM.DEF'

	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:GUIFIL.DEF'
C
C
	BYTE	    OUTBUF(*)
	INTEGER*4   MES_LEN,RET_CODE
C
	INTEGER*4   NUM_COLS,NUM_ROWS
C
	INTEGER*4   TEMP
C
	INTEGER*2   I2TEMP
	BYTE	    I1TEMP(2)
	EQUIVALENCE(I2TEMP,I1TEMP)
C
	INTEGER*4   ST
C
        INTEGER*4   RESET_CNT                    !         !V14
        INTEGER*4   ERR_CNT                      !         !V14
        INTEGER*4   ACK_CNT                      !         !V14
        INTEGER*4   CNT_ACTIVE                   !         !V14
	INTEGER*4   VCOMTYP
	INTEGER*4   ACTERM
	INTEGER*4   PORT
	INTEGER*4   ERROR
	INTEGER*4   ADR_LEN
	CHARACTER   DROP*2
        INTEGER*4   SSTATE                       !
        INTEGER*4   TSTATE                       !
        INTEGER*4   STNSEC                       !
        INTEGER*4   STNHR                        !
        INTEGER*4   STNMIN                       !
        INTEGER*4   TERMSEC                      !
        INTEGER*4   TERMHR                       !
        INTEGER*4   TERMMIN                      !
        INTEGER*4   ERRREP                       !
        INTEGER*4   DELACK                       !
        INTEGER*4   LSTERR                       !
        INTEGER*4   TOTCNF                       !
        INTEGER*4   TOTACT                       !
	INTEGER*4   STN
C
	INTEGER*4   ERR
C
	CHARACTER   CZERO
	INTEGER*4 SPACE/'    '/
C
        ! arguments
        INTEGER*4  TER                     !
C
        ! parameters
        INTEGER*4  I                       !
 
C X2X PARAMETERS.
C
        ! INTEGER*4   GVTVAL                      ! CIRCUIT FOR USAT      !V07
        INTEGER*4   GVTVAL_BIT_OFFSET           ! CIRCUIT OFFSET IN     !V07
        PARAMETER   (GVTVAL_BIT_OFFSET = 16)    ! SATELITE ID           !V07
        ! INTEGER*4   LAPB_INDEX                  ! LAPB INDX FROM SAT ID !V07
        ! CHARACTER*7 TEMPSTR                                             !V07
        CHARACTER*7 GVTSTR                                              !V07
        PARAMETER   (GVTSTR='*GVTid:')                                  !V07
        CHARACTER*7 SATSTR                                              !V07
        PARAMETER   (SATSTR='SATid: ')                                  !V07
        CHARACTER*1 FG_FLAG/'*'/
        ! CHARACTER*1 BLANK(DADRL) /DADRL*' '/
	CHARACTER*1 GVTID(12), XLINE(80)       !V08
        CHARACTER   BCST1_CFG*3                 !BCST1 CONF FLAG        !V14
        CHARACTER   BCST1_ACT*3                 !BCST1 BIT FLAG         !V14
        INTEGER*4   DLINE(20)                                           !V14
        EQUIVALENCE (DLINE,XLINE)                                       !V14
C
        CHARACTER   STNSTATE(0:3)*8             !Stn state description
        CHARACTER   TERSTATE(0:5)*8             !Ter state description
        CHARACTER   CHRSTR(LXADR)*1             !ASCII staion address   !V08
        CHARACTER   COMTBL(0:10)*8              !Communications type    !V14
C
        DATA        STNSTATE    /'inactive','    idle',
     *                           '    init','disabled'/
        DATA        TERSTATE    /'undefine',' defined',
     *                           '  active','slowpoll',
     *                           'disabled','waitresp'/
        DATA        COMTBL      /'Invalid ',                      !V14
     *                           'X21 SWC ','X25 SVC ',           !V14
     *                           'X25 PVC ','X28 PAD ',           !V14
     *                           'X25 FSL ','AYS PVC ',           !V14
     *                           'GT  Dial','        ',           !V07
     *                           '        ','USAT PVC'/           !V07    
        ! DATA        YESNO       /'yes',' no'/
C
        ! INTEGER*4   STATION       !V03
        INTEGER*4   LODNUM
        INTEGER*4   LODNAM
        INTEGER*4   LODSEG
        INTEGER*4   LODAPP        !V03
        REAL*8      LODPER
        INTEGER*4   TOTLOADS
        INTEGER*4   DD
C
        ! REAL*8      CHA(13)       !V07
        ! DATA VALUE/0/
c        DATA DISP/'trmlin ','dropad ','setpri ','dynpty ',
c     *            'trmsta ','trmsup ','linsup ','slocnt ',
c     *            'rtycnt ','errcnt ','rcvnse ','xmtnse ',
c     *            'linerr ','OPStat ','BROnum ','PASnum ',
c     *            '       ','circut ','Xremote','MONitor',   !V14
c     *            'trace  '/
C
c        DATA DAY/'Mon','Tue','Wed','Thu','Fri','Sat','Sun'/
C
c          DATA CHA/'OPStat  ','BROnum  ','PASnum  ',
c     *           'SUMmary ','CLErk   ','MONitor ',
c     *           'PRTstats','SOFt    ','HARd    ',
c     *           'TERstate','GVTid   ','????????',      !V14
c     *           'SATid   '/                            !V14
C
C
        DATA CZERO/Z0/
        ! INTEGER*4   TMP
        ! INTEGER*4   EQUIV_CLS(X2XC_CLASSES)/X2XC_CLASSES*-1/
        ! LOGICAL     FIRST/.TRUE./
C 
C       ***** START V09 CHANGES *****
        INTEGER*4   CLASS                               !STATION CLASS
        INTEGER*4   MAX_DISPLAYED_NETWORK_DELAY,        !DISPLAY THRESHOLDS
     *              MAX_DISPLAYED_TOTAL_DELAY
        PARAMETER   (MAX_DISPLAYED_NETWORK_DELAY = 999999)      !MAX I6 VALUE
        PARAMETER   (MAX_DISPLAYED_TOTAL_DELAY = 99999)         !MAX I5 VALUE
C       ***** END V09 CHANGES *****
C
	CHARACTER ACTIVELOAD*4
	CHARACTER BACKGRLOAD*4
C
C BEGIN PROCESSING
C
        TOTACT  =0
        TOTCNF  =0
        LSTERR  =0
        DELACK  =1
        ERRREP  =1
        TSTATE  =0
        SSTATE  =0
        ADR_LEN =0
        ERR_CNT =0                    !V14
        ACK_CNT =0                    !V14
        CNT_ACTIVE=0                  !V14
        RESET_CNT=0                   !V14
        ! XSTN_PORT = 0

	RET_CODE = 0	
	CALL GUI_GETPARAMS(OUTBUF,ST)
	IF(ST.NE.0) THEN
	    RET_CODE = 11
	    RETURN
	ENDIF
	TER = GUI_ARGVAL(1)

        IF(TER.LT.1 .OR. TER.GT.NUMAGT) TER = 1
C
C GET DATA FROM COMMON
C
	STN = X2XT_STATION_NO(TER)
	DO I = 1,12
	   GVTID(I) = '0'
	ENDDO
	IF(STN.EQ.0) STN = 1
	CALL HTOA(GVTID,1,X2XS_EVSN_LEN(STN),X2XS_EVSN(1,STN),ERR)
	IF(STN.LT.1.OR.STN.GT.X2X_STATIONS) STN = 0   !for now

C IF THE TERMINAL IS ON THE X2X DISTRIBUTED NETWORK, LOAD
C THE X2X NETWORK INFORMATION.
C
        IF(STN.GE.1.AND.STN.LE.X2X_STATIONS) THEN       !V14
C-        XSTN_PORT = ZEXT(BX2XT_STATION_PORT(TER))     !V14
          ADR_LEN=X2XS_ADRESS_LEN(STN)
C
          IF(ADR_LEN.NE.0) THEN
            CALL HTOA(CHRSTR,1,ADR_LEN,X2XS_ADRESS(1,STN),ERROR)
          ELSE
            ADR_LEN=1
            CHRSTR(1)=' '
          ENDIF
C
          CALL ILBYTE(SSTATE,IX2XS_STATE,STN-1)
          CALL ILBYTE(TSTATE,IX2XT_STATE,TER-1)
          STNSEC=X2XS_TIME(STN)
          STNHR=STNSEC/3600
          STNMIN=(STNSEC-STNHR*3600)/60
          STNSEC=STNSEC-(STNHR*3600+STNMIN*60)
          TERMSEC=X2XT_TIME(TER)                                  !V14
          TERMHR=TERMSEC/3600                                     !V14 
          TERMMIN=(TERMSEC-TERMHR*3600)/60                        !V14 
          TERMSEC=TERMSEC-(TERMHR*3600+TERMMIN*60)                !V14 
          DROP=X2XT_DROP_AD(TER)                                  !V14    
          IF(DROP(1:1).EQ.CZERO.AND.DROP(2:2).EQ.CZERO) DROP='  ' !V14 
          IF(NTSBIT(IX2XS_DOWNFLAG,(STN-1)*8+1)) ERRREP=0         !V14
          IF(NTSBIT(IX2XS_DOWNFLAG,(STN-1)*8+2)) DELACK=0         !V14
          LSTERR=0                                                !V14
          IF (STN.GT.0) LSTERR = X2XS_LAST_ERR_CODE(STN)          !V14
          BCST1_CFG=' no'                                         !V14
          IF(X2XS_BCST_NUM(STN).GT.0) BCST1_CFG='yes'             !V14
          BCST1_ACT=' no'                                         !V14
          IF(IAND(X2XS_BCST_ACTIVE_BITMAP(STN),1).NE.0)           !V14
     *                              BCST1_ACT='yes'               !V14
C
C
          DO 230 PORT=1,X2X_MAXPORT
            TOTCNF=TOTCNF+X2XS_NUM_TERMS(PORT,STN)
            TOTACT=TOTACT+ACTERM(STN,PORT)
230       CONTINUE
          ERR_CNT=X2XS_ERR_CNT(STN)                               !V14
          ACK_CNT=X2XS_ACK_CNT(STN)                               !V14
          CNT_ACTIVE=X2XS_CNT_ACTIVE(STN)                         !V14
          RESET_CNT=X2XS_RESET_CNT(STN)                           !V14
        ENDIF
C
C DLL REQUEST TABLE
C
        LODNUM = 0
        LODNAM = SPACE
        LODSEG = 0
        LODPER = 0.0D0
        TOTLOADS=0
C
C
C       ***** Start V13 changes *****
C
        IF(DLLREQ_LAST(LOADNBR,TER).GE.1) THEN
            LODNUM=DLLREQ_LAST(LOADNBR,TER)
            LODSEG=DLLREQ_LAST(SEGNBR,TER)
            LODAPP=DLLREQ_LAST(APPLICATION_NBR,TER)
            IF (LODAPP.NE.0) THEN
              LODNAM=SMFDLNAM(1,LODNUM,LODAPP)
              FG_FLAG=' '
              IF (SMFDLTAB(LODNUM,FOREGROUND_FLAG,LODAPP).EQ.
     *            FOREGROUND_LOAD) THEN
                  FG_FLAG='F'
              ELSE
                  FG_FLAG='B'
              ENDIF
            ENDIF
            DO DD = 1,MAXLOADS
              IF(SMFDLTAB(DD,NBRSEG,LODAPP).NE.0) TOTLOADS=TOTLOADS+1
            ENDDO
            CALL REQPER(LODNUM,LODAPP,LODSEG,LODPER)
            IF(LODSEG.EQ.0.AND.LODNUM.EQ.TOTLOADS) LODPER=0.0D0
            IF(LODPER.EQ.100.AND.LODNUM.EQ.TOTLOADS) THEN
                LODNAM=SPACE
                LODSEG=0
                LODPER=0.0D0
            ENDIF
            IF (LODPER .EQ. 0.0D0) FG_FLAG=' '
        ENDIF
C
        IF (BX2XS_CONN_TYPE(STN) .EQ. X2XSCT_USAT_PVC) THEN
           ! TEMPSTR=SATSTR
           TEMP=1
        ELSE
           ! TEMPSTR=GVTSTR
           TEMP=0
        ENDIF
        IF(X2XT_NETWORK_DELAY(TER).LT.0)X2XT_NETWORK_DELAY(TER)=0
        IF(X2XT_DELAY(TER).LT.0) X2XT_DELAY(TER)= 0

C
C PREPARE X2X DATA 
C
	CLASS = 0
	IF(STN.GE.1 .AND. STN.LE.X2X_STATIONS) THEN
	   CLASS   = X2XS_STNCLS(STN)
	   VCOMTYP = BX2XS_CONN_TYPE(STN)
	ENDIF
	IF(VCOMTYP.EQ.9) VCOMTYP = 7   !this may be for Finland dial only
	IF(VCOMTYP.LT.1 .OR. VCOMTYP.GT.10) VCOMTYP = 0
	IF(CLASS.LE.0 .OR. CLASS.GT.X2XC_CLASSES) THEN
	   CLASS = 0
	ENDIF
C
	WRITE(ACTIVELOAD,100) DLLREQ_ACTIVE(TER)
	WRITE(BACKGRLOAD,101) DLLREQ_BACKGRND(TER)
C
100	FORMAT(Z4.4)
101	FORMAT(Z4.4)
CC
C ENCODE TERMINAL COMM DATA OUTPUT
C
C INITIALIZE OUTPUT 
C
	CALL GUIARG_INIT()
C
	NUM_COLS = 33
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
	CALL GUIARG_INT4(OUTBUF,CLASS)                        !ComClass
	CALL GUIARG_INT4(OUTBUF,VCOMTYP)                      !VComTyp
	CALL GUIARG_CHAR(OUTBUF,%REF(COMTBL(VCOMTYP)),8)      !ComTyp
	IF(CLASS.EQ.0) THEN				      !ClassDesc
	   CALL GUIARG_CHAR(OUTBUF,'NONE        ',12)
	ELSE
	   CALL GUIARG_CHAR(OUTBUF,%REF(X2XC_DESC(CLASS)),12)	
	ENDIF
	CALL GUIARG_INT4(OUTBUF,TOTCNF)		              !TotCnf
	CALL GUIARG_INT4(OUTBUF,TOTACT)                       !TotAct
	CALL GUIARG_CHAR(OUTBUF,%REF(STNSTATE(SSTATE)),8)     !StnState
	CALL GUIARG_TIME(OUTBUF,X2XS_TIME(STN))		      !StnTime
	CALL GUIARG_BYTE(OUTBUF,ERRREP)			      !ErrRep
	CALL GUIARG_INT4(OUTBUF,ERR_CNT)		      !ErrCnt
	CALL GUIARG_INT4(OUTBUF,LSTERR)	                      !LstErr
	CALL GUIARG_BYTE(OUTBUF,DELACK)                       !DelAck
	CALL GUIARG_INT4(OUTBUF,ACK_CNT)		      !AckCnt
	CALL GUIARG_INT4(OUTBUF,CNT_ACTIVE)                   !CntAct
	CALL GUIARG_INT4(OUTBUF,RESET_CNT)                    !CntResets
	CALL GUIARG_CHAR(OUTBUF,%REF(TERSTATE(TSTATE)),8)     !TerState
	CALL GUIARG_TIME(OUTBUF,X2XT_TIME(TER))		      !TerTime
	CALL GUIARG_INT4(OUTBUF,STN)			      !Station
	CALL GUIARG_CHAR(OUTBUF,%REF(CHRSTR(MAX0(1,ADR_LEN-DADRL+1))),ADR_LEN) 
                                                              !StnAddress
	CALL GUIARG_INT2(OUTBUF,X2XS_BCST_NUM(STN))           !BCst1Cfg
	CALL GUIARG_INT2(OUTBUF,X2XS_BCST_ACTIVE_BITMAP(STN)) !BCst1Act
	CALL GUIARG_INT4(OUTBUF,LODSEG)                       !Segment
	CALL GUIARG_CHAR(OUTBUF,%REF(LODNAM),4)		      !LoadName
	CALL GUIARG_INT4(OUTBUF,IDNINT(LODPER))		      !LoadPercent
	CALL GUIARG_CHAR(OUTBUF,%REF(FG_FLAG),1)	      !ForeGrndFlag
	CALL GUIARG_INT4(OUTBUF,DLLREQ_ACTIVE(TER))	      !DLLReqActive
	CALL GUIARG_INT4(OUTBUF,DLLREQ_BACKGRND(TER))	      !DLLReqBckGrnd
	CALL GUIARG_BYTE(OUTBUF,TEMP)                         !GvtORSatFlag
	CALL GUIARG_CHAR(OUTBUF,%REF(GVTID),12)               !Gvtid
        CALL GUIARG_INT4(OUTBUF,MIN(X2XT_NETWORK_DELAY(TER),
     *                              MAX_DISPLAYED_NETWORK_DELAY)) !TerNetDelay
        CALL GUIARG_INT4(OUTBUF,MIN(X2XT_NETWORK_DELAY(TER)+X2XT_DELAY(TER),
     *                              MAX_DISPLAYED_TOTAL_DELAY))   !TerTotalDelay

	CALL GUIARG_CHAR(OUTBUF,%REF(ACTIVELOAD),4)		!Active
	CALL GUIARG_CHAR(OUTBUF,%REF(BACKGRLOAD),4)		!Background
C
C FINALLY SET OUTPUT MESSAGE LENGTH 
C
	CALL GUIARG_SET_MESLEN(MES_LEN)
C
	RETURN
C
	END
