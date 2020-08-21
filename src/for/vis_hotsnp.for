C
C SUBROUTINE HOTSNP
C $Log:   GXAFIP:[GOLS]VIS_HOTSNP.FOV  $

C V19 12-MAR-2010 RXK ePassive changes
C  
C     Rev 1.4   31 Jan 1997 19:39:54   WPW
C  Command for on-line terminal added.
C  SP3 -> agttoi 1, GVT -> agttoi 0.
C  
C     Rev 1.3   17 Jan 1997 15:44:00   RXK
C  Fix for GVT id online change
C  
C     Rev 1.2   13 Jan 1997 16:52:26   RXK
C  Gvt id online change added
C  
C     Rev 1.1   17 May 1996 11:44:16   HXK
C  Update from Wojtek, Siew Mun
C  
C     Rev 1.14   24 Apr 1995 17:02:14   HXK
C  Merge of V5 development with March 10th 1995 bible
C  
C     Rev 1.14   02 Mar 1995 14:27:02   HXK
C  Changed commission calculation for V5
C  
C     Rev 1.13   02 Feb 1994  1:00:16   HXK
C  ADDING JWE'S CHANGE FOR X2X ERROR CODES TO ARCHIVE.
C  
C     Rev 1.12   18 Dec 1993 23:39:26   JWE
C  Move station & port # to beside address
C  
C     Rev 1.11   16 Oct 1993 20:05:32   LMK
C  CHECK FOR VALID STATION NUMBER
C  
C     Rev 1.10   11 Oct 1993 11:52:52   LMK
C  ADD REQ INFO
C  
C     Rev 1.9   22 Sep 1993 17:58:16   JWE
C  Correct the last fixs.  I had to check it in early to get MAKE_TASK
C  to run correctly
C  
C     Rev 1.8   22 Sep 1993 13:25:26   JWE
C  Read station class description out of station calss file
C  Fix a problem with the display of the statin port #
C  
C     Rev 1.7   10 Sep 1993 17:23:16   SXH
C  FIXED BUG WITH CLERKS SHOWING TOTALS NOT THEIR OWN
C  
C     Rev 1.6   04 Sep 1993 14:39:06   WXS
C  FIX OF FORMAT STATEMENT JWE
C  
C     Rev 1.5   30 Aug 1993  6:55:48   JWE
C  Correct the temporary removel of the station address.  The station
C  address display was expanded to 16 digits.
C  
C     Rev 1.4   12 Jul 1993 15:24:18   HXK
C  temporary removal of x2addr
C  
C     Rev 1.3   11 Jun 1993 17:54:58   HXK
C  ADDED AGTINF.DEF, AGTCOM.DEF
C  
C     Rev 1.2   01 Mar 1993 12:02:24   EBD
C  DAS update 3/1/93
C  Changing format of ASF file
C
C ** Source - vis_hotsnp.for **
C
C HOTSNP.FOR
C
C V17 16-MAR-11 GPW NUMAGT=12288
C V16 30-JAN-01 CAS INCLUDED SCML AGENT TYPES
C V15 30-JAN-98 UXN Additional checking for broadcast servers.
C V14 29-FEB-96 wsm  X2X Upgrade: x2x baseline changes update. 
C V13 12-feb-96 DAS CHANGES FOR BACKGROUND LOADS
C V12 02-Feb-96 das fix overflow of net stats
C V11 17-JUL-95 SCD Do not allow BRONUM processing for GVTs. Fix overwriting
C                   of "Input error" and "Value error" messages
C                   by "Enter #Terminal or agent number, or Vision command"
C                   message on display line 23.
C V10 21-JUN-95 SCD REMOVE GVTID processing.  To keep the GEMS and X2X
C                   databases in synch, only allow GVTIDs to be entered
C                   via the GUI.  Also change case of GVTSTR from "GVTid" to
C                   "gvtid" to indicate that this is no longer a command.
C V09 13-JUN-95 SCD REPLACE parameters X2X_GTXDIAL_STATIONS and
C                   X2X_NONDIAL_STATIONS with explicit check for station
C                   class when processing GVTID commands.  Also threshold
C                   displayed delay values when format statement field width
C                   is exceeded.
C V08 02-NOV-94 GPR Change address length to variable for UK
C V07 28-OCT-94 GPR ADD USAT CLASS TYPE
C V06 29-APR-94 GPR CHECK FOR VALID SATELITE ID AND CORRECT FORMAT STATEMENT
C
C V02 24-JAN-92 DAS  DISABLED STATION HARD AND SOFT RESET
C V01 23-OCT-91 GCAN INITIAL RELEASE FOR THE NETHERLANDS
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE HOTSNP(TER,CLINE,LKEY,LTER,GNUMDSP)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:MSGCOM.DEF'         !V14
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:VISCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:X2XSCL.DEF'
        INCLUDE 'INCLIB:X2VIS.DEF'          !V08
        INCLUDE 'INCLIB:REQCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4   SALIND , CANIND , VALIND , DSCIND , CLMIND 
        INTEGER*4   REFIND , LENIND
        PARAMETER   (SALIND=MAXGAM+1)
        PARAMETER   (CANIND=MAXGAM+2)
        PARAMETER   (VALIND=MAXGAM+3)
        PARAMETER   (DSCIND=MAXGAM+4)
        PARAMETER   (CLMIND=MAXGAM+5)   !returns
        PARAMETER   (REFIND=MAXGAM+6)
        PARAMETER   (LENIND=REFIND)
C
        INTEGER*4  RESET_CNT                    !         !V14
        INTEGER*4  ERR_CNT                      !         !V14
        INTEGER*4  ACK_CNT                      !         !V14
        INTEGER*4  CNT_ACTIVE                   !         !V14
        INTEGER*4  VCOMTYP                      !
        INTEGER*4  LIDX                         !
        INTEGER*4  S                            !
        INTEGER*4  ACTERM                       !
        INTEGER*4  PORT                         !
        INTEGER*4  ERROR                        !
        INTEGER*4  ADR_LEN                      !
        INTEGER*4  XSTN_PORT                    !
        INTEGER*4  SSTATE                       !
        INTEGER*4  TSTATE                       !
        INTEGER*4  STNSEC                       !
        INTEGER*4  STNHR                        !
        INTEGER*4  STNMIN                       !
        INTEGER*4  TERMSEC                      !
        INTEGER*4  TERMHR                       !
        INTEGER*4  TERMMIN                      !
        INTEGER*4  ERRREP                       !
        INTEGER*4  DELACK                       !
        INTEGER*4  LSTERR                       !
        INTEGER*4  TOTCNF                       !
        INTEGER*4  TOTACT                       !
        INTEGER*4  MONCNT                       !
        INTEGER*4  LSTW                         !
        INTEGER*4  LAST                         !
        INTEGER*4  AN                           !
        INTEGER*4  OP                           !
        INTEGER*4  COMAMT(2)                    !
        INTEGER*4  NETSAL                       !
        INTEGER*4  GTYP                         !
        INTEGER*4  GIND                         !
        INTEGER*4  GAM                          !
        INTEGER*4  I                            !
        INTEGER*4  J                            !
        INTEGER*4  TOTCOM(2)                    !
        INTEGER*4  ST                           !
        INTEGER*4  TKC                          !
        INTEGER*4  CLRKNUM                      !
        INTEGER*4  PASNUMOFF                    !
        INTEGER*4  KEYNUM                       !
        INTEGER*4  ADJUST(2)                    !
        INTEGER*4  STN                          !
        INTEGER*4  POS                          !
        INTEGER*4  XPOS                         !       !V14
        INTEGER*4  DPOS                         !       !V14
        INTEGER*4  ERR1                         !       !V14
        INTEGER*4  VALUE                        !
        INTEGER*4  LTER                         !
        INTEGER*4  LKEY                         !
        INTEGER*4  TER                          !
        INTEGER*4  TOTDUE(2)                    !
C
C
        INTEGER*4  CLINE(20)                    !
        INTEGER*4  BUF(CDLEN)                   !

        INTEGER*4  SPACE/'    '/                !
        INTEGER*4  CNT(LENIND)                  !
        INTEGER*4  AMT(LENIND)                  !
        INTEGER*4  INDVTAB(AGAMLEN,MAXGAM)      !
        INTEGER*4  GNUMDSP                      ! Game # to display sales for.
        INTEGER*4  GTYPDSP                      ! Game type to display - " - .
        INTEGER*4  GINDDSP                      ! Game index to display- " - .
        INTEGER*4  BITMAP                       !

        INTEGER*2  DBUF(LDATE_LEN)              !

        CHARACTER   CZERO,DROP*2                !
        CHARACTER*3 DAY(7)
        CHARACTER*7 DISP(21)                    !
        CHARACTER*8 REPMOD(2)                   !
C
        CHARACTER   AGENT_STRING*(ALENGTH)
        EQUIVALENCE (AGENT_STRING, ASFBYT)
C
C X2X PARAMETERS.
C
        INTEGER*4   GVTVAL                      ! CIRCUIT FOR USAT      !V07
        INTEGER*4   GVTVAL_BIT_OFFSET           ! CIRCUIT OFFSET IN     !V07
        PARAMETER   (GVTVAL_BIT_OFFSET = 16)    ! SATELITE ID           !V07
        INTEGER*4   LAPB_INDEX                  ! LAPB INDX FROM SAT ID !V07
        CHARACTER*7 TEMPSTR                                             !V07
        CHARACTER*7 GVTSTR                                              !V07
        PARAMETER   (GVTSTR='*GVTid:')                                  !V07
        CHARACTER*7 SATSTR                                              !V07
        PARAMETER   (SATSTR='SATid: ')                                  !V07
        CHARACTER*1 FG_FLAG/'*'/
        CHARACTER*1 BLANK(DADRL) /DADRL*' '/,GVTID(12), XLINE(80)       !V08
        CHARACTER   BCST1_CFG*3                 !BCST1 CONF FLAG        !V14
        CHARACTER   BCST1_ACT*3                 !BCST1 BIT FLAG         !V14
        INTEGER*4   DLINE(20)                                           !V14
        EQUIVALENCE (DLINE,XLINE)                                       !V14
C
        INTEGER*4   CMDVALUE(4)   / 4*-1 /      !Used for command processing
        CHARACTER   STNSTATE(0:3)*8             !Stn state description
        CHARACTER   TERSTATE(0:5)*8             !Ter state description
        CHARACTER   CHRSTR(LXADR)*1             !ASCII staion address   !V08
        CHARACTER   COMTBL(0:10)*8              !Communications type    !V14
        CHARACTER   YESNO(0:1)*3                !Yes/no array
	CHARACTER   CAGTTYP(4)*7	        !SCML AGENT TYPES
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
        DATA        YESNO       /'yes',' no'/
	DATA        CAGTTYP     /'Online ','Offline',
     *                           'Privil.','Banco  '  /
C
        INTEGER*4   STATION       !V03
        INTEGER*4   LODNUM
        INTEGER*4   LODNAM
        INTEGER*4   LODSEG
        INTEGER*4   LODAPP        !V03
        REAL*8      LODPER
        INTEGER*4   TOTLOADS
        INTEGER*4   DD
	INTEGER*4   IAGTTYP
C
        INTEGER*4   DUMMY
        REAL*8      CHA(13)       !V07
        DATA VALUE/0/
        DATA DISP/'trmlin ','dropad ','setpri ','dynpty ',
     *            'trmsta ','trmsup ','linsup ','slocnt ',
     *            'rtycnt ','errcnt ','rcvnse ','xmtnse ',
     *            'linerr ','OPStat ','BROnum ','PASnum ',
     *            '       ','circut ','Xremote','MONitor',   !V14
     *            'trace  '/
C
        DATA DAY/'Mon','Tue','Wed','Thu','Fri','Sat','Sun'/
C
        DATA CHA/'OPStat  ','BROnum  ','PASnum  ',
     *           'SUMmary ','CLErk   ','MONitor ',
     *           'PRTstats','SOFt    ','HARd    ',
     *           'TERstate','GVTid   ','????????',      !V14
     *           'SATid   '/                            !V14
C
        DATA REPMOD/'*CLERK  ','SUMMARY '/
C
        DATA CZERO/Z0/
        INTEGER*4   TMP
	INTEGER*4   EQUIV_CLS(X2XC_CLASSES)/X2XC_CLASSES*-1/
	LOGICAL	    FIRST/.TRUE./
C 
C       ***** START V09 CHANGES *****
        INTEGER*4   CLASS                               !STATION CLASS
        INTEGER*4   MAX_DISPLAYED_NETWORK_DELAY,        !DISPLAY THRESHOLDS
     *              MAX_DISPLAYED_TOTAL_DELAY
        PARAMETER   (MAX_DISPLAYED_NETWORK_DELAY = 999999)      !MAX I6 VALUE
        PARAMETER   (MAX_DISPLAYED_TOTAL_DELAY = 99999)         !MAX I5 VALUE
C       ***** END V09 CHANGES *****
C
C
C CHECK INPUTED GAME NUMBER AND DECODE GAME TYPE AND INDEX
C
        IF(GNUMDSP.LT.1.OR.GNUMDSP.GT.MAXGAM) GNUMDSP=1
        GTYPDSP=GNTTAB(GAMTYP,GNUMDSP)
        GINDDSP=GNTTAB(GAMIDX,GNUMDSP)
C
        IF(GTYPDSP.LT.1.OR.GTYPDSP.GT.MAXTYP) THEN
            WRITE(CLIN23,923) GNUMDSP
            RETURN
        ENDIF
C
C  CHECK FOR ANY OPERATIONAL CHANGE
C
        POS=1
        SMODE=.FALSE.
        IF(TER.LT.1.OR.TER.GT.X2X_TERMS) TER=1
        LSTAGT=AGTTAB(AGTNUM,TER)
        STN=X2XT_STATION_NO(TER)
        DO 2 I= 1,12                                                      !V14
          GVTID(I)='0'                                                    !V14
2       CONTINUE                                                          !V14
        IF(STN.EQ.0) STN=1                                                !V14
        CALL HTOA(GVTID,1,X2XS_EVSN_LEN(STN),X2XS_EVSN(1,STN),ERR1)       !V14
        IF(STN.LT.1.OR.STN.GT.X2X_STATIONS) STN=0   !FOR NOW              !V14
C
        CALL KEY(CLINE,CHA,13,POS,KEYNUM)
        IF(KEYNUM.EQ.4 .OR.
     *     KEYNUM.EQ.7 .OR. KEYNUM.EQ.8 .OR.
     *     KEYNUM.EQ.9) GOTO 5
C
C       Start of V10 change
C       If the operator has entered "GVTID", then KEYNUM will be set to 11.
C       Changing the value of KEYNUM from 11 to 0 will force further processing
C       to be the same as for an INPUT ERROR.
C
CRXK        IF(KEYNUM.EQ.11) KEYNUM = 0             !FORCE KEYNUM TO BE INVALID
C
C       End of V10 change
C
        IF(POS.GT.40) GOTO 100                  !NO INPUT
C
C Clear command line
C
        WRITE(CLIN24,9073)                      !V14
C
        IF(KEYNUM.EQ.0) GOTO 10                 !INPUT ERROR
        CALL NUMB(CLINE,POS,VALUE)              !GET VALUE
        IF(VALUE.LT.0) GOTO 20                  !VALUE ERROR
5       CONTINUE
        GOTO (30,40,50,60,70,80,82,84,86,88,89,91,89) KEYNUM               !V07
10      CONTINUE
        WRITE(CLIN23,9923)
        GOTO 8000                                                       !V11
20      CONTINUE
        WRITE(CLIN23,9924)
        GOTO 8000                                                       !V11
21      CONTINUE                                                        !V11
        WRITE(CLIN23,9925)                                              !V11
        GOTO 8000                                                       !V11
C
C OPSTAT CHANGE
C
30      CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.SERVFD) GOTO 20
        BUF(1)=1
        BUF(2)=VALUE
        BUF(3)=TCAGT
        BUF(5)=TER
        GOTO 90
C
C  BRONUM CHANGE
C
40      CONTINUE
C       ***** START V11 CHANGES *****
C       Disable BRONUM command for GVTs
        STATION=X2XT_STATION_NO(TER)
	IF(STATION.LE.0.OR.STATION.GT.X2X_STATIONS) THEN
          WRITE(CLIN23,9926)
          GOTO 100
        ENDIF
        CLASS=X2XS_STNCLS(STATION)
        IF (CLASS.EQ.X2XC_CLASS_GVT.OR.CLASS.EQ.X2XC_CLASS_X28) GOTO 21
C       ***** END V11 CHANGES *****
        IF(VALUE.LT.1.OR.VALUE.GT.256) GOTO 20
	IF(X2XS_TYPE(STATION).EQ.X2XST_BCST) THEN
            WRITE(CLIN23,1504)
            GOTO 8000
        ENDIF
        BUF(1)=5
        BUF(2)=VALUE
        BUF(3)=TCSPE
        BUF(4)=0
        BUF(5)=TER
        WRITE(CLIN23,1502) VALUE,TER
        GOTO 90
C
C CHANGE PASS NUMBER
C
50      CONTINUE
        IF(VALUE.GT.9999) GOTO 20
        BUF(1)=2
        BUF(2)=VALUE
        BUF(3)=TCAGT
        PASNUMOFF=1     !TEMPORARILY
        BUF(4)=PASNUMOFF
        BUF(5)=TER
        GOTO 90
C
C SUM UP FOR ALL CLERKS FOR THIS TERMINAL
C
60      CONTINUE
        IF(P(CLRKACT).NE.0) GOTO 10
        ALLON=.TRUE.
        CLRKNUM=AGTHTB(AGTPASOFF,TER)
        CALL GETCLERK(TER,CLRKNUM,1,INDVTAB)
        CLERKON=0
        GOTO 120
C
C SET FOR INDIVIDUAL CLERK ONLY
C
70      CONTINUE
        IF(P(CLRKACT).NE.0) GOTO 10
        IF(VALUE.LT.1.OR.VALUE.GT.NUMCLERK) GOTO 20
        ALLON=.FALSE.
        CALL FASTSET(0,INDVTAB,AGAMLEN*MAXGAM)
        CLERKON=VALUE
        IF(CLERKON.EQ.AGTHTB(AGTPASOFF,TER)) GOTO 120
        SMODE=.TRUE.
        CALL GETCLERK(TER,CLERKON,0,INDVTAB)
        TOTCOM(1)=0
        TOTCOM(2)=0
        TKC=0               
        CALL FASTSET(0,AMT,LENIND)
        CALL FASTSET(0,CNT,LENIND)
        GOTO 131
C
C SET THE MONITOR COUNT. NOTE: ONLY DONE IN MEMORY.
C
80      CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.254) GOTO 20
        CALL ISBYTE(VALUE,IX2XT_TRACE_LIMIT,TER-1)
        GOTO 100
C
C SEND A PORT STATISTICS REQUEST.
C
82      CONTINUE
        IF(STN.LE.0.OR.STN.GT.X2X_STATIONS) THEN
          WRITE(CLIN23,9924)
          GOTO 8000                                             !V11
        ENDIF
        BUF(1)=4
        BUF(2)=STN
        BUF(3)=TCX2X
        BUF(8)=TER
        WRITE(CLIN23,9250) STN
        GOTO 90
C
C SEND A SOFT RESET TO THE STATION.
C
84      CONTINUE
        IF(STN.LE.0.OR.STN.GT.X2X_STATIONS) THEN
          WRITE(CLIN23,9926)
          GOTO 100
        ENDIF
        BUF(1)=2
        BUF(2)=STN
        BUF(3)=TCX2X
        BUF(8)=TER
        WRITE(CLIN23,9230) STN
        GOTO 90
C
C SEND A HARD RESET TO THE STATION.
C
86      CONTINUE
        IF(STN.LE.0.OR.STN.GT.X2X_STATIONS) THEN
          WRITE(CLIN23,9926)
          GOTO 100
        ENDIF
C
        IF(X2XS_TYPE(STN).EQ.X2XST_BCST) THEN
          WRITE(CLIN23,1504)
          GOTO 100
        ENDIF
C
        BUF(1)=5
        BUF(2)=STN
        BUF(3)=TCX2X
        BUF(8)=TER
        WRITE(CLIN23,9232) STN
        GOTO 90
C
C TERMINAL STATUS CHANGE.
C
88      CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.4) THEN
          WRITE(CLIN23,9924)
          GOTO 8000                                             !V11
        ENDIF
        CMDVALUE(1)=VALUE
        STATION=X2XT_STATION_NO(TER)                            !V07
        IF (X2XS_TYPE(STATION).NE.X2XST_BCST)                   !V07
     *          CALL X2BLDCMD(1,XTER,TER,5,CMDVALUE,2,1,BUF)    !V07
        GOTO 90
C
C GVTID COMMAND
C
89      CONTINUE
        DO 891 I = 1,20
           DLINE(I)=CLINE(I)
891     CONTINUE
C
        DO 892 XPOS=1,40
           IF(XLINE(XPOS).EQ.BLANK(1))GOTO 893
892     CONTINUE
893     CONTINUE
        DPOS=XPOS+1
        CALL ATOH(XLINE,DPOS,X2XS_EVSN_LEN(STN),CMDVALUE(1),ERR1)
        IF(ERR1.LT.0) THEN
           WRITE(CLIN23,9924)
           GOTO 8000                                    !V11
        ENDIF
	IF(STN.LE.0.OR.STN.GT.X2X_STATIONS) THEN
           WRITE(CLIN23,9926)
           GOTO 8000                                    !V11
	ENDIF
	IF(FIRST) THEN
	    CALL GET_CLASS(EQUIV_CLS)
	    FIRST=.FALSE.
	ENDIF
	CLASS = EQUIV_CLS(X2XS_STNCLS(STN))
	IF(CLASS.NE.X2XC_CLASS_GVT.AND.
     *     CLASS.NE.X2XC_CLASS_X28.AND.    
     *     CLASS.NE.X2XC_CLASS_USAT) THEN
           WRITE(CLIN23,9941)
           GOTO 8000                                    !V11
	ENDIF
	         
        IF (CMDVALUE(1).NE.0.AND.CMDVALUE(2).NE.0) THEN
            DO 894 I=1,X2X_STATIONS                     !V09
	        IF(X2XS_STNCLS(I).LE.0) GOTO 894
                CLASS=EQUIV_CLS(X2XS_STNCLS(I))         !V09
                IF (CLASS .EQ. X2XC_CLASS_GVT .OR.      !V09
     *              CLASS .EQ. X2XC_CLASS_X28 .OR.      !V09
     *              CLASS .EQ. X2XC_CLASS_USAT ) THEN   !V09
C
                    IF (X2XS_EVSN(1,I).NE.0.AND.
     *                  X2XS_EVSN(2,I).NE.0.AND.
     *                  X2XS_EVSN(1,I).EQ.CMDVALUE(1).AND.
     *                  X2XS_EVSN(2,I).EQ.CMDVALUE(2)) THEN
                        WRITE(CLIN23,10023) AGTTAB(AGTNUM,
     *                      X2XS_TERMS(1,1,I))
                        GOTO 100
                    ENDIF
                ENDIF                                   !V09
894         CONTINUE
        ENDIF
C
C       ***** START V07 CHANGES *****
C
C       Check for valid Satelite ID if the Station uses
C       USAT connection type
C
        IF (BX2XS_CONN_TYPE(STN) .EQ. X2XSCT_USAT_PVC) THEN
C
C           Save the Lapb Index and Circuit
C
            LAPB_INDEX = CMDVALUE(1)
            GVTVAL = ISHFT(CMDVALUE(2),-GVTVAL_BIT_OFFSET)
C
C           Write error if Lapb index is not valid
C
            IF ((LAPB_INDEX .LE. 0) .OR.
     *          (LAPB_INDEX .GT. X2X_MAXPVC_LINES) .OR.
     *          (LAPB_INDEX .NE.
     *           X2XPN_PVC_INDEX(X2XS_PHYS(STN)))) THEN
C
                WRITE(CLIN23,9930) LAPB_INDEX
                GOTO 100
C
C           Write error if Circuit is not valid
C
            ELSEIF ((GVTVAL .LE. 0) .OR.
     *          (GVTVAL .GT. X2X_MAXPVC_CKTS)) THEN
C
                WRITE(CLIN23,9940) GVTVAL
                GOTO 100
C
            ENDIF
        ENDIF
C
C       ***** END V07 CHANGES *****
C
C                                        
        CALL X2BLDCMD(1,XSTN,STN,38,CMDVALUE,2,0,BUF)                   !V07
        BUF(6)=IDNUM
        CALL VISCMD(BUF,ST)
        CALL XWAIT(2,1,ST)
C
C UPDATE AGTCOM WITH NEW GVT ID 
C
        BUF(1)=8
        BUF(2)=CMDVALUE(1)
        BUF(3)=TCAGT
        BUF(5)=TER
        BUF(9)=CMDVALUE(2)
        CALL VISCMD(BUF,ST)
        CALL XWAIT(2,1,ST)
C
C CLEAR ON-LINE TERMINAL FLAG FOR GVT
C
        BITMAP=AGTTAB(AGTTYP,TER)
        IF(CMDVALUE(1).EQ.0.AND.CMDVALUE(2).EQ.0) THEN
          CALL BSET(BITMAP,AGTTOI)     !terminal
        ELSE
          CALL BCLR(BITMAP,AGTTOI)     !GVT
        ENDIF
        BUF(1)=3
        BUF(2)=BITMAP
        BUF(3)=TCAGT
        BUF(5)=TER
        GOTO 90
C
C MANUAL CANCELLATION FLAG
C
91      CONTINUE
        GOTO 90
C
C QUEUE COMMAND BUFFER TO SYSTEM INPUT QUEUE
C
90      CONTINUE
        BUF(6)=IDNUM
        CALL VISCMD(BUF,ST)
        CALL XWAIT(2,1,ST)
C
C READ AGENTS RECORD
C
100     CONTINUE
        IF(LKEY.EQ.10.AND.LTER.EQ.TER) GOTO 120
        CALL OPENASF(1)
C        ALLON=.FALSE.
        CLERKON=AGTHTB(AGTPASOFF,TER)
        CALL READASF(TER, ASFREC, ST)
        IF(ST.NE.0) THEN
          CALL CLOSASF
          WRITE(CLIN23,1501) (SFNAMES(J,ASF),J=1,5),ST,TER
          RETURN
        ENDIF
        CALL CLOSASF
        DO I=1, ALENGTH
            IF(ASFBYT(I) .LT. ' ') ASFBYT(I)=' '
        END DO
C
        CALL ILBYTE(MONCNT,IX2XT_TRACE_LIMIT,TER-1)
C
C GET TODAYS SALES FROM MEMMORY
C
120     CONTINUE
        IF(P(CLRKACT).EQ.0) THEN

            IF(CLERKON.NE.AGTHTB(AGTPASOFF,TER).AND.(.NOT.ALLON)) THEN
                CALL FASTSET(0,INDVTAB,AGAMLEN*MAXGAM)
                CLERKON=AGTHTB(AGTPASOFF,TER)
            ENDIF

            IF(CLERKON.NE.AGTHTB(AGTPASOFF,TER).AND.ALLON) THEN
                CLERKON=0
                CLRKNUM=AGTHTB(AGTPASOFF,TER)
                CALL GETCLERK(TER,CLRKNUM,1,INDVTAB)
            ENDIF

            IF(.NOT.(ALLON)) THEN
                CALL FASTSET(0,INDVTAB,AGAMLEN*MAXGAM)
            ENDIF
        ENDIF
C
C
        TOTCOM(1)=0
        TOTCOM(2)=0
        ADJUST(1)=0
        ADJUST(2)=0
        TKC=0
        CALL FASTSET(0,AMT,LENIND)
        CALL FASTSET(0,CNT,LENIND)
        DO 130 GAM = 1,MAXGAM
           GTYP = GNTTAB(GAMTYP,GAM)
           GIND = GNTTAB(GAMIDX,GAM)
           IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) GOTO 130

           CNT(GAM) = CNT(GAM) + AGTGAM(GSCNT,GAM,TER)-
     *                           AGTGAM(GCCNT,GAM,TER)
           AMT(GAM) = AMT(GAM) + AGTGAM(GSAMT,GAM,TER)-
     *                           AGTGAM(GCAMT,GAM,TER)
           CNT(SALIND) = CNT(SALIND) + AGTGAM(GSCNT,GAM,TER)-
     *                              AGTGAM(GCCNT,GAM,TER)
           AMT(SALIND) = AMT(SALIND) + AGTGAM(GSAMT,GAM,TER)-
     *                              AGTGAM(GCAMT,GAM,TER)

           CNT(VALIND) = CNT(VALIND) + AGTGAM(GVCNT,GAM,TER)
           AMT(VALIND) = AMT(VALIND) + AGTGAM(GVAMT,GAM,TER)
           CNT(CANIND) = CNT(CANIND) + AGTGAM(GCCNT,GAM,TER)
           AMT(CANIND) = AMT(CANIND) + AGTGAM(GCAMT,GAM,TER)
           CNT(CLMIND) = CNT(CLMIND) + AGTGAM(GCLCNT,GAM,TER)
           AMT(CLMIND) = AMT(CLMIND) + AGTGAM(GCLAMT,GAM,TER)
           CNT(DSCIND) = CNT(DSCIND) + AGTGAM(GDCNT,GAM,TER)
           AMT(DSCIND) = AMT(DSCIND) + AGTGAM(GDAMT,GAM,TER)
           CNT(REFIND) = CNT(REFIND) + AGTGAM(GRCNT,GAM,TER)
           AMT(REFIND) = AMT(REFIND) + AGTGAM(GRAMT,GAM,TER)
           TKC    = TKC    + AGTGAM(GTKCHG,GAM,TER)
	   NETSAL = AGTGAM(GSAMT,GAM,TER) - AGTGAM(GCAMT,GAM,TER)
           CALL GETCOM(NETSAL,TWAG,GAM,COMAMT,TOTCOM,GTYP,GIND,TER)
130     CONTINUE
        DO 155 I=1,15
            IF(ASFLGR(LGRCDC,I).NE.DAYCDC) GOTO 155
            CALL ADDI8I8(ADJUST,ASFLGR(LGRAMTU,I),BETUNIT)
155     CONTINUE
C
C
131     CONTINUE
        IF(P(CLRKACT).EQ.0) THEN
          DO 135 GAM = 1,MAXGAM
             GTYP = GNTTAB(GAMTYP,GAM)
             GIND = GNTTAB(GAMIDX,GAM)
             IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) GOTO 135
             CNT(GAM) = CNT(GAM) + INDVTAB(GSCNT,GAM)-
     *                             INDVTAB(GCCNT,GAM)
             AMT(GAM) = AMT(GAM) + INDVTAB(GSAMT,GAM)-
     *                             INDVTAB(GCAMT,GAM)
             CNT(SALIND) = CNT(SALIND) + INDVTAB(GSCNT,GAM)-
     *                                   INDVTAB(GCCNT,GAM)
             AMT(SALIND) = AMT(SALIND) + INDVTAB(GSAMT,GAM)-
     *                                   INDVTAB(GCAMT,GAM)
             CNT(VALIND) = CNT(VALIND) + INDVTAB(GVCNT,GAM)
             AMT(VALIND) = AMT(VALIND) + INDVTAB(GVAMT,GAM)
             CNT(CANIND) = CNT(CANIND) + INDVTAB(GCCNT,GAM)
             AMT(CANIND) = AMT(CANIND) + INDVTAB(GCAMT,GAM)
             CNT(CLMIND) = CNT(CLMIND) + INDVTAB(GCLCNT,GAM)
             AMT(CLMIND) = AMT(CLMIND) + INDVTAB(GCLAMT,GAM)
             CNT(DSCIND) = CNT(DSCIND) + INDVTAB(GDCNT,GAM)
             AMT(DSCIND) = AMT(DSCIND) + INDVTAB(GDAMT,GAM)
             CNT(REFIND) = CNT(REFIND) + INDVTAB(GRCNT,GAM)
             AMT(REFIND) = AMT(REFIND) + INDVTAB(GRAMT,GAM)
             TKC    = TKC    + INDVTAB(GTKCHG,GAM)
             NETSAL = INDVTAB(GSAMT,GAM) - INDVTAB(GCAMT,GAM)
             CALL GETCOM(NETSAL,TWAG,GAM,COMAMT,TOTCOM,GTYP,GIND,TER)
135       CONTINUE
        ENDIF
        DUMMY=0
        CALL GETCOM(AMT(VALIND),TVAL,GAM,COMAMT,TOTCOM,DUMMY,DUMMY,DUMMY)
        IF(TSBIT(AGTTAB(AGTTYP,TER),AGTNCM).NE.0) THEN
          TOTCOM(1)=0
          TOTCOM(2)=0
        ENDIF
C
C CALCULATE TOTAL
C
        TOTDUE(1)=0
        TOTDUE(2)=0
        CALL ADDI8I4(TOTDUE,AMT(SALIND),BETUNIT)
        CALL SUBI8I4(TOTDUE,AMT(VALIND),VALUNIT)
        CALL SUBI8I4(TOTDUE,AMT(REFIND),BETUNIT)
        CALL SUBI8I4(TOTDUE,AMT(DSCIND),BETUNIT)
        CALL ADDI8I8(TOTDUE,ADJUST,BETUNIT)
        CALL SUBI8I8(TOTDUE,TOTCOM,BETUNIT)
        CALL ADDI8I4(TOTDUE,TKC,BETUNIT)
C
C
        OP = AGTHTB(AOPSTS,TER)
        AN = AGTTAB(AGTNUM,TER)
        LAST = AGTTAB(ALSTRA,TER)
        LSTW = AGTTAB(ALSWAG,TER)
C
        DBUF(5)=DAYCDC
        CALL LCDATE(DBUF)
        CALL ILBYTE(MONCNT,IX2XT_TRACE_LIMIT,TER-1)
C
C INITIALIZE VARIABLES TO RID OF COMPILER WARNINGS.
C
        TOTACT  =0
        TOTCNF  =0
        LSTERR  =0
        DELACK  =1
        ERRREP  =1
        TERMMIN =0
        TERMHR  =0
        TERMSEC =0
        STNMIN  =0
        STNHR   =0
        STNSEC  =0
        TSTATE  =0
        SSTATE  =0
        ADR_LEN =0
        ERR_CNT =0                    !V14
        ACK_CNT =0                    !V14
        CNT_ACTIVE=0                  !V14
        RESET_CNT=0                   !V14
        XSTN_PORT = 0
C
C CLEAR THE CHANGEABLE PORTION OF THE SNAPSHOT.1
C
        DO 240 I=9,22
          WRITE(XNEW(  I),9200)
9200      FORMAT(80(' '))
240     CONTINUE
C
C IF THE TERMINAL IS ON THE X2X DISTRIBUTED NETWORK, LOAD
C THE X2X NETWORK INFORMATION.
C
        IF(STN.GE.1.AND.STN.LE.X2X_STATIONS) THEN       !V14
C-        XSTN_PORT = ZEXT(BX2XT_STATION_PORT(TER))     !V14
          ADR_LEN=X2XS_ADRESS_LEN(STN)
C
C         ***** Start V08 changes *****
C
          IF(ADR_LEN.NE.0) THEN
            CALL HTOA(CHRSTR,1,ADR_LEN,X2XS_ADRESS(1,STN),ERROR)
          ELSE
            ADR_LEN=1
            CHRSTR(1)=' '
          ENDIF
C
C         ***** End V08 changes *****
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
            DO 235  DD=1,MAXLOADS
              IF(SMFDLTAB(DD,NBRSEG,LODAPP).NE.0) TOTLOADS=TOTLOADS+1
235         CONTINUE
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
C       ***** End V13 changes *****
C
C ENCODE THE HOTLINE SNAPSHOT
C
C GET SCML AGENT TYPE
C
	IF  ( TSBIT(AGTTAB(AGTTYP,TER),AGTTON) ) THEN
	    IAGTTYP = 1						    !ONLINE
	    IF	( TSBIT(AGTTAB(AGTTYP,TER),AGTPRV) ) IAGTTYP = 3    !PRIVILEGED
	    IF	( TSBIT(AGTTAB(AGTTYP,TER),AGTBNK) ) IAGTTYP = 4    !BANK
	ELSE
	    IAGTTYP = 2						    !OFFLINE
	ENDIF
C
        WRITE(CLIN1,901) (DBUF(I),I=7,13)
        WRITE(CLIN2,902) (ASFBYT(S),S=SNAME,ENAME),AN,CAGTTYP(IAGTTYP)
        WRITE(CLIN3,903) (ASFBYT(S),S=SSTRT,ESTRT),
     *                   TER
        WRITE(CLIN4,904) (ASFBYT(S),S=SCITY,ECITY)
        WRITE(CLIN5,905) (ASFBYT(S),S=STELE,ETELE),
     *                    LAST
        WRITE(CLIN6,906) (ASFBYT(S),S=SCONT,ECONT),
     *                    LSTW
        LIDX=AGTHTB(AGTLANG,TER)
C-      CALL ASCBIN(ASFINF,SSCLS,LSCLS,VCOMTYP,ST)
        IF(LIDX.LT.0.OR.LIDX.GT.3) LIDX=0
        IF(P(CLRKACT).EQ.0) THEN
          IF(.NOT.ALLON) THEN
            WRITE(CLIN7,9071)REPMOD(1),CLERKON
          ELSE
            WRITE(CLIN7,9072) REPMOD(2)
          ENDIF
        ELSE
          WRITE(CLIN7,9073)
        ENDIF
C
	CLASS = 0
        IF(STN.GE.1.AND.STN.LE.X2X_STATIONS) THEN
          CLASS   = X2XS_STNCLS(STN)	                       !V14
          VCOMTYP = BX2XS_CONN_TYPE(STN)                         !V14
	ENDIF
        IF(VCOMTYP .EQ. 9 ) VCOMTYP = 7                ! Kluge to handle dial
        IF(VCOMTYP.LT.1.OR.VCOMTYP.GT.10) VCOMTYP = 0             !V07
        IF (CLASS.LE.0.OR.CLASS.GT.X2XC_CLASSES) THEN                      !V16
             WRITE(CLIN8,908) COMTBL(VCOMTYP),CLASS, 'NONE'                !V16
        ELSE                                                               !V16
             WRITE(CLIN8,908) COMTBL(VCOMTYP),CLASS, X2XC_DESC(CLASS)
        ENDIF                                                              !V16
        IF(CLERKON.EQ.0) THEN 
          TMP = APSNUM
        ELSE
          TMP = CLERKON
        ENDIF
C
C TERMINAL IS CONFIGURED ON THE X2X DISTRIBUTED NETWORK.
C
          WRITE(CLIN9,909)  GTNAMES(GTYPDSP),GINDDSP,CNT(GNUMDSP),
     *                       CSMONY(AMT(GNUMDSP),11,BETUNIT),
     *                       TOTCNF
          WRITE(CLIN10,910) STNSTATE(SSTATE), STNHR,STNMIN,STNSEC,
     *                       TOTACT
          WRITE(CLIN11,911) CNT(SALIND),CSMONY(AMT(SALIND),11,BETUNIT),
C-   *                       YESNO(ERRREP), XERR_COUNT,
C-   *                       X2XS_LAST_ERR_CODE(STN)
     *                       YESNO(ERRREP), ERR_CNT, LSTERR               !V14
          WRITE(CLIN12,912) CNT(CANIND),CSMONY(AMT(CANIND),11,BETUNIT),
C-   *                       YESNO(DELACK),XACK_COUNT,
     *                       YESNO(DELACK),ACK_CNT,                       !V14
     *                       DISP(16),AGTTAB(TMP,TER)
          WRITE(CLIN13,913) CNT(DSCIND),CSMONY(AMT(DSCIND),11,BETUNIT),
C-   *                       XACT_COUNT,
C-   *                       XRES_COUNT, DISP(15)
     *                       CNT_ACTIVE, RESET_CNT, DISP(15)              !V14
          WRITE(CLIN14,914) CNT(VALIND),CMONY(AMT(VALIND),11,VALUNIT),
     *                       TERSTATE(TSTATE),TERMHR,TERMMIN,TERMSEC,
     *                       DISP(20),MONCNT
          WRITE(CLIN15,915) CNT(CLMIND),CMONY(AMT(CLMIND),11,VALUNIT),
     *                       DROP,DISP(14),OP
        WRITE(CLIN16,916) CNT(REFIND),CMONY(AMT(REFIND),11,BETUNIT),
C-   *                    DISP(21), X2XT_TRACE_INDEX(TER),
C-   *                    (CHRSTR(I),I=1,ADR_LEN),
C-   *                    (BLANK(I),I=ADR_LEN+1,16)
     *                    DISP(21), X2XT_TRACE_INDEX(TER), STN,           !V14
     *                    (CHRSTR(I),I=MAX0(ADR_LEN-DADRL+1,1),ADR_LEN)   !V14
        WRITE(CLIN17,917) CSMONYI8(TOTCOM,11,BETUNIT),X2XT_PRO(TER),
C-   *                    STN,XSTN_PORT
     *                    BCST1_CFG, BCST1_ACT                            !V14
        WRITE(CLIN18,918) CSMONYI8(ADJUST,11,BETUNIT),
C-   *                    LODNAM,LODSEG,LODPER
     *                    LODSEG, LODPER, FG_FLAG, LODNAM                 !V14
C-      WRITE(CLIN19,919) CSMONY(TKC,11,BETUNIT)
        WRITE(CLIN19,919) CSMONY(TKC,11,BETUNIT),
     *                    DLLREQ_ACTIVE(TER), DLLREQ_BACKGRND(TER)        !V14
        WRITE(CLIN20,920) CSMONYI8(TOTDUE,14,BETUNIT),
     *                    DAY(1),DAY(2),DAY(3),DAY(4),DAY(5),
     *                    DAY(6),DAY(7)
C
C       ***** Start V07 changes *****
C
        IF (BX2XS_CONN_TYPE(STN) .EQ. X2XSCT_USAT_PVC) THEN
           TEMPSTR=SATSTR
        ELSE
           TEMPSTR=GVTSTR
        ENDIF
        IF(X2XT_NETWORK_DELAY(TER).LT.0)X2XT_NETWORK_DELAY(TER)=0
        IF(X2XT_DELAY(TER).LT.0) X2XT_DELAY(TER)= 0
C
C       ***** End V07 changes *****
C
C-      WRITE(CLIN21,921) (ASFBYT(S),S=SMONO,ESUNO)
        WRITE(CLIN21,921) TEMPSTR, GVTID, (ASFBYT(S),S=SMONO,ESUNO)     !V14
C-      WRITE(CLIN22,922) (ASFBYT(S),S=SMONC,ESUNC)
        WRITE(CLIN22,922) MIN (X2XT_NETWORK_DELAY(TER),                 !V14
     *                         MAX_DISPLAYED_NETWORK_DELAY),            !V14
     *                    MIN (X2XT_DELAY(TER)+X2XT_NETWORK_DELAY(TER), !V14
     *                         MAX_DISPLAYED_TOTAL_DELAY),              !V14
     *                    (ASFBYT(S),S=SMONC,ESUNC)                     !V14
8000    CONTINUE                                                        !V14
C
C     ================== FORMAT STATEMENTS ===============
C
901     FORMAT('Hotline Snapshot For ',7A2)
902     FORMAT('Name ',T10,<LNAME>A1,T50,'Agent',T60,I7.7,' - 'A7)
903     FORMAT('Address ',T10,<LSTRT>A1,T50,'Terminal',T60,I5.5)
904     FORMAT(T10,<LCITY>A1)
905     FORMAT('Phone',T10,<LTELE>A1,T50,'Last Tra',T60,I9)
906     FORMAT('Contact',T10,<LCONT>A1,T50,'Last Wag',T60,I9)
9071    FORMAT(1X,'<< MODE >> ',A8,' ID ',I4)
9072    FORMAT(1X,'<< MODE >> ',A8,8(' '))
9073    FORMAT(80(' '))
C-908   FORMAT(14X,'Count',6X,'Amount',2X,9('='),' COMTYP  ',
C-   *         A16,1X,10('=')) 
908     FORMAT(14X,'Count',6X,'Amount',2X,' COMTYP :',        !V14
     *         A8,2X,'CLASS (',I2.2,') :',A12)
909     FORMAT(A8,1X,I1,I8,2X,A11,2X,
     *         T66,'cnfter ',I6)
910     FORMAT(33(' '),'ssta ',A8,3X,'last ',I2.2,':',
     *         I2.2,':',I2.2,3X,'actter ',I6)

C910     FORMAT('Psv. Ret. ',I8,2X,A11,2X,'ssta ',A8,3X,'last ',I2.2,':',
C     *         I2.2,':',I2.2,3X,'actter ',I6)
911     FORMAT('Net sales ',I8,2X,A11,2X,'derr',6X,A3,3X,'errcnt',1X,I6,
     *         3X,'errcde',1X,Z6)
C
912     FORMAT('Cancels   ',I8,2X,A11,2X,'dact',6X,A3,3X,'ackcnt',1X,I6,
     *         2X,'*',A7,1X,I5)
913     FORMAT('Discount  ',I8,2X,A11,2X,'actcnt ',I6,
     *         3X,'rescnt',1X,I6,2X,'*',A7)
914     FORMAT('Cashes    ',I8,2X,A11,2X,'tsta ',A8,3X,'last ',
     *         I2.2,':',I2.2,':',I2.2,2X,'*',A7,1X,I5)
C
915     FORMAT('Returns   ',I8,2X,A11,2X,'drop',7X,A2,16X,
     *         2X,'*',A7,1X,I5)
C-916   FORMAT('Refunds   ',I8,2X,A11,2X,A7,I7,3X,'addr ',16A)
916     FORMAT('Refunds   ',I8,2X,A11,2X,A7,I7,2X,                    !V14
     *         'stn',1X,I5,2X,'adr ',<ADR_LEN>A1)                     !V14
C-917   FORMAT('Commission',10X,A11,2X,'x2xtpro',I7,3X,'stn/prt',
C-   *          I4,'/',I2)
917     FORMAT('Commission',10X,A11,2X,'x2xtpro',1X,I6,2X,            !V14
     *         'BCST1 Cf',1X,A3,2X,'BCST1 Ac',1X,A3)                  !V14
C-918   FORMAT('Adjustment',10X,A11,2X,'Load',3X,A4,5X,'Segment',
C-   *          2X,I4,3X,'%done',1X,F6.2)
918     FORMAT('Adjustment',10X,A11,2X,'Segment',1X,I4,2X,            !V14
     *          '% Done',1X,F6.2,2X,'load',1X,'(',A1,') ',A4)         !V14
C-919   FORMAT('Ticket Charge',7X,A11)
919     FORMAT('Ticket Charge',7X,A11,2X,'Active Load: ',Z4.4,2X,     !V14
     *         'Inactive load: ',Z4.4)                                !V14
920     FORMAT('Amount due',7X,A14,9X,A3,3X,A3,3X,A3,3X,A3,3X,
     *         A3,3X,A3,3X,A3)
C-921   FORMAT(31(' '),1X,'Open: ',7(1X,2A1,':',2A1))
921     FORMAT(A7,12A1,12(' '),1X,'Open: ',7(1X,2A1,':',2A1))         !V14
C-922   FORMAT(31(' '),1X,'Close:',7(1X,2A1,':',2A1))
922     FORMAT('delay:',2X,I6.5,'/',I5.5,11(' '),                     !V14
     *         1X,'Close:',7(1X,2A1,':',2A1))                         !V14
923     FORMAT(1X,'Game number ',I2,' is not active!')
C
1501    FORMAT(5A4,' read error ',I4,' record ',I4)
1502    FORMAT(' Message ',I4,' queued to terminal ',I4)
1504    FORMAT(1X,'Can''t send to broadcast server!')
9230    FORMAT(1X,'Soft Reset command sent to station ',I5)
9232    FORMAT(1X,'Hard Reset command sent to station ',I5)
9250    FORMAT(1X,'Statistics request send to station ',I5)
9923    FORMAT('Input error ')
9924    FORMAT('Value error  ')
9925    FORMAT('BRONUM invalid for GVT')                                  !V11
9926    FORMAT('This terminal is not on the X2X network ')
9930    FORMAT('Invalid Lapb Index ',I5)                                  !V06
9940    FORMAT('Invalid Circuit ',I5)                                     !V06
10023   FORMAT('ID is the same as ',I)                                    !V07
9941	FORMAT('Invalid station class for GVTID command')
C
        RETURN
        END
