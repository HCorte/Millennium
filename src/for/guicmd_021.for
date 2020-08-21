C GUICMD_021.FOR
C
C V02 17-MAR-2015 SCML DO NOT BROADCAST BROTEXT MESSAGES TO X2X NETWORK
C V01 08-FEB-2001 HXK INITIAL RELEASE FOR MILLENNIUM
C
C COMMAND: [CLOSE GAME or SET GAME CLOSE TIME]
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
	SUBROUTINE GUICMD_021(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECUSE.DEF'

        INCLUDE 'INCLIB:NBRCOM.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:TGLCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:SCRCOM.DEF'
        INCLUDE 'INCLIB:WITCOM.DEF'
        INCLUDE 'INCLIB:TSLCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:DBLCOM.DEF'
        INCLUDE 'INCLIB:CPLCOM.DEF'
        INCLUDE 'INCLIB:SSCCOM.DEF'
        INCLUDE 'INCLIB:TRPCOM.DEF'
        INCLUDE 'INCLIB:STRCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'        
C
CC
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 ST,I
	INTEGER*4 NUM_COLS, NUM_ROWS
	INTEGER*4 GNUM,GTYP
	INTEGER*4 GIND
	INTEGER*4 BUF(CDLEN)
	INTEGER*4 STATUS
	CHARACTER*40 STATUS_STR
	INTEGER*4 OPT,LEN
	INTEGER*4 BITMAP(2)

	INTEGER*4 NTIME,HH,MM,SS
	CHARACTER*8 ATIME
	BYTE        BTIME(8)
	EQUIVALENCE(ATIME,BTIME)
	INTEGER*4 CERR,EXT

        INTEGER*4   O_GCLOSE, O_CTIME, O_CTIME_LATER
        PARAMETER   (O_GCLOSE = 1)  ! Game CLOSE
        PARAMETER   (O_CTIME  = 2)  ! Change closing time, and make today
        PARAMETER   (O_CTIME_LATER = 3)  ! Change closing TIME, do not change day
C
C
	RET_CODE = 0
	STATUS   = 0
	STATUS_STR = ' '
C
C
	BITMAP(1) = 0
	BITMAP(2) = 0
C
C
	CALL GUI_GETPARAMS(OUTBUF,ST)
	IF(ST.NE.0) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF

	GNUM = GUI_ARGVAL(1)
	IF(GNUM.LT.1 .OR. GNUM.GT.MAXGAM) THEN
	    RET_CODE = 11
	    RETURN
	ENDIF

        GTYP = GNTTAB(GAMTYP,GNUM)
        GIND = GNTTAB(GAMIDX,GNUM)

	OPT = GUI_ARGVAL(2)
	IF(OPT.LT.1 .OR. OPT.GT.3) THEN
	    STATUS_STR = 'Invalid option'
	    STATUS = 3
	    GOTO 100
	ENDIF

	NTIME = 0
	IF(OPT.EQ.2 .OR. OPT.EQ.3) THEN
	   LEN = GUI_ARGLEN(3)
	   IF(LEN.NE.8) THEN
	      STATUS_STR = 'Time parameter incorrect length'
	      STATUS = 11
	      GOTO 100
	   ENDIF
           DO I=1,8 
              BTIME(I) = B_GUI_ARGCHAR(I,3)
           ENDDO
	   CALL ASCBIN(BTIME,1,2,HH,CERR)
	   IF(CERR.NE.0 .OR. HH.LT.0 .OR. HH.GT.23) THEN
	      WRITE(STATUS_STR,90012) HH,CERR
90012         FORMAT('Invalid hours - ',I8,' - ',I8)
	      ! STATUS_STR = 'Invalid hours'
	      STATUS = 12
	      GOTO 100
	   ENDIF
	   CALL ASCBIN(BTIME,4,2,MM,CERR)
	   IF(CERR.NE.0 .OR. MM.LT.0 .OR. MM.GT.59) THEN
	      STATUS_STR = 'Invalid minutes'
	      STATUS = 13
	      GOTO 100
	   ENDIF
	   CALL ASCBIN(BTIME,7,2,SS,CERR)
	   IF(CERR.NE.0 .OR. SS.LT.0 .OR. SS.GT.59) THEN
	      STATUS_STR = 'Invalid seconds'
	      STATUS = 14
	      GOTO 100
	   ENDIF
	   NTIME = HH*3600 + MM*60 + SS
	ENDIF

	IF(NTIME.LT.0 .OR.NTIME.GT.86399) THEN
	    STATUS_STR = 'Invalid time'
	    STATUS = 19
	    GOTO 100
	ENDIF

        CALL FASTSET(0,BUF,CDLEN)

	IF(GTYP.EQ.TLTO) THEN
      	   IF(OPT.EQ.O_GCLOSE) THEN
              CALL SET_CLOSE(LTOESD(GIND), LTOSTS(GIND),EXT)
              IF (EXT.EQ.0) THEN
                 CALL BSET(BITMAP,GNUM)
                 BUF(3)=TCLTO
              ELSE
	         STATUS_STR = 'Unable to complete'
	         STATUS = 3
	         GOTO 100
              ENDIF
           ELSEIF (OPT.EQ.O_CTIME .OR. OPT.EQ.O_CTIME_LATER) THEN
              CALL SET_TIME(LTOTIM(GIND), LTOESD(GIND), NTIME, OPT, EXT)
              GOTO 100
           ELSE
	      STATUS_STR = 'Incorrect option selected'
	      STATUS = 4
	      GOTO 100
           ENDIF
	ELSEIF(GTYP.EQ.TSPT) THEN
           IF(OPT.EQ.O_GCLOSE) THEN
              CALL SET_CLOSE(SPTESD(GIND), SPTSTS(GIND),EXT)
              IF (EXT.EQ.0) THEN
                 CALL BSET(BITMAP,GNUM)
                 BUF(3)=TCSPT
              ELSE
                 STATUS_STR = 'Unable to complete'
                 STATUS = 3
                 GOTO 100
              ENDIF
           ELSEIF (OPT.EQ.O_CTIME .OR. OPT.EQ.O_CTIME_LATER) THEN
              CALL SET_TIME(SPTTIM(GIND), SPTESD(GIND), NTIME, OPT, EXT)
              GOTO 100
           ELSE
              STATUS_STR = 'Incorrect option selected'
              STATUS = 4
              GOTO 100
           ENDIF
        ELSEIF(GTYP.EQ.TKIK) THEN
           IF(OPT.EQ.O_GCLOSE) THEN
              CALL SET_CLOSE(KIKESD(GIND), KIKSTS(GIND),EXT)
              IF (EXT.EQ.0) THEN
                 CALL BSET(BITMAP,GNUM)
                 BUF(3)=TCKIK
              ELSE
                 STATUS_STR = 'Unable to complete'
                 STATUS = 3
                 GOTO 100
              ENDIF
           ELSEIF (OPT.EQ.O_CTIME .OR. OPT.EQ.O_CTIME_LATER) THEN
              CALL SET_TIME(KIKTIM(GIND), KIKESD(GIND), NTIME, OPT, EXT)
              GOTO 100
           ELSE
              STATUS_STR = 'Incorrect option selected'
              STATUS = 4
              GOTO 100
           ENDIF
        ELSEIF(GTYP.EQ.TTGL) THEN
           IF(OPT.EQ.O_GCLOSE) THEN
              CALL SET_CLOSE(TGLESD(GIND), TGLSTS(GIND),EXT)
              IF (EXT.EQ.0) THEN
                 CALL BSET(BITMAP,GNUM)
                 BUF(3)=TCTGL
              ELSE
                 STATUS_STR = 'Unable to complete'
                 STATUS = 3
                 GOTO 100
              ENDIF
           ELSEIF (OPT.EQ.O_CTIME .OR. OPT.EQ.O_CTIME_LATER) THEN
              CALL SET_TIME(TGLTIM(GIND), TGLESD(GIND), NTIME, OPT, EXT)
              GOTO 100
           ELSE
              STATUS_STR = 'Incorrect option selected'
              STATUS = 4
              GOTO 100
           ENDIF
        ELSEIF(GTYP.EQ.TPAS) THEN
          IF(OPT.EQ.O_GCLOSE) THEN
             CALL SET_CLOSE(PASESD(PASCURDRW(GIND),GIND), PASSTS(PASCURDRW(GIND),GIND),EXT)
             IF (EXT.EQ.0) THEN
                CALL BSET(BITMAP,GNUM)
                BUF(3)=TCPAS
                BUF(9)=PASEMIS(PASCURDRW(GIND),GIND)
             ELSE
                STATUS_STR = 'Unable to complete'
                STATUS = 3
                GOTO 100
             ENDIF
          ELSEIF (OPT.EQ.O_CTIME .OR. OPT.EQ.O_CTIME_LATER) THEN
             CALL SET_TIME(PASTIM(PASCURDRW(GIND),GIND), PASESD(PASCURDRW(GIND),GIND), NTIME, OPT, EXT)
             GOTO 100
          ELSE
             STATUS_STR = 'Incorrect option selected'
             STATUS = 4
             GOTO 100
          ENDIF
	ENDIF

	IF(OPT.EQ.O_GCLOSE) THEN
            BUF(1) = 1
            BUF(2) = GAMBFD
            BUF(6) = 'GUI '
            BUF(8) = GIND
            CALL QUECMD(BUF,ST)
	    IF(ST.NE.0) THEN
               STATUS_STR = 'Game close command failed'
               STATUS = 5
               GOTO 100
	    ENDIF
	ENDIF
C
C SEND NEW GAME MESSAGE
C	
C        IF(BITMAP(1).NE.0.OR.BITMAP(2).NE.0) THEN       !V02
C            BUF(1)=6                                    !V02
C            BUF(2)=BITMAP(1)                            !V02
C            BUF(3)=TCSPE                                !V02
C            BUF(6)='GUI '                               !V02
C            BUF(8)=BITMAP(2)                            !V02
C            CALL QUECMD(BUF,ST)                         !V02
C	    IF(ST.NE.0) THEN                                   !V02
C               STATUS_STR = 'Game close command failed' !V02
C               STATUS = 5                               !V02
C               GOTO 100                                 !V02
C	    ENDIF                                              !V02
C        ENDIF                                           !V02

C SEND DATA TO GUI
C
100	CONTINUE
C
	CALL GUIARG_INIT()
C
	NUM_COLS = 2
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
C STATUS BACK
C
	CALL GUIARG_INT4(OUTBUF,STATUS)
	CALL GUIARG_CHAR(OUTBUF,%REF(STATUS_STR),40)	
C
	CALL GUIARG_SET_MESLEN(MES_LEN)
C
	RETURN
	END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C SUBROUTINE TO CHECK WHETHER GAME/ROW CAN BE CLOSED
C
C RETURN VALUES
C       0   OK TO CLOSE
C       -1  NOT OK

C=======OPTIONS /CHECK=NOOVERFLOW/EXTEND
        SUBROUTINE SET_CLOSE(DATE, STATUS, EXT)
        IMPLICIT NONE

        INCLUDE 'INCLIB:GLOBAL.DEF /NOLIST'
        INCLUDE 'INCLIB:CONCOM.DEF /NOLIST'

        INTEGER*4   DATE, STATUS, EXT

        EXT = -1

        IF (DATE.NE.DAYCDC .OR. STATUS.GE.GAMBFD) RETURN 
        EXT = 0
        RETURN
        END
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C SUBROUTINE TO CHANGE CLOSING TIME FOR GAME/ROW
C
C RETURN VALUES
C       0   TIME CHANGED
C       -1  NO CHANGE MADE

C=======OPTIONS /CHECK=NOOVERFLOW/EXTEND
        SUBROUTINE SET_TIME(TIME, DATE, NTIME, EXT, OPT)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSEXTRN.DEF  /NOLIST'

        INCLUDE 'INCLIB:GLOBAL.DEF  /NOLIST'
        INCLUDE 'INCLIB:CONCOM.DEF  /NOLIST'
        INCLUDE 'INCLIB:DATBUF.DEF  /NOLIST'

        INTEGER*4   TIME, DATE, EXT
        INTEGER*4   TMPTIME
        INTEGER*4   OPT
	INTEGER*4   NTIME

        INTEGER*2   DBUF(LDATE_LEN)
        INTEGER*2   DBUF2(LDATE_LEN)

        DBUF(5)  = DATE
        DBUF2(5) = DAYCDC

        CALL LCDATE(DBUF)
        CALL LCDATE(DBUF2)

        TMPTIME  = NTIME

        EXT = -1

        IF(OPT.EQ.3) THEN
           TMPTIME = TMPTIME + '40000000'X
        ENDIF

        TIME = TMPTIME
        EXT  = 0

        RETURN
        END
 
