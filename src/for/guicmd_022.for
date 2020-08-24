C GUICMD_022.FOR
C
C V02 01-SEP-05 FRP Modify for Natal 2005: Allow to enter 0 in winning serie.
C V01 28-FEB-2001 HXK INITIAL RELEASE
C
C PASSIVE WINNING NUMBERS ENTRY COMMAND
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
	SUBROUTINE GUICMD_022(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:GUIFIL.DEF'
C
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 ST
	INTEGER*4 NUM_COLS, NUM_ROWS
	INTEGER*4 PAR,GIND,DRW,GNUM
	INTEGER*4 YEAR,WEEK
	INTEGER*4 N_WINNING_NUMS
	INTEGER*4 BUF(CDLEN)
	INTEGER*4 STATUS
	CHARACTER*40 STATUS_STR
	INTEGER*4 IND
	INTEGER*4 NUM_DIVS
        INTEGER*4 PRG_DAY

        INTEGER*4 WINSUM,DIVS
        INTEGER*4 EMIOFF,INDEMI,QTD,NBR,FLAG,NUMSER,INISER
        INTEGER*4 DPAHLD(PAGNBR,PAGDIV),DPAHWSER
	LOGICAL   ON_MEMORY
C
	RET_CODE = 0
	STATUS   = 0
	STATUS_STR = ' '
C
C
	CALL GUI_GETPARAMS(OUTBUF,ST)
	IF(ST.NE.0) THEN
	    RET_CODE = 11
	    RETURN
	ENDIF

	PAR = GUI_ARGVAL(1)
	IF(PAR.LT.1 .OR. PAR.GT.2) THEN
	    STATUS_STR = 'Invalid mode'
	    STATUS = 102
	    GOTO 100
	ENDIF

	GIND = GUI_ARGVAL(2)
	IF(GIND.LT.1 .OR. GIND.GT.MAXIND) THEN
	    STATUS_STR = 'Invalid game index'
	    STATUS = 2
	    GOTO 100
	ENDIF

	GNUM = GTNTAB(TPAS,GIND)

	WEEK = GUI_ARGVAL(3) !week
	YEAR = GUI_ARGVAL(4) !year
 
        IF(WEEK.LT.1 .OR. WEEK.GT.53 .OR.
     *     YEAR.LT.2001 .OR. YEAR.GT.2099) THEN
	   STATUS_STR = 'Invalid week or year'
	   STATUS = 103
	   GOTO 100
	ENDIF
C	IF(PASSTS(PASCURDRW(GIND),GIND).LE.GAMOPN) THEN  !postponed draw
C           !get draw corresponding to week/year
C           DRW = GETDRW(YEAR,WEEK,GNUM)           
C           IF(DRW.LE.0) THEN
C	      STATUS_STR = 'Invalid draw'
C	      STATUS = 104
C	      GOTO 100
C           ENDIF
C	   IF(DRW.LT.PAS_DRW_OFFSET+1 .OR. DRW.GT.9999) THEN
C	      STATUS_STR = 'Invalid emission'
C	      STATUS = 3
C	      GOTO 100
C	   ENDIF
	IF(PASSTS(PASCURDRW(GIND),GIND).LT.GAMBFD) THEN
	   STATUS_STR = 'Sales not closed'
	   STATUS = 4
	   GOTO 100
	ELSEIF(PASSTS(PASCURDRW(GIND),GIND).GE.GAMENV) THEN
	   STATUS_STR = 'Drawing over'
	   STATUS = 5
	   GOTO 100 
	ELSE  !current draw is to have results entered
	   DRW = PASEMIS(PASCURDRW(GIND),GIND)
	ENDIF
	ON_MEMORY = .FALSE.
	DO INDEMI = 1, PAGEMI
	   IF(DRW.EQ.PASEMIS(INDEMI,GIND)) THEN
	      ON_MEMORY = .TRUE.
	      EMIOFF    = INDEMI
	   ENDIF
	ENDDO

	GNUM = GTNTAB(TPAS,GIND)

	IF(PAR.EQ.1) THEN
	   ! DPASTS = GAMBFD  this is overwritten!!!
	   IF(ON_MEMORY) THEN
              CALL GAMLOGPAS(EMIOFF,GIND,DPAREC,PASBLK)
	   ELSE
	      CALL READW(GAMFDB(1,GNUM),DRW-PAS_DRW_OFFSET,DPAREC,ST)
	      IF(ST.NE.0) THEN
	         STATUS_STR = 'File read error'
	         STATUS = 7
	         GOTO 100
	      ENDIF
	      IF(DPASTS.NE.GAMENV) THEN
	         STATUS_STR = 'Emission has bad status'
	         STATUS = 8
	         GOTO 100
	      ENDIF
	   ENDIF
        ENDIF

	WINSUM = 0
	QTD    = 0

	NUM_DIVS = 0

	DO DIVS = 1,DPADIV
	   NUM_DIVS = NUM_DIVS + DPAWNUM(DIVS)
	ENDDO

	N_WINNING_NUMS = GUI_ARGVAL(5)

	IF(N_WINNING_NUMS.LT.1 .OR. 
     *     N_WINNING_NUMS.GT.NUM_DIVS) THEN
	    STATUS_STR = 'Number of winning divs incorrect'
	    STATUS = 9
	    GOTO 100
	ENDIF

	IND = 6

	IF(PAR.EQ.1 .AND. DPASTS.EQ.GAMBFD) THEN
	   DO DIVS = 1, DPADIV
              IF(DPAWNUM(DIVS).GT.0) THEN
		 DO NBR=1,DPAWNUM(DIVS)
	            IF(GUI_ARGVAL(IND).LT.0 .OR.
     *                 GUI_ARGVAL(IND).GT.DPANUMTCK - 1) THEN
		       STATUS_STR = 'Winning number out of range'
	               STATUS = 10
	               GOTO 100
	            ELSE
		       DPAWIN(NBR,DIVS) = GUI_ARGVAL(IND)
	               WINSUM = WINSUM + GUI_ARGVAL(IND)
	               QTD    = QTD + 1
		       IND    = IND + 1
	            ENDIF
		 ENDDO
	      ENDIF
	   ENDDO
	   INISER = 0
	   NUMSER = DPANUMSER
	   IF(GIND.EQ.PSBPOP .OR. DPAEMT.EQ.EM_EXT) THEN
	      IF(GIND.EQ.PSBPOP) THEN
	        INISER = 1
	        NUMSER = DPANOFFRA
	      ENDIF
	      IF(GUI_ARGVAL(IND).GE.INISER .AND. GUI_ARGVAL(IND).LE.NUMSER) THEN
	         DPAWSER = GUI_ARGVAL(IND)
	         WINSUM = WINSUM + DPAWSER
	         QTD = QTD + 1
	      ELSE
		 STATUS_STR = 'Invalid series'
	         STATUS = 11
	         GOTO 100
	      ENDIF
	   ENDIF
           IND = IND + 1
	   IF(DPAEMT.EQ.EM_ESP) THEN
	      EXTPRZ = GUI_ARGVAL(IND)
	      IF(EXTPRZ.LT.0 .OR. EXTPRZ.GT.1) THEN
		 STATUS_STR = 'Invalid special prize flag'
	         STATUS = 12
	         GOTO 100
	      ENDIF
	   ENDIF
	   IND = IND + 1
	   IF(WINSUM.NE.GUI_ARGVAL(IND)) THEN
	      STATUS_STR = 'Invalid winning numbers sum'
	      STATUS = 13
	      GOTO 100
	   ENDIF
	   IND = IND + 1
C
    	   DPASTS = GAMEN1
           CUR_GIND = GIND
           CUR_GTYP = TPAS
           CUR_DRAW = DRW
C
C 	   change game status
C
C	   IF(ON_MEMORY) THEN
C      	      BUF(1) = 1
C      	      BUF(2) = DPASTS
C      	      BUF(3) = TCPAS
C      	      BUF(6) = 'GUI '
C      	      BUF(8) = GIND
C      	      BUF(9) = DRW                          ! EMISSION NUMBER
C      	      CALL QUECMD(BUF,ST)
C	      IF(ST.NE.0) THEN
C                 DPASTS=GAMBFD
C	         STATUS_STR = 'Host CHANGE GAME STATUS command failed'
C	         STATUS = 109
C	         GOTO 100
C	      ENDIF
C	   ELSE
C 	      CALL WRITEW(GAMFDB(1,GNUM),DRW-PAS_DRW_OFFSET,DPAREC,ST)
C	      IF(ST.NE.0) THEN
C	         STATUS_STR = 'CHANGE GAME STATUS file write error'
C	         STATUS = 129
C	         GOTO 100
C	      ENDIF
C	   ENDIF

	ELSEIF(PAR.EQ.2 .AND. DPASTS.EQ.GAMEN1) THEN
	   DO DIVS = 1, DPADIV
              IF(DPAWNUM(DIVS).GT.0) THEN
		 DO NBR=1,DPAWNUM(DIVS)
	            IF(GUI_ARGVAL(IND).LT.0 .OR.
     *                 GUI_ARGVAL(IND).GT.DPANUMTCK - 1) THEN
		       STATUS_STR = 'Winning number out of range'
	               STATUS = 20
	               GOTO 100
	            ELSE
		       DPAHLD(NBR,DIVS) = GUI_ARGVAL(IND)
	               WINSUM = WINSUM + GUI_ARGVAL(IND)
	               QTD    = QTD + 1
		       IND    = IND + 1
	            ENDIF
		 ENDDO
	      ENDIF
	   ENDDO
	   IF(GIND.EQ.PSBPOP .OR. DPAEMT.EQ.EM_EXT) THEN
	      INISER = 0
	      NUMSER = DPANUMSER
	      IF(GIND.EQ.PSBPOP) THEN
	        INISER = 1
	        NUMSER = DPANOFFRA
	      ENDIF

	      IF(GUI_ARGVAL(IND).GE.INISER .AND. GUI_ARGVAL(IND).LE.NUMSER) THEN
	         DPAHWSER = GUI_ARGVAL(IND)
	         WINSUM = WINSUM + DPAHWSER
	         QTD = QTD + 1
	      ELSE
		 STATUS_STR = 'Invalid series'
	         STATUS = 21
	         GOTO 100
	      ENDIF
	   ENDIF
	   IND = IND + 1
	   FLAG = 0
	   IF(DPAEMT.EQ.EM_ESP) THEN
	      FLAG = GUI_ARGVAL(IND)
	      IF(FLAG.LT.0 .OR. FLAG.GT.1) THEN
		 STATUS_STR = 'Invalid special prize flag'
	         STATUS = 22
	         GOTO 100
	      ENDIF
	   ENDIF
	   IND = IND + 1
	   IF(WINSUM.NE.GUI_ARGVAL(IND)) THEN
	      STATUS_STR = 'Invalid winning numbers sum'
	      STATUS = 23
	      GOTO 100
	   ENDIF
	   IND = IND + 1

	   DPASTS = GAMENV

           DO DIVS = 1,DPADIV
	      DO NBR = 1,DPAWNUM(DIVS)
	         IF(DPAWIN(NBR,DIVS).NE.DPAHLD(NBR,DIVS)) THEN
                    DPASTS = GAMBFD
		    STATUS_STR = 'Verification error'
                    STATUS = 30
	            GOTO 100
		 ENDIF
	      ENDDO
	   ENDDO
	   IF(GIND.EQ.PSBPOP .OR. DPAEMT.EQ.EM_EXT) THEN
	      IF(DPAWSER.NE.DPAHWSER) THEN
                  DPASTS = GAMBFD
		  STATUS_STR = 'Verification error'
                  STATUS = 31
	          GOTO 100
	      ENDIF
	   ENDIF
	   IF(DPAEMT.EQ.EM_ESP) THEN
	      IF(FLAG.NE.EXTPRZ) THEN
                  DPASTS = GAMBFD
		  STATUS_STR = 'Verification error'
                  STATUS = 32
	          GOTO 100
	      ENDIF
	   ENDIF
C
C SET WINNING NUMBERS
C
           CALL FASTSET(0,BUF,CDLEN)
	   DO DIVS = 1,DPADIV
	        DO NBR = 1,DPAWNUM(DIVS)
	           BUF(1)  = 4
	           BUF(2)  = DPAWIN(NBR,DIVS)
		   BUF(3)  = TCPAS
		   BUF(6)  = 'GUI '
		   BUF(8)  = NBR
		   BUF(9)  = GIND
		   BUF(10) = DIVS
		   BUF(11) = DRW
                   CALL QUECMD(BUF,ST)
	           IF(ST.NE.0) THEN
                      DPASTS=GAMBFD
	              STATUS_STR = 'Host SET NUMBERS command failed'
	              STATUS = 105
	              GOTO 100
	           ENDIF
	           CALL XWAIT(50,1,ST)
	        ENDDO
	   ENDDO
C
C CHANGE PURGE CDC
C
           CALL GET_PAS_PRG_DAY(PRG_DAY, DPAESD, DPAMAXDAYPAY, GNUM)
           BUF(1) = 6
           BUF(2) = PRG_DAY
           BUF(3) = TCPAS
           BUF(6) = 'GUI '
           BUF(8) = GIND  
           BUF(9) = DRW              ! EMISSION NUMBER
           CALL QUECMD(BUF,ST)
	   IF(ST.NE.0) THEN
              DPASTS=GAMBFD
	      STATUS_STR = 'Host CHANGE PURGE CDC command failed'
	      STATUS = 106
	      GOTO 100
	   ENDIF
C
C SET WINNING SERIE (IF POPULAR OR EXTRAORDINARIA)
C

           IF(GIND.EQ.PSBPOP .OR. DPAEMT.EQ.EM_EXT) THEN
              BUF(1) = 5
              BUF(2) = DPAWSER
              BUF(3) = TCPAS
              BUF(6) = 'GUI '
              BUF(8) = GIND
              BUF(9) = DRW
              CALL QUECMD(BUF,ST)
	      IF(ST.NE.0) THEN
                 DPASTS=GAMBFD
	         STATUS_STR = 'Host SET SERIE command failed'
	         STATUS = 107
	         GOTO 100
	      ENDIF
           ENDIF
C
C SET PRIZE VALUE IF ESPECIAL
C  
           IF(DPAEMT.EQ.EM_ESP.AND.DPAEXSHV(1).GT.0.AND.FLAG.EQ.1) THEN
              BUF(1) = 7
              BUF(2) = DPAEXSHV(1)
              BUF(3) = TCPAS
              BUF(6) = 'GUI '
              BUF(8) = GIND
              BUF(9) = DRW
              CALL QUECMD(BUF,ST)
	      IF(ST.NE.0) THEN
                 DPASTS=GAMBFD
	         STATUS_STR = 'Host SET SPECIAL PRIZE command failed'
	         STATUS = 108
	         GOTO 100
	      ENDIF
           ENDIF
C
C CHANGE GAME STATUS
C
           BUF(1) = 1
           BUF(2) = GAMDON
           BUF(3) = TCPAS
           BUF(6) = 'GUI '
           BUF(8) = GIND
           BUF(9) = DRW                          ! EMISSION NUMBER
           CALL QUECMD(BUF,ST)
	   IF(ST.NE.0) THEN
              DPASTS=GAMBFD
	      STATUS_STR = 'Host CHANGE GAME STATUS command failed'
	      STATUS = 109
	      GOTO 100
	   ENDIF

	ELSEIF(DPASTS.EQ.GAMENV) THEN
            STATUS_STR = 'Results already entered and verified' 
	    STATUS = 112
	    GOTO 100
	ELSE
	    STATUS_STR = 'Bad request combination'
	    STATUS = 113
	    GOTO 100
	ENDIF
C
C SEND DATA TO GUI
C
100	CONTINUE

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
