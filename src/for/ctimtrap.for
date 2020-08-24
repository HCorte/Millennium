C
C SUBROUTINE CTIMTRAP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CTIMTRAP.FOV                                 $
C  $Date::   17 Apr 1996 12:45:32                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C *** Pre-Baseline Source - ctimtrap.for ***
C
C V02 07-JAN-91 KWP INITIAL VAX VERSION
C V01 01-AUG-90 XXX RELEASED FOR VAX
C V00 30-DEC-89 MBK ORIGINAL RELEASE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1993 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose: Timer tick  routine:
C          - If there is any timeout scheduled decrease the counter
C          - Start new interval
C          - Release (Reset wait bit in TSW)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE CTIMTRAP(PARTIM)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:CTLCOM.DEF'
	INCLUDE 'INCLIB:CTLEVN.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:X2TDBH.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	BLSIZ,
     *			BLTYP,
     *			BTYPE,
     *			BUF,
     *			CTLX2XTOUT,
     *			CTYPE,
     *			DLEN,
     *			DOFF,
     *			DSAP,
     *			DTYPE,
     *			DUM1,
     *			DUM2,
     *			DUM3,
     *			FLAGS,
     *			GAME_CHECK,
     *			I,
     *			LEN,
     *			MARK,
     *			MYSAP,
     *			NODE,
     *			PARTIM,
     *			RESULT,
     *			RSEQ,
     *			SAP,
     *			SBUF,
     *			SMODE,
     *			SSAP,
     *			ST,
     *			STATUS,
     *			SYSTEM,
     *			TOSAP
C
	CHARACTER*1	BELL	/Z07/	! BEEP !
C
C EXTERNAL DECLARATIONS
C
	LOGICAL*4	NETNODEU
C
	EXTERNAL	NETNODEU
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	IF (PARTIM .EQ. 0) THEN
	  DO 100 SAP = 1, CTLMAXSAP
	    IF (CTLSAPTOUT(SAP) .GT. 0) THEN
	      CTLSAPTOUT(SAP) = CTLSAPTOUT(SAP) - 1
	      IF (CTLSAPTOUT(SAP) .EQ. 0 .AND. CTLTEST .NE. 0)
     *          TYPE *,IAM(), '*** TOUT ***[', SAP, CTLSAPTOUT(SAP), ']'
	    ENDIF
100	  CONTINUE
	ELSE
	  CALL OPS('*** CTL ILLEGAL TIMER TRAP *** ', PARTIM, 0)
	ENDIF
C
C THE FOLLOWING CODE WAS PREVIOUSLY IN THE MAIN LINE OF CTLPRO
C WORK ONLY ON THE PRIMARY OR BACKUP SYSTEM
C
D	TYPE *, 'NODEID = ', NODEID
	IF (NODEID .EQ. 0) GOTO 9999
C
C CHECK DAY STATUS
C
	IF (DAYSTS .EQ. DSCLOS) THEN
C
C CLOSE YOUR OWN SAP
C
	  CALL LANGETX(BUF, STATUS)
	  IF (STATUS .NE. 2) THEN
	    MYSAP = CTLSAPSYS(NODEID)
C
	    LANBUF(LANBTYP,    BUF) = LTYPCMD
	    LANBUF(LANDATAF,   BUF) = CCLOSE
	    LANBUF(LANDATAF+1, BUF) = CCOMMAND
	    LANBUF(LANDATAF+2, BUF) = MYSAP
	    LANBUF(LANDATAF+3, BUF) = CTLAPLQUE
C
	    CTLSAPTOUT(MYSAP) = 0
	    CTLSAPSTA(MYSAP)  = CTLSTADOWN
C
	    CALL SNDLAN(DUM1, DUM2, BUF, DUM3)
	    CALL XWAIT(10, 2, STATUS)
	  ELSE
	    TYPE *, IAM(), '*** BUFFER ALLOCATION ERROR ***'
	  ENDIF
	ENDIF
C
	MYSAP = CTLSAPSYS(NODEID)
C
C SET LOCAL X2X FLAG
C
D	TYPE *, 'GAME_CHECK = ', GAME_CHECK
C
	GAME_CHECK = X2X_GAME_CHECK
	IF (GAME_CHECK .EQ. X2X_GAMEC_UP) THEN
	  CTLX2XLOC      = CTLX2XOK
	  X2X_GAME_CHECK = X2X_GAMEC_REQ
	  CTLX2XTOUT     = CTLX2XMAX
C
	ELSEIF (GAME_CHECK .EQ. X2X_GAMEC_REQ) THEN
	  IF (CTLX2XTOUT .LE. 0) THEN
	    CTLX2XLOC      = CTLX2XBAD
	    X2X_GAME_CHECK = X2X_GAMEC_REQ
	    CTLX2XTOUT     = CTLX2XMAX
	    IF (X2X_GAME_STATE .EQ. X2X_GAMES_UP)
     *        CALL OPS('*** X2XMGR NOT RESPONDING ***', 
     *	               X2X_GAME_CHECK, CTLX2XLOC)
	  ELSE
	    CTLX2XTOUT = CTLX2XTOUT - 1
	  ENDIF
C
	ELSE
	  CALL OPS('*** INVALID X2X GAME CHECK STATUS ***', 
     *	           GAME_CHECK, CTLX2XLOC)
	ENDIF
C
C I'M A CONTROL PROGRAM
C WHO I TALK TO IS A DIFFERENT MATTER
C MAY BE ANOTHER CONTROL PROGRAM OR PRI COM SAP
C
	IF (NODEID .EQ. NETMASTER(WAYINP) .OR.
     *      NODEID .EQ. NETBACKUP(WAYINP)) THEN
C
	  IF (CTLSAPSTA(MYSAP) .EQ. CTLSTADOWN .AND.
     *	      CTLSAPENA(MYSAP) .GT. 0 .AND.
     *        CTLSTATUS .EQ. CTLACTIVE) THEN
C
C OPEN YOUR OWN SAP
C
	    CALL LANGETB(BUF, STATUS)
	    IF (STATUS .EQ. 2) GOTO 9999
C
	    IF (NODEID .EQ. NETMASTER(WAYINP)) THEN
	      SMODE = LANMPRI
	    ELSE
	      SMODE = LANMBKP
	    ENDIF
C
	    LANBUF(LANBTYP,    BUF) = LTYPCMD
	    LANBUF(LANDATAF,   BUF) = COPEN
	    LANBUF(LANDATAF+1, BUF) = CCOMMAND
	    LANBUF(LANDATAF+2, BUF) = MYSAP
	    LANBUF(LANDATAF+3, BUF) = CTLAPLQUE
	    LANBUF(LANDATAF+4, BUF) = MAX0(3,LANBNUM/20)
	    LANBUF(LANDATAF+5, BUF) = SMODE
	    LANBUF(LANDATAF+6, BUF) = LANTBEND
	    LANBUF(LANOWN,     BUF) = 1024
C
	    CTLSAPTOUT(MYSAP) = CTLMAXTOUT
	    CTLSAPSTA(MYSAP)  = CTLSTAPEN
C
	    IF (CTLTEST .NE. 0)
     *        TYPE *, IAM(), '*** OPEN SAP SENT ***[',
     *                NODEID, MYSAP, SMODE, ']'
C
	    CALL SNDLAN(DUM1, DUM2, BUF, DUM3)
C
C PROCESS BUFFERS AND SLEEP
C
	    GOTO 200
C
C OPEN TIMED OUT
C
	  ELSEIF (CTLSAPSTA(MYSAP) .EQ. CTLSTAPEN .AND.
     *            CTLSAPTOUT(MYSAP) .EQ. 0) THEN
D	    TYPE *, 'OPEN TIMED OUT FOR MYSAP',MYSAP
	    CTLSAPSTA(MYSAP) = CTLSTADOWN
	    GOTO 200
C
C NOW SERVICE THE CONNECTIONS TO OTHER SYSTEMS (SAPS)
C YOUR SAP IS OPEN ON LOCAL LANPRO
C
	  ELSEIF (CTLSAPSTA(MYSAP) .EQ. CTLSTAUP) THEN
D	    TYPE *, 'I AM OPEN TRYING TO OPEN OTHER SAP'
	    TOSAP  = 0
	    SYSTEM = 0
	    NODE   = 0
C
	    IF (NODEID .EQ. NETMASTER(WAYINP)) THEN
	      SYSTEM = NETBACKUP(WAYINP)
	      NODE   = SYSTEM
	      IF (SYSTEM .GT. 0) TOSAP = CTLSAPSYS(SYSTEM)
	    ELSE
	      IF (X2X_GAME_STATE .LT. X2X_GAMES_IDLE) GOTO 200
	      TOSAP  = X2X_GAME_SAP
	      SYSTEM = NETMASTER(WAYINP)
	      NODE   = NODEMASTER(WAYINP)
	    ENDIF
c
	    IF (TOSAP  .LE. 0) GOTO 200
	    IF (SYSTEM .LE. 0) GOTO 200
	    IF (NODE   .EQ. 0) GOTO 200
C
C ATTEMPT THE CONNECTION ONLY IF NPLEX UP (IF NETLOG TALKS TO PRI)
C
D           TYPE *, 'OTHER SAP IS ', TOSAP, ' ON SYSTEM ', SYSTEM 
C
	    IF (.NOT. NETNODEU(NODE, WAYINP)) GOTO 200
C
C OPEN CONNECTION TO "TOSAP"
C
	    IF (CTLSAPSTA(TOSAP) .EQ. CTLSTADOWN .AND.
     *          CTLSAPENA(TOSAP) .GT. 0 .AND.
     *          CTLSTATUS .EQ. CTLACTIVE) THEN
C
D	      TYPE *, 'CONNECTION REQUEST TO ', TOSAP 
C
	      CALL LANGETA(BUF, CTLAPLQUE, STATUS)
	      IF (STATUS .EQ. 2) GOTO 200
C
	      LANBUF(LANBTYP,    BUF) = LTYPCMD
	      LANBUF(LANDATAF,   BUF) = CCONREQ
	      LANBUF(LANDATAF+1, BUF) = CCOMMAND
	      LANBUF(LANDATAF+2, BUF) = MYSAP
	      LANBUF(LANDATAF+3, BUF) = TOSAP
	      LANBUF(LANDATAF+4, BUF) = 0
C
	      CTLSAPTOUT(TOSAP) = CTLMAXTOUT
	      CTLSAPSTA(TOSAP)  = CTLSTAPEN
C
	      CALL OPS('*** ATTEMPTING CONNECTION TO SYS/SAP ***', 
     *                 SYSTEM, TOSAP)
C
	      IF (CTLTEST .NE. 0)
     *          TYPE *, IAM(), '*** CONNECT SENT FROM-TO ***[',
     *                  MYSAP, TOSAP, ']'
C
	      CALL SNDLAN(DUM1, DUM2, BUF, DUM3)
C
C PROCESS BUFFERS AND SLEEP
C
	      GOTO 200
C
C CONNECT REQUEST TIMED OUT
C
	    ELSEIF (CTLSAPSTA(TOSAP) .EQ. CTLSTAPEN .AND.
     *              CTLSAPTOUT(TOSAP) .EQ. 0) THEN
C
D	      TYPE *, 'CONNECTION REQ TIMED OUT ...' 
C
	      CTLSAPSTA(TOSAP) = CTLSTADOWN
C
	      IF (NETMASTER(WAYINP) .EQ. NODEID)
     *          CALL OPS('*** BACKUP INOPERATIONAL ON LAN ***', 
     *                    TOSAP, MYSAP)
	      GOTO 200
	    ENDIF
C
	  ELSE
D	    TYPE *, IAM(), '*** LOCAL SAP STATUS ***[',
D    *              CTLSAPSTA(MYSAP), ']'
	  ENDIF
	ELSE
C
C SECONDARY SYSTEM (NOT BACKUP, NOT PRIMARY) or BACKUP JUST RESET
C
D	  TYPE *, 'NEITHER PRIMARY NOR BACKUP'
C
C CLOSE YOUR OWN SAP
C
	  IF (CTLSAPSTA(MYSAP) .EQ. CTLSTAUP) THEN
D           TYPE *, 'AND CLOSING MY SAP'
C
	    CALL LANGETX(BUF, STATUS)
	    IF (STATUS .NE. 2) THEN
	      LANBUF(LANBTYP,    BUF) = LTYPCMD
	      LANBUF(LANDATAF,   BUF) = CCLOSE
	      LANBUF(LANDATAF+1, BUF) = CCOMMAND
	      LANBUF(LANDATAF+2, BUF) = MYSAP
	      LANBUF(LANDATAF+3, BUF) = CTLAPLQUE
C
	      CTLSAPTOUT(MYSAP) = 0
	      CTLSAPSTA(MYSAP)  = CTLSTADOWN
C
	      CALL SNDLAN(DUM1, DUM2, BUF, DUM3)
	      CALL XWAIT(10, 2, STATUS)
	    ELSE
	      TYPE *, IAM(), '*** BUFFER ALLOCATION ERROR ***'
	    ENDIF
	  ENDIF
C
	  GOTO 200
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
200	CONTINUE
	CALL RTL(BUF, LANAPP(1, CTLAPLQUE), ST)
C
	IF (ST .NE. 2) THEN
	  LANBUF(LANOWN, BUF) = OWNFAPPL		! SET OWNER
C
	  BTYPE = LANBUF(LANBTYP, BUF)
	  IF (BTYPE .EQ. LTYPCMD) THEN
	    CTYPE  = LANBUF(LANDATAF+0, BUF)
	    RESULT = LANBUF(LANDATAF+1, BUF)
	    DSAP   = LANBUF(LANDATAF+3, BUF)
	    SSAP   = LANBUF(LANDATAF+2, BUF)
D           TYPE *, 'GOT A BUFFER FROM CTLQUE'
D           TYPE *, 'DSAP IS ', DSAP, ' CTYPE IS ', CTYPE
D           TYPE *, 'SSAP IS ', SSAP, ' RESULT IS ', RESULT
C
	    IF (RESULT .EQ. CCOMMAND .OR. RESULT .EQ. CREPLYOK) THEN
	      IF (RESULT .EQ. CCOMMAND) THEN
		SAP = SSAP
	      ELSE
		IF (CTYPE .EQ. COPEN .OR. CTYPE .EQ. CCLOSE) THEN
		  SAP = SSAP
		ELSE
		  SAP = DSAP
		ENDIF
	      ENDIF
C
	      IF (CTYPE .EQ. COPEN .OR.
     *	          CTYPE .EQ. CCONREQ .OR.
     *	          CTYPE .EQ. CCONNIND) THEN
		CTLSAPSTA(SAP) = CTLSTAUP
		IF (CTYPE .EQ. COPEN)
     *            CALL OPS('*** CONNECTION ESTABLISHED ***', DSAP, SSAP)
	      ELSE
		CTLSAPSTA(SAP)=CTLSTADOWN
	      ENDIF
C
	      CTLSAPTOUT(SAP)= 0
C
	      IF (CTLTEST .NE. 0)
     *          TYPE *, IAM(), 'BUFFER OK [',
     *                  BTYPE, CTYPE, RESULT, SSAP, DSAP, SAP, ']'
C
	    ELSE
	      IF (CTLTEST .NE. 0)
     *          TYPE *, IAM(), 'BUFFER BAD [',
     *                 BTYPE, CTYPE, RESULT, SSAP, DSAP, ']'
	    ENDIF
C
	  ELSEIF (BTYPE .EQ. LTYPDATA) THEN
	    DLEN = LANBUF(LANDLEN, BUF)
	    SSAP = LANBUF(LANDORG, BUF)
	    DSAP = LANBUF(LANDDES, BUF)
C
	    IF (CTLTEST .NE. 0)
     *        TYPE *, IAM(), '*** DATA ARRIVED ***[',
     *                SSAP, DSAP, DLEN, ']'
C
	    IF (CTLSAPSTA(SSAP) .NE. CTLSTAUP) THEN
	      IF (CTLTEST .NE. 0)
     *          CALL OPS('*** SSAP NOT ACTIVE ***', SSAP, DSAP)
	      CALL LANRELB(BUF)
	      GOTO 200
	    ENDIF
C
	    MARK = LANBUF(LANDATAF, BUF)
	    IF (MARK .EQ. X2TDBH_TDBH) THEN
              CALL MOV2TOI4(RSEQ, LANBUF(LANDATAF,BUF), X2TDBH_BLKSEQ-1)
C
	      IF (RSEQ .NE. CTLRSEQ(SSAP))
     *          CALL OPS('*** SEQUENCE NUMBER ERROR ***',
     *                   RSEQ, CTLRSEQ(SSAP))
C
	      CTLRSEQ(SSAP) = MOD(RSEQ + 1, 256)
C
	      IF (SSAP .EQ. X2X_GAME_SAP) THEN		! SEND BACK ELSE RELEASE
		CALL ILBYTE(BLTYP, LANBUF(LANDATAF, BUF),
     *                      X2TDBH_BLKTYP-1)
C
		IF (BLTYP .EQ. X2TDBHT_BEGIN) THEN
		  CALL LANGETA(SBUF, CTLAPLQUE, STATUS)
C
		  IF (STATUS .NE. 2) THEN
		    CALL MOVBYT(LANBUF(LANDATAF,  BUF), 1, 
     *                          LANBUF(LANDATAF, SBUF), 1, 20)
C
		    CALL I4TOBUF2(CTLSSEQ(SSAP), LANBUF(LANDATAF, SBUF),
     *                            X2TDBH_BLKSEQ-1)
C
		    CALL ISBYTE(SSAP, LANBUF(LANDATAF, SBUF), 
     *                          X2TDBH_DSAP-1)
C
		    CALL ISBYTE(DSAP, LANBUF(LANDATAF, SBUF), 
     *                          X2TDBH_SSAP-1)
C
	            BLSIZ = 21
		    CALL I4TOBUF2(BLSIZ, LANBUF(LANDATAF, SBUF), 
     *                            X2TDBH_BLKSIZ-1)
C
		    FLAGS = '00000080'X			! FE ON-LINE
		    CALL ISBYTE(FLAGS, LANBUF(LANDATAF, SBUF), 
     *                          X2TDBH_FLAGS-1)
C
		    BLTYP = X2TDBHT_BEGIN_CONF
		    CALL ISBYTE(BLTYP, LANBUF(LANDATAF, SBUF), 
     *                          X2TDBH_BLKTYP-1)
C
		    DOFF = 21
		    CALL ISBYTE(DOFF, LANBUF(LANDATAF, SBUF), 
     *                          X2TDBH_DATAOFF-1)
C
		    CALL ISBYTE(0, LANBUF(LANDATAF, SBUF), 
     *                          X2TDBH_MSGCNT-1)
C
		    CALL ISBYTE(SSAP, LANBUF(LANDATAF, SBUF), 
     *                          X2TDBH_FE_ID-1)
C
		    CALL ISBYTE(0, LANBUF(LANDATAF, SBUF), 
     *                          X2TDBH_CNFSUM-1)
C
		    IF (CTLTEST .NE. 0)
     *                TYPE *, IAM(), '*** CONFIRM SENT ***', DSAP, SSAP
C
		    LANBUF(LANBTYP, SBUF) = LTYPDATA
C
		    CALL SNDLAN(DSAP, SSAP, SBUF, BLSIZ)
C
		    CTLSSEQ(SSAP) = MOD(CTLSSEQ(SSAP) + 1, 256)
		  ENDIF
		ENDIF
	      ELSE
	        TYPE *, IAM(), '*** NOT FROM GAME SAP ***', SSAP, DSAP
	      ENDIF
C
	    ELSEIF (MARK .EQ. X2NPLX) THEN
	      DTYPE = LANBUF(LANDATAF + 1, BUF)
	      IF (DTYPE .EQ. PLXPRIOK) THEN
		IF (NETBACKUP(WAYINP) .GT. 0) THEN
		  WRITE(5, 9000) ((IAM(), BELL), I = 1, 5)
		  NETBACKUP(WAYINP)=0
		ENDIF
C
	      ELSE
		TYPE *, IAM(), '*** INVALID DTYPE ***',
     *                  DTYPE, SSAP, DSAP
	      ENDIF
C
	    ELSE
	      TYPE *, IAM(), '*** INVALID PROTOCOL ID ***', SSAP, DSAP
	    ENDIF
C
	  ELSE
	    TYPE *, IAM(), '*** ILLEGAL BTYPE ***[', BTYPE, ']'
	  ENDIF
C
	  CALL LANRELB(BUF)
	ELSE
C
C SERVICE SEND QUEUE
C
300	  CONTINUE
	  CALL RTL(BUF, CTLEXECQ, ST)
	  IF (ST .NE. 2) THEN
	    DSAP = LANBUF(LANDATAF+2, BUF)
	    LEN  = LANBUF(LANDATAF+3, BUF)		! IN BYTES
C
	    IF (CTLSAPSTA(DSAP) .NE. CTLSTAUP) THEN
	      CALL OPS('*** NO CONNECTION TO DEST SAP ***', MYSAP, DSAP)
	      CALL LANRELB(BUF)
	      GOTO 300
	    ENDIF
C
	    LANBUF(LANBTYP, BUF) = LTYPDATA
	    CALL SNDLAN(MYSAP, DSAP, BUF, LEN)
	    GOTO 300
	  ELSE
	    GOTO 9999
	  ENDIF
	ENDIF
C
	GOTO 200					! MORE BUFFERS
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FORMAT STATEMENTS
C
9000	FORMAT(X, A, A, '*********************************',/,
     *         X, A, A, '*    DUE TO NETWORK DIFFICULTIES THE      *',/,
     *         X, A, A, '*   TAKEOVER FEATURE HAS BEEN DISABLED    *',/,
     *         X, A, A, '*  THIS SYSTEM IS NO LONGER A HOT BACKUP  *',/,
     *         X, A, A, '*********************************')
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C GO BACK TO SLEEP (TIMER OR TSKTRAP WILL WAKE YOU UP AND RELEASE)
C
9999	CONTINUE
	CALL CT_START_TIME
C
C RETURN & END
C
	RETURN
	END
