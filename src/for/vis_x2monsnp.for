C
C SUBROUTINE X2MONSNP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2MONSNP.FOV                                 $
C  $Date::   17 Apr 1996 16:23:50                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - none ***
C
C
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF, AGTINF.DEF for Finland.
C
C V03 13-DEC-94 GPR Integrate UK changes into X2X Baseline
C V02 20-JUL-94 WS MULTINETWORK CHANGES
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	This vision snapshot will display information on up to 100 GTX's
C	simultaneously. The default screen will display the status of the
C	GTX's (i.e. on/off). Subcommands will allow the display of other
C	information, such as time of last contact, GTX type, capacity and
C	Lan id.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE X2MONSNP(SLINE)
C
	IMPLICIT NONE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C INCLUDE FILES.
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:X2TDBH.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2VIS.DEF'
	INCLUDE 'INCLIB:X2XLPC.DEF'
	INCLUDE 'INCLIB:X2XNPC.DEF'
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PARAMETER DECLARATIONS.
C
	INTEGER*4	DELAY				! REFRESH DELAY
	PARAMETER	(DELAY      =  60)
C
	INTEGER*4	MAXDISP				! MAX SAPS DISPLAYED
	PARAMETER	(MAXDISP    = 100)
C
	INTEGER*4	MAX_IDX,			! MAX IDXS IN DISTBL()
     *			SAP_IDX,			! SAP IDX INTO DISTBL()
     *			STA_IDX,			! STATUS IDX
     *			HOU_IDX,			! HOURS IDX
     *			MIN_IDX,			! MINUTES IDX
     *			SEC_IDX,			! SECONDS IDX
     *			TYP_IDX,			! SAP TYPE IDX
     *			CAP_IDX,			! CAPACITY IDX
     *			LAN_IDX,			! LAN IDX - V02
     *		        SUBNETWORK_IDX,			! SUBNETWORK IDX -V02
     *		        OFF_IDX				!OFFLINE PORTS IDX
	PARAMETER	(MAX_IDX    =  10)		! V02
	PARAMETER	(SAP_IDX    =   1)
	PARAMETER	(STA_IDX    =   2)
	PARAMETER	(HOU_IDX    =   3)
	PARAMETER	(MIN_IDX    =   4)
	PARAMETER	(SEC_IDX    =   5)
	PARAMETER	(TYP_IDX    =   6)
	PARAMETER	(CAP_IDX    =   7)
	PARAMETER	(LAN_IDX    =   8)
	PARAMETER	(SUBNETWORK_IDX=9)		! V02
	PARAMETER	(OFF_IDX    =  10)
C
	INTEGER*4	NUM_COLS,			! NUMBER OF COLUMNS
     *			NCM1				! NUMBER OF COLUMNS - 1
	PARAMETER	(NUM_COLS   =   5)
	PARAMETER	(NCM1       = NUM_COLS - 1)
C
	INTEGER*4	NUM_KEYS
	PARAMETER	(NUM_KEYS   =   4)		! NUMBER OF KEYS
C
	INTEGER*4	CHK_STATUS,
     *			CHK_TIME,
     *			CHK_TYPE,
     *			CHK_LAN
	PARAMETER	(CHK_STATUS =   1)		! CHK STATUS
	PARAMETER	(CHK_TIME   =   2)		! TIME LAST ACTIVE
	PARAMETER	(CHK_TYPE   =   3)		! TYPE & CAPACITY
	PARAMETER	(CHK_LAN    =   4)		! LAN NUMBER
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C LOCAL DECLARATIONS.
C
	REAL*8		KEY_NAME(NUM_KEYS)
     *					/'XSTA    ',
     *					 'XTIM    ',
     *					 'XTYP    ',
     *					 'XLAN    '/
C
	INTEGER*4	SLINE(*)			! FROM CALLER
C
	INTEGER*4	CAPACITY,			! FE CAPACITY
     *			CONTBL(X2X_STATIONS),		! CONNECTION TYPE TABLE
     *			CURTIM,				! CURRENT TIME IN SECS
     *			DAY		/86400/,	! # SECONDS IN 24 HOURS
     *			DISCNT		/0/,		! DISPLAY COUNTER
     *			DISTBL(MAX_IDX, NUM_COLS),	! DISPLAY TABLE
     *			HOUR		/3600/,		! # SECONDS IN  1 HOUR
     *			I, J, K, L,			! LOOP & INDEX VARS.
     *			LAST_REQUEST	/1/,		! LAST REQUEST
     *			LINIDX,				! PRINT LINE INDEX
     *			LPORT,				! LOCAL PORT NUMBER
     *			LSTHR		/0/,		! LAST HOUR
     *			LSTMIN		/0/,		! LAST MINUTE
     *			LSTSEC		/0/,		! LAST SECOND
     *			LSTTIM		/0/,		! LAST TIME IN SECONDS
     *			MINUTE		/60/,		! # SECONDS IN  1 MIN
     *			NUM_LINES,			! # OF DISPLAY LINES
     *			NUM_SAPS,			! # OF SAPS TO DISPLAY
     *			POS,				! POSITION FOR KEY()
     *			REMAINDER,			! REMAINING SAPS
     *			REQUEST,			! FROM KEY()
     *			SAP,				! ARRAY INDEX
     *			SAPTBL(X2X_STATIONS),		! SAP DISPLAY TABLE
     *			ST				! STATUS
C
	CHARACTER*20	X2FILNAM			! FILE NAME FUNCTION
C
	CHARACTER*12	DISP_TYPE(NUM_KEYS)		! SCREEN TITLES
     *					/'Status      ',
     *					 'Last Active ',
     *					 'Type & Cpcty',
     *					 'LAN Connect '/
C
	CHARACTER*6	SAPSTATE(0:2)	/'Not Up',	! SAP STATE
     *					 'Online',
     *					 '*Idle*'/
C
	CHARACTER*5	CONTYPE(0:X2XPT_MAX_TYPE)
     *					/'Undef',	! NETWORK PORT TYPES
     *					 'X.21/',	! (SEE X2XPRM.DEF) V-02
     *					 'X.25/',	! V02
     *					 'Dial/',	! V02
     *					 'Asyn/',	! V02
     *					 'Usat/',	! V02
     *					 'Bcs1/',	! V02
     *					 'Bcs2/'/	! V02
C
	LOGICAL*4	FIRST		/.TRUE./,	! FIRST TIME CALLED ?
     *			LOADED		/.FALSE./	! ARRAY LOADED FLAG

	INTEGER*4	PORT						    !V03
	INTEGER*4	LOCALPORT, LOCALSTATE				    !V03
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IF THIS IS THE FIRST TIME CALLED, THEN LOAD THE CONNECTION TYPE VECTOR
C FROM THE LOCAL PORT & NETWORK PORT CONFIGURATION FILES.
C
	IF (FIRST) THEN
	  CALL FASTSET(0, CONTBL, X2X_STATIONS)
	  CALL FASTSET(0, SAPTBL, X2X_STATIONS)
C
	  CALL OPENX(XLPC, X2FILNAM(XLPC), 4, 0, 0, ST)
	  IF (ST .NE. 0) THEN
	    CALL OS32ER(6, X2FILNAM(XLPC), 'OPENX', ST, 0)
	    GOTO 10000
	  ENDIF
	  CALL IOINIT(X2XLPC_FDB, XLPC, X2XLPC_SECT * 256)
C
	  CALL OPENX(XNPC, X2FILNAM(XNPC), 4, 0, 0, ST)
	  IF (ST .NE. 0) THEN
	    CALL OS32ER(6, X2FILNAM(XNPC), 'OPENX', ST, 0)
	    GOTO 10000
	  ENDIF
	  CALL IOINIT(X2XNPC_FDB, XNPC, X2XNPC_SECT * 256)
C
	  DO 100 LPORT = 1, X2X_LOCAL_PORTS
	    CALL READW(X2XLPC_FDB, LPORT, X2XLPC_REC, ST)
	    IF (ST .NE. 0) THEN
	      CALL OS32ER(6, X2FILNAM(XLPC), 'READW', ST, LPORT)
	      CALL GPAUSE
	      GOTO 100
	    ENDIF
C
	    IF (X2XLPC_SAP .GT. 0) THEN
	      IF (CONTBL(X2XLPC_SAP) .LT. 1) THEN
		CALL READW(X2XNPC_FDB, X2XLPC_NETPORT, X2XNPC_REC, ST)
		IF (ST .NE. 0) THEN
		  CALL OS32ER(6, X2FILNAM(XNPC), 'READW', ST, X2XLPC_NETPORT)
		  CALL GPAUSE
		  GOTO 100
		ENDIF
		CONTBL(X2XLPC_SAP) = X2XNPC_TYPE
	      ENDIF
	    ENDIF
100	  CONTINUE
C
	  CALL CLOSEFIL(X2XLPC_FDB)
	  CALL CLOSEFIL(X2XNPC_FDB)
	  FIRST = .FALSE.
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C IF IT IS TIME TO REFRESH THE SCREEN, RESET THE LOADED FLAG.
C
	IF (LSTTIM + DELAY .LT. P(ACTTIM) .OR.
     *      X2FLDINF(XUPDIDX) .NE. 0) THEN
	  LOADED = .FALSE.
	  X2FLDINF(XUPDIDX) = 0
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C LOAD THE SAP TABLE.
C
	IF (.NOT. LOADED) THEN
	  DISCNT = 0
	  DO 200 SAP = X2X_ACTUAL_SAP, X2X_SAP
	    IF (X2XE_ACT_STATUS(SAP) .GT. 0) THEN
	      DISCNT = DISCNT + 1
	      SAPTBL(DISCNT) = SAP
	    ENDIF
200	  CONTINUE
C
	  LOADED  = .TRUE.
C
C CALCULATE TIME LAST UPDATED.
C
	  LSTTIM = ABS(P(ACTTIM))
	  LSTHR  = LSTTIM / HOUR
	  LSTMIN = MOD(LSTTIM, HOUR) / MINUTE
	  LSTSEC = MOD(LSTTIM, MINUTE)
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C GET REQUEST
C
	POS = 1
C
	CALL KEY(SLINE, KEY_NAME, NUM_KEYS, POS, REQUEST)
C
	IF (REQUEST .LT. CHK_STATUS .OR. REQUEST .GT. NUM_KEYS)
     *    REQUEST = LAST_REQUEST
C
	LAST_REQUEST = REQUEST
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C INITIALIZE THE SCREEN.
C
	CALL FASTSET('20202020'X, NEW, 24 * 20)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C DISPLAY THE SCREEN HEADER.
C
	WRITE(XNEW(1), 9000) DISP_TYPE(REQUEST), LSTHR, LSTMIN, LSTSEC
C
	IF (REQUEST .EQ. CHK_STATUS) THEN
	  WRITE(XNEW(2), 9010)
C
	ELSEIF (REQUEST .EQ. CHK_TIME) THEN
	  WRITE(XNEW(2), 9030)
C
	ELSEIF (REQUEST .EQ. CHK_TYPE) THEN
	  WRITE(XNEW(2), 9050)
C
	ELSEIF (REQUEST .EQ. CHK_LAN) THEN
	  WRITE(XNEW(2),9070)				!V02
C
	ELSE
	  WRITE(XNEW(2), 9010)
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C DISPLAY THE FULL ROWS.
C
	LINIDX    = 2
	NUM_SAPS  = MIN0(DISCNT, MAXDISP)
	NUM_LINES = NUM_SAPS / NUM_COLS
	REMAINDER = MOD(NUM_SAPS, NUM_COLS)
C
	DO 500 I = 1, NUM_LINES
	  J = (I - 1) * NUM_COLS + 1
	  DO 400 K = J, J + NCM1
C
C CALCULATE CURRENT TIME.
C
	    CURTIM = X2X_SYSTIM - X2XE_TIME(SAPTBL(K))
300	    CONTINUE
	    IF (CURTIM .LT. 0) THEN
	      CURTIM = CURTIM + DAY
	      GOTO 300
	    ENDIF
C
C DETERMINE CAPACITY BASED ON FRONTEND TYPE.
C
	    IF (X2XE_FE_TYPE(SAPTBL(K)) .EQ. X2TDBH_FE_TYPE_DIAL_UP) THEN
	      CAPACITY = X2XE_MAX_CAPACITY(SAPTBL(K))
	    ELSE
	      CAPACITY = X2XE_CAPACITY(SAPTBL(K))
	    ENDIF
C
C POPULATE DISPLAY TABLE.
C
	    L = K - J + 1
	    DISTBL(SAP_IDX, L) = SAPTBL(K)
	    DISTBL(STA_IDX, L) = X2XE_ACT_STATUS(SAPTBL(K))
	    DISTBL(HOU_IDX, L) = CURTIM / HOUR
	    DISTBL(MIN_IDX, L) = MOD(CURTIM, HOUR) / MINUTE
	    DISTBL(SEC_IDX, L) = MOD(CURTIM, MINUTE)
	    DISTBL(TYP_IDX, L) = CONTBL(SAPTBL(K))
	    DISTBL(CAP_IDX, L) = CAPACITY
	    DISTBL(LAN_IDX, L) = CURLAN(CONNECTION(X2X_GAME_SAP, SAPTBL(K)))
	    DISTBL(SUBNETWORK_IDX, L) = X2XE_SUBNETWORK(SAPTBL(K))	!V02
C
C	    ***** Start V03 changes *****
C
	    DISTBL(OFF_IDX, L) = 0
	    DO 350, PORT=1,X2X_SAP_PORTS
		SAP=SAPTBL(K)
		IF (X2XE_LOCAL_PORT_STATE(PORT,SAP).EQ.0) GOTO 350
		IF (X2XE_LOCAL_PORT_STATE(PORT,SAP).NE.X2XPS_ON_LINE) THEN
		    DISTBL(OFF_IDX, L)=DISTBL(OFF_IDX, L)+1
		ELSE
		    LOCALPORT=X2XE_LOCAL_PORT(PORT,SAP)
		    IF(LOCALPORT.NE.0) THEN
			LOCALSTATE=X2XPL_STATE(LOCALPORT)
			IF (LOCALSTATE.NE.X2XPS_ON_LINE) THEN
			    DISTBL(OFF_IDX, L)=DISTBL(OFF_IDX, L)+1
			ENDIF
		    ENDIF
		ENDIF
350	    CONTINUE
C
C	    ***** End V03 changes *****
C
400	  CONTINUE
C
C OUTPUT INFORMATION.
C
	  LINIDX = LINIDX + 1
C
	  IF (REQUEST .EQ. CHK_STATUS) THEN
	    WRITE(XNEW(LINIDX), 9020) (DISTBL(SAP_IDX, L),
     *                                 SAPSTATE(DISTBL(STA_IDX, L)),
     *                                 L = 1, NUM_COLS)
	  ELSEIF (REQUEST .EQ. CHK_TIME) THEN
	    WRITE(XNEW(LINIDX), 9040) (DISTBL(SAP_IDX, L),
     *                                 DISTBL(HOU_IDX, L),
     *                                 DISTBL(MIN_IDX, L),
     *                                 DISTBL(SEC_IDX, L),
     *                                 L = 1, NUM_COLS)
	  ELSEIF (REQUEST .EQ. CHK_TYPE) THEN
	    WRITE(XNEW(LINIDX), 9060) (DISTBL(SAP_IDX, L),
     *                                 CONTYPE(DISTBL(TYP_IDX, L)),
     *				       DISTBL(SUBNETWORK_IDX, L),	! V02
     *                                 DISTBL(CAP_IDX, L),
     *                                 L = 1, NUM_COLS)
	  ELSEIF (REQUEST .EQ. CHK_LAN) THEN
	    WRITE(XNEW(LINIDX), 9080) (DISTBL(SAP_IDX, L),
     *                                 DISTBL(LAN_IDX, L),
     *				       DISTBL(SUBNETWORK_IDX, L),	!V 02
     *				       DISTBL(OFF_IDX,L),
     *                                 L = 1, NUM_COLS)
	  ELSE
	    WRITE(XNEW(LINIDX), 9020) (DISTBL(SAP_IDX, L),
     *                                 SAPSTATE(DISTBL(STA_IDX, L)),
     *                                 L = 1, NUM_COLS)
	  ENDIF
500	CONTINUE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C DISPLAY THE REMAINING PARTIAL ROW (IF NECESSARY).
C
	IF (REMAINDER .GT. 0) THEN
	  J = NUM_LINES * NUM_COLS + 1
	  DO 700 K = J, NUM_SAPS
C
C CALCULATE CURRENT TIME.
C
	    CURTIM = X2X_SYSTIM - X2XE_TIME(SAPTBL(K))
600	    CONTINUE
	    IF (CURTIM .LT. 0) THEN
	      CURTIM = CURTIM + DAY
	      GOTO 600
	    ENDIF
C
C DETERMINE CAPACITY BASED ON FRONTEND TYPE.
C
	    IF (X2XE_FE_TYPE(SAPTBL(K)) .EQ. X2TDBH_FE_TYPE_DIAL_UP) THEN
	      CAPACITY = X2XE_MAX_CAPACITY(SAPTBL(K))
	    ELSE
	      CAPACITY = X2XE_CAPACITY(SAPTBL(K))
	    ENDIF
C
C POPULATE DISPLAY TABLE.
C
	    L = K - J + 1
	    DISTBL(SAP_IDX, L) = SAPTBL(K)
	    DISTBL(STA_IDX, L) = X2XE_ACT_STATUS(SAPTBL(K))
	    DISTBL(HOU_IDX, L) = CURTIM / HOUR
	    DISTBL(MIN_IDX, L) = MOD(CURTIM, HOUR) / MINUTE
	    DISTBL(SEC_IDX, L) = MOD(CURTIM, MINUTE)
	    DISTBL(TYP_IDX, L) = CONTBL(SAPTBL(K))
	    DISTBL(CAP_IDX, L) = CAPACITY
	    DISTBL(LAN_IDX, L) = CURLAN(CONNECTION(X2X_GAME_SAP, SAPTBL(K)))
	    DISTBL(SUBNETWORK_IDX, L) = X2XE_SUBNETWORK(SAPTBL(K))	! V02
700	  CONTINUE
C
C OUTPUT REMAINING INFORMATION.
C
	  LINIDX = LINIDX + 1
C
	  IF (REQUEST .EQ. CHK_STATUS) THEN
	    WRITE(XNEW(LINIDX), 9020) (DISTBL(SAP_IDX, L),
     *                                 SAPSTATE(DISTBL(STA_IDX, L)),
     *                                 L = 1, REMAINDER)
	  ELSEIF (REQUEST .EQ. CHK_TIME) THEN
	    WRITE(XNEW(LINIDX), 9040) (DISTBL(SAP_IDX, L),
     *                                 DISTBL(HOU_IDX, L),
     *                                 DISTBL(MIN_IDX, L),
     *                                 DISTBL(SEC_IDX, L),
     *                                 L = 1, REMAINDER)
	  ELSEIF (REQUEST .EQ. CHK_TYPE) THEN
	    WRITE(XNEW(LINIDX), 9060) (DISTBL(SAP_IDX, L),
     *                                 CONTYPE(DISTBL(TYP_IDX, L)),
     *				       DISTBL(SUBNETWORK_IDX, L),	! V02
     *                                 DISTBL(CAP_IDX, L),
     *                                 L = 1, REMAINDER)
	  ELSEIF (REQUEST .EQ. CHK_LAN) THEN
	    WRITE(XNEW(LINIDX), 9080) (DISTBL(SAP_IDX, L),
     *                                 DISTBL(LAN_IDX, L),
     *				       DISTBL(SUBNETWORK_IDX, L),	! V02
     *				       DISTBL(OFF_IDX,L),
     *                                 L = 1, REMAINDER)
	  ELSE
	    WRITE(XNEW(LINIDX), 9020) (DISTBL(SAP_IDX, L),
     *                                 SAPSTATE(DISTBL(STA_IDX, L)),
     *                                 L = 1, REMAINDER)
	  ENDIF
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CLEAR ANY UNUSED SLOTS.
C
	DO 800 I = LINIDX + 1, 22
	  WRITE(XNEW(I), 9090)
800	CONTINUE
C
C DISPLAY REQUEST LINE.
C
	WRITE(XNEW(23), 9100)
C
C CLEAR COMMAND LINE.
C
	WRITE(XNEW(24), 9090)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FORMAT STATEMENTS.
C
9000	FORMAT('SAP Monitor Snapshot - ', A,
     *         T64, 'Update: ', I2.2, 2(':', I2.2))
C
9010	FORMAT('SAP Status', <NCM1>(6X, 'SAP Status'))
9020	FORMAT(I3, X, A6, <NCM1>(6X, I3, X, A6))
C
9030	FORMAT('SAP Last Actv', <NCM1>(3X, 'SAP Last Actv'))
9040	FORMAT(I3, X, I3.2, 2(':', I2.2),
     *         <NCM1>(3X, I3, X, I3.2, 2(':', I2.2)))
C
9050	FORMAT('SAP Ctype Cpcty', <NCM1>(X, 'SAP Ctype Cpcty'))
9060	FORMAT(I3, X, A5, I2, I4, <NCM1>(X, I3, X, A5, I2, I4))	! V02
C
9070	FORMAT('SAP Lan Sub Off', <NCM1>('|', 'SAP Lan Sub Off'))	! V02
9080	FORMAT(I3, X, I3, X, I3,X,I3,  <NCM1>('|', I3, X, I3, X, I3,X,I3))	! V02
C
9090	FORMAT(80(' '))
C
9100	FORMAT('Subcommands are: XSTAtus, XTIMe, XTYPe, & XLAN')
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
10000	CONTINUE
C
	RETURN
	END
