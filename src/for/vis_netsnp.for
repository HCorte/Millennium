C
C *** SUBROUTINE NETSNP ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NETSNP.FOV                                   $
C  $Date::   17 Apr 1996 14:10:30                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - vis_netsnp.for ***
C  
C
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF, AGTINF.DEF for Finland.
C
C V03 01-AUG-90 XXX RELEASED FOR VAX
C V02 05-APR-89 LOU MODIFIED FOR 3 SYSTEM CONFIGURATION.
C V01 01-JUN-88 XXX RELEASED FOR MICHIGAN
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
C	VISION NETWORK SNAPSHOT.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE NETSNP(CLINE)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DN_LINK.DEF'
	INCLUDE 'INCLIB:DN_BLOCK.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
C PARAMETER DECLARATIONS.
C
	INTEGER*4	CONTYP,
     *			MAXTAKE,
     *			MODNUM,
     *			NUM_CMD_ITEMS,
     *			STATNU
C
	PARAMETER	(CONTYP        =      3)
	PARAMETER	(MAXTAKE       = 120000)
	PARAMETER	(MODNUM        =      6)
	PARAMETER	(NUM_CMD_ITEMS =     10)
	PARAMETER	(STATNU        =      3)
C
C LOCAL DECLARATIONS.
C
	REAL*8		DMODE(0:MODNUM)		/'   *    ', 'normal  ',
     *						 'recovery', 'error   ',
     *						 'dummy   ', 'command ',
     *						 'driv cmd'/,
     *			K(40)			/'TAKE    ', 'TOFreez ',
     *						 'BACKup  ', 'ENAAsync',
     *						 'ENABsync', 'ENACsync',
     *						 'SYNCwait', 'CHKpnt  ',
     *						 'ENADsync', 'ENAEsync',
     *					                30 * 'UNUSED  '/,
     *			PHYCON(0:CONTYP-1)	/'no conn ', 'idle    ',
     *						 'active  '/,
     *			RELAT(-2:STATNU-1)	/'dead    ', 'dead    ',
     *						 'not actv', 'secon-ry',
     *						 'primary '/
C
	INTEGER*4	BUF(CDLEN),
     *			CLINE(20),
     *			CURWAY			/0/,
     *			HBK,
     *			I,
     *			IDLETIMD,
     *			IDSYMB(0:MAX_SYSTEMS)	/'*   ', 'A   ', 'B   ',
     *						 'C   ', 'D   ', 'E   '/,
     *			J,
     *			KEYNUM,
     *			LCMD,
     *			LST_ACK(NETSYS),
     *			LST_TRN(NETSYS),
     *			NXT,
     *			OFF,
     *			OFF1,
     *			OFF8,
     *			OFF12,
     *			POS,
     *			SERDISP(NETSYS),
     *			ST,
     *			ST1,
     *			VALUE
C
C EXTERNAL DECLARATIONS
C
	INTEGER*4	QUECNT
	EXTERNAL	QUECNT
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C NETSNP INPUT
C
	CALL SPACES
C
	BUF(6) = IDNUM
	KEYNUM = 0
	POS    = 1
	VALUE  = 0
C
	CALL KEY(CLINE, K, NUM_CMD_ITEMS, POS, KEYNUM)
C
	IF (POS .GT. 40) THEN
	  IF (KEYNUM .EQ. 8) THEN
	    GOTO 300
	  ELSEIF (KEYNUM .LT. 9 .OR. KEYNUM .GT. NUM_CMD_ITEMS) THEN
	    IF (CURWAY .LT. 1 .OR. CURWAY .GT. NUMWAY) CURWAY = WAYINP
	    GOTO 1200
	  ENDIF
	ENDIF
C
	IF (KEYNUM .EQ. 0) GOTO 500
C
	IF (KEYNUM .EQ. 8) GOTO 100
C
	CALL NUMB(CLINE, POS, VALUE)
C
	IF (VALUE .LT. 0) GOTO 900
C
	CALL FASTSET(0, BUF, 5)
C
100	CONTINUE
	GOTO (210, 220, 230, 240, 250, 260, 290, 300, 270, 280) KEYNUM
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C INVALID KEYNUM
C
	GOTO 500
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C TAKEOVER TIME CHANGE
C
210	CONTINUE
	IF (CURWAY .NE. WAYINP) GOTO 1000
	IF (VALUE .LT. 30000 .OR. VALUE .GT. MAXTAKE) GOTO 900
	BUF(1) = 1
	BUF(2) = VALUE
	BUF(3) = TCNET
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C TOFREEZ CHANGE
C
220	IF (VALUE .LT. 0 .OR. VALUE .GT. MAXFREEZ) GOTO 900
	BUF(1) = 2
	BUF(2) = VALUE
	BUF(3) = TCNET
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C BACKUP ID CHANGE
C
230	CONTINUE
	IF (CURWAY .NE. WAYINP) GOTO 1000
	IF (VALUE .LT. 0 .OR.
     *      VALUE .GT. NETSYS .OR.
     *      VALUE .EQ. NETMASTER(WAYINP)) GOTO 900
C
	IF (X2X_GAME_STATE .NE. X2X_GAMES_UP) GOTO 800
C
	IF (VALUE .NE. 0) THEN
	  IF (NETSITE(VALUE) .NE. NETSITE(NODEID)) GOTO 900
C
	  IF (NETROUT(VALUE, WAYINP) .NE. ROUACT .AND.
     *        DN_LINK(VALUE).STATE .NE. STATE_RUNNING) GOTO 700
C
	  IF (NETMODE(VALUE, WAYINP) .EQ. FILMD) GOTO 600
	ENDIF
C
	BUF(1) = 3
	BUF(2) = VALUE
	BUF(3) = TCNET
	BUF(9) = CURWAY
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ENABLE RESYNCH CHANGE - SYS A
C
240	CONTINUE
	IF (NETMASTER(CURWAY) .NE. NODEID) GOTO 1000
	IF (VALUE .LT. 0 .OR. VALUE .GT. 2) GOTO 900
	BUF(1) = 2
	BUF(2) = VALUE + 100				! 100 / 100 = SYS A ID
	BUF(3) = TCSPE
	BUF(9) = CURWAY
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ENABLE RESYNCH CHANGE - SYS B
C
250	CONTINUE
	IF (NETMASTER(CURWAY) .NE. NODEID) GOTO 1000
	IF (VALUE .LT. 0 .OR. VALUE .GT. 2) GOTO 900
	BUF(1) = 2
	BUF(2) = VALUE + 200				! 200 / 100 = SYS B ID
	BUF(3) = TCSPE
	BUF(9) = CURWAY
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ENABLE RESYNCH CHANGE - SYS C
C
260	CONTINUE
	IF (NETMASTER(CURWAY) .NE. NODEID) GOTO 1000
	IF (VALUE .LT. 0 .OR. VALUE .GT. 2) GOTO 900
	BUF(1) = 2
	BUF(2) = VALUE + 300				! 300 / 100 = SYS C ID
	BUF(3) = TCSPE
	BUF(9) = CURWAY
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ENABLE RESYNCH CHANGE - SYS D
C
270	CONTINUE
	IF (NETMASTER(CURWAY) .NE. NODEID) GOTO 1000
	IF (VALUE .LT. 0 .OR. VALUE .GT. 2) GOTO 900
	BUF(1) = 2
	BUF(2) = VALUE + 400				! 400 / 100 = SYS D ID
	BUF(3) = TCSPE
	BUF(9) = CURWAY
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ENABLE RESYNCH CHANGE - SYS E
C
280	CONTINUE
	IF (NETMASTER(CURWAY) .NE. NODEID) GOTO 1000
	IF (VALUE .LT. 0 .OR. VALUE .GT. 2) GOTO 900
	BUF(1) = 2
	BUF(2) = VALUE + 500				! 500 / 100 = SYS E ID
	BUF(3) = TCSPE
	BUF(9) = CURWAY
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C SYNCWAIT CHANGE
C
290	CONTINUE
	IF (VALUE .LT. 0 .OR. VALUE .GT. 3000) GOTO 900
	BUF(1) = 6
	BUF(2) = VALUE
	BUF(3) = TCNET
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CHECKPOINT COMMAND
C
300	CONTINUE
	BUF(1) = 1
	BUF(3) = TCGEN
	BUF(6) = IDNUM
	GOTO 1100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C INPUT ERROR
C
500	CONTINUE
	WRITE(CLIN23, 9000)
	GOTO 9999
C
600	CONTINUE
	WRITE(CLIN23, 9010)
	GOTO 9999
C
700	CONTINUE
	WRITE(CLIN23, 9020)
	GOTO 9999
C
800	CONTINUE
	WRITE(CLIN23, 9030)
	GOTO 9999 
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C VALUE ERROR
C
900	CONTINUE
	WRITE(CLIN23, 9040)
	GOTO 9999
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PATH ERROR
C
1000	CONTINUE
	WRITE(CLIN23, 9050)
	GOTO 9999
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C QUEUE COMMAND BUFFER TO SYSYTEM INPUT QUEUE
C
1100	CONTINUE
	CALL VISCMD(BUF, ST)
	CALL XWAIT(2, 1, ST1)
	IF (ST .NE. 0) WRITE(CLIN23, 9060)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ENCODE SCREEN IMAGE
C
1200	CONTINUE
C
	IF (CURWAY .LE. 0) THEN
	  WRITE(CLIN23, 9070)
	  GOTO 9999
	ENDIF
C
	WRITE(CLIN1, 9080)
	WRITE(CLIN2, 9090)
C
	IDLETIMD = IDLETIM
	IF (CURWAY .NE. WAYINP) IDLETIMD = 0
C
	WRITE(CLIN3, 9100) IDSYMB(NODEID), NODEID,
     *                    IDSYMB(NETBACKUP(CURWAY)), NETBACKUP(CURWAY),
     *                    IDSYMB(NETMASTER(CURWAY)), NETMASTER(CURWAY),
     *                    IDLETIMD
C
	WRITE(CLIN4, 9110) (SNDIOCHK(I),  I = 1, NETSYS),
     *                    (READIOCHK(I), I = 1, NETSYS)
C
	WRITE(CLIN5, 9120) (IDSYMB(I), NETOPN(I, CURWAY), I = 1, NETSYS)
C
	WRITE(CLIN6, 9130) (PHYCON(NETROUT(I, CURWAY)), I = 1, NETSYS)
C
	WRITE(CLIN7, 9140) (RELAT(NETSTAT(I, CURWAY)), I = 1, NETSYS)
C
	WRITE(CLIN8, 9150)
C
	DO 1300 OFF = 1, NETSYS
 	  OFF1 = OFF
	  IF (NETSTAT(OFF, CURWAY) .EQ. NSTASEC .AND.
     *        NETROUT(OFF, CURWAY) .EQ. ROUACT) OFF1 = NODEID
	  OFF8 = (OFF - 1) * 12 + 21
	  WRITE(XNEW(8)(OFF8:OFF8+11), 9160) DMODE(NETMODE(OFF1,CURWAY))
1300	CONTINUE
C
	WRITE(CLIN9, 9170) (NETTIMER - NETTIM(I, CURWAY), I = 1, NETSYS)
C
	WRITE(CLIN10, 9180) (NETERR(I, CURWAY), I = 1, NETSYS)
C
	DO 1400 I = 1, NETSYS
	  IF (NET_LAST_TRANS_SENT(I) .NE. 0) THEN
	    LST_TRN(I) = P(ACTTIM) - NET_LAST_TRANS_SENT(I)
	  ELSE
	    LST_TRN(I) = 0
	  ENDIF
1400	CONTINUE
	WRITE(CLIN11, 9190) (LST_TRN(I), I = 1, NETSYS)
C
	DO 1500 I = 1, NETSYS
	  IF (NET_LAST_ACK_TIME(I) .NE. 0) THEN
	    LST_ACK(I) = P(ACTTIM) - NET_LAST_ACK_TIME(I)
	  ELSE
	    LST_ACK(I) = 0
	  ENDIF
1500	CONTINUE
	WRITE(CLIN12, 9200) (LST_ACK(I), I = 1, NETSYS)
C
	WRITE(CLIN13, 9210)
C
	DO 1600 J = 1, NETSYS
	  SERDISP(J) = 0
	  IF (NETROUT(J, CURWAY) .EQ. ROUACT) THEN
C
	    IF (NETSTAT(J, CURWAY) .EQ. NSTASEC) THEN
	      SERDISP(J) = NXTSER
C
CCAL	      IF (CURWAY .NE. WAYINP) SERDISP(J) = RLGSER	! CALIFORNIA ?
C
	    ELSEIF (NETSTAT(J, CURWAY) .EQ. NSTAPRIM) THEN
	      IF (NETMODE(J, CURWAY) .EQ. FILMD) THEN
		SERDISP(J) = NETSER(J, CURWAY)
	      ELSE
		SERDISP(J) = NETHSER(J, CURWAY)
	      ENDIF
	    ENDIF
	  ENDIF

C	  SERDISP(J) = MOD(SERDISP(J), SYSOFF)
1600	CONTINUE
C
	DO 1700 J = 1, NETSYS
	  OFF12 = (J - 1) * 12 + 20
	  WRITE(XNEW(13)(OFF12:OFF12+11), 9220) SERDISP(J)
1700	CONTINUE
C
	NXT = NXTSER
C
CCAL	IF (CURWAY .NE. WAYINP .AND.
CCAL *      CURWAY .NE. WAYLOG) NXT = RLGSER			! CALIFORNIA ?
C
	LCMD = P(LSTCMD)
C
CCAL	IF (CURWAY .NE. WAYINP .AND.				! CALIFORNIA ?
CCAL *      CURWAY .NE. WAYLOG) LCMD = RLGLST
C
	HBK = P(PPISR3)
	IF (CURWAY .NE. WAYINP) HBK = 0
C
	WRITE(CLIN14, 9230) NXT, HBK, LCMD, P(CMDFRZ)
C
	WRITE(CLIN16, 9240) NETCMDFRZ, P(NETFLU)
C
	WRITE(CLIN17, 9250)
C
	WRITE(XNEW(17)( 9:16), 9260) QUECNT(NETFREE(1, CURWAY))
	WRITE(XNEW(17)(17:24), 9260) QUECNT(NETEXEC(1, CURWAY))
	WRITE(XNEW(17)(25:32), 9260) QUECNT(NETFIL(1, CURWAY))
	WRITE(XNEW(17)(33:40), 9260) QUECNT(NETFINISH(1))
	WRITE(XNEW(17)(41:48), 9260) QUECNT(REMFINISH(1))
	WRITE(XNEW(17)(49:56), 9260) QUECNT(NETEXTRA(1, CURWAY))
	WRITE(XNEW(17)(57:64), 9260) QUECNT(RECOVQUE(1))
	WRITE(XNEW(17)(65:80), 9270) TOUNFREEZ
C
	WRITE(CLIN20, 9280) (NETENA(J, CURWAY), J = 1, NETSYS)
C
	WRITE(CLIN21, 9290) NETIO(1, CURWAY), NETIO(2, CURWAY),
     *                     SYNCWAIT, TOFREEZ
C
	WRITE(CLIN22, 9300) NETWAIT, RETTIM, MAXFREEZ
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FORMAT STATEMENTS.
C
9000	FORMAT('Input error')
9010	FORMAT('System in recovery mode, please wait and try again')
9020	FORMAT('Link not up yet, please wait and try again')
9030	FORMAT('Gamestate not set, please wait and try again')
9040	FORMAT('Value error')
9050	FORMAT('Path error')
9060	FORMAT('System error! Check input queue')
9070	FORMAT('Network not defined yet, please wait and try again')
9080	FORMAT('<< > System Network < >>')
9090	FORMAT(80(' '))
9100	FORMAT('System id: ',  A1, '(', I1, ')', 2X,
     *         '*BACKup id: ', A1, '(', I1, ')', 2X,
     *         'Primary id: ', A1, '(', I1, ')', 2X,
     *         '*TAKEover time: ', I10)
9110	FORMAT('Send cnt', 2X, <NETSYS>(I4), 2X,
     *         'Read cnt', 2X, <NETSYS>(I4))
9120	FORMAT(20X, <NETSYS>(3X, A1, '(', I1, ')', 5X))
9130	FORMAT('Activity status     ', <NETSYS>(A8, 4X))
9140	FORMAT('Connection type     ', <NETSYS>(A8, 4X))
9150	FORMAT('Data transfer mode  ')
9160	FORMAT(A8, 4X)
9170	FORMAT('Time from last I/O  ', <NETSYS>(I8,4X))
9180	FORMAT('I/O errors          ', <NETSYS>(I8,4X))
9190	FORMAT('Time last trans sent', <NETSYS>(I8,4X))
9200	FORMAT('Time last ack recv''d',<NETSYS>(I8,4X))
9210	FORMAT('Serial sent/received')
9220	FORMAT(I9, 3X)
9230	FORMAT('Nxtser:',       I9, 2X, 'High to backup:', I9, 2X,
     *         'Last balance:', I9, 2X, 'Cmdfrz:', I5)
9240	FORMAT('Queues *', 3X,
     *         'Free   ',  'Exec    ', 'File    ',
     *         'Finish  ', '  Rem   ', ' Extra  ', 'Recov   ',
     *         'Ncmdfz:', I4, '(', I1, ')')
9250	FORMAT(3X, 'net *')
9260	FORMAT(I7, X)
9270	FORMAT(2X, 'Tounfr: ', I6)
9280	FORMAT('*ENAAsync:', I3, 3X, '*ENABsync:', I3, 3X,
     *         '*ENACsync:', I3, 3X, '*ENADsync:', I3, 2X,
     *         '*ENAEsync:', I3)
9290	FORMAT('I/O''s in progress: Input-> ', I4, ' Recov-> ', I4, 2X,
     *         '*SYNCWT:', I8, ' *TOFreez:', I8)
9300	FORMAT(34X, 'Netwait:', I6, 'Rettime:', I6, 1X, 'Maxfreez:', I8)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
