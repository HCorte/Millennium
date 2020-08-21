C
C *** PROGRAM LNKCH ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]LNKCH.FOV                                    $
C  $Date::   17 Apr 1996 13:51:04                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - lnkch.for ***
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
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
C Purpose:
C	PROGRAM FOR LINK MANIPULATION ...
C	IT MAY ADD A NEW LINK, REMOVE THE EXISTING ONE,
C	OR SET THE SECONDARY SYSTEM TO MASTER MODE.
C	NONRELEVANT REQUESTS ARE REJECTED.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	PROGRAM LNKCH
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DCNEVN.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
C
        INCLUDE '($SYSSRVNAM)'
C
C PARAMETER DECLARATIONS
C
	INTEGER*4	CON,
     *			CONNUM,
     *			MODNUM,
     *			STATNU
C
	PARAMETER	(CON    = 5)
	PARAMETER	(CONNUM = 3)
	PARAMETER	(MODNUM = 6)
	PARAMETER	(STATNU = 4)
C
C LOCAL DECLARATIONS
C
	REAL*8		LNKNAM	/'LNKCH   '/,
     *			MENAME
C
	INTEGER*4	ANSWER,
     *			ARROW			/' -> '/,
     *			BACKUP,
     *			BUF,
     *			BUF2,
     *			CNT,
     *			CONEX,
     *			CONN,
     *			CONNECT(0:NETSYS),
     *			COUNT,
     *			CSYS,
     *			CURWAY,
     *			DASH			/' -- '/,
     *			E,
     *			I,
     *			K,
     *			LINE1(20),
     *			LINE2(20),
     *			LIVE,
     *			LUN,
     *			MD,
     *			NAME(0:MAX_SYSTEMS)	/'****', 'A   ',
     *						 'B   ', 'C   ',
     *						 'D   ', 'E   '/,
     *			NBUF(CDLEN),
     *			NT,
     *			OFF,
     *			OFFX,
     *			OPT,
     *			OPTION,
     *			REQ,
     *			RET,
     *			ROUTYP,
     *			SECOND,
     *			ST,
     *			STATUS,
     *			SYS,
     *			SYS1,
     *			SYSCNT,
     *			TAB(0:NETSYS),
     *			TEMP,
     *			UNIT
C
	CHARACTER*80	CLINE1,
     *			CLINE2
C
	CHARACTER*43	LINE
     *			/'                                           '/
C
	CHARACTER*24	BACKMES,
     *			ISBACKUP/'  ARROW POINTS TO BACKUP'/,
     *			NOBACKUP/'                        '/
C
	CHARACTER*20	PASPAS
C
	CHARACTER*12	CONNEC(0:CONNUM-1)	/'NO CONNECTN ',
     *						 'NOT ACTIVE  ',
     *						 'ACTIVE      '/,
     *			CONTYP(-2:STATNU-1)	/'SECONDARY   ',
     *						 'PRIMARY     ',
     *						 'NOT ACTIVE  ',
     *						 'PRIMARY     ',
     *						 'SECONDARY   ',
     *						 'DEAD        '/,
     *			DMODE(0:MODNUM)		/'************',
     *						 'NORMAL      ',
     *						 'RECOVERY    ',
     *						 'ERROR       ',
     *						 'DUMMY       ',
     *						 'COMMAND     ',
     *						 'DRIVER CMD  '/,
     *			WAYS(3)			/'            ',
     *						 'REMOTE SEND ',
     *						 'REMOTE RECV '/
C
	CHARACTER*8	NAMESYS(0:3)		/'********', 'PRIMARY ',
     *						 'BACKUP  ', 'SEC-DARY'/
C
	CHARACTER*5	SIGNON
C
	CHARACTER*4	GXEVNNAM			! NAME FUNCTION
C
	CHARACTER*1	ANS,
     *			LOWER_CASE(MAX_SYSTEMS)	/'a', 'b',
     *						 'c', 'd', 'e'/,
     *			UPPER_CASE(MAX_SYSTEMS)	/'A', 'B',
     *						 'C', 'D', 'E'/
C
	LOGICAL*4	FORGETIT,
     *			PRIV,
     *			SSA
C
	EQUIVALENCE	(CLINE1, LINE1(1))
	EQUIVALENCE	(CLINE2, LINE2(1))
	EQUIVALENCE	(PASPAS, SIGNON)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C CALL COPYRITE & GET MY TASK NAME.
C
	CALL COPYRITE
C
	CALL GETNAM(MENAME)
C
C CREATE THE COMMON EVENT FLAG CLUSTER.
C
	STATUS = SYS$ASCEFC(%VAL(DN_EVNTIMER),
     *                      GXEVNNAM() // DN_EVNNAME, 0, 0)
	IF (.NOT. STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
C
	PRIV = .FALSE.
	IF (MENAME .NE. LNKNAM) PRIV = .TRUE.
C
	IF (PRIV) WRITE(CON, 9000) IAM(), IAM(), IAM(), IAM()
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C MAIN LOOP
C
100	CONTINUE
	OPTION = 1
C
	IF (OPTION .EQ. 1) THEN
	  CURWAY = WAYINP
	  OPT    = 1
	ELSEIF (OPTION .EQ. 2) THEN
	  IF (WAYLOG .NE. 0) THEN
	    CURWAY = WAYLOG
	    OPT    = 2
	  ENDIF
	ELSEIF (OPTION .EQ. 3) THEN
	  IF (OTHRLOG .GT. 0) THEN
	    CURWAY = OTHRLOG
	    OPT    = 3
	  ENDIF
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C GET PASSWORD TO ENTER VISPAS
C
	IF (NODEID .NE. NETMASTER(CURWAY)) THEN
	  CNT = 0
150	  CONTINUE
	  CALL CLRSCR(5)
	  CALL PASSWORD(5, PASPAS)
	  IF (SIGNON .NE. 'ELMER') THEN
	    CNT = CNT + 1
	    IF (CNT .GT. 5) THEN
	      TYPE *, IAM(), CHAR(7),
     *                'YOU HAVE USED ALL OF YOUR CHANCES... BYE...'
	      CALL USRCLOS1(5)
	      CALL GSTOP(GEXIT_FATAL)
	    ENDIF
	    GOTO 150
	  ENDIF
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C DISPLAY CONFIGURATION
C
	WRITE(CON, 9010) IAM(), IAM(), IAM(),
     *                   WAYS(OPTION), IAM(), IAM()
C
	MD = 3
	IF (NETMASTER(CURWAY) .EQ. NODEID) MD = 1
	IF (NETBACKUP(CURWAY) .EQ. NODEID) MD = 2
	WRITE(CON, 9020) IAM(), NAME(NODEID), NAMESYS(MD)
C
	IF (NODEID .NE. NETMASTER(CURWAY))
     *    WRITE(CON, 9030) IAM(), NAME(NETMASTER(CURWAY))
C
	IF (NODEID .NE. NETBACKUP(CURWAY) .AND.
     *      NETBACKUP(CURWAY) .NE. 0)
     *    WRITE(CON, 9040) IAM(), NAME(NETBACKUP(CURWAY))
C
	WRITE(CON, 9050) IAM()
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	CALL FASTSET('20202020'X, LINE1, 20)
C
	CONEX    = 0
	OFF      = 1
	LINE1(1) = 'Syst'
	LINE1(2) = 'em i'
	LINE1(3) = 'd   '
C
	DO 200 SYS = 1, NETSYS
	  IF (NETROUT(SYS, CURWAY) .GE. ROUIDLE .AND.
     *        SYS .NE. NODEID) THEN
	    LINE1((OFF - 1) * 3 + 6) = NAME(SYS)
	    OFF = OFF + 1
	  ENDIF
200	CONTINUE
C
	WRITE(CON, 9060) IAM(), LINE1
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	CALL FASTSET('20202020'X, LINE1, 20)
C
	OFF      = 1
	LINE1(1) = 'Stat'
	LINE1(2) = 'us  '
C
	DO 300 SYS = 1, NETSYS
	  IF (NETROUT(SYS, CURWAY) .GE. ROUIDLE .AND.
     *        SYS .NE. NODEID) THEN
	    OFFX = (OFF - 1) * 12 + 20
	    WRITE(CLINE1(OFFX:80), 9070) CONNEC(NETROUT(SYS, CURWAY))
	    CONEX = 1
	    OFF   = OFF + 1
	  ENDIF
300	CONTINUE
C
	WRITE(CON, 9060) IAM(), LINE1
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	CALL FASTSET('20202020'X, LINE1, 20)
	CALL FASTSET('20202020'X, LINE2, 20)
C
	OFF      = 1
	LINE1(1) = 'Conn'
	LINE1(2) = 'ecti'
	LINE1(3) = 'on t'
	LINE1(4) = 'ype '
	LINE2(1) = 'Tran'
	LINE2(2) = 'sfer'
	LINE2(3) = ' mod'
	LINE2(4) = 'e   '
C
	DO 400 SYS = 1, NETSYS
	  SYS1 = SYS
	  IF (NETROUT(SYS, CURWAY) .GE. ROUIDLE .AND.
     *        NETSTAT(SYS, CURWAY) .NE. NSTAIDLE .AND.
     *        SYS .NE. NODEID) THEN
	    IF (NETSTAT(SYS, CURWAY) .EQ. NSTASEC) SYS1 = NODEID
	    OFFX = (OFF - 1) * 12 + 20
	    WRITE(CLINE1(OFFX:80), 9070) CONTYP(NETSTAT(SYS, CURWAY))
	    WRITE(CLINE2(OFFX:80), 9070) DMODE(NETMODE(SYS1, CURWAY))
	    CONEX = 1
	    OFF   = OFF + 1
	  ENDIF
400	CONTINUE
C
	WRITE(CON, 9060) IAM(), LINE1
	WRITE(CON, 9060) IAM(), LINE2
C
	IF (CONEX .EQ. 0) WRITE(CON, 9080) IAM(), IAM()
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C MENU
C
500	CONTINUE
	DO 600 K = 0, NETSYS
	  CONNECT(K) = 0
	  TAB(K)     = 0
600	CONTINUE
C
	LIVE = NETMASTER(WAYINP)
	IF (NODEID .NE. LIVE) CONNECT(NODEID) = LIVE
	CONNECT(LIVE) = 0
C
	BACKUP      = NETBACKUP(WAYINP)
	SECOND      = 0
	SYS         = 0
	TAB(BACKUP) = BACKUP
	TAB(LIVE)   = LIVE
	TAB(NODEID) = NODEID
C
	DO 700 OFF = 1, NETSYS
	  IF (CONNECT(OFF) .NE. 0) THEN
	    IF (SECOND .EQ. 0) THEN
	      SECOND = OFF
	    ELSE
	      SYS    = OFF
	    ENDIF
	  ENDIF
700	CONTINUE
C
	DO 800 K = 1, NETSYS
	  IF (NETROUT(K, WAYINP) .EQ. ROUACT) THEN
	    TAB(K) = K
	    IF (NETSTAT(K, WAYINP) .EQ. NSTAPRIM) THEN
	      CONNECT(K) = NODEID
	    ELSEIF (NETSTAT(K, WAYINP) .EQ. NSTASEC) THEN
	      IF (SYS .NE. 0) THEN
		IF (.NOT. (NETSTAT(SYS, WAYINP) .EQ. NSTASEC .AND.
     *                     NETROUT(SYS, WAYINP) .EQ. ROUACT) .OR.
     *              K .NE. LIVE) CONNECT(NODEID) = K
	      ENDIF
	    ENDIF
	  ENDIF
800	CONTINUE
C
	IF (BACKUP .NE. 0)
     *    CONNECT(BACKUP) = LIVE
C
	IF (NODEMASTER(WAYINP) .NE. 0)
     *    CONNECT(NODEID) = NODEMASTER(WAYINP)
C
	IF (NODEID .NE. LIVE .AND. CONNECT(NODEID) .NE. LIVE)
     *    CONNECT(SYS) = LIVE
C
	SECOND = 0
	DO 900 K = 1, NETSYS
	  IF (CONNECT(K) .NE. 0) THEN
	    SECOND = K
	  ELSE
	    SYS    = K
	  ENDIF
900	CONTINUE
C
	SYSCNT = 0
	DO 1000 K = 1, NETSYS
	  IF (TAB(K) .NE. 0) SYSCNT = SYSCNT + 1
1000	CONTINUE
C
	IF (SYSCNT .NE. 1) THEN
	  TYPE *, IAM()
	  BACKMES = NOBACKUP
	  CONN    = DASH
	  IF (BACKUP .NE. 0) THEN
	    BACKMES = ISBACKUP
	    CONN    = ARROW
	  ENDIF
	  IF (SYSCNT .EQ. 2) THEN
	    TYPE 9090, IAM(), NAME(LIVE), CONN, NAME(SECOND), BACKMES
	  ELSE
	    IF (SYS .NE. 0) THEN
	      IF (CONNECT(SYS) .EQ. LIVE) THEN		! CONNECTED TO PRIMARY
		IF (BACKUP .EQ. SYS) THEN
		  TEMP   = SYS
		  SYS    = SECOND
		  SECOND = TEMP
		ENDIF
		TYPE 9100, IAM(), NAME(LIVE), CONN, NAME(SECOND),
     *                    BACKMES, IAM(), IAM(), NAME(SYS)
	      ELSE
		TYPE 9110, IAM(), NAME(LIVE), CONN, NAME(SECOND),
     *                    NAME(SYS), BACKMES
	      ENDIF
	    ENDIF
	  ENDIF
	ENDIF
	TYPE *, IAM()
C
D	TYPE *, 'CONNECT   ', CONNECT
D	TYPE *, 'NETASSN   ', NETASSN
D	TYPE *, 'NETATR    ', NETATR
D	TYPE *, 'NETBACKUP ', NETBACKUP
D	TYPE *, 'NETMASTER ', NETMASTER
D	TYPE *, 'NETOPN    ', NETOPN
D	TYPE *, 'NETROUT   ', NETROUT
D	TYPE *, 'NETSTAT   ', NETSTAT
D	TYPE *, 'OPNNET    ', OPNNET
D	TYPE *, 'TAB       ', TAB
D	TYPE *, 'WAYINP    ', WAYINP
D	TYPE *, 'WAYLOG    ', WAYLOG
D	PAUSE
D	GOTO 2000
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C MENU SELECTION
C
1100	CONTINUE
	WRITE(CON, 9115) IAM(), IAM(), IAM(), IAM(), IAM(), IAM()
C
	CALL INPNUM('ENTER REQUEST [1-3]:', REQ, 1, 3, E)
	IF (E .LT. 0) CALL GSTOP(GEXIT_OPABORT)
C
	GOTO(1200, 1300, 1400) REQ
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ADD LINK
C
1200	CONTINUE
	CALL WIMG(CON, 'ADD LINK TO WHICH SYSTEM [LETTER CODE]:')
	READ(CON, 9120) ANS
C      
        DO 1210 CSYS = 1, NETSYS
	  IF (ANS .EQ. LOWER_CASE(CSYS) .OR.
     *        ANS .EQ. UPPER_CASE(CSYS)) GOTO 1220
1210	CONTINUE
	WRITE(CON, 9130) IAM()
	GOTO 1200
C
1220	CONTINUE
	IF (CSYS .EQ. NODEID) THEN
	  WRITE(CON, 9130) IAM()
	  GOTO 1200
	ENDIF
C
	IF (NETROUT(CSYS, CURWAY) .EQ. ROUACT) THEN
	  WRITE(CON, 9140) IAM(), NAME(CSYS)
	  CALL WIMG(CON, 'CONTINUE [Y/N]:')
	  CALL YESNO(ANSWER)
	  IF (ANSWER .NE. 1) GOTO 1100
	ENDIF
C
	WRITE(CON, 9150) IAM(), NAME(CSYS), IAM(), NAME(CSYS)
C
	CALL INPNUM('ENTER 1 OR 2:', ROUTYP, 1, 2, E)
	IF (E .LT. 0) CALL GSTOP(GEXIT_OPABORT)
C
	IF ((NODEID .EQ. NETMASTER(CURWAY) .AND.
     *       ROUTYP .EQ. NSTASEC) .OR.
     *      (CSYS   .EQ. NETMASTER(CURWAY) .AND.
     *       ROUTYP .EQ. NSTAPRIM)) THEN
	  WRITE(CON, 9130) IAM()
	  GOTO 1200
	ENDIF
C
	IF (P(SYSTYP) .NE. LIVSYS .AND. ROUTYP .EQ. NSTAPRIM) THEN
	  WRITE(CON, 9160) IAM(), IAM(), IAM(), IAM(), IAM(),
     *                    NAME(NETMASTER(WAYINP)), NAME(CSYS),
     *                    IAM(), IAM(), IAM()
	  CALL WIMG(CON, 'CONTINUE [Y/N]:')
	  CALL YESNO(ANSWER)
	  IF (ANSWER .NE. 1) THEN
	    WRITE(CON, 9170) IAM()
	    GOTO 2000
	  ENDIF
	ENDIF
C
1230	CONTINUE
	CALL EXTRABUF(BUF, CURWAY, ST)
	IF (ST .EQ. 2) THEN
	  CALL XWAIT(20, 1, ST)
	  GOTO 1230
	ENDIF
C
	NETBUF(NEXT,     BUF) = HDRSIZ + 5
	NETBUF(MODE,     BUF) = DRVMD
	NETBUF(WAYNR,    BUF) = CURWAY
	NETBUF(HDRSIZ+1, BUF) = ADDLINK
	NETBUF(HDRSIZ+2, BUF) = CSYS
	NETBUF(HDRSIZ+3, BUF) = ROUTYP
	NETBUF(HDRSIZ+4, BUF) = CURWAY
	NETROUT(CSYS, CURWAY) = ROUACT
	NETSTAT(CSYS, CURWAY) = ROUTYP
C
	CALL TSNDNET(BUF, CURWAY)
C
	TYPE *, IAM(), 'ADDLINK SENT TO LNK_CMDNET'
C
	IF (NODEID .EQ. NETMASTER(CURWAY)) THEN
	  CALL XWAIT (5, 2, ST)
	  CALL GAMECMD(ADDLINK, CSYS, ROUTYP, CURWAY)
	ENDIF
C
	GOTO 2000					! DONE ... WHAT ELSE ?
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C REMOVE LINK
C
1300	CONTINUE
	CALL WIMG(CON, 'REMOVE LINK WITH WHICH SYSTEM [LETTER CODE]:')
	READ(CON, 9120) ANS
C
	DO 1310 CSYS = 1, NETSYS
	  IF (ANS .EQ. LOWER_CASE(CSYS) .OR.
     *        ANS .EQ. UPPER_CASE(CSYS)) GOTO 1320
1310	CONTINUE
	WRITE(CON, 9130) IAM()
	GOTO 1300
C
1320	CONTINUE
	IF (CSYS .EQ. NODEID) THEN
	  WRITE(CON, 9130) IAM()
	  GOTO 1300
	ENDIF
C
	CALL MODUL(CSYS, CURWAY, RET)
	IF (RET .NE. 0) THEN				! CAN'T HELP YOU
	  WRITE(CON, 9180) IAM(), NAME(CSYS)
	  GOTO 2000
	ENDIF
C
	FORGETIT = .FALSE.
C
	IF (P(SYSTYP) .NE. LIVSYS .AND.
     *      CURWAY .EQ. WAYLOG) FORGETIT = .TRUE.
C
	IF (P(SYSTYP) .NE. LIVSYS .AND.
     *      CURWAY .EQ. WAYINP .AND.
     *      .NOT. PRIV .AND.
     *      NETMODE(NODEID, CURWAY) .NE. FILMD) FORGETIT = .TRUE.
C
	IF (P(SYSTYP) .EQ. LIVSYS .AND.
     *      CURWAY .EQ. WAYINP .AND.
     *      .NOT. PRIV .AND.
     *      NETMODE(CSYS, CURWAY) .NE. FILMD ) FORGETIT = .TRUE.
C
	IF (CSYS .EQ. NETMASTER(CURWAY) .OR.
     *      NETSTAT(CSYS, CURWAY) .EQ. NSTASEC) THEN
	  CALL WIMG(CON, 'YOU ARE CUTTING OFF PRIMARY, CONTINUE [Y/N]:')
	  CALL YESNO(ANSWER)
	  IF (ANSWER .NE. 1) GOTO 2000
	ELSE
	  IF (CURWAY .EQ. WAYINP .AND. CSYS .EQ. NETBACKUP(WAYINP)) THEN
	    WRITE(CON, 9190) IAM(), NAME(CSYS), IAM(), IAM()
	    CALL WIMG(CON, 'CONTINUE [Y/N]:')
	    CALL YESNO(ANSWER)
	    IF (ANSWER .NE. 1) GOTO 2000
	  ENDIF
	ENDIF
C
1325	CONTINUE
	CALL EXTRABUF(BUF, CURWAY, ST)
	IF (ST .EQ. 2) THEN
	  CALL XWAIT(20, 1, ST)
	  GOTO 1325
	ENDIF
C
C IF ATTEMPTING TO REMOVE THE LINK TO THE HOT BACKUP,
C SEND A BACKUP ID CHANGE FIRST.
C THIS WILL ALLOW THAT SYSTEM TO BE RELINKED IN IF NECESSARY.
C IF A BUFFER IS NOT AVAILABLE ASK USER IF HE WANTS TO CONTINUE.
C TELL THAT HE CANNOT RELINK.
C
	IF (NETBACKUP(CURWAY) .EQ. CSYS) THEN
	  COUNT   = 0
	  NBUF(1) = 3
	  NBUF(2) = 0
	  NBUF(3) = TCNET
	  NBUF(6) = 'LNKC'
	  NBUF(9) = CURWAY
1330	  CONTINUE
	  CALL QUECMD(NBUF, STATUS)
	  IF (STATUS .NE. 0) THEN
	    IF (COUNT .LE. 5) THEN
	      COUNT = COUNT + 1
	      TYPE *, IAM(), 'UNABLE TO GET BUFFER, RETRYING ...'
	      CALL XWAIT(20, 1, ST)
	      GOTO 1330
	    ELSE
	      TYPE *, IAM(), 'UNABLE TO GET BUFFER ...'
	      TYPE *, IAM(), 'YOU CANNOT RELINK IN SYS ',CSYS
	      TYPE *, IAM(), 'WITHOUT FIRST DOING A KILSYS ON SYS ',CSYS
	    ENDIF
	  ENDIF
	  CALL XWAIT(1, 2, ST)				! WAIT A SECOND
	ENDIF 
C
	NETBUF(NEXT,     BUF) = HDRSIZ + 5
	NETBUF(MODE,     BUF) = DRVMD
	NETBUF(WAYNR,    BUF) = CURWAY
	NETBUF(HDRSIZ+1, BUF) = REMLINK
	NETBUF(HDRSIZ+2, BUF) = CSYS
	NETBUF(HDRSIZ+3, BUF) = 0
	NETBUF(HDRSIZ+4, BUF) = CURWAY
	NETSTAT(CSYS, CURWAY) = -IABS(NETSTAT(CSYS, CURWAY))
	NETROUT(CSYS, CURWAY) = ROUIDLE
C
	CALL TSNDNET(BUF, CURWAY)
C
	IF (NODEID .EQ. NETMASTER(CURWAY))
     *    CALL GAMECMD(REMLINK, CSYS, 0, CURWAY)
C
C THIS CODE IS TO GO AROUND SSA DRIVER BUGS
C
	LUN = NETOPN(CSYS, CURWAY)
	IF (LUN .GT. 0) THEN
	  CALL XWAIT(10, 2, ST)				! WAIT 10 MSEC
	  SSA  = IAND(NETDEV(1, LUN), 'FFFFFF00'X) .EQ. '53534100'X
	  UNIT = LUN
	  IF (SSA) UNIT = LUN + 1
	  IF (READIOCHK(UNIT) .NE. 0) THEN
	    TYPE *, IAM(), 'I/O TRAP MISSING ', UNIT
	    TYPE *, IAM(), 'CONTACT SOFTWARE SUPPORT'
	  ENDIF
	  READIOCHK(UNIT) = 0
	  IF (NETIO(1, CURWAY) .NE. 0 .OR.
     *        NETIO(2, CURWAY) .NE. 0) THEN
	    TYPE *, IAM(), 'SEND TRAP NOT TERMINATED ',
     *              NETIO(1, CURWAY), NETIO(2, CURWAY)
	    TYPE *, IAM(), 'CONTACT SOFTWARE SUPPORT'
	    NETIO(1, CURWAY) = 0
	    NETIO(2, CURWAY) = 0
	    TYPE *, IAM()
	    TYPE *, IAM(), 'TRY TO REMOVE LINK AGAIN'
	  ENDIF
	ENDIF
C
	GOTO 2000					! DONE ... WHAT ELSE ?
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C SETMASTER
C
1400	CONTINUE
	IF (.NOT. PRIV) THEN
	  WRITE(CON, 9200) IAM(), IAM()
	  GOTO 2000
	ENDIF
C
	IF (CURWAY .NE. WAYINP) THEN
	  WRITE(CON, 9210) IAM()
	  GOTO 2000
	ENDIF
C
	CALL WIMG(CON, 'WHICH SYSTEM SET AS PRIMARY [LETTER CODE]:')
	READ(CON, 9120) ANS
C
	DO 1410 CSYS = 1, NETSYS
	  IF (ANS .EQ. LOWER_CASE(CSYS) .OR.
     *        ANS .EQ. UPPER_CASE(CSYS)) GOTO 1420
1410	CONTINUE
	WRITE(CON, 9130) IAM()
	GOTO 1400
C              
1420	CONTINUE
	IF (CSYS .EQ. NETMASTER(CURWAY)) THEN
	  WRITE(CON, 9220) IAM()
	  GOTO 2000
	ENDIF
C
	IF (CSYS .NE. NODEID) THEN
	  WRITE(CON, 9170) IAM()
	  GOTO 2000
	ENDIF
C
	CALL SETMAS(CSYS, RET)
	IF (RET .EQ. 1 .OR. RET .EQ. 2) THEN
	  TYPE *, IAM(), 'SET MASTER NOT CONFIRMED'
	  IF (CSYS .EQ. NODEID) GOTO 2000		! DONT CALL LNK_CMDNET
C
1430	  CONTINUE
	  CALL EXTRABUF(BUF2, CURWAY, ST)
	  IF (ST .EQ. 2) THEN
	    CALL XWAIT(20, 1, ST)
	    GOTO 1430
	  ENDIF
C
	  NETBACKUP(CURWAY) = 0
C
	  NETBUF(MODE,     BUF2) = CMDMD
	  NETBUF(WAYNR,    BUF2) = CURWAY
	  NETBUF(HDRSIZ+1, BUF2) = SETMASTER
	  NETBUF(HDRSIZ+2, BUF2) = CSYS
	  NETBUF(HDRSIZ+3, BUF2) = 0
	  NETBUF(HDRSIZ+4, BUF2) = CURWAY
C
	  CALL LNK_CMDNET(BUF2)
D	  TYPE *, IAM(), 'ADD LINK PRI TO ', CSYS
	ENDIF
C
	IF (RET .EQ. 3) THEN
	  TYPE *, IAM(), 'SETMASTER NOT ATTEMPTED'
	  GOTO 100
	ENDIF
C
	GOTO 2000					! DONE ... WHAT ELSE ?
C
2000	CONTINUE
	CALL WIMG(CON, 'JOB DONE, ANYTHING ELSE [Y/N]:')
	CALL YESNO(ANSWER)
	IF (ANSWER .EQ. 1) GOTO 100
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FORMAT STATEMENTS.
C
9000	FORMAT(X, A, /,
     *         X, A, ' THE PROGRAM IS CURRENTLY IN PRIVILEGED MODE', /,
     *         X, A, /,
     *         X, A)
9010	FORMAT(X, A, /,
     *         X, A, /,
     *         X, A, ' CURRENT NETWORK CONFIGURATION ', A12, /,
     *         X, A, /,
     *         X, A)
9020	FORMAT(X, A, ' THIS IS SYSTEM ', A1,
     *               ' WORKING IN ', A8, ' MODE')
9030	FORMAT(X, A, ' PRIMARY SYSTEM IS ', A1, /)
9040	FORMAT(X, A, ' BACKUP  SYSTEM IS ', A1, /)
9050	FORMAT(X, A, ' OTHER CONNECTED SYSTEMS:')
9060	FORMAT(X, A, X, 20A4)
9070	FORMAT(A12)
9080	FORMAT(X, A, /,
     *         X, A, ' NO OTHER CONNECTIONS CONFIGURED')
9090	FORMAT(X, A, 5X, '(', A1, ')', A4, A1, 10X, A24)
9100	FORMAT(X, A, 5X, '(', A1, ')', A4, A1, 10X, A24, /,
     *         X, A, 6X, '|', /,
     *         X, A, 6X, A1)
9110	FORMAT(X, A, 5X, '(', A1, ')', A4, A1, ' -- ', A1, 5X, A24)
9115	FORMAT(X, A, /,
     *         X, A, ' MENU:  ADDLINK   - ENTER (1)', /,
     *         X, A, '        REMLINK   - ENTER (2)', /,
     *         X, A, '        SETMASTER - ENTER (3)', /,
     *         X, A, /,
     *         X, A, '                    ENTER (E) TO EXIT')
9120	FORMAT(A1)
9130	FORMAT(X, A, ' INPUT INCORRECT OR INCONSISTENT ... TRY AGAIN')
9140	FORMAT(X, A, ' SYSTEM ', A1, ' IS ALREADY ACTIVE')
9150	FORMAT(X, A, ' IF YOU WANT TO WRITE TO SYSTEM  ', A1,
     *               ' - ENTER (1)', /,
     *         X, A, ' IF YOU WANT TO READ FROM SYSTEM ', A1,
     *               ' - ENTER (2)')
9160	FORMAT(X, A, ' *****************************************', /,
     *         X, A, ' **************** WARNING ****************', /,
     *         X, A, ' *****************************************', /,
     *         X, A, ' **  BEFORE CONTINUING, MAKE SURE THAT  **', /,
     *         X, A, ' ** SYSTEM ', A1,
     *               ' IS NOT WRITING TO SYSTEM ', A1,      ' **', /,
     *         X, A, ' *****************************************', /,
     *         X, A, ' **************** WARNING ****************', /,
     *         X, A, ' *****************************************')
9170	FORMAT(X, A, ' REQUEST NOT ADEQUATE TO THE SYSTEMS CONFIGURATION')
9180	FORMAT(X, A, ' SYSTEM ', A1, ' IS NOT CONFIGURED')
9190	FORMAT(X, A, ' SYSTEM ', A1, ' IS CONNECTED AS YOUR SECONDARY ', /,
     *         X, A, ' THE OPERATION YOU ARE ABOUT TO UNDERTAKE', /
     *         X, A, ' MAY CAUSE AN UNNECESSARY TAKEOVER')
9200	FORMAT(X, A, /,
     *         X, A, ' THIS OPERATION ONLY ALLOWED IN PRIVILEDGED MODE')
9210	FORMAT(X, A, ' SORRY ... WE CANNOT DO IT ON NON-LOCAL PATH !')
9220	FORMAT(X, A, ' IT IS ALREADY PRIMARY')
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C END.
C
	TYPE *, IAM(), 'NORMAL TERMINATION'
	CALL GSTOP(GEXIT_SUCCESS)
	END
