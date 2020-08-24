C SUBROUTINE GETSYS
C
C V11 01-APR-2016 SCML M16 PROJECT: added EURS_XRFOFF
C V10 18-JAN-2011 FJG TIR#2293 Out of bounds error
C V09 18-SEP-2000 UXN Changes to determine defined systems automatically
C                     through GTECH_REMSYS% logicals. ASK_CONN_IN_TRIPLEX
C                     added.
C V08 03-MAR-1996 WSM X2X Upgrade: Changed to single site for Finland. 
C V07 10-JAN-1994 RXD X2X baseline update for U.k. Gsat,async,dial,x.28.
C V06 01-ARP-1993 DAS Removed all references to BACKUP, 
C                     since it is no longer set in this routine.
C V05 30-MAR-1993 DAS Fixed problem with SYSSER
C V04 26-MAR-1993 DAS Rewritten to handle 5 systems (A,B,C,D and E)
C V03 13-DEC-1991 JPJ UPDATED FOR LOUISIANA (3 - SYSTEMS AT PRIMARY)
C	  			      	    (1 - REMOTE SYSTEM)
C V02 29-MAY-1991 ??? INITIAL RELEASE FOR MARYLAND
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C NAME: GETSYS
C
C FUNC: GET THE SYSTEM NAME (A, B, C), AND SYSTEM CONNECTION  DATA
C       FROM THE OPERATOR. (set NETWORK tables)
C
C
C     THIS PROGRAM SUPPORTS CONFIGURATION AS BELOW AND ALL CONFIGS
C     OBTAINED BY PERMUTATIONS OF SYSTEM NAMES
C
C     A-B - INPUT PATH NR 1
C     |
C     C
C
C     OR
C
C     A-B-C - INPUT PATH 1
C
C
C
C INPUT:
C       FROM OPERATOR
C OUTPUT:
C       FOLLOWING /CONCOM/ FIELDS ARE SET:
C         P(SYSTYP)
C         P(SYSNAM)
C       FOLLOWING /NETCOM/ FIELDS ARE SET:
C         NETROUT,NETSTAT,NETMASTER,NETBACKUP,NETRDY
C	  NETSITE
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
C
	SUBROUTINE GETSYS
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:EURCOM.DEF'                                                   !V11
C
	REAL*8 SYSTXT(0:3)/'********','PRIMARY ','BACKUP  ',' SPARE  '/
C
	INTEGER*4 NODE, ACTIVE_SYSTEMS, TEMP
	INTEGER*4 PRIMARY, FLAG, ST, OFF2, OFF1, OFF, SECOND
	INTEGER*4 SYS, SYSNO, NODEID_PORT
	INTEGER*4 TAB(0:NETSYS)
	INTEGER*4 TMPNAM /'SYS@'/
	INTEGER*4 OPNNAM(2) /'SYS@','.ID '/ !SYS?
	INTEGER*4 CONNECT(0:NETSYS)
	INTEGER*4 LUN, SYSTEM(MAX_SYSTEMS)
	INTEGER*4 NAME(0:MAX_SYSTEMS)/'*   ','A   ','B   ','C   ',
     *                                'D   ','E   '/
	INTEGER*4 IND,DASH
C
	CHARACTER*50 LINE
	CHARACTER*1  CSYS(4)
	CHARACTER*1  UPPER_CASE(0:MAX_SYSTEMS)/'*','A','B','C','D','E'/
	CHARACTER*1  LOWER_CASE(0:MAX_SYSTEMS)  /'*','a','b','c','d','e'/

	DATA LINE/'                                 '/
	DATA DASH/' -- '/
C
	EQUIVALENCE (SYS,CSYS)
	CHARACTER*15 SYSTEM_NAME(MAX_SYSTEMS)
	CHARACTER*15 NODENAME
	INTEGER*4    NODENAME_LEN,I
	CHARACTER*20 PROMPT
C
	LOGICAL*4 ASK_CONN_IN_TRIPLEX /.FALSE./
C
C       INITIALIZE NETWORK DATA
C       INITIALISE DEVICES 
C
C       NOTE: THIS MUST BE CHANGED IF NUMBER OF SYSTEMS CHANGE.
C
        NETSITE(1) = SITEONE
	NETSITE(2) = SITEONE
        NETSITE(3) = SITEONE
	NETSITE(4) = SITEONE
        NETSITE(5) = SITEONE
C-	NETSITE(3) = SITETWO
C-	NETSITE(4) = SITETWO
C
        SYSTEM(1)  = SYSTMA
        SYSTEM(2)  = SYSTMB
        SYSTEM(3)  = SYSTMC
        SYSTEM(4)  = SYSTMD
        SYSTEM(5)  = SYSTME
C
	SECOND=0
	DO 6 OFF=1,NETLUN
	   DO 5 OFF1=1,2
	      NETDEV(OFF1,OFF)=-1
5	   CONTINUE
6       CONTINUE
C
C
C
	WAYINP=1
	WAYLOG=0
	OTHRLOG=0
	CONNECT(0)=0
C
C
C
10	CONTINUE
       	DO 16 OFF2=1,NUMWAY
           DO 15 OFF=1,NETSYS
	      NETSTAT(OFF,OFF2)=0
	      NETROUT(OFF,OFF2)=0
	      CONNECT(OFF)=0
15	   CONTINUE
16	CONTINUE
C
	NETATR(1)=INP
C
	DO 18 OFF1=1,NUMWAY
	   DO 17 OFF=1,NETLUN
	       OPNNET(OFF,OFF1) = 0
17	   CONTINUE
	   NETMASTER(OFF1)  = 0
	   NETBACKUP(OFF1)  = 0
18	CONTINUE
C
	TAB(0)=0
        NODEID=0
	DO 19 OFF=1,NETSYS
	   TAB(OFF)=0
19	CONTINUE
20	CONTINUE
C
C Translate all GTECH_REMSYS% logicals
C
	PROMPT = ' '
	IND = 0
        TYPE*,IAM()
        TYPE*,IAM(),'System configuration'
        TYPE*,IAM(),'==========================='
	DO I=1,MAX_SYSTEMS
	  CALL LNMTRN('GTECH_REMSYS'//CHAR(ICHAR('0')+I),SYSTEM_NAME(I),
     *                'LNM$SYSTEM',ST)
	  IF(ST.NE.0) THEN
	      SYSTEM_NAME(I) = ' '
	  ELSE
	      TYPE*,IAM(),'System ',UPPER_CASE(I),' is ',SYSTEM_NAME(I)
	      IF(IND.GT.0) THEN
                 PROMPT(IND:) = ', '
                 IND = IND + 2
              ENDIF
	      IND = IND + 1
	      PROMPT(IND:IND) = UPPER_CASE(I)
              IND = IND + 1
	  ENDIF
	END DO
        TYPE*,IAM(),'==========================='
C
C Find out the name of the system
C
	CALL GET_NODENAME(NODENAME,NODENAME_LEN)
	SYSNO = 0
	DO I=1,MAX_SYSTEMS
           IF(NODENAME(:NODENAME_LEN).EQ.SYSTEM_NAME(I)) SYSNO = I
        ENDDO
C
	IF(SYSNO.EQ.0) THEN
	   TYPE*,IAM()
	   TYPE*,IAM(),'GTECH_REMSYS% logicals are not set properly'
	   TYPE*,IAM(),'Define these logicals and then CONTinue'
	   CALL GPAUSE
	  GOTO 20
	ENDIF
	TYPE*,IAM()
	TYPE*,IAM()
        TYPE*,IAM(),'This is system ', UPPER_CASE(SYSNO),
     *             ' ( ',NODENAME, ' )'
	TYPE*,IAM()
C
        P(SYSNAM)   = SYSTEM(SYSNO)
	SYSSER      = SYSOFF*(SYSNO-1)
	EURS_XRFOFF = EXRFOFF*(SYSNO-1)                                               !V11
	NODEID      = P(SYSNAM)
	TAB(NODEID) = NODEID
        ACTIVE_SYSTEMS = 1
C
C       A FILE SHOULD EXIST ON EACH SYSTEM FOR VERIFICATION PURPOSES
C       IF NOT PRESENT THEN SOMETHING COULD BE AMISS
C
	NODEID_PORT = 0
	CALL ISBYTE(NODEID,NODEID_PORT,3)
	OPNNAM(1) = TMPNAM + NODEID_PORT
	CALL OPENW(1,OPNNAM(1),4,0,0,ST)
	IF (ST.NE.0) THEN
	  WRITE(6,901) IAM(),OPNNAM(1),OPNNAM(2),ST
	  TYPE *,IAM(),' This is not the right system'
	  CALL INPYESNO(' Do you want to continue [Y/N]?',FLAG)
	  IF (FLAG.NE.1) GOTO 10
	ENDIF
	CALL USRCLOS1(1)
C
C       NOW ASK WHICH SYSTEM IS GOING TO BE THE PRIMARY 
C
	CALL WIMG(6,'Which system is the PRIMARY system ( '//
     +               PROMPT(:IND)//' ?)')
	READ (5,900) SYS
C
	PRIMARY = 0
        DO 60 SYSNO = 1,NETSYS
	   IF(SYSTEM_NAME(SYSNO).EQ.' ') GOTO 60
           IF(CSYS(1).EQ.UPPER_CASE(SYSNO) .OR.
     *        CSYS(1).EQ.LOWER_CASE(SYSNO)) GO TO 65
60      CONTINUE
        GO TO 8000
C
C       SET UP CONNECTION TABLE 
C
65      CONTINUE
	PRIMARY          = SYSTEM(SYSNO)
	CONNECT(NODEID)  = PRIMARY     !ASSUME IT'S CONNECTED TO PRIMARY
	CONNECT(PRIMARY) = 0
        IF(NODEID .NE. PRIMARY) ACTIVE_SYSTEMS = ACTIVE_SYSTEMS + 1
C
C       DO NOT SET BACKUP THIS IS DONE VIA VISION AFTER COMMUNICATIONS
C       HAS BEEN BROUGHT UP
C
C       WHAT OTHER SYSTEMS ARE TO BE BROUGHT ONLINE
C
	TAB(PRIMARY) = PRIMARY
	DO 70 SYSNO = 1,NETSYS
	   IF(SYSTEM_NAME(SYSNO).EQ.' ') GOTO 70
	   IF(ACTIVE_SYSTEMS.EQ.3) GOTO 70
	   IF (TAB(SYSNO).EQ.0) THEN
              LINE='Is system  '//UPPER_CASE(SYSNO)//
     *             ' being brought up?'
	      CALL INPYESNO(LINE,FLAG)
	      IF(FLAG.EQ.1) THEN
                 TAB(SYSNO)       = SYSNO
	         CONNECT(SYSNO)   = PRIMARY 
	         ACTIVE_SYSTEMS = ACTIVE_SYSTEMS + 1
              ENDIF  
           ENDIF
70      CONTINUE
C
C       SYSTEM IS IN TRIPLEX 
C
	IF (ACTIVE_SYSTEMS.EQ.3.AND.ASK_CONN_IN_TRIPLEX) THEN
	   DO 80 NODE = 1,NETSYS
	      IF(NODE.NE.PRIMARY .AND. TAB(NODE).NE.0) THEN
                 LINE = 'Which system is talking to system '
     *                  //UPPER_CASE(NODE)//'? '
	         CALL WIMG(6,LINE)
	         ACCEPT 900,SYS
	         TEMP = 0
C
                 DO 85 SYSNO = 1,NETSYS
                    IF(CSYS(1).EQ.UPPER_CASE(SYSNO) .OR.
     *                 CSYS(1).EQ.LOWER_CASE(SYSNO))TEMP = SYSTEM(SYSNO)
85               CONTINUE
C
	         IF(TEMP.LE.0 .OR. TEMP.EQ.NODE) GOTO 8000
                 CONNECT(NODE) = TEMP
	      ENDIF
80	   CONTINUE
C
C          CHECK FOR CROSSED CONNECTIONS AND REJECT THEM
C
           DO 90 NODE = 1,NETSYS
              IF(CONNECT(CONNECT(NODE)).EQ.NODE) GOTO 8000
90         CONTINUE
C
	ENDIF    !/* END OF IF(ACTIVE_SYSTEMS.EQ.3) */
C
C
C       NOW SETUP ALL NETWORKING TABLES
C       INITIALISE NOW FOR NETWORK MANAGER
C
	NETMASTER(WAYINP) = PRIMARY
	NETBACKUP(WAYINP) = 0
C
C       SETUP UNITS FOR SYSIO
C
        DO 100 LUN=1,NETLUN
	   OPNNET(LUN,WAYINP)=LUN
100     CONTINUE
C
C       SETUP CONNECTIONS TO SECONDARY SYSTEM
C
	IF (NODEID.EQ.PRIMARY) THEN
            DO 200 SYSNO = 1,NETSYS
               IF(SYSNO.NE.NODEID.AND.SYSNO.NE.PRIMARY) THEN
                 IF(TAB(SYSNO) .NE. 0) THEN
                    NETSTAT(SYSNO,WAYINP) = NSTAPRIM
C
C                   IF THE SYSTEM IS CONNECTED TO US MAKE HIM ACTIVE
C
                    IF(CONNECT(SYSNO).EQ.NODEID)THEN
	               NETROUT(SYSNO,WAYINP) = ROUACT
	            ELSE
		       NETROUT(SYSNO,WAYINP) = ROUIDLE
	            ENDIF
                 ENDIF
               ENDIF
200         CONTINUE
	ELSE
C
C          ALL OTHER SYSTEMS ARE CONSIDERED SPARES UNTIL 
C          BACKUP ID IS SET
C
            DO 250 SYSNO = 1,NETSYS
               IF(TAB(SYSNO).NE.0 .AND. SYSNO.NE.NODEID) THEN
	         NETSTAT(SYSNO,WAYINP) = NSTASEC
	         NETROUT(SYSNO,WAYINP) = ROUIDLE
C
C                SYSNO IS PRIMARY SYSTEM ??
C
                 IF(CONNECT(SYSNO).EQ.NODEID) THEN   
                   NETSTAT(SYSNO,WAYINP) = NSTAPRIM
                   NETROUT(SYSNO,WAYINP) = ROUACT
                 ENDIF
               ENDIF
250         CONTINUE 
	ENDIF
C
C       SET THE SYSTEM TO EITHER SPARE OR LIVE (PRIMARY)
C
	P(SYSTYP) = SPRSYS
	IF (NODEID.EQ.PRIMARY) P(SYSTYP) = LIVSYS
C
C       NOW SUMMARIZE THE SITUATION FOR THE OPERATOR
C
	IF (ACTIVE_SYSTEMS.EQ.1) THEN
           WRITE (6,9014)IAM(),IAM(),IAM(),
     *	   NAME(P(SYSNAM)),NAME(P(SYSNAM)),SYSTXT(P(SYSTYP))
	   TYPE *,IAM(),' Game brought up in SIMPLEX - ',
     *                  ' only 1 system configured'
	ENDIF
C
C       PREPARE TO DRAW SYSTEM CONFIGURATION
C
	IF (ACTIVE_SYSTEMS.NE.1) THEN
	   WRITE (6,9011) IAM(),IAM(),IAM(),
     *     NAME(P(SYSNAM)),SYSTXT(P(SYSTYP)),
     *     IAM(),NAME(PRIMARY)
C
           WRITE(5,9015) IAM(),ACTIVE_SYSTEMS 
	   DO 500 OFF = 1,NETSYS
	      IF (TAB(OFF).EQ.0) WRITE(5,9013) IAM(),NAME(OFF)
500	   CONTINUE
C
	   SECOND = 0
	   SYS    = 0
	   DO 600 OFF=1,NETSYS
              IF(CONNECT(OFF).NE.0) THEN
	        IF(SECOND.EQ.0) THEN
	           SECOND=OFF
		ELSE
		   SYS=OFF
		ENDIF
	      ENDIF
600	   CONTINUE
C
	   TYPE *,IAM(),' '
C
C          DISPLAY DUPLEX CONFIGURATION
C
	   IF (ACTIVE_SYSTEMS.EQ.2) THEN
	      TYPE 950,IAM(),NAME(PRIMARY),DASH,NAME(SECOND)
              TYPE*,IAM()
	   ELSE
C
C             TRIPLEX CONFIGURATION
C
	      IF (CONNECT(SYS)   .EQ. PRIMARY.AND.
     *            CONNECT(SECOND).EQ. PRIMARY) THEN
		  TYPE 970,IAM(),NAME(PRIMARY),DASH,NAME(SECOND),
     *		           IAM(),IAM(),NAME(SYS)
	      ELSE
	         TYPE 980,IAM(),NAME(PRIMARY),DASH,NAME(SECOND),NAME(SYS)
	      ENDIF
	   ENDIF
	ENDIF
	TYPE *,IAM(),' '
C
C
	CALL INPYESNO('Is this correct  [Y/N] ?',FLAG)
	IF (FLAG.NE.1) GOTO 10
C
C
C
	DO 700 OFF = 1,NETSYS       !ENABLE RESYNCHRONIZATION
           NETENA(OFF,WAYINP)=0
700	CONTINUE
C
C   	SET         NETRDY to  NETRDY_RESET
C	NETLOG	    waits for NETRDY_RESET  and sets to NETRDY_NETLOG
C	NETMGR	    waits for NETRDY_NETLOG and sets to NETRDY_NETMGR
C	DISPAT	    waits for NETRDY_NETMGR and sets to NETRDY_DISPAT
C
	NETRDY=NETRDY_RESET                  !ALLOW NETMGR TO START
C
D	TYPE *,'WAYINP, WAYLOG ',WAYINP,WAYLOG
D	TYPE *,'NETATR    ',NETATR
D	TYPE *,'NETMASTER ',NETMASTER
D	TYPE *,'NETBACKUP ',NETBACKUP
D	TYPE *,'NETSTAT   ',NETSTAT
D	TYPE *,'NETROUT   ',NETROUT
D	TYPE *,'OPNNET    ',OPNNET
D	TYPE *,'NETOPN    ',NETOPN
D	TYPE *,'NETASSN   ',NETASSN
D	TYPE *,'TAB       ',TAB
D	TYPE *,'CONNECT   ',CONNECT
C
        GOTO 8999
C
C
8000	CONTINUE
	WRITE (6,9012)IAM(),IAM(),IAM()
	TYPE *,IAM(),' Invalid entry, please re-enter'
	GOTO 10
C
8100    CONTINUE
	WRITE (6,9012)IAM(),IAM(),IAM()
	TYPE *,IAM(),' Too many entries, please re-enter'
	GOTO 10
C
8999    CONTINUE
        RETURN
C
C       FORMAT STATEMENTS
C
900	FORMAT(A4)
901	FORMAT(1X,A,1X,2(A4),' file open error',I)
950	FORMAT(1X,A,5X,' (',A1,')',A4,A1,10X,A24)
970	FORMAT(1X,A,5X,' (',A1,')',A4,A1,
     *	        /,1X,A,7X,'|',/,1X,A,7X,A)
980	FORMAT(1X,A,5X,' (',A1,')',A4,A1,' -- ',A1,5X,A24)
9011	FORMAT(1X,A,/,1X,A,' Systems assignments: ',/,1X,A,10X,
     *          ' This is system ',A1,' ( ',A8,' system )',/,1X,A,10X,
     *          ' System ',A1,' is primary system ')
9012	FORMAT(1X,A,1X,/,1X,A,/,1X,A,
     *         ' *** Specification error! re-enter please    ***')
9013	FORMAT(1X,A,' System ',A1,' is not configured')
9014	FORMAT(1X,A,/,1X,A,' Systems assignments: ',/,1X,A,10X,
     *	   ' This is system ',A1,'. System ',A1,' is ',A8,' system')
9015    FORMAT(1X,A,10X,' There are ',I3,' systems configured')
C
	END
