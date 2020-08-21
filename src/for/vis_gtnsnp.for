C
C *** SUBROUTINE GTNSNP ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]GTNSNP.FOV                                   $
C  $Date::   17 Apr 1996 13:28:16                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - vis_gtnsnp.for ***
C  
C
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF, AGTINF.DEF for Finland.
C
C V04 16-FEB-11 RXK FIX FOR DIMENSION OF IDSYS
C V03 01-AUG-90 XXX RELEASED FOR VAX
C V02 05-APR-89 LOU MODIFIED FOR 3 SYSTEM CONFIG.
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE GTNSNP(CLINE)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:CTLCOM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DN_LINK.DEF'
	INCLUDE 'INCLIB:DN_BLOCK.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
C LOCAL DECLARATIONS
C
	REAL*8		K(40)		/'BACKup  ', 'CHKPnt  ',
     *                                   'ENAAsync', 'ENABsysn',
     *                                   'ENACsync', 'ENADsync',
     *                                   'ENAEsync', 'UNUSED  ',
     *                                          32 * 'UNUSED  '/
C
	INTEGER*4	BEEPFLG		/0/,
     *			BUF(CDLEN),
     *			CLINE(20),
     *			ENA,
     *			I4_SPACES	/'    '/,
     *			I,
     *			IND,
     *			J,
     *			KEYNUM,
     *			L,
     *			PNT,
     *			POS,
     *			SAP,
     *			SERDISP(NETSYS),
     *			ST,
     *			ST1,
     *			STASYS(NETSYS),
     *			STATE,
     *			SYS,
     *			SYS1,
     *			SYST(MAX_SYSTEMS),
     *			VALUE
C
	LOGICAL*4	TAKEOVER
C
	CHARACTER*80	CLINXX(1)
C
	CHARACTER*11	CONSTA(0:5)	/'           ',
     *                                   '   idle    ',
     *                                   '   dead    ',
     *                                   ' not init  ',
     *                                   ' recovery  ',
     *                                   '  normal   '/,
     *			IDSYMB(0:MAX_SYSTEMS)
        CHARACTER*1     IDSYS(0:MAX_SYSTEMS)
C
	CHARACTER*9	ENALIN(MAX_SYSTEMS)
C
	CHARACTER*8	AENA(0:1)	/'Disabled',
     *                                   ' Enabled'/,
     *			ASTATE(0:2)	/'  Closed',
     *                                   ' Pending',
     *                                   '    Open'/
C
	CHARACTER*3	ENASTA(0:2)	/'on ', 'spe', 'off'/
C
	CHARACTER*1	BEEP		/Z07/,
     *			BLANK		/' '/
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C INITIALIZE 'IDSYMB()', 'IDSYS()', & 'ENALIN()' VARIABLES.
C THESE USED TO BE INITIALIZED WITH DATA STATEMENTS,
C BUT THAT APPROACH REQUIRED CODE MODIFICATION
C IF THE PARAMETER 'MAX_SYSTEMS' CHANGED.
C
	IDSYMB(0) = ' ********  '
	IDSYS(0)  = '*'
	DO 100 I = 1, MAX_SYSTEMS
	  IDSYS(I)  = CHAR(I + 64)
	  IDSYMB(I) = ' System ' // IDSYS(I) // '  '
	  ENALIN(I) = 'ENA' // IDSYS(I) // '(   )'
100	CONTINUE
C
C INITIALIZE THE SCREEN
C
	CALL FASTSET(I4_SPACES, NEW(1, 1), 22 * 20)
C
	SYST(1) = SYSTMA
	SYST(2) = SYSTMB
	SYST(3) = SYSTMC
	SYST(4) = SYSTMD
	SYST(5) = SYSTME
C
	CALL SPACES
C
C GTNSNP INPUT
C
	BUF(6) = IDNUM
	KEYNUM = 0
	POS    = 1
	VALUE  = 0
C
	CALL KEY(CLINE, K, 14, POS, KEYNUM)
C
	IF (POS .GT. 40) THEN
	  IF (KEYNUM .EQ. 2) THEN
            GOTO 200
	  ELSE
	    GOTO 500
	  ENDIF
	ENDIF
C
	IF (KEYNUM .EQ. 0) GOTO 300
C
	CALL NUMB(CLINE, POS, VALUE)
C
	IF (VALUE .LT. 0) GOTO 340
C
	CALL FASTSET(0, BUF, CDLEN)
C
200	CONTINUE
	GOTO (210, 220, 230, 240, 250, 260, 270) KEYNUM
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C INVALID KEYNUM
C
	GOTO 300
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C BACKUP ID CHANGE
C
210	CONTINUE
	IF (VALUE .LT. 0 .OR.
     *      VALUE .GT. NETSYS .OR.
     *      VALUE .EQ. NETMASTER(WAYINP)) GOTO 340
C
	IF (X2X_GAME_STATE .NE. X2X_GAMES_UP) GOTO 330
C
	IF (VALUE .NE. 0) THEN
	  IF (NETSITE(VALUE) .NE. NETSITE(NODEID)) GOTO 340
C
          IF (NETROUT(VALUE, WAYINP) .NE. ROUACT .AND. 
     *        DN_LINK(VALUE).STATE .NE. STATE_RUNNING) GOTO 320
C
	  IF (NETMODE(VALUE, WAYINP) .EQ. FILMD) GOTO 310
	ENDIF
C
	BUF(1) = 3
	BUF(2) = VALUE
	BUF(3) = TCNET
	BUF(6) = IDNUM
	BUF(9) = WAYINP
	GOTO 400
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FORCED CHECKPOINT
C
220	CONTINUE
	BUF(1) = 1
	BUF(3) = TCGEN
	BUF(6) = IDNUM
	GOTO 400
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ENABLE RESYNCH CHANGE - SYS A LOCAL
C
230	CONTINUE
	IF (NETMASTER(WAYINP) .NE. NODEID) GOTO 350
	IF (VALUE .LT. 0 .OR. VALUE .GT. 2) GOTO 340
	BUF(1) = 2
	BUF(2) = VALUE + 100				! 100 / 100 = SYS A ID
	BUF(3) = TCNET
	BUF(6) = IDNUM
	BUF(9) = WAYINP
	GOTO 400
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ENABLE RESYNCH CHANGE - SYS B LOCAL
C
240	CONTINUE
	IF (NETMASTER(WAYINP) .NE. NODEID) GOTO 350
	IF (VALUE .LT. 0 .OR. VALUE .GT. 2) GOTO 340
	BUF(1) = 2
	BUF(2) = VALUE + 200				! 200 / 100 = SYS B ID
	BUF(3) = TCSPE
	BUF(6) = IDNUM
	BUF(9) = WAYINP
	GOTO 400
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ENABLE RESYNCH CHANGE - SYS C LOCAL
C
250	CONTINUE
	IF (NETMASTER(WAYINP) .NE. NODEID) GOTO 350
	IF (VALUE .LT. 0 .OR. VALUE .GT. 2) GOTO 340
	BUF(1) = 2
	BUF(2) = VALUE + 300				! 300 / 100 = SYS C ID
	BUF(3) = TCSPE
	BUF(6) = IDNUM
	BUF(9) = WAYINP
	GOTO 400
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ENABLE RESYNCH CHANGE - SYS D LOCAL
C
260	CONTINUE
	IF (NETMASTER(WAYINP) .NE. NODEID) GOTO 350
	IF (VALUE .LT. 0 .OR. VALUE .GT. 2) GOTO 340
	BUF(1) = 2
	BUF(2) = VALUE + 400				! 400 / 100 = SYS D ID
	BUF(3) = TCSPE
	BUF(6) = IDNUM
	BUF(9) = WAYINP
	GOTO 400
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ENABLE RESYNCH CHANGE - SYS E LOCAL
C
270	CONTINUE
	IF (NETMASTER(WAYINP) .NE. NODEID) GOTO 350
	IF (VALUE .LT. 0 .OR. VALUE .GT. 2) GOTO 340
	BUF(1) = 2
	BUF(2) = VALUE + 500				! 500 / 100 = SYS E ID
	BUF(3) = TCSPE
	BUF(6) = IDNUM
	BUF(9) = WAYINP
	GOTO 400
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C INPUT ERROR
C
300	CONTINUE
	WRITE(CLIN23, 9000)
	GOTO 9999
C
310	CONTINUE
	WRITE(CLIN23, 9010)
	GOTO 9999
C
320	CONTINUE
	WRITE(CLIN23, 9020)
	GOTO 9999
C
330	CONTINUE
	WRITE(CLIN23, 9030)
	GOTO 9999 
C
C VALUE ERROR
C
340	CONTINUE
	WRITE(CLIN23, 9040)
	GOTO 9999
C
C PATH ERROR
C
350	CONTINUE
	WRITE(CLIN23, 9050)
	GOTO 9999
C
C QUEUE COMMAND BUFFER TO SYSYTEM INPUT QUEUE
C
400	CONTINUE
	CALL VISCMD(BUF, ST)
	CALL XWAIT(2, 1, ST1)
	IF (ST .NE. 0) WRITE(CLIN23, 9060)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C ENCODE SCREEN IMAGE
C
500	CONTINUE
	IF (WAYINP .LE. 0) THEN
	  WRITE(CLIN23, 9070)
	  GOTO 9999
	ENDIF
C
	WRITE(CLIN1, 9080)
	WRITE (XNEW(1)(69:80), 9090) IDSYS(NODEID)
	WRITE(CLIN2, 9100)
C
	DO 600 J  =  1, NETSYS
	  ENALIN(J)(6:8) = ENASTA(NETENA(SYST(J), WAYINP))
600	CONTINUE        
C
	WRITE(CLIN3, 9110) IDSYS(NETMASTER(WAYINP)), NETMASTER(WAYINP), 
     *                     IDSYS(NETBACKUP(WAYINP)), NETBACKUP(WAYINP), 
     *                     NXTSER
C
	IF (NETBACKUP(WAYINP) .LE. 0) THEN
	  WRITE (XNEW(3)(39:50), 9120)
	ELSE
	  WRITE (XNEW(3)(39:50), 9130)
	ENDIF
C
	TAKEOVER = LANGO .NE. LANPRODIS .AND. 
     *             THISSTA .NE. STADOWN .AND. 
     *             CTLX2XLOC .EQ. CTLX2XOK .AND. 
     *             CTLSAPSTA(X2X_GAME_SAP) .NE. CTLSTAUP .AND. 
     *             NETBACKUP(WAYINP) .EQ. NODEID
C
	WRITE (XNEW(4)(32:56), 9140)
	IF (TAKEOVER) THEN
	  WRITE (XNEW(4)(32:56), 9150)
	  IF (BEEPFLG .EQ. 80) THEN
	    WRITE (XNEW(4)(80:80), 9160) BEEP
	    WRITE (XNEW(4)(79:79), 9160) BLANK
	    BEEPFLG = 79
	  ELSE
	    WRITE (XNEW(4)(79:79), 9160) BEEP
	    WRITE (XNEW(4)(80:80), 9160) BLANK
	    BEEPFLG = 80
	  ENDIF
	ELSE
	  WRITE (XNEW(4)(32:56), 9140)
	ENDIF
C
	WRITE(CLIN5, 9100)
C
	WRITE(CLIN6, 9170) (IDSYMB(IND), IND = 1, NETSYS)
C
	WRITE(CLIN7, 9100)
C
C LOCAL PATH
C
	DO 700 SYS = 1, NETSYS
	  STASYS(SYS) = 3				! NOT INIT
	  IF (NETROUT(SYS, WAYINP) .EQ. ROUNO) THEN
	    STASYS(SYS) = 0				! NO CONNECTION
	  ELSEIF (NETSTAT(SYS, WAYINP) .EQ. NSTADEADP .OR.
     *            NETSTAT(SYS, WAYINP) .EQ. NSTADEADS) THEN
	    STASYS(SYS) = 2				! DEAD
	  ELSEIF (NETROUT(SYS, WAYINP) .EQ. ROUIDLE) THEN
	    STASYS(SYS) = 1				! IDLE
	  ELSEIF (NETROUT(SYS, WAYINP) .EQ. ROUACT) THEN
	    SYS1 = SYS
	    IF (NETSTAT(SYS, WAYINP) .EQ. NSTASEC) SYS1 = NODEID
	    IF (NETMODE(SYS1, WAYINP) .EQ. FILMD) THEN
	      STASYS(SYS) = 4				! RECOVERY
	    ELSE
	      STASYS(SYS) = 5				! NORMAL
	    ENDIF
	  ENDIF
700	CONTINUE
C
	DO 800 J = 1, NETSYS
	  SERDISP(J) = 0
	  IF (NETROUT(J, WAYINP) .EQ. ROUACT) THEN
	    IF (NETSTAT(J, WAYINP) .EQ. NSTASEC) THEN
	      SERDISP(J) = NXTSER
	    ELSEIF (NETSTAT(J, WAYINP) .EQ. NSTAPRIM) THEN
	      IF (NETMODE(J, WAYINP) .EQ. FILMD) THEN
		SERDISP(J) = NETSER(J, WAYINP)
	      ELSE
		SERDISP(J) = NETHSER(J, WAYINP)
	      ENDIF
	    ENDIF
	  ENDIF
	  SERDISP(J) = MOD(SERDISP(J), SYSOFF)
800	CONTINUE
C
	WRITE(CLIN8, 9180) (CONSTA(STASYS(I)), I = 1, NETSYS)
	WRITE(CLIN9, 9190) (SERDISP(I), I = 1, NETSYS)
C
	IF (NETBACKUP(WAYINP) .GT. 0)
     *    WRITE (XNEW(12)(61:80), 9200) P(PPISR3)
C
	WRITE(CLIN10, 9210) (NETRETRY(I, WAYINP), NETERR(I, WAYINP),
     *                      I = 1, NETSYS)
C
	WRITE(CLIN11, 9220) (NETTIMER-NETTIM(I, WAYINP), I = 1, NETSYS)
C
	DO 900 SYS = 1, NETSYS
	  IF (STASYS(SYS) .EQ. 0)
     *      CALL CLR_GTNSCR(8, 14 + (SYS - 1) * 12)
900	CONTINUE
C
	WRITE(CLIN13, 9230) CTLMAXTOUT
C
	WRITE(CLIN21, 9240) IDSYS(NETMASTER(WAYINP)), X2X_GAME_SAP
C
	WRITE(CLIN22, 9250) LANGO, THISSTA, CTLX2XLOC,
     *                     CTLSAPSTA(X2X_GAME_SAP)
C
	WRITE(CLIN14, 9260)
C
	DO 1000 SYS = 1, CTLMAXSYS
	  SAP   = CTLSAPSYS(SYS)
	  STATE = CTLSAPSTA(SAP)
	  ENA   = CTLSAPENA(SAP)
	  WRITE(CLINXX(1), 9270) IDSYS(SYS), SAP, ASTATE(STATE),
     *                           CTLSAPTOUT(SAP), AENA(ENA),
     *                           CTLSSEQ(SAP), CTLRSEQ(SAP)
	  IF (SYS .EQ. 1) THEN
	    CLIN15(1) = CLINXX(1)
	  ELSEIF (SYS .EQ. 2) THEN
	    CLIN16(1) = CLINXX(1)
	  ELSEIF (SYS .EQ. 3) THEN
	    CLIN17(1) = CLINXX(1)
	  ELSEIF (SYS .EQ. 4) THEN
	    CLIN18(1) = CLINXX(1)
	  ELSEIF (SYS .EQ. 5) THEN
	    CLIN19(1) = CLINXX(1)
	  ENDIF
1000	CONTINUE
C
	SYS = NETMASTER(WAYINP)
	SAP = X2X_GAME_SAP
	IF (SAP .GT. 0) THEN
	  STATE = CTLSAPSTA(SAP)
	  ENA   = CTLSAPENA(SAP)
	  WRITE(CLIN20, 9270) IDSYS(SYS), SAP, ASTATE(STATE),
     *                        CTLSAPTOUT(SAP), AENA(ENA),
     *                        CTLSSEQ(SAP), CTLRSEQ(SAP)
	  GOTO 9999
	ENDIF
C
	PNT = 15
	DO 1100 L = PNT, PNT + 5
	  WRITE(XNEW(L), 9100)
1100	CONTINUE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FORMAT STATEMENTS.
C
9000	FORMAT('Input error')
9010	FORMAT('System in recovery mode, please wait and try again')
9020	FORMAT('Link not up yet, please wait and try again')
9030	FORMAT('Gamestate not set, please wait and try again')
9040	FORMAT('Value error')
9050	FORMAT('Unauthorized change on that path !!')
9060	FORMAT('System error! Check input queue')
9070	FORMAT('Network not defined yet, please wait and try again')
9080	FORMAT(' << > Network Summary < >>')
9090	FORMAT(' System -> ', A1)
9100	FORMAT(80(' '))
9110	FORMAT('Site primary', A1, '(', I1, ')', 5X,
     *         '*BACKup ',     A1, '(', I1, ')', 20X,
     *         'Nxtser: ', I9, ' *CHKPnt')
9120	FORMAT('No Backup !!')
9130	FORMAT('            ')
9140	FORMAT('                         ')
9150	FORMAT('SYSTEM READY TO TAKE OVER')
9160	FORMAT(A1)
9170	FORMAT(13X, <NETSYS>(A11, 1X))
9180	FORMAT('Status      +', <NETSYS>(A11, '*'), A11, '+')
9190	FORMAT('Last ser #  +', <NETSYS>(I11, '*'), I11, '+')
9200	FORMAT('Backup  # ', I10)
9210	FORMAT('Retries/err +',
     *         <NETSYS>(I5, '/', I5, '*'), I5, '/', I5, '+', 4X)
9220	FORMAT('Last I/O    +', <NETSYS>(I11, '*'), I11, '+')
9230	FORMAT(' << > LAN -- Nplex Control Summary < >>', ' TOUT: ', I8)
9240	FORMAT('Primary system - ', A1, ' Primary comm SAP - ', I3)
9250	FORMAT('LANPRO state   - ', I1, ' LANSTATION state - ', I3,
     *        ' X2XMGR state   - ', I1, ' CTLSAP state - ', I3)
9260	FORMAT('  SYSTEM     SAP   STATE TIMEOUT  ENABLE SENDSEQ  RECSEQ')
9270	FORMAT(7X, A1, I8, A8, I8, A8, 2I8)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
