C
C PROGRAM DISPAT
C
C DISPAT.FOR
C
C V11 13-MAY-2015 SCML Bugfix with transaction recording while in Duplex Mode
C V10 06-MAR-2014 SCML Added support to PLACARD Project - IGS
C V09 09-NOV-2000 UXN  TYPGUI added.
C V08 29-JAN-1999 UXN  Second phase unfraction added (TYPUNF)
C                      Also FRAC.DEF added.
C V07 14-NOV-1993 JWE  Save command size before flushing to use after flush
C                      Kick SPESRV if there is something for it to do during 
C                      a flush (Fractions)
C V06 10-SEP-1993 GXA  Removed hardcoded CSIZE for commands with Freeze, use 
C                      SIZE from GETQUE since commands now can be longer then 
C                      one logrecord.
C V05 27-AUG-1993 JWE  BASELINE update: Event flag name should have been 
C                      prepended with the project name.  There was also an 
C                      incorrect event flag being set.
C V04 03-AUG-1993 GXA  Released for Finland Dec Conversion / Oddset.
C                      Added Check for Second Phase transactions (Spede, Ravi, 
C                      Fractions) and Generate multi tickets (serial#) for 
C                      Fractions.
C V03 21-JAN-1993 DAB  Initial Release
C                      Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                      DEC Baseline
C V02 07-OCT-1991 MTK  INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM DISPAT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:QUECOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:DCNEVN.DEF'
	INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
	INCLUDE 'INCLIB:FRAC.DEF'
C----+------------------------------------------------------------------
C V10| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:IGSDEBUG.DEF'
C----+------------------------------------------------------------------
C V10| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
	INTEGER*4  CHECKS
	PARAMETER (CHECKS=2*MAXGAM)
C
	INTEGER*4 TAB(0:MAXGAM*NUMTOT),DIF(0:CHECKS)
	INTEGER*4 ENASYNC, TRCADR, ERRTYP, GAME, OFF, ACTIVE
	INTEGER*4 I, STATUS, SIZE, QUE, TYPE, J, MBATCH
	INTEGER*4 ST, BUFNET, BUF
	INTEGER*4 DELAY
	PARAMETER(DELAY=10) ! wait 10 ms
	INTEGER*4 COMMAND_BUFFER
	INTEGER*4 COMMAND_SIZE  
C
	INTEGER*4 NUMTKT		!# Tickets to Fraction.
C
	CHARACTER*4 GXEVNNAM		!Name Function
C
	LOGICAL FLUSH,BYPASS,TOSEND,DMPDBG
C
	INTEGER*2 ERROR_LENGTH
	CHARACTER ERROR_TEXT*256    !Text from $GETMSG system call
C
	DATA DIF/CHECKS*0,0/
C
	CALL COPYRITE
	CALL SNIF_AND_WRKSET
C
C CREATE THE COMMON EVENT FLAG CLUSTER.
C
        !A permanent event flag cluster continues to exist until it is marked explicitly for deletion with the Delete Common Event Flag Cluster (SYS$DLCEFC) 
        STATUS=SYS$ASCEFC(%VAL(DN_EVNTIMER), !(DN_EVNTIMER=64) cluster
	1 GXEVNNAM()//DN_EVNNAME,0,0) !-> XXXXXDNFLAGS
		IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS)) !Signal Exception Condition
		!A routine calls LIB$SIGNAL to indicate an exception condition or output a
		!message rather than return a status code to its caller
		!LIB$SIGNAL also creates a mechanism argument vector that contains the state
		!of the process at the time of the exception. LIB$SIGNAL then searches for a
		!condition handler to process the exception condition.
C
C  WAIT FOR SOMETHING TO DO
C
	FLUSH=.FALSE.
	COMMAND_BUFFER=0         !COMMAND BUFFER
	COMMAND_SIZE = 0	 	 !Command size
	BUF=0                    !GAME BUFFER NUMBER
	BUFNET=0                 !NETWORK BUFFER NUMBER NOT INIT
	TOSEND=.TRUE.            !NETLOG NEEDS SOME AEROBIC EXERCISE
C
C				NETRDY = ??? 0
C	RESET			NETRDY = NETRDY_RESET
C	NETLOG			wait for NETRDY_RESET -> NETRDY_NETLOG
C	NETMGR			wait for NETRDY_NETLOG -> NETRDY_NETMGR
C	DISPAT			wait for NETRDY_NETMGR -> NETRDY_DISPAT
C
5	CONTINUE
        IF(IGSDEBUG(IA_DISPAT)) THEN
            CALL OPS('5:DISPAT', 0,0)
        ENDIF
	IF(NETRDY.LT.NETRDY_NETMGR.AND.NETRDY.LT.NETRDY_DISPAT)THEN
	    CALL XWAIT(1,2,ST)
	    GOTO 5
	ENDIF	    	   
	NETRDY=NETRDY_DISPAT    !Let NETMGR go...
C
10	CONTINUE
	CALL XWAIT(DELAY,1,ST)
C
C KICK NETLOG
C
	IF(TOSEND)THEN
		IF(IGSDEBUG(IA_DISPAT)) THEN
        	CALL OPS('124:DISPAT: TOSEND', TOSEND,TOSEND)
		ENDIF
	    STATUS=SYS$SETEF(%VAL(NET_EVENT)) !(NET_EVENT = 67) !BIT 3 OF DN_EVNMASK, The Set Event Flag (SYS$SETEF) and Clear Event Flag (SYS$CLREF) 
		!The codes returned are SS$_WASSET and SS$_WASCLR.
		IF(IGSDEBUG(IA_DISPAT)) THEN
			CALL OPS('126:DISPAT: STATUS', STATUS,STATUS)
		ENDIF
	    IF(.NOT.STATUS)THEN	!Coundn't wake NETLOG
		CALL SYS$GETMSG(%VAL(STATUS),ERROR_LENGTH,
	1	    ERROR_TEXT,,)
		CALL OPS(ERROR_TEXT,STATUS,0)
	    ENDIF
	ENDIF
	TOSEND=.FALSE.
C
C  CHECK IF A COMMAND WITH SYSTEM FREEZE BEING PROCESSED
C
	IF(P(CMDFRZ).NE.0) THEN !não processa nenhum pedido/mensagem enquanto o systema estiver em modo freeze
	  IF(IGSDEBUG(IA_DISPAT)) THEN
		  CALL OPS('138:DISPAT: CMD FRZ', 0,0)
	  ENDIF !NAMES FOR ALL SYSTEM TASKS ---- (CMD=9) !COMMAND TASK #
	  CALL RELSE(TSKNAM(CMD),ST) !TSKNAM(NUMTSK) TASK NAMES) -> taskid.def
	  GOTO 10
	ENDIF
C
C     FLUSH IMPLEMENTATION
C
!NETWORK MANAGER TASK:
!	THIS TASK WILL SERVICE ALL I/O DONE REQUESTS AND COMMANDS
	IF(P(NETFLU).EQ.FLUSHED) GOTO 10 !WAIT UNTIL NETMGR LET YOU GO
	IF(P(NETFLU).EQ.FLUREQ.OR.P(NETFLU).EQ.FLURQ1)GOTO 300!REQ FLUS
C
C START DEQUEING TRANSACTIONS
C
	MBATCH=P(MAXTRA)
	CALL GETTIM(P(ACTTIM))
20	CONTINUE
	DO 100 J=1,MBATCH
C
C STATUS = GLIST_STAT_EMPTY <-> PARAMETER	(GLIST_STAT_EMPTY = 2)	!List empty (RTL,RBL)
C STATUS = GLIST_STAT_LASTONE <-> PARAMETER	(GLIST_STAT_FULL = 1)	!List is full (ABL,ATL)
C STATUS = GLIST_STAT_GOOD <-> PARAMETER	(GLIST_STAT_GOOD = 0)	!Success (all routines)
C		
	CALL RTL(BUF,QUETAB(1,DIS),ST)    !DEQUE NEXT TRANSACTION Remove top of the list
	IF(ST.EQ.2) BUF=0 !List empty (RTL,RBL)
C
C IF FLUSH FLAG IS SET THEN DEQUEUE AND DISPATCH SECOND
C PHASE TRANSACTIONS ONLY.
C
	IF(FLUSH) THEN
      IF(IGSDEBUG(IA_DISPAT)) THEN
          CALL OPS('162:DISPAT:FLUSH BUF', BUF,BUF)
	  ENDIF
	  IF(BUF.EQ.0) GOTO 300 !List empty (RTL,RBL) -> FREEZE INPUT AND FLUSH ALL TRANSACTIONS IN PROCESS ?? acabar o processamento das transactions que já estão a meio do processamento???
      IF(IGSDEBUG(IA_DISPAT)) THEN
          CALL OPS('164:DISPAT:FLUSH BUF', BUF,BUF)
	  ENDIF
	  TYPE=HPRO(TRCODE,BUF) ! lê do buffer o campo TRCODE (TRANSACTION CODE)
	  IF(TYPE.NE.TYPWCN.AND.TYPE.NE.TYPWDL.AND. ! (TYPWCN=9) !LOTTO/SPORTS CANCELLATION && (TYPWDL=10) !LOTTO/SPORTS DELETION 
     *       TYPE.NE.TYPNCN.AND.TYPE.NE.TYPNDL.AND. !(TYPNCN=11) !NUMBERS CANCELLATION && (TYPNDL=12) !NUMBERS DELETION
     *       TYPE.NE.TYPOCN.AND.TYPE.NE.TYPODL.AND. !(TYPOCN=23) !ODDS GAME CANCELLATION && (TYPODL=24) !ODDS GAME DELETION
     *	     TYPE.NE.TYPPCN.AND.TYPE.NE.TYPPDL.AND. !(TYPPCN=25) !BINGO GAME CANCELLATION && (TYPPDL=26) !BINGO GAME DELETION
     *	     TYPE.NE.TYPGUI.AND.TYPE.NE.TYPECH.AND. !PARAMETER (TYPGUI=34) !GUIMGR && (TYPECH=8)  !EXCHANGE TRANSACTION
     *       TYPE.NE.TYPFRA.AND.TYPE.NE.TYPUNF) THEN !(TYPFRA=16) !FRACTION WAGER 2ND PHASE &&  PARAMETER (TYPUNF=35) !UNFRACTION WAGER 2ND PHASE
	    CALL ATL(BUF,QUETAB(1,DIS),ST) !obter o BUF que é PROCOM buffer number -> Probuf
		IF(IGSDEBUG(IA_DISPAT)) THEN
             CALL OPS('173:DISPAT:TYPE', TYPE,TYPE)
		ENDIF
	    GOTO 300 !P(CMDFLU)=1 -> FREEZE INPUT AND FLUSH ALL TRANSACTIONS IN PROCESS
	  ENDIF
	ENDIF
	IF(BUF.EQ.0) GOTO 110 !List empty (RTL,RBL) aviza pela network que a queue está vazia os pedidos/mensagens foram processados
C
C GET APPLICATIONS QUEUE NUMBER AND NUMBER OF LOG RECORDS
C REQUIRED FOR THIS TRANSACTION. IF TRANSACTION ALREADY
C HAS A SERIAL NUMBER THEN BYPASS FLAG IS SET TO TRUE.
C ON MASTER SEND TRANSACTIONS THAT ARE BYPASSED
C
C EURO MIL PROJECT - IF QUE IS EUI AND TYPE IS TYPREG SO DON'T SEND TO OTHER MACHINES
C                    IF QUE IS SPE AND TYPE IS TYPREG SO DON'T SEND TO OTHER MACHINES
C
		!ACTUAL SYS TIME SET BY DISPAT IN SECS
        IF(HPRO(TRCODE,BUF).EQ.TYPREG) PRO(TIMINOFF,BUF) = P(ACTTIM) !gets timestamp de agora (por dia só por o stopsys ser diario que limpa TMF do registo das transações)
		CALL OPS('DISPAT -> TRCODE',ZEXT(HPRO(TRCODE,BUF)),ZEXT(HPRO(TRCODE,BUF)))
		CALL GETQUE(BUF,QUE,SIZE,BYPASS) !obtêm QUE (application task queue number) do PROCOM/PROBUF
		CALL OPS('DISPAT -> QUE',ZEXT(QUE),ZEXT(QUE))
		CALL OPS('DISPAT -> SIZE',ZEXT(SIZE),ZEXT(SIZE))
		!CODE = HPRO(TRCODE,BUF)
        IF(IGSDEBUG(IA_DISPAT)) THEN 
            CALL OPS('188:DISPAT:GETQUE BUF', BUF,BUF)
            CALL OPS('188:DISPAT:GETQUE QUE', QUE,QUE)
            CALL OPS('188:DISPAT:GETQUE SIZ', SIZE,SIZE)
            CALL OPS('188:DISPAT:GETQUE BYP', BYPASS,BYPASS)
            CALL OPS('188:DISPAT:HPRO(TRCODE,BUF)', ZEXT(HPRO(TRCODE,BUF)),ZEXT(HPRO(TRCODE,BUF)))
            CALL OPSTXT('DUMP INPTAB MESSAGE:')
            CALL DUMP_MESSAGE(0,0,BPRO(BINPTAB,BUF),HPRO(INPLEN,BUF))
            CALL OPSTXT('DUMP WRKTAB MESSAGE:')
            CALL DUMP_MESSAGE(0,0,BPRO(WRKTAB*4-3+1,BUF),HPRO(OUTLEN,BUF))
        ENDIF
	IF(BYPASS) THEN !needs to pass thrue
	  IF(HPRO(TRCODE,BUF).EQ.TYPREG.AND.QUE.EQ.INI) GOTO 30 !QUEUE TRANSACTION TO APPROPIATE APPLICATIONS TASK
	  IF(HPRO(TRCODE,BUF).EQ.TYPREG.AND.QUE.EQ.EUI) GOTO 30 !QUEUE TRANSACTION TO APPROPIATE APPLICATIONS TASK
C----+------------------------------------------------------------------
C V10| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
          IF(HPRO(TRCODE,BUF).EQ.TYPREG.AND.QUE.EQ.IGI) THEN
              IF(IGSDEBUG(IA_DISPAT)) THEN 
                 CALL OPS('186:DISPAT QUE = IGI', QUE, IGI)
              ENDIF
              GOTO 30 !QUEUE TRANSACTION TO APPROPIATE APPLICATIONS TASK
          ENDIF
C----+------------------------------------------------------------------
C V10| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C----+------------------------------------------------------------------
C V11| Bugfix with transaction recording while in Duplex Mode
C----+------------------------------------------------------------------
C         IF(HPRO(TRCODE,BUF).EQ.TYPREG.AND.QUE.EQ.SPE) GOTO 30
C         IF(P(SYSTYP).EQ.LIVSYS)  CALL WNET(BUFNET,BUF,ST)
          IF(HPRO(TRCODE,BUF) .EQ. TYPREG .AND. QUE .EQ. SPE) THEN
              IF(IGSDEBUG(IA_DISPAT)) THEN 
                 CALL OPS('243:DISPAT TYPREG AND QUE = SPE', QUE, SPE)
              ENDIF
              GOTO 30 !QUEUE TRANSACTION TO APPROPIATE APPLICATIONS TASK
          ENDIF
          IF(P(SYSTYP).EQ.LIVSYS) THEN
              IF(IGSDEBUG(IA_DISPAT)) THEN 
                 CALL OPS('243:DISPAT ANTE WNET - HPRO(TRCODE,BUF)', 
     *                    ZEXT(HPRO(TRCODE,BUF)), ZEXT(HPRO(TRCODE,BUF)))
                 CALL OPS('243:DISPAT ANTE WNET - BUFNET,BUF', BUFNET, BUF)
              ENDIF
              CALL WNET(BUFNET,BUF,ST) !envia o buffer para newtwork de modo a outros sistemas/maquinas
              IF(IGSDEBUG(IA_DISPAT)) THEN 
                 CALL OPS('243:DISPAT POST WNET - ST', ST, ST)
              ENDIF
          ENDIF
C----+------------------------------------------------------------------
C V11| Bugfix with transaction recording while in Duplex Mode
C----+------------------------------------------------------------------
          IF(P(SYSTYP).EQ.LIVSYS) TOSEND=.TRUE.
	  GOTO 30
	ENDIF
C
C IF TRANSACTION IS AND ERROR MESSAGE THEN QUEUE IT TO
C ERROR QUEUE WITHOUT ASSIGNING A SERIAL NUMBER
C SINCE MESSAGES DO NOT GET LOGGED TO THE TRANSACTION
C FILE.  IF THE MAXIMUM NUMBER OF MESSAGES ARE QUEUED,
C THEN SET THE MESSAGE FULL FLAG AND RELEASE THE BUFFER.
C
	  IF(QUE.EQ.ERR) THEN
	    IF(ACTTSK(ERR).GE.P(MAXMES)) THEN
	      CALL RELBUF(BUF)!RELEASE THE BUFFER
	      P(MESFUL)=1 !SET THE MESSAGE FULL FLAG
	    ELSE
	      CALL QUETRA(ERR,BUF) !QUEUE IT TO ERROR QUEUE WITHOUT ASSIGNING A SERIAL NUMBER
	    ENDIF
	    GOTO 100 !acaba aqui não faz mais nada no dispatch
	  ENDIF
C
C
C----+------------------------------------------------------------------
C V11| Bugfix with transaction recording while in Duplex Mode
C----+------------------------------------------------------------------
        IF(IGSDEBUG(IA_DISPAT)) THEN
            CALL OPS('285:DISPAT:QUE',QUE,QUE)
            CALL OPS('285:DISPAT:BUF',BUF,BUF)
            CALL OPS('285:DISPAT:P(SYSTYP)',P(SYSTYP),P(SYSTYP))
            CALL OPS('285:DISPAT:TCSPE',TCSPE,TCSPE)
            CALL OPS('285:DISPAT:TCX2X',TCX2X,TCX2X)
            CALL OPS('285:DISPAT:BUFNET',BUFNET,BUFNET)
            CALL OPS('285:DISPAT:PRO(SERIAL,BUF)',PRO(SERIAL,BUF),PRO(SERIAL,BUF))
            CALL OPS('285:DISPAT:HPRO(TRCODE,BUF)',ZEXT(HPRO(TRCODE,BUF)),ZEXT(HPRO(TRCODE,BUF)))
        ENDIF
C----+------------------------------------------------------------------
C V11| Bugfix with transaction recording while in Duplex Mode (Primario e Secundario(backup))
C----+------------------------------------------------------------------
	IF(P(SYSTYP).EQ.LIVSYS) THEN
	   IF(QUE.NE.CMD.OR.
     *	      PRO(CTYP,BUF).EQ.TCSPE.OR.
     *	      PRO(CTYP,BUF).EQ.TCX2X)    THEN
C
	      IF(HPRO(TRCODE,BUF).EQ.TYPFRA) THEN    !GET ALL FRACTION SERIAL #
	         NUMTKT = HPRO(NBRTRA,BUF) !PARAMETER (NBRTRA=41)  !# OF TRANSACTIONS IN BUFFER
	         DO I = 0,NUMTKT - 1
                    IF(IGSDEBUG(IA_DISPAT)) THEN
                        CALL OPSTXT('260:DISPAT:GETSER')
                    ENDIF
				CALL GETSER(PRO(SERIAL,BUF),SIZE) !gets the next serial number
				!PARAMETER (FRA_FRC    = WRKTAB+2)  ! serial #s of fractioned tickets
		    PRO(FRA_FRC+I,BUF) = PRO(SERIAL,BUF) !defines the serial of this factioned tickets as the serial generated above (o conjunto de de bilhetes/frações ficam com um range de numeros de série)
	         END DO !confirmar o numero de serie aqui é o interno? pois o serial number se for 3 segmentos incrementa 3 se for 2 segmentos incrementa 2 e se for 1 segemento incrementa 1 em relação ao serial number anterior
	      ELSE
                 IF(IGSDEBUG(IA_DISPAT)) THEN
                     CALL OPSTXT('267:DISPAT:GETSER')
                 ENDIF
	         CALL GETSER(PRO(SERIAL,BUF),SIZE)
	      ENDIF
	      PRO(TSTAMP,BUF)=P(ACTTIM)
C----+------------------------------------------------------------------
C V11| Bugfix with transaction recording while in Duplex Mode
C----+------------------------------------------------------------------
              IF(IGSDEBUG(IA_DISPAT)) THEN
                  CALL OPS('301:DISPAT ANTE WNET - HPRO(TRCODE,BUF)', 
     *                     ZEXT(HPRO(TRCODE,BUF)), ZEXT(HPRO(TRCODE,BUF)))
                  CALL OPS('301:DISPAT:WNET ANTE - BUFNET,BUF',BUFNET,BUF)
              ENDIF
C----+------------------------------------------------------------------
C V11| Bugfix with transaction recording while in Duplex Mode
C----+------------------------------------------------------------------
	      CALL WNET(BUFNET,BUF,STATUS) !passa a informação/mensagem para a maquina B/secundario para ser processado?
C----+------------------------------------------------------------------
C V11| Bugfix with transaction recording while in Duplex Mode
C----+------------------------------------------------------------------
              IF(IGSDEBUG(IA_DISPAT)) THEN
                  CALL OPS('301:DISPAT:WNET POST - ST',ST,ST)
              ENDIF
C----+------------------------------------------------------------------
C V11| Bugfix with transaction recording while in Duplex Mode
C----+------------------------------------------------------------------
	      TOSEND=.TRUE.
	   ELSE
	      IF(BUFNET.NE.0) TOSEND=.TRUE.
	      CALL SENDBUF(BUFNET)        !SEND BUFFERS IF FREEZE
	   ENDIF
	ELSE
	   IF(PRO(SERIAL,BUF).EQ.0) THEN
	      CALL RELBUF(BUF) !liberta o buffer sendo enviado para o campo de PROCOM que é FREEQ(NUMPRO+QHEDSZ) que é um array de buffers livres/zerados
	      GOTO 100
	   ELSE 
	      IF(HPRO(TRCODE,BUF).EQ.TYPFRA) THEN
	         NUMTKT = HPRO(NBRTRA,BUF) !PARAMETER (NBRTRA=41)   !# OF TRANSACTIONS IN BUFFER
		 DO I = 0,NUMTKT - 1
			!confirmar se está a ser dado o offset 32.000.000 entre o sistema principal e o sistema secundario
		    CALL CHKSER(PRO(FRA_FRC+I,BUF),SIZE) !SUBROUTINE TO CHECK SERIAL NUMBERS IN SECONDARY SYSTEMS
		 END DO
	      ELSE
	         CALL CHKSER(PRO(SERIAL,BUF),SIZE) !SUBROUTINE TO CHECK SERIAL NUMBERS IN SECONDARY SYSTEMS
	      ENDIF
	   ENDIF
	ENDIF
C
C IF TRANSACTION IS A COMMAND REQUIREING SYSTEM FREEZE THEN
C STOP DEQUEING TRANSACTIONS, AND PROCESS COMMAND.
C NOTE: X2X COMMANDS WILL NOT FREEZE SYSTEM.
C

	CALL OPS('DISPAT -> CTYP',ZEXT(PRO(CTYP,BUF)),ZEXT(PRO(CTYP,BUF)))
	IF(QUE.EQ.CMD.AND.PRO(CTYP,BUF).EQ.TCX2X) GOTO 30 !envia o buffer/probuf para a aplicação/processo
	IF(QUE.EQ.CMD.AND.PRO(CTYP,BUF).NE.TCSPE) GOTO 300 !FREEZE INPUT AND FLUSH ALL TRANSACTIONS IN PROCESS
C
C QUEUE TRANSACTION TO APPROPIATE APPLICATIONS TASK
C AND INCREMENT ACTIVE TASK LIST.
C
30	CONTINUE
C
C DEBUG
C
	IF(P(XXDEBUG).EQ.0) THEN
	   DMPDBG=.FALSE.
	   IF(P(XXDTRLN).EQ.0) DMPDBG=.TRUE.
	   IF(P(XXDTRLN).LT.0) THEN
	      IF(ABS(P(XXDTRLN)).EQ.HPRO(LINENO,BUF)) DMPDBG=.TRUE.
	   ENDIF
	   IF(P(XXDTRLN).GT.0) THEN
	      IF(P(XXDTRLN).EQ.HPRO(TERNUM,BUF)) DMPDBG=.TRUE.
	   ENDIF
	   IF(DMPDBG) CALL PRTOUT(BUF)
	ENDIF
	CALL TRNTRACE(BUF,0)       !TRY TO TRACE INPUT BUF
	CALL QUETRA(QUE,BUF) !envia o buffer/probuf para a aplicação/processo obtido na subroutina do GETQUE a variavel de saida QUE podendo ser WAG, etc...
100	CONTINUE
C
C END OF TRANSACTION INPUT
C SET APPLICATIONS ACTIVE
C (TIMER WILL ACTIVATE LOGGER)
C
110	CONTINUE
C
C ON PRIMARY SEND TO NETWORK IF QUEUE EMPTIES OR BATCH DONE
C
	IF(P(SYSTYP).EQ.LIVSYS.AND.BUFNET.NE.0) TOSEND=.TRUE.
	IF(P(SYSTYP).EQ.LIVSYS) CALL SENDBUF(BUFNET)
C
	DO 120 I=1,NUMAPPQUE    !**PXN WAS NUMASK
	IF(ACTTSK(I).NE.0) CALL RELSE(TSKNAM(I),STATUS)
120	CONTINUE
C
C IF END OF DAY STOP, ELSE GO BACK TO WAIT STATE
C
190	CONTINUE
	IF(DAYSTS.EQ.DSOPEN) GOTO 10
	IF(DAYSTS.EQ.DSSUSP) THEN
	  CALL HOLD(0,ST)
	  GOTO 190
	ENDIF
C
C
	DO 200 I=1,NUMTSK
	IF(TSKNAM(I).NE.'        ') CALL RELSE(TSKNAM(I),STATUS)
200	CONTINUE
	CALL GSTOP(GEXIT_SUCCESS)
C
C **** COMMAND PROCESSING
C
300	CONTINUE
	P(CMDFLU)=1
	CALL OPS('DISPAT -> P(CMDFLU)=',P(CMDFLU),P(CMDFLU))
C
C FREEZE INPUT AND FLUSH ALL TRANSACTIONS IN PROCESS
C
310	CONTINUE
	ACTIVE=0
C
C
	DO 320 I=1,NUMAPPQUE      !**PXN WAS NUMTSK
C		CALL OPS('NUMAPPQUE I:',I,I)
C		CALL OPSTXT('ERR=10,LOG=11,RPC=13,DIS=12,CRS=26')
C		CALL OPS('ACTTSK(I):',ACTTSK(I),ACTTSK(I))
      	    IF(I.EQ.ERR) GOTO 320
      	    IF(I.EQ.LOG) GOTO 320
      	    IF(I.EQ.RPC) GOTO 320
	    IF(I.EQ.DIS) GOTO 320
	    IF(I.EQ.CRS) GOTO 320
	    IF(ACTTSK(I).NE.0) THEN
	      CALL RELSE(TSKNAM(I),STATUS)
	      ACTIVE=ACTIVE+1
	    ENDIF
320	CONTINUE
	CALL OPSTXT('NUMAPPQUE out')
C
C IF APPLICATIONS STILL ACTIVE, WAIT A WHILE THEN
C CHECK AGAIN, ELSE ACTIVATE COMMAND PROCESSOR
C AND GO BACK TO WAIT STATE.
C
350	CONTINUE
	CALL OPS('ACTIVE--->',ACTIVE,ACTIVE)
	IF(ACTIVE.NE.0) THEN
	  CALL XWAIT(10,1,STATUS)
	  GOTO 310
	ENDIF
	CALL OPS('FLUSH--->',FLUSH,FLUSH)
	IF(FLUSH) GOTO 400
C
C FLUSH ALL SECOND PHASE TRANSACTIONS FROM THE INPUT QUEUE
C
	CALL OPSTXT('COPY COMMAND_BUFFER TO BUF --------')
	COMMAND_BUFFER = BUF
	COMMAND_SIZE = SIZE !Save size of command for use after flush
	FLUSH=.TRUE.
	MBATCH=NUMPRO
	GOTO 20
C
C
400	CONTINUE
	CALL OPSTXT('JUMPED to label 400')
	P(CMDFRZ)=1
	P(CMDFLU)=0
	FLUSH=.FALSE.
	IF(P(NETFLU).EQ.FLUREQ.OR.P(NETFLU).EQ.FLURQ1) THEN
	  IF(P(NETFLU).EQ.FLUREQ.AND.P(SYSTYP).NE.LIVSYS) THEN
405	    CONTINUE
	    CALL RTL(BUF,QUETAB(1,DIS),ST) !DEQUE NEXT TRANSACTION
	    IF(ST.EQ.2) BUF=0
	    IF(BUF.GT.0) THEN
	       CALL RELBUF(BUF)
	       GOTO 405
	    ENDIF
	  ENDIF
	  P(NETFLU)=FLUSHED
	  GOTO 10      !FLUSHING DONE
	ENDIF
C
C IF PRIMARY SYSTEM THEN PUT BALANCING INFORMATION FOLLOWING
C COMMAND DATA IN THE PROCOM BUFFER. IF NOT THE PRIMARY THEN
C CHECK THE BALANCING INFORMATION FROM THE PRIMARY SYSTEM
C
	CALL OPSTXT('BEFORE LABEL 430')
	IF(P(SYSTYP).EQ.LIVSYS) THEN
          IF(IGSDEBUG(IA_DISPAT)) THEN
              CALL OPSTXT('420:DISPAT:GETSER')
          ENDIF
	  CALL GETSER(PRO(SERIAL,COMMAND_BUFFER),COMMAND_SIZE)
	  PRO(TSTAMP,COMMAND_BUFFER)=P(ACTTIM)
	ENDIF
	OFF=0
	TAB(OFF)=P(NXTTRA)
	OFF=OFF+1
	DO 420 GAME=1,MAXGAM
	TAB(OFF)=DAYTYP(TRACNT,1,GAME)
	TAB(OFF+1)=DAYTYP(DOLAMT,1,GAME)
	DO 410 TYPE=2,NUMFIN
	TAB(OFF)=TAB(OFF)+DAYTYP(TRACNT,TYPE,GAME)
	TAB(OFF+1)=TAB(OFF+1)-DAYTYP(DOLAMT,TYPE,GAME)
410	CONTINUE
	OFF=OFF+2
420	CONTINUE
C
C
	IF(P(SYSTYP).EQ.LIVSYS) THEN
	  DO 430 OFF=0,MAXGAM*NUMTOT
	  PRO(CBAL+OFF,COMMAND_BUFFER)=TAB(OFF)
430	  CONTINUE
	  CALL OPSTXT('AFTER LABEL 430')
C----+------------------------------------------------------------------
C V11| Bugfix with transaction recording while in Duplex Mode
C----+------------------------------------------------------------------
          IF(IGSDEBUG(IA_DISPAT)) THEN 
              CALL OPS('493:DISPAT ANTE WNET - HPRO(TRCODE,CMD_BUF)', 
     *        ZEXT(HPRO(TRCODE,COMMAND_BUFFER)), ZEXT(HPRO(TRCODE,COMMAND_BUFFER)))
              CALL OPS('493:DISPAT WNET ANTE - BUFNET, CMD_BUF', BUFNET, COMMAND_BUFFER)
          ENDIF
C----+------------------------------------------------------------------
C V11| Bugfix with transaction recording while in Duplex Mode
C----+------------------------------------------------------------------
	  CALL WNET(BUFNET,COMMAND_BUFFER,STATUS)
C----+------------------------------------------------------------------
C V11| Bugfix with transaction recording while in Duplex Mode
C----+------------------------------------------------------------------
          IF(IGSDEBUG(IA_DISPAT)) THEN 
             CALL OPS('493:DISPAT WNET POST - STATUS', STATUS, STATUS)
          ENDIF
C----+------------------------------------------------------------------
C V11| Bugfix with transaction recording while in Duplex Mode
C----+------------------------------------------------------------------
	  TOSEND=.TRUE.
	  P(LSTCMD)=NXTSER
	ELSE
	  ERRTYP=0
	  DO 440 OFF=0,MAXGAM*NUMTOT
	  IF(PRO(CBAL+OFF,COMMAND_BUFFER).NE.TAB(OFF)) THEN
	    IF(ERRTYP.EQ.0) ERRTYP=1         !ASSUME OLD DIFFERENCE
	    IF(PRO(CBAL+OFF,COMMAND_BUFFER)-TAB(OFF).NE.DIF(OFF))
	1	ERRTYP=2
	  ENDIF
	  DIF(OFF)=PRO(CBAL+OFF,COMMAND_BUFFER)-TAB(OFF)
440	  CONTINUE
	  CALL OPSTXT('AFTER LABEL 440')
C
C REPORT DISCREPENCY
C
	  IF(ERRTYP.NE.0) THEN
	    CALL NOTIFY(TRCADR,NOTSYNC,ERRTYP,WAYINP)
	    IF(ERRTYP.NE.1) THEN
	      DO 445 OFF=0,MAXGAM*NUMTOT
	      IF(DIF(OFF).NE.0) THEN
	        CALL NOTIFY1(OFF,NOTDISCR,DIF(OFF),WAYINP)
	      ENDIF
445	      CONTINUE
	    ENDIF
C
C ASK FOR RESYNC
C
	    ENASYNC=NETENA(NODEID,WAYINP)
	    IF(ENASYNC.EQ.0.AND.ERRTYP.GE.1
     *	     .OR.ENASYNC.EQ.1.AND.ERRTYP.GT.1) THEN
	      IF(.NOT.(PRO(CTYP,COMMAND_BUFFER).EQ.TCGEN.AND. !NOT DAYSTS CHANGE
     *	         PRO(CNUM,COMMAND_BUFFER).EQ.2)) THEN
	         CALL ASKSYNC(P(LSTCMD),WAYINP) !ASK FOR RESYNC (INP)
	      ENDIF
	      IF(.NOT.(ENASYNC.EQ.1.AND.ERRTYP.EQ.1)) THEN
	        DO 450 OFF=0,MAXGAM*NUMTOT
	        DIF(OFF)=0    !ZERO DIFFERENCES IF REQUESTED
450	        CONTINUE
	      ENDIF
	    ENDIF
	  ELSE
	    IF(PRO(CTYP,COMMAND_BUFFER).EQ.TCGEN.AND.    !CHECKPOINT COMMAND
     *	       PRO(CNUM,COMMAND_BUFFER).EQ.1) P(LSTCMD)=NXTSER
	  ENDIF
	ENDIF
C
C
	IF(COMMAND_BUFFER.EQ.0) GOTO 10
	CALL OPSTXT('SEND COMMAND_BUFFER TO CMD QUEUE........')
	CALL QUETRA(CMD,COMMAND_BUFFER)
	COMMAND_BUFFER=0
	COMMAND_SIZE = 0
	CALL RELSE(TSKNAM(CMD),STATUS)
	GOTO 10
	END



        
        
        
        
        
        
        
        SUBROUTINE DUMP_MESSAGE(MESSAGE_ID, LINE_ID, OUTBUF, MESLEN)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        BYTE OUTBUF(*)
        INTEGER*4 MESLEN
        INTEGER*4 MESSAGE_ID, LINE_ID

        CHARACTER*255 BUF
        CHARACTER*3 ARR(16)
        INTEGER*4 I, J, K, DIV, REMAIN, OFFSET
        
        DO I = 1, 255
            BUF(I:I) = CHAR(0)
        ENDDO
        
        DIV = MESLEN / 16
        REMAIN = MOD(MESLEN,16)
        
        WRITE(BUF, 900) MESSAGE_ID, LINE_ID, MESLEN
        TYPE *, IAM(), '', TRIM(BUF)
        CALL OPSTXT(TRIM(BUF))
        
        DO K = 1, DIV
           DO I = 1, 16
               DO J = 1, 2
                   ARR(I)(J:J) = ' '
               ENDDO
               ARR(I)(3:3) = CHAR(0)
           ENDDO
           DO I = 1, 16
               OFFSET = ((K - 1) * 16) + I
               WRITE(ARR(I), 901) OUTBUF(OFFSET)
           ENDDO
           OFFSET = ((K - 1) * 16)
           WRITE(BUF, 902) OFFSET + 1, OFFSET + 16,( ARR(I), I = 1, 16)
           TYPE *, '', TRIM(BUF)
           CALL OPSTXT(TRIM(BUF))
        ENDDO
        IF(REMAIN .NE. 0) THEN
           DO I = 1, 16
               DO J = 1, 2
                   ARR(I)(J:J) = ' '
               ENDDO
               ARR(I)(3:3) = CHAR(0)
           ENDDO
           DO I = 1, REMAIN
               OFFSET = ((K - 1) * 16) + I
               WRITE(ARR(I), 901) OUTBUF(OFFSET)
           ENDDO
           OFFSET = ((K - 1) * 16)
           WRITE(BUF, 902) OFFSET + 1, OFFSET + REMAIN, (ARR(I), I = 1, 16)
           TYPE *, '', TRIM(BUF)
           CALL OPSTXT(TRIM(BUF))
        ENDIF
        TYPE *, ''

900     FORMAT('PARSED MESSAGE #',I8,' (@ LINE #',I8,') : LEN = ', I8)
901     FORMAT(Z2.2)
902     FORMAT('[',I4,':',I4,'] = ',16(A2,1X))

        RETURN
        END
