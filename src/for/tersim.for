C TERSIM.FOR
C
C V10 13-DEC-2010 HXK LOTTO 2 CHANGES - ADDED MANUAL BETS
C V09 13-APR-2010 RXK Changes for ePassive
C V08 25-MAR-2009 MMO REMOVED REQUEST JOKER NUMBER IN WAGERS FOR JOKER
C V07 06-FEB-2001 ANG Added Passive game
C V06 03-DEc-2000 UXN Totogolo added.
C V05 08-JUN-2000 OXK Non-used variables removed
C V04 31-JAN-2000 OXK Removed hardcoded GIND=1 for TSPT (Vakio changes)
C V03 13-OCT-1999 RXK World Tour added.
C V02 17-MAY-1999 UXN Super Triple added.
C V01 XX-XXX-XXXX XXX Initial release.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM PTERSIM
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
C
C
        INTEGER*4 PERQP, NREGBRD
        INTEGER*4 DRWMAX, BRDMAX, NDRWS
        BYTE      NBRDS
C
        LOGICAL   QP
        LOGICAL   CYCLE
        INTEGER*4 SQP
        LOGICAL   VSENT
        INTEGER*4 I, J, EXT
        INTEGER*4 BUF, ST
        INTEGER*4 BUFERR, TOTNR, WAGNO, QPNO, TNBRDS
        INTEGER*4 IPSNO
C
C       ! manual betting data
	COMMON /MANUAL/ MANTAB,LUCKY,MANBET,SYS_SIZ,NUMPAN
	INTEGER*4 MANTAB(11,0:10,10)   ! (#nums,#panels,#wagers)
	INTEGER*4 LUCKY(10)          ! (#wagers)
	INTEGER*4 MANBET, SYS_SIZ, NUMPAN
C
        LOGICAL   SIGNED/.FALSE./
        LOGICAL   NOLIMIT
        INTEGER*4 NAME(2), CBUF(CDLEN)                     !FOR 'CALL QUECMD'
        LOGICAL   CHGDES/.FALSE./
C
        INTEGER*4 LIB$DISABLE_CTRL, LIB$ENABLE_CTRL
        EXTERNAL  LIB$DISABLE_CTRL, LIB$ENABLE_CTRL
C
C
C
        CALL COPYRITE
C
5       CONTINUE
C
        CDC = DAYCDC
        CALL TERSIMINIT                             !INITIALIZE GAME PARAMETERS
C
        CALL GETNAM(NAME)                              !FOR QUECMD
C
        CALL GETSIMOPT(TOTNR,PERQP,NREGBRD,DRWMAX,BRDMAX,
     *                 CYCLE,SQP,VSENT,SIGNED)
C
        CALL CHCKDIS(ST)
        IF(ST.NE.0) THEN
           TYPE *, ' '
           TYPE *, IAM(),'System is NOT ACTIVE!' 
           TYPE *, ' '
        ENDIF
c...    IF(X2X_GAME_STATE.GT.X2X_GAMES_IDLE) THEN
c...       TYPE *, ' '
c...       TYPE *, IAM(),'Communication is ACTIVE!' 
c...       TYPE *, ' '
c...       CALL GSTOP(GEXIT_FATAL)
c...    ENDIF
C
        NOLIMIT=.FALSE.
        IF(TOTNR.EQ.0) NOLIMIT=.TRUE.
C
        P(SUPWAG)=0
        P(SUPCAN)=0
        P(SUPVAL)=0
        P(SUPRET)=0
        IF(P(DESFLG).EQ.0) THEN
           CALL FASTSET(0,CBUF,CDLEN)
           CBUF(1)=DESFLG
           CBUF(2)=1
           CBUF(3)=TCPAR
           CBUF(6)=NAME(1)
           CALL QUECMD(CBUF,ST)
           CALL XWAIT(2,1,ST)
           CHGDES=.TRUE.
        ENDIF
C
        P(SIMLAT)=1
C
C Sign on  all terminals
C
        IF(.NOT.SIGNED .AND. SIGON) THEN
           CALL FASTSET(0,CBUF,CDLEN)
           CBUF(1)=1
           CBUF(2)=2
           CBUF(3)=TCAGT
           CBUF(6)=NAME(1)
           DO 10 I=LOTER,HITER
              CBUF(5)=I
              CALL QUECMD(CBUF,ST)
              CALL XWAIT(2,1,ST)
              IF(P(CLRKACT).EQ.0) AGTHTB(AGTNCL,I)=1    !CLERKS ENABLED
              AGTHTB(AOPSTS,I)=SIGNON
10         CONTINUE
           SIGNED=.TRUE.
           TYPE *,IAM(),' AGENTS HAVE BEEN SIGNED ON'
        ENDIF
C
        TER=MAX(LOTER,1)
C
        DO 7 I=LOTER,HITER
           SEQNO(I) = MOD(AGTHTB(ATRNUM,I)+1.AND.'000F'X,8)
7       CONTINUE
C
        WAGNO=0
        TNBRDS=0
        QPNO=0
1000    CONTINUE
C
C validation
C
        IF(TRATYP.EQ.TRTYPV) THEN 
           IF (.NOT.PASSVAL) THEN
               CALL VALSIM(EXT)
           ELSE
               CALL VALPASS(EXT)
           ENDIF
           IF(EXT.LT.0) GOTO 5
C
C cancellation
C 
        ELSE IF(TRATYP.EQ.TRTYPC) THEN 
           CALL CANSIM(EXT)
           IF(EXT.LT.0) GOTO 5
C
C signon
C
        ELSE IF(TRATYP.EQ.TRTYPN) THEN 
           CALL SONSIM(EXT)
           IF(EXT.LT.0) GOTO 5
C
C signoff
C
        ELSE IF(TRATYP.EQ.TRTYPF) THEN 
           CALL SOFSIM(EXT)
           IF(EXT.LT.0) GOTO 5
C
C reports
C
        ELSE IF(TRATYP.EQ.TRTYPR) THEN 
           IF(TRASUB.EQ.1) CALL GAMSIM(EXT)
           IF(TRASUB.EQ.2) GOTO 5
           IF(TRASUB.EQ.3) CALL FINSIM(EXT)
           IF(TRASUB.EQ.4) CALL NEWSIM(EXT)
           IF(EXT.LT.0) GOTO 5
C
C reprints
C
         ELSE IF(TRATYP.EQ.TRTYPP) THEN
           GOTO 5                       !unsupported yet
C
C instant ticket
C
        ELSE IF(TRATYP.EQ.TRTYPI) THEN
           IF(TER.GT.HITER) TER=LOTER
           CALL IPSSIM(EXT)
           IPSNO=IPSNO+1
           IF(EXT.EQ.-1) GOTO 5
C
C order/service request/confirmation
C
        ELSE IF(TRATYP.EQ.TRTYPO) THEN
           CALL ORDSRVSIM(EXT)
           IF(EXT.EQ.-1) GOTO 5
C
C group betting
C
        ELSE IF(TRATYP.EQ.TRTYPG) THEN 
           CALL FRACSIM(EXT)
           IF(EXT.LT.0) GOTO 5
C
C pPassive returns
C
        ELSE IF(TRATYP.EQ.TRTYPT) THEN 
           CALL UNSPASS(EXT)
           IF(EXT.LT.0) GOTO 5
C
C wager processing
C
        ELSE IF(TRATYP.EQ.TRTYPW) THEN
           IF(TER.GT.HITER) TER=LOTER
C
           EXT=0
C
           IF(GTYP.EQ.TLTO) THEN
              CALL LTOSIM(WAGNO,NBRDS,QP,PERQP,NREGBRD,
     *                    BRDMAX,NDRWS,MANTAB,LUCKY,EXT)
              IF(EXT.EQ.-2) GOTO 5              !invalid system #
              IF(EXT.EQ.-1) GOTO 990
           ELSE IF(GTYP.EQ.TPAS) THEN
              CALL PASSIM(WAGNO,NBRDS,QP,PERQP,DRWMAX,
     *                    BRDMAX,NDRWS,EXT)
              IF(EXT.EQ.-1) GOTO 990
           ELSE IF(GTYP.EQ.TKIK) THEN
              CALL KIKSIM(WAGNO,NBRDS,QP,PERQP,DRWMAX,
     *                    BRDMAX,NDRWS,EXT)
              IF(EXT.EQ.-1) GOTO 990
           ELSE IF(GTYP.EQ.TSPT) THEN
              CALL SPTSIM(WAGNO,NBRDS,QP,PERQP,DRWMAX,
     *                    BRDMAX,NDRWS,EXT)
              IF(EXT.EQ.-2) GOTO 5
              IF(EXT.EQ.-1) GOTO 990
           ELSE IF(GTYP.EQ.TTGL) THEN
              CALL TGLSIM(WAGNO,NBRDS,QP,PERQP,DRWMAX,
     *                    BRDMAX,NDRWS,EXT)
              IF(EXT.EQ.-2) GOTO 5
              IF(EXT.EQ.-1) GOTO 990
           ELSE IF(GTYP.EQ.TTSL) THEN
              CALL TSLSIM(WAGNO,NBRDS,QP,PERQP,DRWMAX,
     *                    BRDMAX,NDRWS,EXT)
              IF(EXT.EQ.-1) GOTO 990
           ELSE IF(GTYP.EQ.TSCR) THEN
              CALL SCRSIM(WAGNO,NBRDS,QP,PERQP,DRWMAX,
     *                    BRDMAX,NDRWS,EXT)
              IF(EXT.EQ.-1) GOTO 990
           ELSE IF(GTYP.EQ.TSSC) THEN
              CALL SSCSIM(WAGNO,NBRDS,QP,PERQP,DRWMAX,
     *                    BRDMAX,NDRWS,EXT)
              IF(EXT.EQ.-1) GOTO 990
           ELSE IF(GTYP.EQ.TWIT) THEN
              CALL WITSIM(WAGNO,NBRDS,QP,PERQP,DRWMAX,
     *                    BRDMAX,NDRWS,EXT)
              IF(EXT.EQ.-1) GOTO 990
           ELSE IF(GTYP.EQ.TDBL) THEN
              CALL DBLSIM(WAGNO,NBRDS,QP,PERQP,DRWMAX,
     *                    BRDMAX,NDRWS,EXT)
              IF(EXT.EQ.-1) GOTO 990
C EURO MIL PROJECT
           ELSE IF(GTYP.EQ.TEUM) THEN 
                CALL EUROSIM( WAGNO, NBRDS, QP, PERQP, DRWMAX, BRDMAX, NDRWS, EXT)
              IF(EXT.EQ.-1) GOTO 990
           ELSE IF(GTYP.EQ.TCPL) THEN
              CALL CPLSIM(WAGNO,NBRDS,QP,PERQP,DRWMAX,
     *                    BRDMAX,NDRWS,EXT)
              IF(EXT.EQ.-1) GOTO 990
           ELSE IF(GTYP.EQ.TTRP) THEN
              CALL TRPSIM(WAGNO,NBRDS,QP,PERQP,DRWMAX,
     *                    BRDMAX,NDRWS,EXT)
              IF(EXT.EQ.-1) GOTO 990
           ELSE IF(GTYP.EQ.TSTR) THEN
              CALL STRSIM(WAGNO,NBRDS,QP,PERQP,DRWMAX,
     *                    BRDMAX,NDRWS,EXT)
              IF(EXT.EQ.-1) GOTO 990
           ELSE IF(GTYP.EQ.TBNG) THEN
              CALL BNGSIM(WAGNO,EXT)
              IF(EXT.EQ.-1) GOTO 990
C***       ELSE IF(GTYP.EQ.TVIC) THEN
C***          CALL VICSIM(WAGNO,NBRDS,QP,PERQP,NREGBRD,
C*** *                    DRWMAX,BRDMAX,SQP,VSENT,NDRWS,EXT)
C***          IF(EXT.EQ.-1) GOTO 990
C***       ELSE IF(GTYP.EQ.TKNO) THEN
C***          CALL KNOSIM(WAGNO,NBRDS,QP,KNO_BOARD,PERQP,NREGBRD,
C*** *                    DRWMAX,BRDMAX,NDRWS,PICK,EXT)
C***          IF(EXT.EQ.-1) GOTO 990
           ENDIF
C
           WAGNO=WAGNO+1
           IF(QP) THEN
              QPNO=QPNO+1
           ELSE
              TNBRDS=TNBRDS+NBRDS
           ENDIF
        ELSE
           GOTO 5                           !unsupported message type
        ENDIF
C
C
        ST=LIB$DISABLE_CTRL('02000000'X)
16      CONTINUE
        CALL GETBUF(BUF)
        IF(BUF.LE.0) THEN
           BUFERR=BUFERR+1
           IF(MOD(BUFERR,100).EQ.0) THEN
              TYPE*,'BUFFER ALLOCATION ERROR ...',BUFERR,' TIMES'
              IF(BUFERR.EQ.500) THEN
                 TYPE*,' '
                 IF(WAGNO.EQ.0) THEN
                    TYPE 900,0,0,0.0
                 ELSE
                    TYPE 900, WAGNO, QPNO, 100.0*QPNO/WAGNO
                 ENDIF
                 TYPE 901, TNBRDS
                 ST=LIB$ENABLE_CTRL('02000000'X)
                 GOTO 600
              ENDIF
           ENDIF
           CALL XWAIT(50,1,ST)
           GOTO 16
        ELSE
           BUFERR=0
        ENDIF
C
C
        HPRO(SIMLTR,BUF)=TER
        HPRO(SIMHTR,BUF)=TER
        HPRO(SIMMOD,BUF)=-999
        HPRO(TERNUM,BUF)=TER
        HPRO(INPLEN,BUF)=MESLEN
        HPRO(TRCODE,BUF)=TYPREG
C
C        CALL SETCHKSUM(MESBUF1,MESLEN)
	CALL MOVBYT(MESBUF1,1,BPRO(1,BUF), BINPTAB, MESLEN)
        CALL QUEINP(BUF,ST)

        ST=LIB$ENABLE_CTRL('02000000'X)
C
        SEQNO(TER) = MOD(ZEXT(SEQNO(TER))+1,8)
        IF(DELY.GT.0) CALL XWAIT(DELY,1,ST)
C       
        IF(NODISP) GOTO 500
C
        IF(TRATYP.EQ.TRTYPC) THEN
        ELSE IF(TRATYP.EQ.TRTYPV) THEN
        ELSE IF(TRATYP.EQ.TRTYPN) THEN
        ELSE IF(TRATYP.EQ.TRTYPF) THEN
        ELSE IF(TRATYP.EQ.TRTYPR.AND.TRASUB.EQ.1) THEN
        ELSE IF(TRATYP.EQ.TRTYPR.AND.TRASUB.EQ.3) THEN
        ELSE IF(TRATYP.EQ.TRTYPR.AND.TRASUB.EQ.4) THEN
        ELSE IF(TRATYP.EQ.TRTYPO) THEN                                          
        ENDIF
        TYPE *,IAM(),' '
C
500     CONTINUE
        IF(TRATYP.NE.TRTYPW .AND. TRATYP.NE.TRTYPI) GOTO 990 
        IF(TRATYP.EQ.TRTYPI) THEN
           IF(NOLIMIT .OR. IPSNO.LT.TOTNR) THEN
              TER=TER+1
              IF(TER.GT.NUMAGT) TER = 1
              GOTO 1000
           ELSE
              GOTO 5
           ENDIF
        ENDIF
        IF(MOD(WAGNO,100).EQ.0.OR.WAGNO.EQ.TOTNR) THEN
           TYPE 900, WAGNO, QPNO, 100.0*QPNO/WAGNO
           TYPE 901, TNBRDS
        ENDIF
        IF(NOLIMIT .OR. WAGNO.LT.TOTNR) THEN
           TER=TER+1
           GOTO 1000
        ENDIF
C
C LAST TRANSACTION RETRY
C
        IF(RETRY) THEN
        	
           CALL XWAIT(20,2,ST)
           CALL GETBUF(BUF)
           IF(BUF.LE.0) THEN
              TYPE*,'BUFFER ALLOCATION ERROR. RETRY ABORTED...'
              GOTO 600
           ENDIF
           HPRO(SIMLTR,BUF)=TER
           HPRO(SIMHTR,BUF)=TER
           HPRO(SIMMOD,BUF)=-999  !! Check correspond ...pro, retry's are for
                                 !! simmod excluded
 
           HPRO(TERNUM,BUF)=TER
           HPRO(INPLEN,BUF)=MESLEN
           HPRO(TRCODE,BUF)=TYPREG
           MESBUF1(1) = MESBUF1(1) .OR. '40'X
           DO 720 J=1,BUFLEN4
              PRO(INPTAB+J-1,BUF)=MESBUF4(J)
720        CONTINUE
           CALL QUEINP(BUF,ST)
        ENDIF
C
C LAST TRANSACTION TERMINAL DELETION
C
        IF(INCAN) THEN
           CALL XWAIT(1,2,ST)
           CALL GETBUF(BUF)
           IF(BUF.LE.0) THEN
              TYPE*,'BUFFER ALLOCATION ERROR. DELETION ABORTED...'
              GOTO 600
           ENDIF
           HPRO(SIMLTR,BUF)=TER
           HPRO(SIMHTR,BUF)=TER
           HPRO(SIMMOD,BUF)=-999
           HPRO(TERNUM,BUF)=TER
           HPRO(INPLEN,BUF)=MESLEN
           HPRO(TRCODE,BUF)=TYPREG
           MESBUF1(1) = MESBUF1(1) .OR. '80'X
           DO 740 J=1,BUFLEN4
              PRO(INPTAB+J-1,BUF)=MESBUF4(J)
740        CONTINUE
           CALL QUEINP(BUF,ST)
           CALL XWAIT(250,1,ST)
        ENDIF
C
990     CONTINUE
        CALL PRMYESNO('Continue with new selections? [Y/N]',ST)
        IF(ST.EQ.1) GOTO 5                          !NEXT TURN
C
C
600     CONTINUE
        IF(CHGDES) THEN                     !RESTORE THE PREVIOUS VALUE
           TYPE*,IAM(),' '
           TYPE*,IAM(),'Restoring the previous value of P(DESFLG)'
           TYPE*,IAM(),'        ... WAIT 10 SECONDS'
           CALL XWAIT(10,2,ST)
           CALL FASTSET(0,CBUF,CDLEN)
           CBUF(1)=DESFLG
           CBUF(2)=0
           CBUF(3)=TCPAR
           CBUF(6)=NAME(1)
           CALL QUECMD(CBUF,ST)
           CALL XWAIT(2,1,ST)
        ENDIF
        TYPE*,' '
        CALL GSTOP(GEXIT_SUCCESS)
C
900     FORMAT(/1X,I10,' wagers generated',
     *             I10,' Quick-Picks (',F5.1,' %)')
901     FORMAT(1X,I10,' boards')
C
        END
C
C
        SUBROUTINE GETSIMOPT(TOTNR,PERQP,NREGBRD,DRWMAX,BRDMAX,
     *                       CYCLE,SQP,VSENT,SIGNED)
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
        INTEGER*4 I
        INTEGER*4 TOTNR,PERQP,NREGBRD,DRWMAX,BRDMAX
        LOGICAL   CYCLE                  
        INTEGER*4 SQP                                                    
        LOGICAL   VSENT                                                  
        LOGICAL   SIGNED                                 
C
	! manual betting data
	COMMON /MANUAL/ MANTAB,LUCKY,MANBET,SYS_SIZ,NUMPAN
	INTEGER*4 MANTAB(11,0:10,10)   ! (#nums,#panels,#wagers)
	INTEGER*4 LUCKY(10)          ! (#wagers)
	INTEGER*4 MANBET, SYS_SIZ, NUMPAN
C
	CHARACTER*10 STR
        CHARACTER*5 CYC                                                  
        CHARACTER*7 SQPTXT                                               
        INTEGER*4 OPTION,ST
        INTEGER*4 CFGLU,CFDB(7)
        INTEGER*4 CFGNAM(5)
        DATA CFGNAM/'TERS','IM.C','FG  ',2*'    '/
C
        INTEGER*4 NOENDDUR
        EXTERNAL  NOENDDUR
C
        CHARACTER*8 JOKTXT(0:3)/'        ','(right) ','(left)  ','(both)  '/
C
C
C       Condition Handling Routine established to prevent "stack dump"
C       after pressing CTRL-Z (Exit). Now CTRL-Z results in continuing 
C       this subroutine, no error message is displayed.
C       The routine is defined at the end of this file.
C
        CALL LIB$ESTABLISH(NOENDDUR)
C
C
C
C OPEN THE TERSIM.CFG FILE AND READ THE CONFIGURATION
C
        ST=0
        CFGLU=1
C
        CALL OPENW(CFGLU,CFGNAM,4,0,0,ST)
        CALL IOINIT(CFDB,CFGLU,SIMSEC*256)
C
C Create a new TERSIM.CFG IF FAILED TO OPEN OLD!!!
C
	IF(ST.NE.0) THEN
	  CALL CRTFIL(CFGNAM,SIMSEC,ST)
	  IF(ST.NE.0) THEN
	     TYPE*,IAM(),'FAILED TO CREATE TERSIM.CFG!!!'
	     CALL GSTOP(GEXIT_FATAL)
	  ENDIF
        ELSE
          CALL READW(CFDB,1,SIMREC,ST)
	  CALL CLOSEFIL(CFDB)
	ENDIF
C
        IF(MGNUM.NE.0) THEN
C
          IF(MGNUM.LT.1.OR.MGNUM.GT.MAXGAM) MGNUM=1
          GTYP = PAR(GMTYP,MGNUM)
          GIND = PAR(GMIND,MGNUM)
          GMAX = PAR(GMMAX,MGNUM)
          GBET = PAR(GMBET,MGNUM)
          GBON = PAR(GMBON,MGNUM)
          GBRD = PAR(GMBRD,MGNUM)
          GDRW = PAR(GMDRW,MGNUM)
          GMIN = PAR(GMMIN,MGNUM)
          GJOK = PAR(GMJOK,MGNUM)       
          IF(TELEBET) GJOK=0
          IF(GJOK.GT.0) MJOK(MGNUM)=.TRUE.
          IF(TELEBET) MJOK(MGNUM)=.FALSE.
C
	  IF(GIND.LE.0.OR.GIND.GT.MAXIND) THEN	    
	     TYPE*,'IND > ',GIND,' MGNUM =', MGNUM
	     GIND = 1
	  ENDIF
         IF(MSQP(GIND).EQ.0) THEN                                  
            SQPTXT=' NORMAL'
         ELSE IF(MSQP(GIND).EQ.1) THEN
            SQPTXT='SPECIAL'
          ELSE
            SQPTXT='MIXTURE'
          ENDIF                                          
          IF (PASSFLG.GT.0) THEN
              PASSVAL = .TRUE.
          ELSE
              PASSVAL = .FALSE.
          ENDIF                                          
        ELSE
          LOTER=1
          HITER=1
          SIGON=.TRUE.
          NODISP=.FALSE.
          DELY=50
          RETRY=.FALSE.
          INCAN=.FALSE.
          MANCHG=.FALSE.                                                  
          CYC='  (r)'                                                     
          SQPTXT=' NORMAL'                                                
          DO 5 I=1,MAXGAM
            MTOTNR(I)=100
            MPERQP(I)=50
            MNREGBRD(I)=20
            MDRWMAX(I)=5
            MBRDMAX(I)=3
            MCYCLE(I)=.FALSE.
            MSQP(I)=0
            MVSENT(I)=.FALSE.
            MJOK(I)=.FALSE.                     !V05
5         CONTINUE
          MGNUM=1
          GTYP = PAR(GMTYP,MGNUM)
          GIND = PAR(GMIND,MGNUM)
          GMAX = PAR(GMMAX,MGNUM)
          GBET = PAR(GMBET,MGNUM)
          GBRD = PAR(GMBRD,MGNUM)
          GDRW = PAR(GMDRW,MGNUM)
          GBON = PAR(GMBON,MGNUM)
          GMIN = PAR(GMMIN,MGNUM)
          GJOK = PAR(GMJOK,MGNUM)               !V05
          IF(TELEBET) GJOK=0
          IF(GJOK.GT.0) MJOK(MGNUM)=.TRUE.
          PASSFLG = 0
          PASSVAL = .FALSE.
          TRATYP=TRTYPW
        ENDIF
C
C
C
1       CONTINUE
        CALL CLRSCR(5)
        TYPE*,' ***       TERSIM SIMULATOR OPTIONS SETTING      ***'
        TYPE*, ' '
        IF(TRATYP.EQ.TRTYPW) THEN
          TYPE'(''  (0) TRANSACTIONS TYPE ..................    '',A7,X,A8,I3)',
     *		TYPTXT(TRATYP), GTNAMES(GTYP),GIND
        ELSE IF(TRATYP.EQ.TRTYPR) THEN
          TYPE'(''  (0) TRANSACTIONS TYPE ..................    '',A7,X,A8)',
     *		TYPTXT(TRATYP), REPORT(TRASUB)
        ELSE IF ((TRATYP.EQ.TRTYPV.OR.TRATYP.EQ.TRTYPC).AND.PASSVAL) THEN
          TYPE'(''  (0) TRANSACTIONS TYPE ..................    '',A7,X,A14)',
     *          TYPTXT(TRATYP),'(PASSIVE GAME)'
        ELSE
          TYPE'(''  (0) TRANSACTIONS TYPE ..................    '',A7)',
     *          TYPTXT(TRATYP)
        ENDIF
        IF(NODISP) THEN
          TYPE*,' (1) DISPLAY ALL MESSAGES AND WAGERS ....         NO'
        ELSE
          TYPE*,' (1) DISPLAY ALL MESSAGES AND WAGERS ....        YES'
        ENDIF
        IF(MANCHG) THEN                                                       
          TYPE*,' (2) MANUAL CHANGES IN MESS. ALLOWED ....        YES'
        ELSE
          TYPE*,' (2) MANUAL CHANGES IN MESS. ALLOWED ....         NO'
        ENDIF 
        TYPE'(''  (3) FIRST TERMINAL NUMBER............... '',I10)', LOTER
        TYPE'(''  (4) LAST  TERMINAL NUMBER............... '',I10)', HITER
        IF(SIGON) THEN
          IF(.NOT.SIGNED) THEN
            TYPE*,' (5) SIGN ON TERMINALS ..................        YES'
          ELSE
            TYPE*,' (5) SIGN ON TERMINALS ..................         NO',
     *            '  (already done)'
          ENDIF
        ELSE
          TYPE*,' (5) SIGN ON TERMINALS ..................         NO'
        ENDIF
C
C WAGERS
C
        IF(TRATYP.EQ.TRTYPW) THEN
C
	  IF(GTYP.EQ.TLTO.AND.GIND.GT.2) THEN
            TYPE'(''  (6) GAME INDEX.......................... '',I10)', GIND
            TYPE'(''        manual wagers .................... '',I10)', MANBET
	  ELSE
            TYPE'(''  (6) GAME INDEX.......................... '',I10)', GIND
          ENDIF
          IF(GTYP.EQ.TWIT.OR.GTYP.EQ.TSCR.OR.GTYP.EQ.TTSL.OR.
     *       GTYP.EQ.TDBL.OR.GTYP.EQ.TCPL.OR.
     *       GTYP.EQ.TSSC.OR.GTYP.EQ.TTRP.OR.
     *       GTYP.EQ.TSTR) THEN
             TYPE*,' (7) MAX NUMBER OF DRAWS.................        ',
     *             'n/a'  
          ELSE
             TYPE'(''  (7) MAX NUMBER OF DRAWS................. '',I10)', 
     *             MDRWMAX(MGNUM)
          ENDIF
          TYPE'(''  (8) MAX NUMBER OF BOARDS (OR PAIRS)..... '',I10)', 
     *          MBRDMAX(MGNUM)
          IF(MTOTNR(MGNUM).GT.0) THEN
            TYPE'(''  (9) TOTAL NUMBER OF WAGERS ............. '',I10)', 
     *            MTOTNR(MGNUM)
          ELSE
            TYPE*,' (9) TOTAL NUMBER OF WAGERS .............   no limit'
          ENDIF
          IF(MVSENT(MGNUM)) THEN                                            
            TYPE*,'(10) BONUS #''S SENT IN non-QP WAGERS ....        n/a'
          ELSE
            TYPE*,'(10) BONUS #''S SENT IN non-QP WAGERS ....        n/a'
          ENDIF                                                         
          TYPE'('' (11) PERCENTAGE OF QP''''S ................. '',I10)', 
     *          MPERQP(MGNUM)
          IF(MPERQP(MGNUM).GT.0) THEN
            TYPE'('' (12) TYPE OF QP''''S .......................    '',A7)',
     *            SQPTXT
          ELSE
            TYPE*,'(12) TYPE OF QP''S .......................        n/a'
          ENDIF
          IF(MPERQP(MGNUM).LT.100) THEN
Crxk        TYPE'('' (13) NUMBER OF DIFFERENT COMBINATIONS ... '',I10,A5)', 
Crxk     *                MNREGBRD(MGNUM), CYC
            TYPE*,'(13) NUMBER OF DIFFERENT COMBINATIONS ...        n/a' 
          ELSE
            TYPE*,'(13) NUMBER OF DIFFERENT COMBINATIONS ...        n/a'
          ENDIF


          TYPE'('' (14) DELAY BETWEEN TRANSACTIONS (ms) .... '',I10)', DELY
          IF(RETRY) THEN
            TYPE*,'(15) RETRY OF THE LAST TRANSACTION ......        YES'
          ELSE
            TYPE*,'(15) RETRY OF THE LAST TRANSACTION ......         NO'
          ENDIF
          IF(INCAN) THEN
            TYPE*,'(16) INTERNAL DELETION OF LAST WAGER ....        YES'
          ELSE
            TYPE*,'(16) TERMINAL DELETION OF LAST WAGER ....         NO'
          ENDIF
          IF(GTYP.EQ.TWIT.OR.GTYP.EQ.TSCR.OR.GTYP.EQ.TTSL.OR.
     *       GTYP.EQ.TDBL.OR.GTYP.EQ.TCPL.OR.GTYP.EQ.TBNG.OR.
     *       GTYP.EQ.TSSC.OR.GTYP.EQ.TTRP.OR.GTYP.EQ.TSTR.OR.
     *       GTYP.EQ.TPAS) THEN
             TYPE*,'(17) REQUEST JOKER NUMBER IN WAGERS .....        ',
     *               'n/a'
          ELSEIF(MJOK(MGNUM).AND.GTYP.NE.TKIK) THEN
            TYPE*,'(17) REQUEST JOKER NUMBER IN WAGERS .....        ',
     *             'YES ',JOKTXT(GJOK)
          ELSE
            TYPE*,'(17) REQUEST JOKER NUMBER IN WAGERS .....         NO'
          ENDIF
          IF(GTYP.EQ.TWIT.OR.GTYP.EQ.TBNG.OR.GTYP.EQ.TPAS) THEN
            TYPE*,'(18) SYSTEM BET .........................        n/a'
          ELSEIF(SYSBET) THEN
            IF(GTYP.EQ.TSCR.OR.GTYP.EQ.TDBL.OR.GTYP.EQ.TCPL.OR.
     *         GTYP.EQ.TSSC.OR.GTYP.EQ.TTRP.OR.GTYP.EQ.TKIK.OR.
     *         GTYP.EQ.TSTR) THEN
               TYPE*,'(18) SYSTEM BET .........................       ',
     *               ' YES'
            ELSE
               TYPE 181,'(18) SYSTEM BET .........................    ',
     *                 '    YES (system #',SYSBNUM,')'
181            FORMAT(1X,A,A,I4,A)
            ENDIF
          ELSE
            TYPE*,'(18) SYSTEM BET .........................         NO'
          ENDIF
          IF(BANK) THEN
            TYPE*,'(19) BANK ATTRIBUTES ....................        YES'
          ELSE
            TYPE*,'(19) BANK ATTRIBUTES ....................         NO'
          ENDIF
          IF(TELEBET.AND.
     *      (GTYP.EQ.TWIT.OR.GTYP.EQ.TSCR.OR.GTYP.EQ.TTSL.OR.
     *       GTYP.EQ.TDBL.OR.GTYP.EQ.TCPL.OR.GTYP.EQ.TLTO.OR.
     *       GTYP.EQ.TSSC.OR.GTYP.EQ.TTRP.OR.GTYP.EQ.TSTR)) THEN
             GJOK=0
             MJOK(MGNUM)=.FALSE.
            TYPE*,'(20) TELEBET ............................        YES'
          ELSE
            TYPE*,'(20) TELEBET ............................         NO'
          ENDIF
          IF(SERIALEM) THEN
            TYPE*,'(21) EM SERIAL NUMBER ...................        YES'
          ELSE
            TYPE*,'(21) EM SERIAL NUMBER ...................         NO'
          ENDIF
C
C NON-WAGER TRANSACTIONS
C
        ELSEIF(TRATYP.EQ.TRTYPI) THEN
          TYPE*,' (6)      n/a for this transaction type'
          TYPE*,' (7)      n/a for this transaction type'
          TYPE*,' (8)      n/a for this transaction type'
          IF(MTOTNR(MGNUM).GT.0) THEN
            TYPE'(''  (9) TOTAL NUMBER OF WAGERS ............. '',I10)', 
     *            MTOTNR(MGNUM)
          ELSE
            TYPE*,' (9) TOTAL NUMBER OF WAGERS .............   no limit'
          ENDIF
          TYPE*,'(10)      n/a for this transaction type'
          TYPE*,'(11)      n/a for this transaction type'
          TYPE*,'(12)      n/a for this transaction type'
          TYPE*,'(13)      n/a for this transaction type'
          TYPE*,'(14)      n/a for this transaction type'
          TYPE*,'(15)      n/a for this transaction type'
          TYPE*,'(16)      n/a for this transaction type'
          TYPE*,'(17)      n/a for this transaction type'
        ELSE IF (TRATYP.EQ.TRTYPV.OR.TRATYP.EQ.TRTYPC) THEN
          IF (TRATYP.EQ.TRTYPV) STR = 'VALIDATION'
          IF (TRATYP.EQ.TRTYPT) STR = 'RETURN    '
          IF (PASSVAL) THEN
              TYPE*,' (6) PASSIVE GAME '//STR//'.............        YES'
          ELSE
              TYPE*,' (6) PASSIVE GAME '//STR//'.............         NO'
          ENDIF
          TYPE*,' (7)      n/a for this transaction type'
          TYPE*,' (8)      n/a for this transaction type'
          TYPE*,' (9)      n/a for this transaction type'
          TYPE*,'(10)      n/a for this transaction type'
          TYPE*,'(11)      n/a for this transaction type'
          TYPE*,'(12)      n/a for this transaction type'
          TYPE*,'(13)      n/a for this transaction type'
          TYPE*,'(14)      n/a for this transaction type'
          TYPE*,'(15)      n/a for this transaction type'
          TYPE*,'(16)      n/a for this transaction type'
          TYPE*,'(17)      n/a for this transaction type'
        ELSE
          TYPE*,' (6)      n/a for this transaction type'
          TYPE*,' (7)      n/a for this transaction type'
          TYPE*,' (8)      n/a for this transaction type'
          TYPE*,' (9)      n/a for this transaction type'
          TYPE*,'(10)      n/a for this transaction type'
          TYPE*,'(11)      n/a for this transaction type'
          TYPE*,'(12)      n/a for this transaction type'
          TYPE*,'(13)      n/a for this transaction type'
          TYPE*,'(14)      n/a for this transaction type'
          TYPE*,'(15)      n/a for this transaction type'
          TYPE*,'(16)      n/a for this transaction type'
          TYPE*,'(17)      n/a for this transaction type'
        ENDIF           !V02
C
C
        TYPE*, ' ---------------------------------------------------'
        OPTION=0
        CALL INPNUM(' CHOOSE OPTION # TO CHANGE OR ''C'' TO CONTINUE: ',
     *              OPTION,0,21,ST)                                       !V05
        IF(ST.EQ.-1) CALL GSTOP(GEXIT_OPABORT)
        IF(ST.EQ.-5) THEN
            TOTNR=MTOTNR(MGNUM)
            type*,'totnum wagers:',totnr
            PERQP=MPERQP(MGNUM)
            DRWMAX=MDRWMAX(MGNUM)
          NREGBRD=MNREGBRD(MGNUM)
          BRDMAX=MBRDMAX(MGNUM)
          CYCLE=MCYCLE(MGNUM)
          SQP=MSQP(MGNUM)
          VSENT=MVSENT(MGNUM)
C  
          ST=0
          CALL OPENW(CFGLU,CFGNAM,4,0,0,ST)
          CALL IOINIT(CFDB,CFGLU,SIMSEC*256)
          CALL WRITEW(CFDB,1,SIMREC,ST)
          IF(ST.NE.0) CALL FILERR(CFGNAM,3,ST,1)
          CALL CLOSEFIL(CFDB)
          RETURN

        ELSEIF(OPTION.EQ.0) THEN
          CALL CLRSCR(5)
          TYPE*
          TYPE*, IAM(), ' Choose message type'
          TYPE*
          TYPE*, IAM(), '     0 = WAGER'
          TYPE*, IAM(), '     1 = VALIDATION'
          TYPE*, IAM(), '     2 = CANCELLATION'
          TYPE*, IAM(), '     3 = SIGNON'
          TYPE*, IAM(), '     4 = SIGNOFF'
          TYPE*, IAM(), '     5 = DOWNLOAD REQUEST         (N/A)'
          TYPE*, IAM(), '     6 = REPORT                   (N/A)' 
          TYPE*, IAM(), '     7 = RESET                    (N/A)'
          TYPE*, IAM(), '     8 = REPRINT                  (N/A)'
          TYPE*, IAM(), '     9 = ERROR MESSAGE            (N/A)'
          TYPE*, IAM(), '    10 = LOOPBACK                 (N/A)'
          TYPE*, IAM(), '    11 = UNSOLICITED MESSAGE      (N/A)'
          TYPE*, IAM(), '    12 = IPS'
          TYPE*, IAM(), '    13 = ORDER/SERVICE REQUEST    (N/A)'
          TYPE*, IAM(), '    14 = FRACTION'
          TYPE*, IAM(), '    15 = PPASSIVE RETURN'
          TYPE*
          CALL INPNUM(' ENTER OPTION', TRATYP ,0,MXTRTYP,ST)
          IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
C
          IF(TRATYP.EQ.TRTYPW) THEN
            TYPE*
            TYPE*, IAM(), ' Choose game type'
            TYPE*
            DO 2 I=1,MAXTYP
              TYPE'(1X,A,4X,I2,A,A)',IAM(),I,' = ',GTNAMES(I)
2           CONTINUE
            TYPE*
            CALL INPNUM(' ENTER OPTION', GTYP ,1,MAXTYP,ST)
            IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
            IF(GTYP.EQ.TTSL.OR.GTYP.EQ.TKIK.OR.GTYP.EQ.TBNG)THEN
               GIND=1
            ELSE
               CALL INPNUM(' ENTER GAME INDEX', GIND ,1,MAXIND,ST)
               IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
            ENDIF
            DO 3 I=1,MAXGAM
              MGNUM=I
              IF(PAR(GMTYP,I).EQ.GTYP.AND.PAR(GMIND,I).EQ.GIND) GOTO 4
3           CONTINUE
            IF(MGNUM.LT.1.OR.MGNUM.GT.MAXGAM) MGNUM=1
4           CONTINUE
            GMAX = PAR(GMMAX,MGNUM)
            GBET = PAR(GMBET,MGNUM)
CRXK??      GDRW = MIN(GDRW,PAR(GMDRW,MGNUM))
CRXK??      GBRD = MIN(GBRD,PAR(GMBRD,MGNUM))
            GDRW = PAR(GMDRW,MGNUM)
            GBRD = PAR(GMBRD,MGNUM)
            GBON = PAR(GMBON,MGNUM)
            GMIN = PAR(GMMIN,MGNUM)
            GJOK = PAR(GMJOK,MGNUM)                                 !V05
            IF(TELEBET) GJOK=0
            IF(GJOK.GT.0) MJOK(MGNUM)=.TRUE.
            IF(TELEBET) MJOK(MGNUM)=.FALSE.
	    IF(GTYP.EQ.TLTO.AND.GIND.GT.2) THEN
	      CALL SETMAN
	    ENDIF
          ELSE IF(TRATYP.EQ.TRTYPR) THEN
            TYPE*
            TYPE*, IAM(), ' Choose report type'
            TYPE*
            TYPE*, IAM(), '     1 = GAME RESULTS'
            TYPE*, IAM(), '     2 = JACKPOT'
            TYPE*, IAM(), '     3 = FINANCIAL'
            TYPE*, IAM(), '     4 = NEWS'
            TYPE*
            CALL INPNUM(' ENTER OPTION', TRASUB ,1,4,ST)
            IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
          ENDIF
          GOTO 1

        ELSEIF(OPTION.EQ.1) THEN
          NODISP=.NOT.NODISP
          GOTO 1

        ELSEIF(OPTION.EQ.2) THEN                                      
          MANCHG=.NOT.MANCHG
          GOTO 1

        ELSEIF(OPTION.EQ.3) THEN
          TYPE*, IAM()
          CALL INPNUM(' Enter first terminal number: ',LOTER,1,NUMAGT,ST)
          IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
          IF(LOTER.GT.HITER) HITER=LOTER
          SIGNED=.FALSE.
          GOTO 1

        ELSEIF(OPTION.EQ.4) THEN
          TYPE*, IAM()
          CALL INPNUM(' Enter last terminal number: ',HITER,
     *                 LOTER,NUMAGT,ST)
          IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
          SIGNED=.FALSE.
          GOTO 1

        ELSEIF(OPTION.EQ.5) THEN
          IF(SIGNED) THEN
            SIGNED=.FALSE.
            SIGON=.TRUE.
          ELSE
            SIGON=.NOT.SIGON
          ENDIF
          GOTO 1
C
        ENDIF
C
          IF(OPTION.EQ.6) THEN
            IF(GTYP.EQ.TTSL.OR.GTYP.EQ.TKIK.OR.GTYP.EQ.TBNG)THEN
               GIND=1
            ELSE
               TYPE*, IAM()
               CALL INPNUM(' Enter game index: ',GIND,1,MAXIND,ST)
               IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
            ENDIF
C
C CHECK FOR MANUAL BETS
C
	    IF(GTYP.EQ.TLTO.AND.GIND.GT.2) THEN
              CALL SETMAN
	    ENDIF
            MGNUM=GTNTAB(GTYP,GIND)
            GMAX = PAR(GMMAX,MGNUM)
            GBET = PAR(GMBET,MGNUM)
            GBON = PAR(GMBON,MGNUM)
            GBRD = PAR(GMBRD,MGNUM)
            GDRW = PAR(GMDRW,MGNUM)
            GMIN = PAR(GMMIN,MGNUM)
            GJOK = PAR(GMJOK,MGNUM)     
            IF(TELEBET) GJOK=0
            IF(GJOK.GT.0) MJOK(MGNUM)=.TRUE.
            IF(TELEBET) MJOK(MGNUM)=.FALSE.
	    MTOTNR(MGNUM)=MANBET
            GOTO 1
          ELSEIF(OPTION.EQ.7) THEN
            TYPE*, IAM()
            TYPE *,IAM(), ' Enter maximum number of draws on a wager'
            CALL INPNUM(' Your choice: ', MDRWMAX(MGNUM) ,1,GDRW,ST)
            IF(ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
            GOTO 1
          ELSEIF(OPTION.EQ.8) THEN
            TYPE*, IAM()
            TYPE *,IAM(), ' Enter maximum number of boards on a wager'
            CALL INPNUM(' Your choice: ', MBRDMAX(MGNUM) ,GMIN,GBRD,ST)
            IF(ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
            GOTO 1
          ELSEIF(OPTION.EQ.9) THEN
            TYPE*, IAM()
            TYPE *,IAM(), ' Enter total number of wagers to generate'
            CALL INPNUM(' Your choice (0 = no limit): ', 
     *                  MTOTNR(MGNUM), 0,1000000,ST)
            IF(ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
	    IF(MANBET.NE.0.AND.MTOTNR(MGNUM).NE.MANBET) THEN
	      TYPE*,IAM()
	      TYPE*,IAM(),'CANNOT DIFFER FROM NUMBER OF MANUAL BETS',MANBET
	      MTOTNR(MGNUM)=MANBET
	    ENDIF
            GOTO 1
          ELSEIF(OPTION.EQ.10) THEN                                    
            MVSENT(MGNUM)=.NOT.MVSENT(MGNUM)
            GOTO 1                                                
          ELSEIF(OPTION.EQ.11) THEN
            TYPE*, IAM()
            TYPE *,IAM(), ' Enter percentage of Quick-Picks among',
     *             ' all Wagers'
            CALL INPNUM(' Your choice: ', MPERQP(MGNUM) ,0,100,ST)
            IF(ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
            GOTO 1
          ELSEIF(OPTION.EQ.12) THEN
C    Special QP's:
            IF(MPERQP(MGNUM).GT.0) THEN                                        
              IF(MSQP(MGNUM).EQ.0) THEN
                MSQP(MGNUM)=1
                SQPTXT='SPECIAL'
              ELSE IF(MSQP(MGNUM).EQ.1) THEN
                MSQP(MGNUM)=2     
                SQPTXT='MIXTURE'
              ELSE IF(MSQP(MGNUM).EQ.2) THEN
                MSQP(MGNUM)=0
                SQPTXT=' NORMAL'
              ENDIF
            ENDIF                                                        
            GOTO 1
          ELSEIF(OPTION.EQ.13.AND.MPERQP(MGNUM).LT.100) THEN
            TYPE*, IAM()
            TYPE *,IAM(), ' Enter number of different',
     *                    ' combinations'
            CALL INPNUM(' Your choice: ', MNREGBRD(MGNUM) ,0,1000,ST)
            IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
C                                                                        
            TYPE*
            CALL PRMYESNO(' Cyclic repetition? [Y/N]', ST)
            IF(ST.EQ.1) THEN
              MCYCLE(MGNUM)=.TRUE.
              CYC='  (c)'
            ENDIF
            IF(ST.EQ.2) THEN
              MCYCLE(MGNUM)=.FALSE.
              CYC='  (r)'
            ENDIF
C                                                                    
            GOTO 1
C
        ENDIF
C
C
        IF(OPTION.EQ.14) THEN
          TYPE*, IAM()
          TYPE *,IAM(), 
     *           ' Enter delay between transactions (miliseconds)'
          CALL INPNUM(' Your choice (0=no delay): ', DELY ,0,200,ST)
          IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
          GOTO 1
        ELSEIF(OPTION.EQ.15) THEN
          RETRY=.NOT.RETRY
          GOTO 1
        ELSEIF(OPTION.EQ.16) THEN
          INCAN=.NOT.INCAN
          GOTO 1
        ELSEIF(OPTION.EQ.17) THEN
            TYPE*, IAM()
            TYPE*,'               0 - no joker'
            TYPE*,'               1 - first joker rightt'
            TYPE*,'               2 - first joker left'
            TYPE*,'               3 - first joker both dir'
            CALL INPNUM(' Your choice: ', GJOK ,0,3,ST)
            IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
            TYPE*
CRXK      MJOK(MGNUM) = .NOT.MJOK(MGNUM) .AND. GJOK
            MJOK(MGNUM)=.TRUE. 
            IF(TELEBET) GJOK=0
            IF(GJOK.EQ.0) MJOK(MGNUM)=.FALSE.
            IF(TELEBET) MJOK(MGNUM)=.FALSE.
          GOTO 1
        ELSEIF(OPTION.EQ.18) THEN
          SYSBET=.NOT.SYSBET
          IF(SYSBET) THEN
             IF(GTYP.EQ.TLTO) THEN
                CALL INPNUM(' Enter system # : ',
     *                        SYSBNUM ,8,18,ST)
                IF(SYSBNUM.EQ.17) THEN
                   TYPE*, IAM(),'  Invalid system number'
                   CALL XWAIT(2,2,ST)
                   SYSBNUM=0
                   SYSBET=.FALSE.
                   GOTO 1
                ENDIF
             ELSE IF(GTYP.EQ.TSPT) THEN
                CALL INPNUM(' Enter system # : ',
     *                        SYSBNUM ,1,9999,ST)
             ELSE IF(GTYP.EQ.TTGL) THEN
                CALL INPNUM(' Enter system # : ',
     *                        SYSBNUM ,1,9999,ST)
             ELSE IF(GTYP.EQ.TTSL) THEN
                CALL INPNUM(' Enter system # : ',
     *                        SYSBNUM ,4,20,ST)
             ENDIF
          ELSE
             SYSBNUM=0
          ENDIF
          GOTO 1
        ELSEIF(OPTION.EQ.19) THEN
          BANK=.NOT.BANK
          IF(BANK) THEN
             CALL INPNUM(' Enter bank id (6 digits): ',
     *                     BANKID ,0,999999,ST)
             IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
             CALL INPNUM(' Enter bank account (8 digits): ',
     *                     BANKACC ,0,99999999,ST)
             IF (ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
          ENDIF
          GOTO 1
        ELSEIF(OPTION.EQ.20) THEN
          TELEBET=.NOT.TELEBET
          IF(TELEBET) GJOK=0
          IF(TELEBET) MJOK(MGNUM)=.FALSE.
          IF(TELEBET.AND..NOT.BANK) THEN 
            TYPE*, IAM(),'  Bank requisites missing'
            CALL XWAIT(2,2,ST)
          ENDIF       
          IF(TELEBET.AND.LOTER.LE.4000 .OR.
     *       .NOT.TELEBET.AND.LOTER.GT.4000) THEN
             TYPE*, IAM(),'  Terminal range incorrect'
             CALL XWAIT(2,2,ST)
          ENDIF
          GOTO 1
        ELSEIF(OPTION .EQ. 21) THEN
          SERIALEM = .NOT. SERIALEM
          IF(SERIALEM) THEN
             CALL INPNUM(' Enter External serial number (8 digits): ',
     *                     SERIALEM_NR ,0,99999999,ST)
             IF (ST .NE. 0) CALL GSTOP(GEXIT_OPABORT)
             CALL INPNUM(' Enter check digits (3 digits): ',
     *                     SERIALEM_CC ,0,999,ST)
             IF (ST .NE. 0) CALL GSTOP(GEXIT_OPABORT)
          ENDIF
          GOTO 1
        ELSE
          GOTO 1
        ENDIF
        END
C
C
C
C SUBROUTINES TO GENERATE/MANIPULATE SEQUENCES, PERMUTATIONS, COMBINATIONS
C   INTERVAL CODES, OFFSETS ETC.
C
       SUBROUTINE RANCMB(CMB,N,M,SEED)
       INTEGER*4 N,M,CMB(N),SEED
       INTEGER*4 SEQ(64)                     ! 64 for KENO is o.k.
       INTEGER*4 I,IND
C
       DO 5 I=1,M
5      SEQ(I)=I
C
       IF(N.GT.M) THEN
         TYPE*, 'ERROR IN RANCMB !!!'
         RETURN
       ENDIF
       DO 10 I=1,N
         IND=INT(RAN(SEED)*(M-I+1))+1
         CMB(I)=SEQ(IND)
         SEQ(IND)=SEQ(M-I+1)
10     CONTINUE
       RETURN
       END
C
C
       SUBROUTINE BUBSORT(SEQ,N)
       INTEGER*4 N,SEQ(N)
       LOGICAL CHANGED
       INTEGER*4 I,TEMP
C
1      CHANGED=.FALSE.
       DO 10 I=1,N-1
         IF(SEQ(I).GT.SEQ(I+1)) THEN
           TEMP=SEQ(I)
           SEQ(I)=SEQ(I+1)
           SEQ(I+1)=TEMP
           CHANGED=.TRUE.
         ENDIF
10     CONTINUE       
       IF (CHANGED) GOTO 1
       RETURN
       END
C
C
       SUBROUTINE INT2CMB(INTVLS, CMB, N)
       INTEGER*4 N,INTVLS(N),CMB(N)
       INTEGER*4 I,NUM
C
       NUM=0
       DO 10 I=1,N
         NUM=NUM+INTVLS(I)
         CMB(I)=NUM
10     CONTINUE
       RETURN
       END
C
C
       SUBROUTINE RANPER(SEQ,N,SEED)
       INTEGER*4 N,SEQ(N),SEED
       INTEGER*4 I,IND,TMP
C
       DO 10 I=N,2,-1
         IND=INT(RAN(SEED)*I)+1
         TMP=SEQ(I)
         SEQ(I)=SEQ(IND)
         SEQ(IND)=TMP
10     CONTINUE
       RETURN
       END
C
C
       SUBROUTINE CMB2INT(CMB,INTVLS,N)
       INTEGER*4 N,CMB(N),INTVLS(N)
       INTEGER*4 I
C
       INTVLS(1)=CMB(1)
       DO 10 I=2,N
         INTVLS(I)=CMB(I)-CMB(I-1)
10     CONTINUE
       RETURN
       END
C
C
        INTEGER*4 FUNCTION NOENDDUR(SIGARGS, MECHARGS)
        IMPLICIT NONE
C
        INCLUDE     'INCLIB:SYSPARAM.DEF'
        INCLUDE     '(LIB$ROUTINES)'
        INCLUDE     '($SSDEF)'
        INCLUDE     '($FORDEF)'
C
        INTEGER*4   SIGARGS(*)
        INTEGER*4   MECHARGS(*)
C
        INTEGER*4   INDEX
C
C
C See if condition passed is a Fortran END-OF-FILE DURING READ error.
C If so, simply continue.  If not, resignal the error to a higher level.
C
        INDEX = LIB$MATCH_COND (SIGARGS(2), FOR$_ENDDURREA)
C
        IF(INDEX.EQ.0)THEN
          NOENDDUR = SS$_RESIGNAL
        ELSE
C
C    use the following CALL if CTRL-Z is to return to the module where
C    the condition handler is established
          CALL SYS$UNWIND(MECHARGS(3),)   
C
C    this CALL results in EXIT from the program (no stack dump!!)
C
        ENDIF
C
        END
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE SETCHKSUM(OUTTAB,OUTLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
C
        BYTE      OUTTAB(*)
        INTEGER*4 OUTLEN
C
        INTEGER*4 MYCHKSUM, CHKLEN
C
C
        IF(TELEBET) THEN
           OUTTAB(3) = 0
           OUTTAB(4) = 0
           RETURN
        ENDIF
C
        BASECHKSUM = IAND(CDC,'FFFF'X)
        I4CCITT   = IAND(BASECHKSUM+TER,'FFFF'X)
        OUTTAB(3) = I1CCITT(2)
        OUTTAB(4) = I1CCITT(1)
        CHKLEN=OUTLEN-1
        CALL GETCCITT(OUTTAB,1,CHKLEN,MYCHKSUM)
        I4CCITT = MYCHKSUM
        OUTTAB(3) = I1CCITT(2)
        OUTTAB(4) = I1CCITT(1)
C
        RETURN
        END
C
C
        SUBROUTINE PERTUR(EXT)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
C
        INTEGER*4 EXT
C
        INTEGER*4 I, BYT2CHG, ST, DISP
        BYTE      BTMP
C
        EXT=0
        BYT2CHG=0
CRXK    CALL CLRSCR(5)
        TYPE*
        TYPE*
        TYPE*
C
          IF(TRATYP.EQ.TRTYPC) THEN
            TYPE*, 'GENERATED MESSAGE (Cancellation):'
          ELSE IF(TRATYP.EQ.TRTYPV) THEN
            TYPE*, 'GENERATED MESSAGE (Validation):'
          ELSE IF(TRATYP.EQ.TRTYPO) THEN
            TYPE*, 'GENERATED MESSAGE (Supply/service):'
          ELSE IF(TRATYP.EQ.TRTYPW) THEN
            TYPE*, 'GENERATED MESSAGE (Wager):'
          ELSE IF(TRATYP.EQ.TRTYPG) THEN
            TYPE*, 'GENERATED MESSAGE (Fraction):'
          ELSE IF(TRATYP.EQ.TRTYPI) THEN
            TYPE*, 'GENERATED MESSAGE (IPS):'
          ENDIF

449     CONTINUE
          TYPE*
          IF(TRATYP.EQ.TRTYPW) THEN
            DISP=MESLEN
            IF(MESLEN.GT.36) DISP=36
            TYPE '('' BYTE #:  '', 13I3)', (I,I=1,13)
            TYPE 950, (MESBUF1(I),I=1,13)
            TYPE*
            TYPE '('' BYTE #:  '', <DISP-13>I3)', (I,I=14,DISP)
            TYPE 955, (MESBUF1(I),I=14,DISP)
          ELSE IF(TRATYP.EQ.TRTYPV) THEN
            DISP=MESLEN
            TYPE '('' BYTE #:  '', 13I3)', (I,I=1,13)
            TYPE 950, (MESBUF1(I),I=1,13)
            TYPE*
          ELSE IF(TRATYP.EQ.TRTYPI) THEN          
            DISP=19
            TYPE '('' BYTE #:  '', 19I3)', (I,I=1,19)
            TYPE 950, (MESBUF1(I),I=1,19)
            TYPE*
            DISP=MESLEN-19
            IF(DISP.GT.0) TYPE '('' BYTE #:  '', <DISP>I3)',(I,I=1,DISP)
            IF(DISP.GT.0) TYPE 950, (MESBUF1(I),I=20,DISP)
            TYPE*
          ELSE
            TYPE '('' BYTE #:  '', 13I3)', (I,I=1,DISP)
            TYPE 950, (MESBUF1(I),I=1,DISP)
          ENDIF
950     FORMAT('          ',13Z3.2)
955     FORMAT('          ',<DISP-13>Z3.2)
          TYPE*
        CALL INPNUM('CHOOSE BYTE # TO CHANGE OR ''C'' TO CONTINUE ',
     *               BYT2CHG,1,MESLEN,ST)
        IF(ST.GE.0) THEN
            TYPE '(19X,A,$)', 'NEW VALUE (Z2) >'
            READ(5,'(Z2)') BTMP
            MESBUF1(BYT2CHG)=BTMP
            TYPE*
CRXK        CALL CLRSCR(5)
            TYPE*,'MODIFIED MESSAGE:'
            GOTO 449
        ENDIF
        IF(ST.LT.-1) THEN
          IF(BYT2CHG.NE.0) THEN
              TYPE*
              CALL PRMYESNO('Update checksum [Y/N] ', ST)
              IF(ST.EQ.1) CALL SETCHKSUM(MESBUF1,MESLEN)
          ENDIF
        ELSE
          EXT=-1
        ENDIF
        RETURN
        END     
C
C
        SUBROUTINE SETMAN
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
C
	INTEGER*4 ST,I,J,K
C
	CHARACTER*33 STRING901
	CHARACTER*26 STRING902
	CHARACTER*30 STRING903
C
       ! manual betting data
	COMMON /MANUAL/ MANTAB,LUCKY,MANBET,SYS_SIZ,NUMPAN
	INTEGER*4 MANTAB(11,0:10,10)   ! (#nums,#panels,#wagers)
	INTEGER*4 LUCKY(10)            ! (#wagers)
	INTEGER*4 MANBET, SYS_SIZ, NUMPAN
C
C
	CALL FASTSET(0,MANTAB,11*11*10)
	CALL FASTSET(0,LUCKY,10)
	IF(GTYP.EQ.TLTO.AND.GIND.GT.2) THEN
          TYPE*, IAM()
          CALL INPNUM(' Enter number of manual bets [0=none]: ',
     *                  MANBET,0,10,ST)
          IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
	ELSE
	  TYPE*,IAM()
	  TYPE*,IAM(),' MANUAL BETTING NOT AVAILABLE FOR THIS GAME'
	  TYPE*,IAM()
	  MANBET = 0
	  RETURN
	ENDIF
	IF(MANBET.NE.0) THEN
	  MTOTNR(MGNUM)=MANBET
          TYPE*,IAM()
          TYPE*,IAM(),'* PLEASE ENTER IF SYSTEM BET THEN '
          TYPE*,IAM(),'* ENTER NUMBER OF PANELS IN BET THEN '
          TYPE*,IAM(),'* ENTER NUMBERS FOR EACH PANEL IN '
          TYPE*,IAM(),'* ASCENDING ORDER, AND THEN ENTER '
          TYPE*,IAM(),'* LUCKY NUMBER '
          TYPE*,IAM()
 	  DO I=1,MANBET
900	    CONTINUE
	    CALL INPNUM('Enter system [0=none]:',SYS_SIZ,0,11,ST)
	    IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
	    IF(SYS_SIZ.GT.0.AND.SYS_SIZ.LT.4) THEN
	      TYPE*,IAM(),'INVALID SYSTEM ENTERED:',SYS_SIZ
	      GOTO 900
	    ENDIF
	    IF(SYS_SIZ.EQ.0.OR.SYS_SIZ.EQ.5) THEN
	      SYS_SIZ=5
	      WRITE(STRING901,901) I
901           FORMAT('For Bet ',I2,' enter number of panels') 
              CALL INPNUM(STRING901,NUMPAN,1,10,ST)
	      IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
	    ELSE
	      NUMPAN=1
	      SYSBET=.TRUE.
	    ENDIF
            MANTAB(1,0,I)=NUMPAN
            DO J=1,NUMPAN
	      DO K=1,SYS_SIZ
	        WRITE(STRING902,902) I,J,K
902             FORMAT('For Bet ',I2,' panel ',I2.2,' num',I2.2,':')
                CALL INPNUM(STRING902,MANTAB(K,J,I),1,49,ST)
	        IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
	        IF(K.GT.1) THEN
	          IF(MANTAB(K,J,I).LE.MANTAB(K-1,J,I)) THEN
	            TYPE*,IAM(),'number NOT entered in ascending order'
	            TYPE*,IAM(),'Restart entering data for bet',I
	            GOTO 900
	          ENDIF
	        ENDIF
              ENDDO
              WRITE(*,9001) J,(MANTAB(K,J,I),K=1,SYS_SIZ)
9001          FORMAT(1X,'Panel ',I2.2,': ',<SYS_SIZ>(I2.2,1X))
            ENDDO
	    WRITE(STRING903,903) I
903         FORMAT('For Bet ',I2,' enter Lucky Number:')
            CALL INPNUM(STRING903,LUCKY(I),1,13,ST)
	    IF(ST.LT.0) CALL GSTOP(GEXIT_OPABORT)
 	  ENDDO
	ENDIF
C	    
	RETURN
C
	END
