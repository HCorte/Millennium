C PROGRAM WAGPRO
C
C V28 09-DEZ-2020 SCML New Terminals Project - Olimpo
C V27 17-FEB-2010 FJG Fix error in TKIK retries
C V26 21-JUN-2002 JHR Checksum & sequence # update for cancellations moved here.
C V25 26-MAR-2001 JHR SUPPRESSION OF PRIVILEGE TERMINAL WAGERING
C V24 25-JAN-2001 UXN All cancellations are now 2 phase transactions. 
C V23 02-DEC-2000 UXN TOTOGOLO ADDED.
C V22 04-OCT-2000 UXN GLOBAL RFSS #91
C V21 26-JUN-2000 OXK BTEST -> TSBIT
C V20 13-OCT-1999 RXK World Tour added.
C V19 29-MAR-1999 UXN&WS FIX FOR FAILURE TO READ FROM TMF IN CASE
C		         IF ORIGINAL WAGER RECORD NOT WRITTEN YET BY APULOG
C V18 18-MAR-1999 RXK  Game type/game index change.
C V17 29-JAN-1999 UXN  Fractions and unfractions added.
C V16 21-MAY-1996 HXK  Updates from Mike Pindrik for TEBE 
C V15 02-SEP-1994 HXK  Merge of May,June RFSS batch 
C V14 21-MAY-1994 HXK  CHANGE CHECK FOR 2 BYTE CHECKSUM; CHECK FOR LSTSER.EQ.0 
C V13 17-SEP-1993 GXA  Added Kicker Retry Logic.
C V12 14-SEP-1993 GXA  Initialize TCDC_SOLD to DAYCDC 
C V11 03-SEP-1993 HXK  Remove suppression of privilege ter wagering
C V10 06-AUG-1993 HXK  CHANGED SPGRMX TO MAXSPT
C V09 16-JUL-1993 GXA  Added initialization of TSUBERR.
C V08 17-MAY-1993 HXK  Cleaned up DKICK call.
C V07 17-MAY-1993 HXK  REMOVED SPEDEN GAME (Speden is now handled by SPEDPRO)
C V06 11-MAY-1993 HXK  ADDED SPEDEN GAME
C V05 21-MAY-1992 HDB  ADDED UPDSTA CALL FOR STATISTICS
C V04 13-APR-1992 GCAN CHECK KICKER (Y/N) FLAG, BEFORE SUPPRESSION DECICION.
C V03 22-JAN-1992 GCAN ALOW FOR KICKER BETS TO GO THROUGH.
C V02 07-OCT-1991 MTK  INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C LOTTO/SPORTS/KICKER WAGER PROCESSING TASK
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM WAGPRO
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:TGLCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
C
        ! variables
        INTEGER*4  MESS(EDLEN)       !
        INTEGER*4  LOGREC(LREC*3)    !
        INTEGER*4  WRKBUF(TRALEN)    !
        INTEGER*4  ERCODE            !
        INTEGER*4  MAX               !
        INTEGER*4  I                 !
        INTEGER*4  ST                !
        INTEGER*4  LSTSER            !
        INTEGER*4  ENCMES            !
        INTEGER*4  ENCACT            !
        INTEGER*4  KGAM              !
        INTEGER*4  GAME              !
        INTEGER*4  AGAME             !
        INTEGER*4  GTYP              !
        INTEGER*4  TYPE              !
        INTEGER*4  TER               !
        INTEGER*4  BUF               !
        INTEGER*4  STATUS            !
        INTEGER*4  TASK              !
	INTEGER*4  KIND
C
        INTEGER*4  SIMOUTTAB(40)     !

        INTEGER*2  SIMOUTLEN         !

        INTEGER*4  I4TEMP               ! rev 1.10
        INTEGER*2  I2TEMP(2)            ! rev 1.10
        EQUIVALENCE (I4TEMP,I2TEMP(1))  ! rev 1.10
C
        INTEGER*4   NOFTLSIG
        EXTERNAL    NOFTLSIG
C
        ! start of code

        CALL COPYRITE
        CALL LIB$ESTABLISH(NOFTLSIG)        !No fatal errors
        CALL SNIF_AND_WRKSET
C
        TASK    = WAG
        MESS(1) = TASK
5       CONTINUE
C        CALL OPS('WAGPRO DAYCDC',DAYCDC,DAYCDC)
        BASECHKSUM=IAND(DAYCDC,'FFFF'X)
C        CALL OPS('WAGPRO BASECHKSUM',BASECHKSUM,BASECHKSUM)
C
C WAIT FOR SOMETHING TO DO
C IF END OF DAY THEN CALL GSTOP(GEXIT_SUCCESS)
C
10      CONTINUE
        IF(DAYSTS.EQ.DSCLOS) CALL GSTOP(GEXIT_SUCCESS)
        IF(DAYSTS.EQ.DSSUSP) THEN
            CALL HOLD(0,STATUS)
            IF(DAYSTS.EQ.DSOPEN) GOTO 5
            GOTO 10
        ENDIF
        CALL HOLD(0,STATUS)
C
C GET BUFFER NUMBER FROM TOP OF QUEUE.
C IF NO WAGERS QUEUED, GO BACK TO WAIT STATE.
C
20      CONTINUE
        CALL TOPQUE(TASK,BUF)
        IF(BUF.EQ.0) GOTO 10
C
C DECODE TERMINAL MESSAGE INTO INTERNAL TRANSACTION FORMAT
C
        TER  = HPRO(TERNUM,BUF)
        TYPE = HPRO(TRCODE,BUF)
        IF(TYPE.EQ.TYPWCN .OR. TYPE.EQ.TYPWDL) GOTO 1000 !LOTTO/SPORTS
C                                                        !CANCELS AND DELETES
	IF(TYPE.EQ.TYPFRA) THEN				 ! FRACTIONS
	    CALL FRACTWO(TRABUF,BUF,TASK)
	    CALL QUETRA(LOG,BUF)
            CALL DQUTRA(TASK,BUF)
            GOTO 20
	ENDIF
C
	IF(TYPE.EQ.TYPUNF) THEN			        ! UNFRACTIONS
	    CALL UNFRACTWO(TRABUF,BUF,TASK)
	    CALL QUETRA(LOG,BUF)
            CALL DQUTRA(TASK,BUF)
            GOTO 20
	ENDIF
C
        CALL FASTSET(0,TRABUF,TRALEN)
        TRABUF(TSTAT) = GOOD
        TRABUF(TERR)  = NOER
        TRABUF(TSUBERR) = NOER
        TRABUF(TTYP)  = TWAG
        TRABUF(TCDC)  = DAYCDC
        TRABUF(TCDC_SOLD) = DAYCDC
        TRABUF(TTER)  = TER
        TRABUF(TSER)  = PRO(SERIAL,BUF)
        TRABUF(TTIM)  = PRO(TSTAMP,BUF)
        TRABUF(TSIZE) = HPRO(NUMLRC,BUF)
        TRABUF(TAGT)  = AGTTAB(AGTNUM,TER)
C
C----+------------------------------------------------------------------
C V28| New Terminals Project - Olimpo
C BPRO(CHOLM_OLM) equal to 1 means that comes from the communication channel Olimpo
C other wise comes from X2X or MXS
C----+------------------------------------------------------------------
C       begin - Olimpo Serial Number & Message Id & Communication Flag
        IF(BPRO(CHOLM_OLM,BUF) .EQ. 1) THEN 
            TRABUF(TWCOLMSERL_TLTO)=PRO(SEROLM_INT_OLM,BUF)
            TRABUF(TWCOLMSERM_TLTO)=PRO(SEROLM_INT_OLM+1,BUF)
            TRABUF(TWCOLMSERH_TLTO)=BPRO(SEROLM_OLM+8,BUF)
            TRABUF(TWCOLMMIDL_TLTO)=PRO(MESSID_INT_OLM,BUF)
            TRABUF(TWCOLMMIDH_TLTO)=BPRO(MESSID_OLM+4,BUF)
            TRABUF(TWCOLMCOMF_TLTO)=BPRO(CHOLM_OLM,BUF)
    
            TRABUF(TVOLMSERL_TLTO)=PRO(SEROLM_INT_OLM,BUF)
            TRABUF(TVOLMSERM_TLTO)=PRO(SEROLM_INT_OLM+1,BUF)
            TRABUF(TVOLMSERH_TLTO)=BPRO(SEROLM_OLM+8,BUF)
            TRABUF(TVOLMMIDL_TLTO)=PRO(MESSID_INT_OLM,BUF)
            TRABUF(TVOLMMIDH_TLTO)=BPRO(MESSID_OLM+4,BUF)
            TRABUF(TVOLMCOMF_TLTO)=BPRO(CHOLM_OLM,BUF)
        ENDIF
C       end - Olimpo Serial Number & Message Id & Communication Flag
C----+------------------------------------------------------------------
C V28| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
        GTYP = BPRO(BINPTAB+5,BUF)
        IF(GTYP.EQ.TLTO) THEN
            CALL DLOTTO(PRO(INPTAB,BUF),TRABUF,HPRO(INPLEN,BUF))
        ELSE IF(GTYP.EQ.TSPT) THEN
            CALL DSPORT(PRO(INPTAB,BUF),TRABUF,HPRO(INPLEN,BUF))
        ELSE IF(GTYP.EQ.TTGL) THEN
            CALL DTGOLO(PRO(INPTAB,BUF),TRABUF,HPRO(INPLEN,BUF))
        ELSE
            CALL DKICK(PRO(INPTAB,BUF),TRABUF,HPRO(INPLEN,BUF))
        ENDIF
C
C CHECK IF TRANSACTION TYPE IS SUPRESSED ON
C AGENT AND GLOBAL LEVELS
C
        AGAME = 0
        GAME  = TRABUF(TGAM)
        IF(GAME.NE.0) AGAME=AGTGAM(GFLAGS,GAME,TER)
        IF(TSBIT(AGTTAB(AGTTYP,TER),AGTWAG))   TRABUF(TERR)=SUPR
        IF(TSBIT(AGAME,AGTWAG))                TRABUF(TERR)=SUPR
        IF(TSBIT(P(SUPGWA),GAME))              TRABUF(TERR)=SUPR
        IF(P(SUPWAG).NE.0)                     TRABUF(TERR)=SUPR
C
C TERMINALS CAN NOT WAGER IF GAME IS NOT ACTIVE FOR TODAY
C
        IF(DAYDRW(GAME) .LE. 0) TRABUF(TERR) = BSTS 
C
C PRIVILEGED TERMINALS CAN NOT WAGER
C
        IF(TSBIT(AGTTAB(AGTTYP, TER), AGTPRV)) TRABUF(TERR) = SUPR
C
C CHECK AGENT AND SYSTEM STATUS
C
        IF(AGTHTB(AOPSTS,TER).NE.SIGNON)       TRABUF(TERR)=NOTON
        IF(P(SYSSTS).EQ.SYSDRW)                TRABUF(TERR)=SDRW
        IF(P(SYSSTS).EQ.SYSDOR)                TRABUF(TERR)=SDOR
C
C IF BET WITH KICKER THEN CHECK IF SUPRESSED
C (IF KICKER ONLY THEN CHECKED ALREADY)
C
        IF(TRABUF(TGAMTYP).NE.TKIK) THEN
           KGAM=TRABUF(TWKGME)
           IF(KGAM.NE.0) THEN
              IF(DAYDRW(KGAM).LE.0) TRABUF(TERR) = BSTS
           ENDIF
           IF(TRABUF(TWKFLG).EQ.0 .AND. TRABUF(TWKFLG2).EQ.0) KGAM = 0
           IF(KGAM.NE.0) THEN
              AGAME=AGTGAM(GFLAGS,KGAM,TER)
              IF(TSBIT(AGAME,AGTWAG))              TRABUF(TERR)=SUPR
              IF(TSBIT(P(SUPGWA),KGAM))            TRABUF(TERR)=SUPR
           ENDIF
        ENDIF
C
C CHECK FOR DES ERROR
C
        IF(P(DESACT).EQ.0) THEN
            ENCMES = BPRO(BINPTAB,BUF)
            ENCMES = IAND(ENCMES,'08'X)
            IF(P(DESFLG).EQ.0.AND.
     *         TSBIT(AGTTAB(AGTTYP,TER),AGTDES)) THEN
               ENCACT = '08'X
            ELSE
               ENCACT = 0
            ENDIF
            IF(ENCMES.NE.ENCACT) TRABUF(TERR)=DESMOD
        ENDIF
        IF(TRABUF(TERR).NE.NOER) TRABUF(TSTAT)=REJT
C
C IF TERMINAL RETRY, AND TRANSACTION STATUS IS GOOD, AND
C TRANSACTION SEQUENCE NUMBER MATCHES THE LAST SEQUENCE
C NUMBER FOR THIS TERMINAL, CONTINUE RETRY PROCESSING, ELSE
C PROCESS AS NORMAL.
C

        LSTSER = AGTTAB(ALSTRA,TER)          ! rev 1.10
        IF(LSTSER.EQ.0) GOTO 80              ! rev 1.10

        IF(HPRO(SIMMOD,BUF).EQ.-999) GOTO 80    !NO RETRIES FOR SIM
C
        IF(TRABUF(TTRN).EQ.AGTHTB(ATRNUM,TER)) THEN

            IF(TRABUF(TCHK).NE.ZEXT(AGTHTB(ACHKSM,TER))) THEN  

                LSTSER = AGTTAB(ALSTRA,TER)
                CALL RLOG(LSTSER,LOGREC,TASK,ST)
CV18
		IF (ST.NE.0) THEN
		    CALL WAIT_APUQUE
      		    CALL RLOG(LSTSER,LOGREC,TASK,ST)
		ENDIF
CEV18
                IF(ST.NE.0) GOTO 80
                CALL LOGTRA(WRKBUF,LOGREC)
                IF(WRKBUF(TTYP).NE.TWAG)             GOTO 80
		IF(WRKBUF(TCHK).NE.TRABUF(TCHK))     GOTO 80
		IF(WRKBUF(TTYP).NE.TRABUF(TTYP))     GOTO 80
                IF(WRKBUF(TTRN).NE.TRABUF(TTRN))     GOTO 80
                IF(WRKBUF(TGAM).NE.TRABUF(TGAM))     GOTO 80
C
                IF(TRABUF(TGAMTYP).NE.TKIK) THEN
                   DO I=TWBORD,TWBEND
                      IF(WRKBUF(I).NE.TRABUF(I))       GOTO 80
                   ENDDO
                ENDIF  
		IF(TRABUF(TWBEG).GT.WRKBUF(TWBEG)) THEN
                   IF(TRABUF(TTIM)-WRKBUF(TTIM).GT.60) GOTO 80
	           IF(WRKBUF(TGAMTYP).EQ.TLTO) THEN
                      IF(LTOSTS(WRKBUF(TGAMIND)).GT.GAMBFD) GOTO 80
                   ENDIF
	           IF(WRKBUF(TGAMTYP).EQ.TSPT) THEN
                      IF(SPTSTS(WRKBUF(TGAMIND)).GT.GAMBFD) GOTO 80
                   ENDIF
                   IF(WRKBUF(TGAMTYP).EQ.TTGL) THEN
                      IF(TGLSTS(WRKBUF(TGAMIND)).GT.GAMBFD) GOTO 80
                   ENDIF
	           IF(WRKBUF(TGAMTYP).EQ.TKIK) THEN
                      IF(KIKSTS(WRKBUF(TGAMIND)).GT.GAMBFD) GOTO 80
                   ENDIF
	           TRABUF(TWBEG) = WRKBUF(TWBEG)
	           TRABUF(TWEND) = WRKBUF(TWEND)
                ENDIF 
                IF(KGAM.NE.0.AND.TRABUF(TWKBEG).GT.WRKBUF(TWKBEG)) THEN
                   IF(TRABUF(TTIM)-WRKBUF(TTIM).GT.60) GOTO 80
                   KIND = GNTTAB(GAMIDX,KGAM)
                   IF(KIKSTS(KIND).GT.GAMBFD) GOTO 80
                   TRABUF(TWKBEG) = WRKBUF(TWKBEG)
                   TRABUF(TWKEND) = WRKBUF(TWKEND)
                ENDIF

                IF(TRABUF(TWBEG).NE.WRKBUF(TWBEG)) GOTO 80 
                IF(TRABUF(TWEND).NE.WRKBUF(TWEND)) GOTO 80 
            ENDIF

            TRABUF(TSTAT) = REJT
            TRABUF(TERR ) = RETY
C=V27===========================================================================            
            TRABUF(TWKICK)  = AGTTAB(AGTLKN,TER)
            TRABUF(TWKICK2) = AGTTAB(AGTLKN2,TER)  
C=V27===========================================================================                        
            CALL TRALOG(TRABUF,PRO(WRKTAB,BUF))
            TRABUF(TSER)    = AGTTAB(ALSTRA,TER)
C=V27===========================================================================            
C           IF(TRABUF(TGAMTYP).NE.TKIK) THEN
C              TRABUF(TWKICK)  = AGTTAB(AGTLKN,TER)
C              TRABUF(TWKICK2) = AGTTAB(AGTLKN2,TER)
C           ENDIF 
C=V27===========================================================================            
            TRABUF(TSTAT) = GOOD
            TRABUF(TERR)  = NOER
            CALL OUTWAG(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
            GOTO 90
        ENDIF
C
C UPDATE POOLS AND CHECK INTERVAL CODE
C
80      CONTINUE
        IF(TRABUF(TSTAT).EQ.GOOD) THEN
            IF(TRABUF(TGAMTYP).EQ.TLTO) THEN
                CALL UPDLPL(TRABUF,0,ST)
                IF(ST.GT.0) THEN
                    MAX=LTOMAX(TRABUF(TGAMIND))
                    CALL SCHKINT(TRABUF(TWNBET),TRABUF(TWNMRK),MAX,
     *                           TRABUF(TWBORD),ST)
                ENDIF
                IF(ST.NE.0) THEN
                    SYNTERRCOD=200
                    TRABUF(TSTAT) = REJT
                    TRABUF(TERR)  = SYNT
                ENDIF
            ELSE IF(TRABUF(TGAMTYP).EQ.TSPT) THEN
                CALL UPDSPT(TRABUF,0,ST)
                CALL SET_CANCELATION_EVENTS_BITMAP(TRABUF)
            ELSE IF(TRABUF(TGAMTYP).NE.TKIK.AND.TRABUF(TGAMTYP).NE.TTGL) THEN
                SYNTERRCOD=205
                TRABUF(TSTAT)=REJT
                TRABUF(TERR)=SYNT
            ENDIF
        ENDIF
C
C UPDATE FINANCIAL INFORMATION
C
        IF(TRABUF(TSTAT).EQ.GOOD) THEN
            CALL UPDSUB(TRABUF)
            CALL UPDSTA(TRABUF)
            PERFRM(1,GAME)=PERFRM(1,GAME)+1
        ENDIF

        CALL TRALOG(TRABUF,PRO(WRKTAB,BUF))
        IF(TRABUF(TERR).EQ.TBAD) HPRO(ENCOVR,BUF)=-1!TBAD=BAD TERMINAL NUMBER | ENCOVR=encryption override flag
        IF(HPRO(SIMMOD,BUF).EQ.-999) THEN
            CALL OUTWAG(TRABUF,SIMOUTTAB,SIMOUTLEN)   !DON'T DESTROY INPUT
        ELSE
C----+------------------------------------------------------------------
C V28| New Terminals Project - Olimpo
C----+------------------------------------------------------------------             
            I4TEMP = TRABUF(TWTOT)
            CALL OPS('Wager Amount to Pay (wagpro):',ZEXT(I4TEMP),ZEXT(I4TEMP))
            HPRO(TWTOT_HALFW_OLM+0,BUF) = I2TEMP(1)
            HPRO(TWTOT_HALFW_OLM+1,BUF) = I2TEMP(2)
C----+------------------------------------------------------------------
C V28| New Terminals Project - Olimpo
C----+------------------------------------------------------------------                
            CALL OUTWAG(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF)) !afther construting the output message (OUTTAB in the buffer BUF)
        ENDIF
C
C IF SYNTAX ERROR THEN PRINT ERROR CODE
C ON THE CONSOLE.
C
        IF(P(SUPSYN).EQ.0.AND.SYNTERRCOD.NE.0.AND.
     *     TRABUF(TERR).NE.NOTON) THEN
            MESS(2) = TEGEN !(TEGEN=4) !GENERAL MESSAGES
            MESS(3) = 10
            MESS(4) = SYNTERRCOD !ERROR CODE IF SYNTAX ERROR
            MESS(5) = TER !numero interno do terminal
            MESS(6) = TRABUF(TGAMTYP) !TGAMTYP game type
            MESS(7) = TRABUF(TGAMIND) !TGAMIND game index
            MESS(8) = TRABUF(TSER) !TSER internal serial
            CALL QUEMES(MESS) !QUEUE MESSAGE TO ERRLOG QUEUE
        ENDIF
C
C QUEUE TRANSACTION TO LOGGER OUTPUT QUEUE
C
90      CONTINUE
        AGTHTB(ATRNUM,TER) = TRABUF(TTRN)
        IF(TRABUF(TSTAT).NE.GOOD.AND.TRABUF(TERR).NE.RETY) THEN
            AGTHTB(ACHKSM,TER)=-1
        ELSE
            AGTHTB(ACHKSM,TER)=TRABUF(TCHK)
        ENDIF
        CALL QUETRA(APU, BUF) !send BUF/message to APU application queue
        CALL DQUTRA(TASK,BUF) !after sending the message to APU application queue can now remove the current application queue that is TASK=WAG
        GOTO 20
C
C PROCESS LOTTO/SPORTS CANCELLATIONS AND DELETIONS.
C CANCELLATION AND WAGER ARE ENCODED IN THE WORK
C AREA OF THE PROCOM BUFFER (FROM CANPRO/INCPRO).
C UPDATE SALES DATA FOR LOTTO/SPORTS CANCELLATIONS.
C
1000    CONTINUE
        CALL LOGTRA(TRABUF,PRO(WRKTAB,BUF))
C 
        AGTHTB(ATRNUM,TER)=TRABUF(TTRN)        ! V26
        AGTHTB(ACHKSM,TER)=-1                  ! V26
C
        TERMCHKSUM = TRABUF(TCHK)
	ERCODE     = TRABUF(TSUBERR) ! carry ERCODE from 1st phase transaction.
	
	IF(TRABUF(TSTAT).EQ.REJT) GOTO 1100
	
        CALL RLOG(TRABUF(TWCSER),LOGREC,TASK,STATUS)
CV18
	IF (STATUS.NE.0) THEN
	    CALL WAIT_APUQUE
      	    CALL RLOG(TRABUF(TWCSER),LOGREC,TASK,STATUS)
	ENDIF
CEV18
        CALL LOGTRA(WRKBUF,LOGREC)
        IF(STATUS.NE.0.OR.WRKBUF(TSER).NE.TRABUF(TWCSER)) THEN
            TRABUF(TSTAT)=REJT
            TRABUF(TERR)=INVL
            CALL TRALOG(TRABUF,PRO(WRKTAB,BUF))
            ERCODE=1
	ELSE	    
            WRKBUF(TSTAT)=VOID
            IF(TRABUF(TTYP).EQ.TINC) WRKBUF(TSTAT)=INCA
            WRKBUF(TWCSER)=TRABUF(TSER)
            WRKBUF(TWCTER)=TRABUF(TTER)
            CALL TRALOG(WRKBUF,LOGREC)
            CALL UPDSUB(TRABUF)
            CALL UPDSTA(TRABUF)

            IF(TRABUF(TGAMTYP).EQ.TLTO) CALL UPDLPL(TRABUF,0,ST)
            IF(TRABUF(TGAMTYP).EQ.TSPT) CALL UPDSPT(TRABUF,0,ST)
            CALL WLOG(WRKBUF(TSER),LOGREC,TASK)
            ERCODE=0
        ENDIF
C
1100	CONTINUE
C
        IF(TRABUF(TTYP).EQ.TCAN) THEN
            IF(HPRO(SIMMOD,BUF).EQ.-999) THEN
                CALL OUTCAN(TRABUF,SIMOUTTAB,SIMOUTLEN,ERCODE)
            ELSE
                CALL OUTCAN(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),ERCODE)
            ENDIF
        ELSE
            CALL OUTDEL(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF))
        ENDIF

        HPRO(TRCODE,BUF)=TYPREG
        CALL QUETRA(APU, BUF)
        CALL DQUTRA(TASK,BUF)
        GOTO 20
        END

C ******************************************************************************
C
C     SUBROUTINE: SET_CANCELATION_EVENTS_BITMAP
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 03 / 04 / 2017
C
C ******************************************************************************
C
C FUNCTION TO GE THE TOTAL NUMBER OF CANCELLED EVENTS FROM COMMON MEMORY
C
C==== OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE SET_CANCELATION_EVENTS_BITMAP(TRABUF)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:DESTRA.DEF'
      INCLUDE 'INCLIB:SPTCOM.DEF'
C
      INTEGER * 4 ROW_IDX       ! ROW INDEX COUNTER
C
      IF(TRABUF(TSTAT)   .NE. GOOD) RETURN
      IF(TRABUF(TERR)    .NE. NOER) RETURN
      IF(TRABUF(TGAMTYP) .NE. TSPT) RETURN   ! ONLY SPORTS GAME TYPE
C
      IF(MIN(SPTMAX(TRABUF(TGAMIND)), SPGNBR) .GE. 33) THEN
        CALL OPSTXT('WRONG DRAW SETUP TO TO UPDATE EVENT CANCELTAION BITMAP !!!')
        RETURN
      ENDIF
C
      TRABUF(TWCEBM) = 0
C
      DO ROW_IDX = 1, MIN(SPTMAX(TRABUF(TGAMIND)), SPGNBR)
        IF(SPTECD(ROW_IDX, TRABUF(TGAMIND)) .NE. 0) CALL BSET(TRABUF(TWCEBM), 32 - ROW_IDX)
      ENDDO
C
      END

