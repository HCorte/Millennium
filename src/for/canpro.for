C
C PROGRAM CANPRO
C
C V15 15-APR-2021 SCM New Termninals Project - OLM
C V14 11-AUG-2015 SCML Correcting message lengths after changes in PLACARD
C V13 16-MAR-2010 RXK Changes for ePassive
C V12 21-JUN-2002 JHR Updating checksum and sequence # moved to wagpro's.
C V11 26-MAR-2001 JHR SUPPRESSION OF PRIVILEGE TERMINAL CANCELATIONS
C V10 25-JAN-2001 UXN Make all cancellations to have 2 phases. Don't process
C                     1st phase on secondary system. WAIT_APUQUE added.
C V09 29-NOV-00 UXN TOTOGOLA ADDED.
C V08 13-OCT-1999 RXK World Tour added.
C V07 02-0CT-1994 HXK Added Bingo
C V06 04-AUG-1993 GXA Do not allow cancelation of fractions.
C V05 30-JUN-1993 GXA Added Speden and Ravi (V65) and added check for second
C                     Kicker played.
C V04 30-JUN-1993 GXA Initial revision.
C V03 08-APR-1992 GCAN ROUTE ODDSET CANCELATIONS TO CORRECT TASK.
C                     (TRCODE SET INCORRECT FOR ODDSET CANCELLATIONS).
C V02 13-APR-1992 GCAN CHECK KICKER (Y/N) FLAG, BEFORE SUPPRESSION DECICION.
C V01 07-OCT-1991 MTK  INITIAL RELEASE FOR NETHERLANDS
C
C CANPRO.FOR
C
C CANCELLATION PROCESSING TASK
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
C Copyright 2001 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM CANPRO
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
C
        INTEGER*4 LOGREC(LREC*3),WRKBUF(TRALEN)
        INTEGER*4 RETBUF(TRALEN)
C
        INTEGER*4 TIME, LSTSER, ST, KGAM, AGAME, GAME, TER, ERCODE
        INTEGER*4 BUF, STATUS, TASK
C
C
        CALL COPYRITE
        CALL SNIF_AND_WRKSET
C
        TASK=CAN
5       CONTINUE
        BASECHKSUM=IAND(DAYCDC,'FFFF'X)
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
C IF NO CANCELS QUEUED, GO BACK TO WAIT STATE.
C
20      CONTINUE
        CALL TOPQUE(TASK,BUF)
        IF(BUF.EQ.0) GOTO 10
CV10
C       ONLY PROCESS FIRST PHASE CANCELLATIONS ON THE PRIMARY SYSTEM
C
        IF(P(SYSTYP).NE.LIVSYS) THEN
          CALL DQUTRA(TASK,BUF)
          CALL RELBUF(BUF)
          GOTO 20
        ENDIF
CEV10
        CALL FASTSET(0,TRABUF,TRALEN)
        CALL FASTSET(0,WRKBUF,TRALEN)
C
C DECODE CANCEL MESSAGE AND FILL IN HEADER INFORMATION
C
        ERCODE=0
        TER=HPRO(TERNUM,BUF)
        TRABUF(TSTAT) =GOOD
        TRABUF(TERR)  =NOER
        TRABUF(TCDC)  =DAYCDC
        TRABUF(TSER)  =PRO(SERIAL,BUF)
        TRABUF(TTIM)  =PRO(TSTAMP,BUF)
        TRABUF(TSIZE) =HPRO(NUMLRC,BUF)
        TRABUF(TTER)  =TER
        TRABUF(TAGT)  =AGTTAB(AGTNUM,TER)
        TRABUF(TTYP)  =TCAN
        CALL DCAN(PRO(INPTAB,BUF),TRABUF,HPRO(INPLEN,BUF))

        IF(TRABUF(TERR).NE.NOER) THEN
          ERCODE=2
          GOTO 100
        ENDIF
C
C CHECK IF TRANSACTION TYPE IS SUPRESSED ON
C AGENT AND GLOBAL LEVELS
C
        IF(TSBIT(AGTTAB(AGTTYP,TER),AGTCAN))   TRABUF(TERR)=SUPR
        IF(P(SUPCAN).NE.0)                     TRABUF(TERR)=SUPR
C
C PRIVILEGED TERMINALS CAN NOT CANCEL
C
        IF(TSBIT(AGTTAB(AGTTYP, TER), AGTPRV)) TRABUF(TERR) = SUPR
C
C CHECK AGENT AND SYSTEM STATUS
C
        IF(AGTHTB(AOPSTS,TER).NE.SIGNON)       TRABUF(TERR)=NOTON
        IF(P(SYSSTS).EQ.SYSDRW)                TRABUF(TERR)=SDRW
        IF(P(SYSSTS).EQ.SYSDOR)                TRABUF(TERR)=SDOR
        IF(TRABUF(TERR).NE.NOER) GOTO 100
C
C READ WAGER FROM LOG FILE
C
        CALL RLOG(TRABUF(TWCSER),LOGREC,TASK,STATUS)
CV10
	IF(STATUS.NE.0) THEN
	   CALL WAIT_APUQUE
	   CALL RLOG(TRABUF(TWCSER),LOGREC,TASK,STATUS)
	ENDIF
CEV10
        IF(STATUS.NE.0) THEN
          TRABUF(TERR)=INVL
          ERCODE=2
          GOTO 100
        ENDIF
C
        CALL LOGTRA(WRKBUF,LOGREC)
        IF(WRKBUF(TTYP).NE.TWAG.OR.TRABUF(TWCSER).NE.WRKBUF(TSER).OR.
     *     (WRKBUF(TSTAT).NE.GOOD.AND.WRKBUF(TSTAT).NE.VOID)) THEN
           TRABUF(TERR)=INVL
           ERCODE=2
           GOTO 100
        ENDIF
C
C COPY WAGER INFORMATION TO CANCELLATION RECORD
C
        CALL FASTMOV(WRKBUF(TWBEG),TRABUF(TWBEG),TRALEN-TWBEG+1)
        TRABUF(TGAM)=WRKBUF(TGAM)
        TRABUF(TGAMTYP)=WRKBUF(TGAMTYP)
        TRABUF(TGAMIND)=WRKBUF(TGAMIND)
        TRABUF(TWCSER)=WRKBUF(TSER)
        TRABUF(TWCTER)=WRKBUF(TTER)
        TRABUF(TCDC_SOLD)=WRKBUF(TCDC_SOLD)
C
C IF TERMINAL RETRY, AND TRANSACTION STATUS IS GOOD, AND
C TRANSACTION SEQUENCE NUMBER MATCHES THE LAST SEQUENCE
C NUMBER FOR THIS TERMINAL, CONTINUE RETRY PROCESSING, ELSE
C PROCESS AS A REGULAR CANCELLATION.
C
C The New Terminals don't have retrys so jumps to the normal process        
        IF(BPRO(CHOLM_OLM,BUF).EQ.1) GOTO 40 
C        
        IF(TRABUF(TTRN).EQ.AGTHTB(ATRNUM,TER)) THEN
          LSTSER=AGTTAB(ALSTRA,TER)

          CALL RLOG(LSTSER,LOGREC,TASK,STATUS)
CV10
	  IF(STATUS.NE.0) THEN
	     CALL WAIT_APUQUE
	     CALL RLOG(LSTSER,LOGREC,TASK,STATUS)
	  ENDIF
CEV10
          IF(STATUS.NE.0) GOTO 40

          CALL LOGTRA(RETBUF,LOGREC)
          IF(RETBUF(TTYP).NE.TCAN) GOTO 40
          IF(RETBUF(TWCSER).NE.TRABUF(TWCSER)) GOTO 40
          IF(RETBUF(TTRN).NE.TRABUF(TTRN)) GOTO 40

          TRABUF(TSTAT)=REJT
          TRABUF(TERR )=RETY
	  ERCODE = 0
	  GOTO 100	  
        ENDIF
C
40      CONTINUE
C
C CHECK IF SUPRESSED BY GAME
C
        GAME=TRABUF(TGAM)
        AGAME=AGTGAM(GFLAGS,GAME,TER)
        IF(TSBIT(AGAME,AGTCAN))                TRABUF(TERR)=SUPR
        IF(TSBIT(P(SUPGCA),GAME))              TRABUF(TERR)=SUPR
C
C IF KICKER THEN CHECK IF SUPRESSED
C
        KGAM=TRABUF(TWKGME)
        IF(TRABUF(TWKFLG).EQ.0.AND.TRABUF(TWKFLG2).EQ.0) KGAM = 0
C
        IF (KGAM.GT.MAXGAM .OR. KGAM.LT.0) THEN
           TRABUF(TERR)=INVL
        ELSEIF(KGAM.NE.0) THEN
           AGAME=AGTGAM(GFLAGS,KGAM,TER)
           IF(TSBIT(AGAME,AGTCAN))              TRABUF(TERR)=SUPR
           IF(TSBIT(P(SUPGCA),KGAM))            TRABUF(TERR)=SUPR
        ENDIF
        IF(TRABUF(TERR).NE.NOER) GOTO 100
C
C CHCEK IF THE SPORT DRAW HAS BEEN CANCELLED
C
        IF(TRABUF(TGAMTYP) .EQ. TSPT) THEN
           IF(SPTDCD(TRABUF(TGAMIND)) .NE. 0) THEN   ! THE FULL DRAW HAS BEEN CANCELLED
              TRABUF(TSTAT) = REJT
              TRABUF(TERR)  = SFDC
              ERCODE        = 8
              GOTO 100
           ENDIF
        ENDIF
C
C CHECK IF AFTER DRAW
C
        CALL CHKCAN(WRKBUF,ST)
        IF(ST.NE.0) THEN
          TRABUF(TERR)=INVL
          ERCODE=2
          GOTO 100
        ENDIF
C
C IF NOT THE PRIMARY SYSTEM AND THE TICKET IS ALREADY CANCELLED
C THEN CHECK IF SECOND PHASE OF CANCELLATION WAS PROCESSED FIRST.
C
        IF(WRKBUF(TSTAT).NE.GOOD) TRABUF(TERR)=INVL
C
C CHECK IF TIME LIMIT HAS EXPIRED
C
        IF(.NOT.TSBIT(AGTTAB(AGTTYP,TER),AGTCTM)) THEN
          TIME=TRABUF(TTIM)-WRKBUF(TTIM)
          IF(TRABUF(TGAMTYP).EQ.TPAS) THEN
            IF(TIME.GT.P(PCANTIM)*60.AND.P(PCANTIM).NE.0) THEN
              TRABUF(TERR)=INVL
              ERCODE=1
            ENDIF
          ELSE
            IF(TIME.GT.P(CANTIM)*60.AND.P(CANTIM).NE.0) THEN
              TRABUF(TERR)=INVL
              ERCODE=1
            ENDIF
          ENDIF
        ENDIF
C
C CHECK FOR WRONG TERMINAL
C
        IF(TRABUF(TTER).NE.WRKBUF(TTER)) THEN
          TRABUF(TERR)=INVL
          ERCODE=4
        ENDIF
C
C CHECK FOR ALREADY CANCELLED
C
        IF(WRKBUF(TSTAT).EQ.VOID) THEN
          TRABUF(TERR)=INVL
          ERCODE=3
        ENDIF
C
C CHECK FOR FRACTIONED WAGER, (NO CANCELS ALLOWED OF FRACTIONS)
C
        IF(WRKBUF(TWFFLG).NE.0) THEN
           TRABUF(TERR) = INVL
           ERCODE = 2
        ENDIF
C
C IF EPASSIVE CHECK OPERATION REQUEST OF THE TRANSACTON 
C
        IF(TRABUF(TGAMTYP).EQ.TPAS) THEN
           IF(WRKBUF(TWEPOP).NE.EPASSAL) THEN   
              TRABUF(TERR)= INVL
              ERCODE=2
              GOTO 100       
           ENDIF 
C
C IF EPASSIVE CHECK IF LIMIT OF ALLOWED CANCELLATIONS REACHED
C
           IF(P(PMAXCAN).GT.0.AND.AGTTAB(AGTEPC,TER).GE.P(PMAXCAN)) THEN
              TRABUF(TERR)= INVL
              ERCODE=5
              GOTO 100       
           ENDIF
        ENDIF
C
        IF(TRABUF(TERR).NE.NOER) GOTO 100
C
C ENCODE THE CANCELLATION IN THE WORK AREA
C OF THE PROCESSING BUFFER AND REQUEUE IT TO THE DISPAT QUEUE (TOP).
C THE WAGER PROCESSING TASKS WILL UPDATE FINANCIAL AND POOL DATA
C AND BOTH TRANSACTIONS WILL BE LOGGED DURING THE SECOND PHASE
C OF PROCESSING.
C
        PERFRM(1,PERCAN)=PERFRM(1,PERCAN)+1
C
100	CONTINUE
C
        IF(TRABUF(TERR).NE.NOER) THEN
	   TRABUF(TSTAT)   = REJT
	   TRABUF(TSUBERR) = ERCODE ! to carry ERCODE to 2nd phase.
	ENDIF
        IF(TRABUF(TERR).EQ.TBAD) HPRO(ENCOVR,BUF)=-1

C----+------------------------------------------------------------------
C V15| New Terminals Project - Olimpo
C----+------------------------------------------------------------------        
C        IF(BPRO(CHOLM_OLM,BUF) .EQ. 1) THEN 
C           TRABUF(TWCOLMSERL_TLTO)=PRO(SEROLM_INT_OLM,BUF)
C           TRABUF(TWCOLMSERM_TLTO)=PRO(SEROLM_INT_OLM+1,BUF)
C           TRABUF(TWCOLMSERH_TLTO)=BPRO(SEROLM_OLM+8,BUF)
C           TRABUF(TWCOLMMIDL_TLTO)=PRO(MESSID_INT_OLM,BUF)
C           TRABUF(TWCOLMMIDH_TLTO)=BPRO(MESSID_OLM+4,BUF)
C           TRABUF(TWCOLMCOMF_TLTO)=BPRO(CHOLM_OLM,BUF)
C        ENDIF
        IF(BPRO(CHOLM_OLM,BUF) .EQ. 1) THEN 
           TRABUF(TCOLMSERL_TLTO)=PRO(SEROLM_INT_OLM,BUF)
           TRABUF(TCOLMSERM_TLTO)=PRO(SEROLM_INT_OLM+1,BUF)
           TRABUF(TCOLMSERH_TLTO)=BPRO(SEROLM_OLM+8,BUF)
           TRABUF(TCOLMMIDL_TLTO)=PRO(MESSID_INT_OLM,BUF)
           TRABUF(TCOLMMIDH_TLTO)=BPRO(MESSID_OLM+4,BUF)
           TRABUF(TCOLMCOMF_TLTO)=BPRO(CHOLM_OLM,BUF)  
CCCCCCCCCCC RESETS THE VALUE OF THE WAGER TRANSACTION NEW FIELD (MESSID,SERIALOLM,FLAGCH)CCC          
           TRABUF(TWCOLMSERL_TLTO)=0                
           TRABUF(TWCOLMSERM_TLTO)=0                
           TRABUF(TWCOLMSERH_TLTO)=0
           TRABUF(TWCOLMMIDL_TLTO)=0
           TRABUF(TWCOLMMIDH_TLTO)=0
           TRABUF(TWCOLMCOMF_TLTO)=0
        ENDIF        
C----+------------------------------------------------------------------
C V15| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
        CALL TRALOG(TRABUF,PRO(WRKTAB,BUF))
C	HPRO(INPLEN,BUF)=320   !INCREASED DO SUPPORT 3 RECORDS
C----+------------------------------------------------------------------
C V14| Correcting message lengths after changes in PLACARD
C----+------------------------------------------------------------------
C        HPRO(INPLEN,BUF)=384	
        HPRO(INPLEN,BUF)=448
C----+------------------------------------------------------------------
C V14| Correcting message lengths after changes in PLACARD
C----+------------------------------------------------------------------
        HPRO(TRCODE,BUF) = TYPOCN
        IF(TRABUF(TGAMTYP).EQ.TLTO.OR.
     *     TRABUF(TGAMTYP).EQ.TSPT.OR.
     *     TRABUF(TGAMTYP).EQ.TTGL.OR.
     *     TRABUF(TGAMTYP).EQ.TKIK)     THEN
           HPRO(TRCODE,BUF) = TYPWCN
        ELSEIF(TRABUF(TGAMTYP).EQ.TNBR) THEN
           HPRO(TRCODE,BUF) = TYPNCN
        ELSEIF(TRABUF(TGAMTYP).EQ.TBNG) THEN
           HPRO(TRCODE,BUF) = TYPPCN
        ELSEIF(TRABUF(TGAMTYP).EQ.TPAS) THEN
           HPRO(TRCODE,BUF) = TYPEPC
        ENDIF
C
C V12, REMOVED TO UPDATING CHECKSUM AND SEQUENCE NUMBER MOVED TO WAGPRO'S
C
C        AGTHTB(ATRNUM,TER)=TRABUF(TTRN)
C        AGTHTB(ACHKSM,TER)=-1
C
        CALL ATL(BUF,QUETAB(1,DIS),ST)
        CALL DQUTRA(TASK,BUF)

        GOTO 20
        END
