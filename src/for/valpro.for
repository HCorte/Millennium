C
C PROGRAM VALPRO
C
C V08 15-APR-2021 SCML New Terminals Project
C V07 11-AUG-2015 SCML Correcting message lengths after changes in PLACARD
C V06 09-OCT-2013 SCML New Validation Messages
C V05 12-MAR-2010 RXK TCLM commented out
C V04 05-OCT-2000 UXN AlphaIPS changes. CHKFMT() added.
C V03 18-MAR-1999 RXK Temp.change for old Viking bonus numbers removed.
C V02 07-OCT-1991 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C VALIDATION PROCESSING TASK
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
        PROGRAM VALPRO
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
        INTEGER*4 LBUF(LREC*3),LASTRA(TRALEN)
        INTEGER*4 WRKBUF(TRALEN),MESS(EDLEN)
        INTEGER*4 I, STATUS, LSTSER, TYPE, TER, STAT, BUF, ST
        INTEGER*4 DUMMY, TASK, LSTSUP
        LOGICAL EXCFLG
        DATA LSTSUP/1/
        
C
        CALL COPYRITE
        CALL SNIF_AND_WRKSET
C
C
        TASK=VAL
        MESS(1)=VAL
5       CONTINUE
        BASECHKSUM=IAND(DAYCDC,'FFFF'X)
C
C
10      CONTINUE
        IF(DAYSTS.EQ.DSCLOS) THEN
          CALL LISTTOP(DUMMY,REPVQUE,ST)
          IF(ST.NE.0) CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
C
C
        IF(DAYSTS.EQ.DSSUSP) THEN
          CALL CLSVAL
          LSTSUP=1
20        CONTINUE
          CALL HOLD(0,ST)
          IF(DAYSTS.EQ.DSOPEN) GOTO 5
          IF(DAYSTS.EQ.DSCLOS) CALL GSTOP(GEXIT_SUCCESS)
          GOTO 20
        ENDIF
C
C
        CALL HOLD(0,ST)
30      CONTINUE
        IF(P(SUPVAL).NE.LSTSUP) THEN
          IF(P(SUPVAL).EQ.0) THEN
            LSTSUP=P(SUPVAL)
            CALL OPEVAL
          ELSE
            CALL LISTTOP(DUMMY,REPVQUE,ST)
            IF(ST.NE.0) THEN
              LSTSUP=P(SUPVAL)
              CALL CLSVAL
            ENDIF
          ENDIF
        ENDIF
C
C
        CALL TOPQUE(TASK,BUF)
        CALL REPVAL(STAT,LSTSUP)
        IF(BUF.LE.0.AND.STAT.NE.0) GOTO 10
        IF(BUF.LE.0) GOTO 30
C
C
        CALL FASTSET(0,TRABUF,TRALEN)
        EXCFLG=.FALSE.
C
        TER=HPRO(TERNUM,BUF)
        TYPE=HPRO(TRCODE,BUF)
        IF(TYPE.EQ.TYPECH) GOTO 2000
C
C BUILD VALIDATION TRANSACTION
C
        TRABUF(TSTAT)=GOOD
        TRABUF(TERR)=NOER
        TRABUF(TTYP)=TVAL
        TRABUF(TTER)=TER
        TRABUF(TAGT)=AGTTAB(AGTNUM,TER)
        TRABUF(TCDC)=DAYCDC
        TRABUF(TSER)=PRO(SERIAL,BUF)
        TRABUF(TTIM)=PRO(TSTAMP,BUF)
        TRABUF(TSIZE)=HPRO(NUMLRC,BUF)
        TRABUF(TVCODE)=NOWIN
C----+------------------------------------------------------------------
C V08| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
        IF(BPRO(CHOLM_OLM,BUF) .EQ. 1) THEN 
           TRABUF(TVOLMSERL_TLTO)=PRO(SEROLM_INT_OLM,BUF)
           TRABUF(TVOLMSERM_TLTO)=PRO(SEROLM_INT_OLM+1,BUF)
           TRABUF(TVOLMSERH_TLTO)=BPRO(SEROLM_OLM+8,BUF)
           TRABUF(TVOLMMIDL_TLTO)=PRO(MESSID_INT_OLM,BUF)
           TRABUF(TVOLMMIDH_TLTO)=BPRO(MESSID_OLM+4,BUF)
           TRABUF(TVOLMCOMF_TLTO)=BPRO(CHOLM_OLM,BUF)
        ENDIF        
C----+------------------------------------------------------------------
C V08| New Terminals Project - Olimpo
C----+------------------------------------------------------------------
        CALL DVAL(PRO(INPTAB,BUF),TRABUF,HPRO(INPLEN,BUF))
C
C CHECK AGENT AND SYSTEM STATUS
C
        IF(P(SUPVAL).NE.0)                     TRABUF(TERR)=SUPR
        IF(AGTHTB(AOPSTS,TER).NE.SIGNON)       TRABUF(TERR)=NOTON
        IF(P(SYSSTS).EQ.SYSDRW)                TRABUF(TERR)=SDRW
        IF(P(SYSSTS).EQ.SYSDOR)                TRABUF(TERR)=SDOR
        IF(TRABUF(TERR).NE.NOER)               TRABUF(TSTAT)=REJT
        IF(TRABUF(TSTAT).NE.GOOD) GOTO 60
C
C RETRY PROCESSING
C

C The New Terminals don't have retrys so jumps to the normal process        
        IF(BPRO(CHOLM_OLM,BUF).EQ.1) GOTO 50 
C        
        IF(TRABUF(TTRN).EQ.AGTHTB(ATRNUM,TER).AND.
     *     TRABUF(TSTAT).EQ.GOOD) THEN
          LSTSER=AGTTAB(ALSTRA,TER)
          CALL RLOG(LSTSER,LBUF,TASK,STATUS)
          IF(STATUS.NE.0) GOTO 50
          CALL LOGTRA(LASTRA,LBUF)
C          IF(LASTRA(TTYP).NE.TVAL.AND.LASTRA(TTYP).NE.TCLM.AND.
C     *       LASTRA(TTYP).NE.TREF)           GOTO 50

C         TVAL=4         TREF=6        
          IF(LASTRA(TTYP).NE.TVAL.AND.LASTRA(TTYP).NE.TREF) GOTO 50
          IF(LASTRA(TVSER).NE.TRABUF(TVSER)) GOTO 50
          IF(LASTRA(TTRN).NE.TRABUF(TTRN))   GOTO 50
C----+------------------------------------------------------------------
C V06| New Validation Messages
C----+------------------------------------------------------------------
          IF(LASTRA(TVTYPE) .EQ. VNBNK) THEN
              IF(LASTRA(TVTYPE)    .NE. TRABUF(TVTYPE)    ) GOTO 50
              IF(LASTRA(TVPLIDTYP) .NE. TRABUF(TVPLIDTYP) ) GOTO 50
              IF(LASTRA(TVPLCARD)  .NE. TRABUF(TVPLCARD)  ) GOTO 50
              IF(LASTRA(TVNIBBB)   .NE. TRABUF(TVNIBBB)   ) GOTO 50
              IF(LASTRA(TVNIBBO)   .NE. TRABUF(TVNIBBO)   ) GOTO 50
              IF(LASTRA(TVNIBBA1)  .NE. TRABUF(TVNIBBA1)  ) GOTO 50
              IF(LASTRA(TVNIBBA2)  .NE. TRABUF(TVNIBBA2)  ) GOTO 50
              IF(LASTRA(TVNIBCD)   .NE. TRABUF(TVNIBCD)   ) GOTO 50
          ENDIF
C----+------------------------------------------------------------------
C V06| New Validation Messages
C----+------------------------------------------------------------------
          TRABUF(TSTAT)=REJT
          TRABUF(TERR) =RETY
          DO 40 I=TVSTS,TRALEN
          TRABUF(I)=LASTRA(I)
40        CONTINUE
          CALL TRALOG(TRABUF,PRO(WRKTAB,BUF))
          CALL WLOG(PRO(SERIAL,BUF),PRO(WRKTAB,BUF),TASK)
          IF(LASTRA(TVSTS).EQ.VCASHX.OR.LASTRA(TVSTS).EQ.VCLAMX) THEN
            CALL RLOG(LASTRA(TVEXC),LBUF,TASK,STATUS)
            IF(STATUS.EQ.0) CALL LOGTRA(WRKBUF,LBUF)
            IF(WRKBUF(TSTAT).NE.EXCH.OR.STATUS.NE.0) THEN
              IF(LASTRA(TVSTS).EQ.VCASHX) LASTRA(TVSTS)=VCASH
              IF(LASTRA(TVSTS).EQ.VCLAMX) LASTRA(TVSTS)=VCLAM
            ENDIF
          ENDIF
         CALL OUTVAL(LASTRA,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),WRKBUF,
     *                VALREC,.TRUE.)
          GOTO 100
        ENDIF
C
C END OF RETRY PROCESSING
C
50      CONTINUE
        CALL VALUPD(TRABUF,WRKBUF,EXCFLG,VALREC)
        IF(EXCFLG) GOTO 1000
C
C IF CASHED ON A PRIVELEDGED TERMINAL SEND CHECKWRITER
C MESSAGE TO STRATUS FROM LIVE SYSTEM ONLY
C
C
C REMOVED CHECK WRITER MESSAGE THAT TIMO ASKED FOR
C
         TRABUF(TVCWT) = 0
C
C        IF(P(CHKWRT).EQ.0) THEN
C           IF(TRABUF(TSTAT).EQ.GOOD. AND. 
C     *        P(SYSTYP).EQ.LIVSYS .AND.
C     *        TSBIT(AGTTAB(AGTTYP,TER),AGTPRV) .AND.
C     *        P(SUPINS).EQ.0) CALL CHKFMT(TRABUF)
C        ELSE
C          TRABUF(TVCWT)=0
C        ENDIF
C
C UPDATE FINANCIAL AND PERFORMANCE DATA FOR
C GOOD VALIDATIONS. BUILD OUTPUT MESSAGE AND
C QUEUE TRANSACTION TO LOGGER OUTPUT QUEUE.
C
        IF(TRABUF(TSTAT).EQ.GOOD) THEN
          CALL UPDSUB(TRABUF)
          PERFRM(1,PERVAL)=PERFRM(1,PERVAL)+1
        ENDIF
C
C
60      CONTINUE
        CALL TRALOG(TRABUF,PRO(WRKTAB,BUF))
        CALL WLOG(PRO(SERIAL,BUF),PRO(WRKTAB,BUF),TASK)
        CALL OUTVAL(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),WRKBUF,
     *              VALREC,.FALSE.)
C
C
100     CONTINUE
        AGTHTB(ATRNUM,TER)=TRABUF(TTRN)
        AGTHTB(ACHKSM,TER)=-1
        CALL QUETRA(LOG,BUF)
        CALL DQUTRA(TASK,BUF)
        GOTO 30
C
C EXCHANGE TICKET PROCESSING PHASE ONE.
C STORE VALIDATION BODY IN THE INPUT
C AREA OF THE PROCESSING BUFFER, SET
C THE TRANSACTION TYPE TO EXCHANGE,
C AND QUEUE THE BUFFER BACK TO THE
C DISPATCHER QUEUE (TOP) FOR SERIAL NUMBER
C ASSIGNMENT IF THIS IS THE LIVE SYSTEM,
C OR RELEASE THE BUFFER IF NOT.
C
1000    CONTINUE
        CALL TRALOG(TRABUF,PRO(WRKTAB,BUF))
        HPRO(TRCODE,BUF)=TYPECH
        AGTHTB(ATRNUM,TER)=TRABUF(TTRN)
        AGTHTB(ACHKSM,TER)=-1
C	HPRO(INPLEN,BUF)=320   !INCREASED DO SUPPORT 3 RECORDS
C----+------------------------------------------------------------------
C V07| Correcting message lengths after changes in PLACARD
C----+------------------------------------------------------------------
C       HPRO(INPLEN,BUF)=384	
        HPRO(INPLEN,BUF)=448
C----+------------------------------------------------------------------
C V07| Correcting message lengths after changes in PLACARD
C----+------------------------------------------------------------------
        CALL DQUTRA(TASK,BUF)
        IF(P(SYSTYP).EQ.LIVSYS) THEN
          CALL ATL(BUF,QUETAB(1,DIS),ST)
        ELSE
          CALL RELBUF(BUF)
        ENDIF
        GOTO 30
C
C EXCHANGE TICKET PROCESSING PHASE TWO.
C RESTORE ORIGINAL VALIDATION TRANSACTION STORED
C IN THE INPUT AREA OF THE PROCESSING BUFFER AND
C SET THE EXCHANGE TICKET SERIAL NUMBER.
C
2000    CONTINUE
        CALL LOGTRA(TRABUF,PRO(WRKTAB,BUF))
        TERMCHKSUM=TRABUF(TCHK)
        TRABUF(TVEXC)=PRO(SERIAL,BUF)
        EXCFLG=.TRUE.
        CALL VALUPD(TRABUF,WRKBUF,EXCFLG,VALREC)
C
C IF CASHED ON A PRIVELEDGED TERMINAL SEND CHECKWRITER
C MESSAGE TO STRATUS FROM LIVE SYSTEM ONLY
C
C
C REMOVED CHECK WRITER MESSAGE THAT TIMO ASKED FOR
C
        TRABUF(TVCWT) = 0
C
C       IF(P(CHKWRT).EQ.0) THEN
C           IF(TRABUF(TSTAT).EQ.GOOD. AND. 
C     *        P(SYSTYP).EQ.LIVSYS .AND.
C     *        TSBIT(AGTTAB(AGTTYP,TER),AGTPRV) .AND.
C     *        P(SUPINS).EQ.0) CALL CHKFMT(TRABUF)
C        ELSE
C          TRABUF(TVCWT)=0
C        ENDIF
C
C UPDATE FINANCIAL AND PERFORMANCE DATA
C
        IF(TRABUF(TSTAT).EQ.GOOD) THEN
          CALL UPDSUB(TRABUF)
          PERFRM(1,PERVAL)=PERFRM(1,PERVAL)+1
        ENDIF
C
C
        CALL TRALOG(TRABUF,PRO(WRKTAB,BUF))
        CALL WLOG(TRABUF(TSER),PRO(WRKTAB,BUF),TASK)
        IF(TRABUF(TSTAT).EQ.GOOD) THEN
          WRKBUF(TTIM)=PRO(TSTAMP,BUF)
          CALL TRALOG(WRKBUF,PRO(WRKTAB,BUF))
          CALL WLOG(WRKBUF(TSER),PRO(WRKTAB,BUF),TASK)
        ELSE
          MESS(2)=TEGEN
          MESS(3)=8
          MESS(4)=TRABUF(TVSER)
          MESS(5)=TRABUF(TVCDC)
          CALL QUEMES(MESS)
        ENDIF
C
C
        CALL OUTVAL(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),WRKBUF,
     *              VALREC,.FALSE.)
        HPRO(TRCODE,BUF)=TYPREG
        CALL QUETRA(LOG,BUF)
        CALL DQUTRA(TASK,BUF)
        GOTO 30
        END
