C SYSTRPT.FOR
C
C V27 20-APR-2010 RXK Changes for ePassive 
C V26 08-JUN-2000 UXN TTNAMES.DEF added.
C V25 22-DEC-1999 PXO Fix for instant validation counters
C V24 21-MAY-1999 UXN Fix for total counter if winning prize is 0.00
C V23 04-FEB-1999 UXN Fix for big sales.
C V22 13-NOV-1997 UXN TMFREP.DEF added.
C V21 14-MAY-1997 UXN Writing instant validations into BALANS.FIL
C V20 06-MAR-1997 RXK Fix for cartel, instant validations added into report 
C V19 31-JAN-1997 WPW Totals for instant trans added.
C V18 19-JAN-1997 RXK New transaaction types (tcrs,tpth) added to report 
C V17 08-AUG-1995 HXK Changes for refund totals
C V16 30-MAR-1995 HXK Fix for overflows, showing penny amounts in 5 penny units
C V15 27-APR-1994 JXP COPY=0
C V14 17-NOV-1993 SXH Fixed problem with counts/amounts offset by 1 hour wrt 
C                     time interval
C V13 19-OCT-1993 HXK TIDIED UP LAYOUT.
C V12 09-SEP-1993 SXH Make the TIME tables 24 hours
C V11 08-SEP-1993 SXH DO TOTSUMS for NUMFIN-2
C V10 07-SEP-1993 SXH Corrected BALWRI CALL, de-commented SPOOL
C V09 03-SEP-1993 SXH COpy=1, added IAM()
C V08 30-AUG-1993 HXN Changed conversion unit from 100 to 20.
C V07 10-JUN-1993 HXN Initial revision.
C V06 06-NOV-1992 HJK FIX FOR CARTEL PROBLEM
C V05 13-OCT-1992 HJK CHANGED FOR SPEDEN GAME #!#
C V04 20-JUN-1991 PP  ADDED SUBROUTINE CALL: BALWRI
C V03 13-MAY-1991 MTK IGNORE BANK SET VALIDATIONS
C V01 29-MAY-1990 HHE SYSTEM TRANSACTION REPORT BY CARTELS
C V02 11-JUL-1990 HHE COUNT FOR ACTIVE AGENTS ADDED
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, W.Greenwich, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
        SUBROUTINE SYSTRPT
        IMPLICIT NONE
	INCLUDE '(LIB$ROUTINES)'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB: GLOBAL.DEF'
	INCLUDE 'INCLIB: CONCOM.DEF'
	INCLUDE 'INCLIB: DESTRA.DEF'
	INCLUDE 'INCLIB: PRMLOG.DEF'
	INCLUDE 'INCLIB: RECREP.DEF'
	INCLUDE 'INCLIB: DATBUF.DEF'
	INCLUDE 'INCLIB: TNAMES.DEF'
	INCLUDE 'INCLIB: GTNAMES.DEF'
	INCLUDE 'INCLIB: TTNAMES.DEF'
	INCLUDE 'INCLIB: AGTCOM.DEF'
	INCLUDE 'INCLIB:TMFREP.DEF'
C
	INTEGER*4  LUN
C
        INTEGER*4  MAXCART
        PARAMETER (MAXCART   = 10000)

        INTEGER*4  CONV_UNIT 
        PARAMETER  (CONV_UNIT = 100)    ! EURO, WITH TWO DECIMALS

C SYSSUM REPORT VARIABLES
C -----------------------

        INTEGER*4  TRNSTAT(TCRS,FRAC)     !TRANSACTION STATUS(TYPE,STATUS)
	SAVE	   TRNSTAT
        INTEGER*4  TOTSTAT(FRAC)          !TRANSACTION TOTALS(TYPE,STATUS)
	SAVE	   TOTSTAT
        INTEGER*4  WAGSTAT(MAXGAM,FRAC)   !WAGER STATUS TABLE(GAME,STATUS)
	SAVE	   WAGSTAT
        INTEGER*4  TOTWAGS(FRAC)          !WAGER STATUS TOTALS(TYPE,STATUS)
	SAVE	   TOTWAGS
        INTEGER*4  FINANCE(MAXGAM,NUMFIN,2) !FINANCIALS(GAME,TYPE,COUNT/AMT)
	SAVE	   FINANCE
        INTEGER*4  TOTFINA(NUMFIN,2)        !FINANCIALS(GAME,TYPE,COUNT/AMT)
	SAVE	   TOTFINA
        INTEGER*4  TIMTAB(0:23,TRET,2)      !TIME TABLE
	SAVE	   TIMTAB
        INTEGER*4  TOTTIM(TRET,2)           !TOTAL TIME TABLE
	SAVE	   TOTTIM
        INTEGER*4  CTIMTAB(0:23,TRET,2,MAXCART)! CARTEL TIME TABLE
	SAVE	   CTIMTAB
        INTEGER*4  CTOTTIM(TRET,2,MAXCART)   ! CARTEL TOTAL TIME TABLE
	SAVE	   CTOTTIM
        INTEGER*4  ATIMTAB(0:23,TRET,2,NUMAGT) ! AGENT TIME TABLE
	SAVE	   ATIMTAB
        INTEGER*4  ATOTTIM(TRET,2,NUMAGT)    ! AGENT TOTAL TIME TABLE
	SAVE	   ATOTTIM
        INTEGER*4  TERTAB(NUMAGT)         ! TERMINALS USED
	SAVE	   TERTAB
        INTEGER*4  SORTTAB(2,NUMAGT)      ! SORT TABLE (TERMINAL,AGENT)
	SAVE	   SORTTAB
        INTEGER*4  VAL_REF(MAXGAM,2)      ! VALIDATION AND REFUND FOR BALANS.REP
	SAVE	   VAL_REF
	INTEGER*4  TIMED_OUT(2)	! INSTANT VALIDATIONS WHICH ARE TIMED OUT.
C
        INTEGER*4  COPY
        INTEGER*4  EXT
        INTEGER*4  DETCART
	SAVE	   DETCART
        INTEGER*4  PAGE
        INTEGER*4  SER
        INTEGER*4  S
        INTEGER*4  T
        INTEGER*4  G
        INTEGER*4  TOFF
        INTEGER*4  TRM
        INTEGER*4  CARTEL
        INTEGER*4  CNT,AMT
        INTEGER*4  KG
        INTEGER*4  ST
        INTEGER*4  WEEK
	INTEGER*4  YEAR       ! Year number in 4 digits
        INTEGER*4  K
        INTEGER*4  I
        INTEGER*4  GTYP
        INTEGER*4  L
        INTEGER*4  TCARTA
        INTEGER*4  ATCARTA
        INTEGER*4  CTOTTMT
        INTEGER*4  CARTA
        INTEGER*4  ACARTA
        INTEGER*4  TM
        INTEGER*4  J
        INTEGER*4  ATOTTMT
        INTEGER*4  GAM
        INTEGER*4  TYP

        INTEGER*4  INSFIN(NUMCRS,2)
        INTEGER*4  ALLFIN(NUMCRS,2)
        INTEGER*4  GVTFIN(NUMCRS,2)
	INTEGER*4  INSVAL(2)
	INTEGER*4  OFFSET,TSKSTS

C arguments to BALWRI-subprogram    V04

        INTEGER*4 RAPCODE
        INTEGER*4 GAMESUMS(MAXGAM,NUMFIN,NUMTOT)
        INTEGER*4 TOTSUMS(NO_BALSUMS)

        REAL*8    TOTREAL

        CHARACTER  HEAD*42

        LOGICAL FIRST  /.TRUE./
        LOGICAL KICKER /.FALSE./
C
C BEGIN CODE -------------------------------------
C
      IF (EOF)        GOTO 500
      IF (FIRST) THEN
        FIRST = .FALSE.
        TYPE *
        TYPE *, IAM(),'<<< TMREPS (SYSTRPT) V01 (SYSSUM.REP)         >>>'
        TYPE *, IAM(),'<<< System Transaction Report                 >>>'
        TYPE *
C
C       CALL INPNUM('Enter number of report copies ',COPY,0,10,EXT)
C       IF (EXT.NE.0) THEN
C         NOREP = .TRUE.
C         GOTO 1000
C       ENDIF
C
        COPY=0
	TIMED_OUT(1) = 0
	TIMED_OUT(2) = 0	
C
C IF THIS PROCESS IS A SUBPROCESS THEN USE DEFAULT VALUES.
C	
	CALL STTSK(8HSTSYSTEM,TSKSTS,ST)
	IF(.NOT.ISSUBPROC().OR.ST.EQ.4) THEN
           CALL PRMNUM('Enter CARTEL number for detail report ',
     *                DETCART,0,MAXCART-1,EXT)
           IF (EXT.NE.0) DETCART = -1
	ELSE
	   TYPE*,IAM(),'Using default cartel number - 7 '
	   DETCART = 7
	ENDIF
C
C CLEAR/SET VARIABLES
C -------------------
C
        PAGE=0
        SER=1
        CALL FASTSET(0,TRNSTAT,TCRS*FRAC)
        CALL FASTSET(0,WAGSTAT,MAXGAM*FRAC)
        CALL FASTSET(0,FINANCE,MAXGAM*NUMFIN*2)
        CALL FASTSET(0,TOTSTAT,FRAC)
        CALL FASTSET(0,TOTWAGS,FRAC)
        CALL FASTSET(0,TOTFINA,NUMFIN*2)
        CALL FASTSET(0,TIMTAB,24*TRET*2)
        CALL FASTSET(0,TOTTIM,2*TRET)
        CALL FASTSET(0,CTIMTAB,24*TRET*2*MAXCART)
        CALL FASTSET(0,CTOTTIM,TRET*2*MAXCART)
        CALL FASTSET(0,ATIMTAB,24*TRET*2*NUMAGT)
        CALL FASTSET(0,ATOTTIM,TRET*2*NUMAGT)
        CALL FASTSET(0,TERTAB,NUMAGT)
        CALL FASTSET(0,INSFIN,NUMCRS*2)
        CALL FASTSET(0,ALLFIN,NUMCRS*2)
        CALL FASTSET(0,GVTFIN,NUMCRS*2)
	CALL FASTSET(0,INSVAL,2)
      ENDIF
C
C EXTRACT TRANSACTION INFORMATION
C -------------------------------
C
      S = TRABUF(TSTAT)
      IF(S.LT.1.OR.S.GT.FRAC) GOTO 1000
C
C INSTANTS HAVE THEIR OWN BANK FIELDS
C (IF INSTANT GAME NUMBER WAS 5 (==VPTB), VALIDATIONS GOT SKIPPED)
C 
      IF(TRABUF(TTYP).NE.TCRS.AND.TRABUF(TVTYPE).EQ.VPTB) GOTO 1000
C
      T           = TRABUF(TTYP)
      IF(T.LT.1.OR.T.GT.TCRS) GOTO 1000
C
      G           = TRABUF(TGAM)
      TOFF        = TRABUF(TTIM)/3600
      TRM         = TRABUF(TTER)
      GTYP        = TRABUF(TGAMTYP)
C
      IF(TRM.GE.1.AND.TRM.LE.NUMAGT) THEN
         CARTEL      = CARTAB(1,TRM)+1    ! CARTELS START FORM 0
         TERTAB(TRM) = TERTAB(TRM)+1
      ENDIF
C
      KICKER = .FALSE.
C
      CNT=1
      IF(T.EQ.TWAG.OR.T.EQ.TINC.OR.T.EQ.TCAN) THEN
        KG=TRABUF(TWKGME)
        IF(TRABUF(TWKAMT).GT.0) KICKER=.TRUE.
        IF (G.NE.KG) THEN             ! THAT IS NOT THE JOKERI GAME
           IF(TRABUF(TWAMT).GT.0.AND.TRABUF(TWKAMT).GT.0)CNT=2
        ENDIF
      ENDIF
C
      TRNSTAT(T,S) = TRNSTAT(T,S) + CNT
      TOTSTAT(S)   = TOTSTAT(S)   + CNT
C
      IF(T.EQ.TWAG) THEN
         IF (G.EQ.KG) THEN             ! THAT IS THE JOKERI GAME
	    IF(TRABUF(TWAMT).GT.0) THEN
	      WAGSTAT(G,S)=WAGSTAT(G,S)+1
	      TOTWAGS(S)=TOTWAGS(S)+1
	    ENDIF
	 ELSE
	    IF(TRABUF(TWAMT).GT.0) THEN
	      WAGSTAT(G,S)=WAGSTAT(G,S)+1
	      TOTWAGS(S)=TOTWAGS(S)+1
	    ENDIF
	    IF(TRABUF(TWKAMT).GT.0) THEN
	      WAGSTAT(KG,S)=WAGSTAT(KG,S)+1
	      TOTWAGS(S)=TOTWAGS(S)+1
	    ENDIF
         ENDIF
      ENDIF
C
C FINANCIAL TABLE
C ---------------
C
      IF(T.EQ.TWAG.AND.TRABUF(TWFFLG).NE.0) GOTO 1000
      IF(T.EQ.TWAG.AND.GTYP.EQ.TPAS) THEN
         IF(TRABUF(TWEPOP).NE.EPASSAL) GOTO 1000  
      ENDIF
C
C WAGERS
C ------
C
      IF((T.EQ.TWAG.OR.T.EQ.TINC.OR.T.EQ.TCAN).AND.
     *   (S.EQ.GOOD.OR.S.EQ.FRAC)) THEN
         IF (G.NE.KG) THEN             ! THAT IS NOT THE JOKERI GAME
	    IF(TRABUF(TWKAMT).NE.0) THEN
	       FINANCE(KG,T,1) = FINANCE(KG,T,1) + 1
	       FINANCE(KG,T,2) = FINANCE(KG,T,2) +
     *                           (TRABUF(TWKAMT)*TRABUF(TWKDUR))
	    ENDIF
	 ENDIF
C
         IF(TRABUF(TWAMT).NE.0) THEN
            FINANCE(G,T,1) = FINANCE(G,T,1) + 1
            FINANCE(G,T,2) = FINANCE(G,T,2) +
     *                      (TRABUF(TWAMT)*TRABUF(TWDUR))
         ENDIF
C
 	 TOTFINA(T,1)= TOTFINA(T,1) + CNT
         TOTFINA(T,2)= TOTFINA(T,2) + TRABUF(TWTOT)
C
         TOTTIM(T,1) = TOTTIM(T,1) + CNT
         TOTTIM(T,2) = TOTTIM(T,2) + TRABUF(TWTOT)
C
         CTOTTIM(T,1,CARTEL) =
     *      CTOTTIM(T,1,CARTEL) + CNT
         CTOTTIM(T,2,CARTEL) =
     *      CTOTTIM(T,2,CARTEL) + TRABUF(TWTOT)
C
         ATOTTIM(T,1,TRM) =
     *      ATOTTIM(T,1,TRM) + CNT
         ATOTTIM(T,2,TRM) =
     *      ATOTTIM(T,2,TRM) + TRABUF(TWTOT)
C
         TIMTAB(TOFF,T,1) = TIMTAB(TOFF,T,1) + CNT
         TIMTAB(TOFF,T,2) = TIMTAB(TOFF,T,2) + TRABUF(TWTOT)
C
         CTIMTAB(TOFF,T,1,CARTEL) =
     *      CTIMTAB(TOFF,T,1,CARTEL) + CNT
         CTIMTAB(TOFF,T,2,CARTEL) =
     *      CTIMTAB(TOFF,T,2,CARTEL) + TRABUF(TWTOT)
C
         ATIMTAB(TOFF,T,1,TRM) =
     *      ATIMTAB(TOFF,T,1,TRM) + CNT
         ATIMTAB(TOFF,T,2,TRM) =
     *      ATIMTAB(TOFF,T,2,TRM) + TRABUF(TWTOT)
      ENDIF
C
C VALIDATIONS
C -----------
C
      IF(T.EQ.TVAL .AND. S.EQ.GOOD .AND. GTYP.NE.TPAS) THEN
         CNT=1
         IF(TRABUF(TVPAY).NE.0.AND.TRABUF(TVKPAY).NE.0) CNT=2
         IF(TRABUF(TVKPAY).GT.0) THEN
            KG=TRABUF(TVKGME)
            FINANCE(KG,T,1) = FINANCE(KG,T,1) + 1
            FINANCE(KG,T,2) = FINANCE(KG,T,2) + TRABUF(TVKPAY)
            TOTFINA(T,1)   = TOTFINA(T,1) + 1
            TOTFINA(T,2)   = TOTFINA(T,2) + TRABUF(TVKPAY)
         ENDIF
C
         IF(TRABUF(TVPAY).GT.0) THEN
            FINANCE(G,T,1) = FINANCE(G,T,1) + 1
            FINANCE(G,T,2) = FINANCE(G,T,2) + TRABUF(TVPAY)
            TOTFINA(T,1)   = TOTFINA(T,1) + 1
            TOTFINA(T,2)   = TOTFINA(T,2) + TRABUF(TVPAY)
         ENDIF
C
         IF(TRABUF(TVREF).GT.0) THEN
            VAL_REF(G,1) = VAL_REF(G,1) + 1
            VAL_REF(G,2) = VAL_REF(G,2) + TRABUF(TVREF)
            TOTFINA(TREF,1) = TOTFINA(TREF,1) + 1
            TOTFINA(TREF,2) = TOTFINA(TREF,2) + TRABUF(TVREF)
         ENDIF
C
         TIMTAB(TOFF,T,1) = TIMTAB(TOFF,T,1) + CNT
         TIMTAB(TOFF,T,2) = TIMTAB(TOFF,T,2) +
     *      TRABUF(TVPAY) + TRABUF(TVKPAY)
C
         CTIMTAB(TOFF,T,1,CARTEL) =
     *      CTIMTAB(TOFF,T,1,CARTEL) + CNT
         CTIMTAB(TOFF,T,2,CARTEL) =
     *      CTIMTAB(TOFF,T,2,CARTEL) +
     *      TRABUF(TVPAY) + TRABUF(TVKPAY)
C
         ATIMTAB(TOFF,T,1,TRM) =
     *      ATIMTAB(TOFF,T,1,TRM) + CNT
         ATIMTAB(TOFF,T,2,TRM) =
     *      ATIMTAB(TOFF,T,2,TRM) +
     *      TRABUF(TVPAY) + TRABUF(TVKPAY)
C
         TOTTIM(T,1) = TOTTIM(T,1) + CNT
         TOTTIM(T,2) = TOTTIM(T,2) +
     *      TRABUF(TVPAY) + TRABUF(TVKPAY)
C
         CTOTTIM(T,1,CARTEL) = CTOTTIM(T,1,CARTEL) + CNT
         CTOTTIM(T,2,CARTEL) = CTOTTIM(T,2,CARTEL) +
     *      TRABUF(TVPAY) + TRABUF(TVKPAY)
C
         ATOTTIM(T,1,TRM) = ATOTTIM(T,1,TRM) + CNT
         ATOTTIM(T,2,TRM) = ATOTTIM(T,2,TRM) +
     *      TRABUF(TVPAY) + TRABUF(TVKPAY)
      ENDIF
C
C PASSIVE VALIDATIONS
C
      IF(T.EQ.TVAL .AND. S.EQ.GOOD .AND. GTYP.EQ.TPAS) THEN
         ! incl payments to bank IF(TRABUF(TVTYPE).EQ.VPNBNK) GOTO 1000
         IF(TRABUF(TERR).NE.0) GOTO 1000
         CNT = 0
         AMT = 0
         DO I = 1, TRABUF(TPTCK)
            IF(TRABUF(TPSTS1+OFFTRA*(I-1)).EQ.VWINNER) THEN
               AMT = AMT + TRABUF(TPPAY1+OFFTRA*(I-1))
               CNT = CNT +1
            ENDIF
         ENDDO
 
         IF(AMT.GT.0.OR.CNT.GT.0) THEN
            FINANCE(G,T,1) = FINANCE(G,T,1) + CNT
            FINANCE(G,T,2) = FINANCE(G,T,2) + AMT
            TOTFINA(T,1)   = TOTFINA(T,1) + CNT
            TOTFINA(T,2)   = TOTFINA(T,2) + AMT
C
            TIMTAB(TOFF,T,1) = TIMTAB(TOFF,T,1) + CNT
            TIMTAB(TOFF,T,2) = TIMTAB(TOFF,T,2) + AMT 
C
            CTIMTAB(TOFF,T,1,CARTEL) = CTIMTAB(TOFF,T,1,CARTEL) + CNT
            CTIMTAB(TOFF,T,2,CARTEL) = CTIMTAB(TOFF,T,2,CARTEL) + AMT
C
            ATIMTAB(TOFF,T,1,TRM) = ATIMTAB(TOFF,T,1,TRM) + CNT
            ATIMTAB(TOFF,T,2,TRM) = ATIMTAB(TOFF,T,2,TRM) + AMT
C
            TOTTIM(T,1) = TOTTIM(T,1) + CNT
            TOTTIM(T,2) = TOTTIM(T,2) + AMT
C
            CTOTTIM(T,1,CARTEL) = CTOTTIM(T,1,CARTEL) + CNT
            CTOTTIM(T,2,CARTEL) = CTOTTIM(T,2,CARTEL) + AMT
C
            ATOTTIM(T,1,TRM) = ATOTTIM(T,1,TRM) + CNT
            ATOTTIM(T,2,TRM) = ATOTTIM(T,2,TRM) + AMT
	 ENDIF
      ENDIF
C
C RETURNS
C -------
C
      IF(T.EQ.TRET .AND. S.EQ.GOOD) THEN
         AMT = 0
         DO I = 1, TRABUF(TPTCK)
            IF(TRABUF(TPSTS1+OFFTRA*(I-1)) .EQ. RETURND.OR.
     *         TRABUF(TPSTS1+OFFTRA*(I-1)) .EQ. RETAFDR) THEN
               AMT = AMT + TRABUF(TPPAY1+OFFTRA*(I-1))
            ENDIF
         ENDDO
         CNT = TRABUF(TPFRCNT)
C
         IF(AMT.GT.0.OR.CNT.GT.0) THEN
            FINANCE(G,T,1) = FINANCE(G,T,1) + CNT
            FINANCE(G,T,2) = FINANCE(G,T,2) + AMT
            TOTFINA(T,1) = TOTFINA(T,1) + CNT
            TOTFINA(T,2) = TOTFINA(T,2) + AMT
C     
            TOTTIM(T,1) = TOTTIM(T,1) + CNT
            TOTTIM(T,2) = TOTTIM(T,2) + AMT
C
            CTOTTIM(T,1,CARTEL) = CTOTTIM(T,1,CARTEL) + CNT
            CTOTTIM(T,2,CARTEL) = CTOTTIM(T,2,CARTEL) + AMT
C
            ATOTTIM(T,1,TRM) = ATOTTIM(T,1,TRM) + CNT
            ATOTTIM(T,2,TRM) = ATOTTIM(T,2,TRM) + AMT
C
            TIMTAB(TOFF,T,1) = TIMTAB(TOFF,T,1) + CNT
            TIMTAB(TOFF,T,2) = TIMTAB(TOFF,T,2) + AMT
C
            CTIMTAB(TOFF,T,1,CARTEL) = CTIMTAB(TOFF,T,1,CARTEL) + CNT
            CTIMTAB(TOFF,T,2,CARTEL) = CTIMTAB(TOFF,T,2,CARTEL) + AMT
C
            ATIMTAB(TOFF,T,1,TRM) = ATIMTAB(TOFF,T,1,TRM) + CNT
            ATIMTAB(TOFF,T,2,TRM) = ATIMTAB(TOFF,T,2,TRM) + AMT
         ENDIF
      ENDIF
C
C REFUNDS
C -------
C
      IF(T.EQ.TREF .AND. S.EQ.GOOD) THEN
         VAL_REF(G,1) = VAL_REF(G,1) + 1
         VAL_REF(G,2) = VAL_REF(G,2) + TRABUF(TVREF)
         TOTFINA(T,1) = TOTFINA(T,1) + 1
         TOTFINA(T,2) = TOTFINA(T,2) + TRABUF(TVREF)
      ENDIF
C
C INSTANT
C -------
C
      IF(T.EQ.TCRS) THEN
	 OFFSET=TRABUF(TITYP)+1
	 ALLFIN(OFFSET,1) = ALLFIN(OFFSET,1) + 1           !all instant
 	 IF(S.EQ.GOOD) THEN
	    OFFSET=TRABUF(TITYP)+1
	    INSFIN(OFFSET,1) = INSFIN(OFFSET,1) + 1         !only good instant
C
C PORTUGAL DON'T HAVE GVT, SO REMOVE THIS CODE LINES
C
C	    IF(.NOT.BTEST(AGTTAB(AGTTYP,TRM),AGTTOI)) THEN
C	       OFFSET=TRABUF(TITYP)+1
C	       GVTFIN(OFFSET,1) = GVTFIN(OFFSET,1) + 1	  !only gvt
C	    ENDIF
            IF(TRABUF(TITYP).EQ.IVAL) THEN
               DO I = 0,TRABUF(TIBCH) - 1
                  IF(I.LE.TIVMX) THEN
                     IF(TRABUF(TISTS1+I).EQ.INOER) THEN
                        INSVAL(1) = INSVAL(1) + 1
                        INSVAL(2) = INSVAL(2) + TRABUF(TIPRZ1+I)
		     ENDIF
                  ENDIF
               ENDDO
	    ENDIF
	 ELSE
C
C COUNT TIMED OUT VALIDATIONS...
C
            IF(TRABUF(TITYP).EQ.IVAL.AND.TRABUF(TIERR).EQ.INTIM.AND.
     *         TRABUF(TTSTCS).EQ.1) THEN ! GVT'S ONLY....
               DO I = 0,TRABUF(TIBCH) - 1
                  IF(I.LE.TIVMX) THEN
                     IF(TRABUF(TISTS1+I).EQ.INTIM) THEN
                        TIMED_OUT(1) = TIMED_OUT(1) + 1
                        TIMED_OUT(2) = TIMED_OUT(2) + TRABUF(TIPRZ1+I)
		     ENDIF
                  ENDIF
               ENDDO
	    ENDIF
	 ENDIF	  
      ENDIF
C
      GOTO 1000
C
500   CONTINUE
      TYPE*,IAM(),'Producing System Sales Report'
      ST = LIB$GET_LUN(LUN)
      IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))	
      CALL ROPEN('SYSSUM.REP',LUN,ST)
      IF(ST.NE.0) THEN
         TYPE*,IAM(),'Error opening SYSSUM.REP > ',ST
         CLOSE(LUN)
         CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
C PRODUCE REPORT
C --------------
C
      CALL FIGWEK(DAYCDC,WEEK,YEAR)
      ENCODE(42,8001,HEAD) WEEK, YEAR
C
      CALL TITLE(HEAD,'TMSCAN  ',1,LUN,PAGE,DAYCDC)
      WRITE(LUN,9000)
C
C TRANSACTION STATUS TABLE
C ------------------------
C
      WRITE(LUN,9001)
      WRITE(LUN,9002) (STAT(K),K=1,FRAC)
      DO 800 I=1,TCRS
         WRITE(LUN,9003) TTNAMES(I),(TRNSTAT(I,K),K=1,FRAC)
800   CONTINUE
      WRITE(LUN,9004) (TOTSTAT(K),K=1,FRAC)
C
C WAGER STATUS TABLE
C ------------------
C
      WRITE(LUN,9005)
      WRITE(LUN,9002) (STAT(K),K=1,FRAC)
      DO 810 I=1,MAXGAM
         GTYP=GNTTAB(GAMTYP,I)
         IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) GOTO 810
         WRITE(LUN,9024) (GLNAMES(L,I),L=1,4),
     *                     (WAGSTAT(I,K),K=1,FRAC)
810   CONTINUE
      WRITE(LUN,9004) (TOTWAGS(K),K=1,FRAC)
C
C FINANCIAL TABLE
C ---------------
C
      WRITE(LUN,9006)
      DO 820 I=1,MAXGAM
         GTYP=GNTTAB(GAMTYP,I)
         IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) GOTO 820
         WRITE(LUN,9007) (GLNAMES(K,I),K=1,4),
     *                (FINANCE(I,K,1),FINANCE(I,K,2)/CONV_UNIT,
     *                 (MOD(FINANCE(I,K,2),CONV_UNIT)),K=1,TRET)
820   CONTINUE
C
      WRITE(LUN,9011) (TOTFINA(K,1),TOTFINA(K,2)/CONV_UNIT,
     *               (MOD(TOTFINA(K,2),CONV_UNIT)),K=1,TRET)
C
C INSTANT TABLES
C --------------
C
      WRITE(LUN,9030)
      WRITE(LUN,9033) GTNAMES(TINS),
     *                  (GVTFIN(K,1),K=1,NUMCRS)
C
      WRITE(LUN,9031)
      WRITE(LUN,9033) GTNAMES(TINS),
     *                  (INSFIN(K,1),K=1,NUMCRS)
C
      WRITE(LUN,9032)
      WRITE(LUN,9033) GTNAMES(TINS),
     *                  (ALLFIN(K,1),K=1,NUMCRS)
      WRITE(LUN,9034)
      WRITE(LUN,9035) INSVAL(1),CSMONY(INSVAL(2),10,1)
      WRITE(LUN,9036) TIMED_OUT(1),TIMED_OUT(2)/10,MOD(TIMED_OUT(2)*10,100)
C
C PRODUCE TIME INTERVAL TABLE PORTION
C -----------------------------------
C
      TCARTA  = 0
      ATCARTA = 0
C
      DO 840 CARTEL=1,MAXCART
         CTOTTMT = 0
C
         DO 825 I = 1,TRET
            CTOTTMT = CTOTTMT + CTOTTIM(I,2,CARTEL)
825      CONTINUE
C
         IF (CTOTTMT.EQ.0) GOTO 840
C
         CARTA  = 0
         ACARTA = 0
C
         DO 826 I=1,NUMAGT
            IF(CARTAB(2,I).EQ.0) GOTO 826 
            IF (CARTAB(1,I).EQ.CARTEL-1) THEN
               CARTA  = CARTA + 1
               TCARTA = TCARTA + 1
               IF (TERTAB(I).NE.0) THEN
                  ACARTA  = ACARTA + 1
                  ATCARTA = ATCARTA + 1
               ENDIF
            ENDIF
826      CONTINUE
C
         CALL TITLE(HEAD,'TMSCAN  ',1,LUN,PAGE,DAYCDC)
         WRITE(LUN,9000)
         WRITE(LUN,9020) CARTEL-1,CARTA,ACARTA
         WRITE(LUN,9008)
C
         DO 830 I=1,24
            TM=I-1
            WRITE(LUN,9009) TM,TM,
     *            (CTIMTAB(TM,J,1,CARTEL),
     *            CTIMTAB(TM,J,2,CARTEL)/CONV_UNIT,
     *            (MOD(CTIMTAB(TM,J,2,CARTEL),CONV_UNIT)),J=1,TRET)
830      CONTINUE
C
         WRITE(LUN,9010) (CTOTTIM(I,1,CARTEL),
     *         CTOTTIM(I,2,CARTEL)/CONV_UNIT,
     *         (MOD(CTOTTIM(I,2,CARTEL),CONV_UNIT)),I=1,TRET)
840   CONTINUE
C
C TOTALS, IE. ALL CARTELS TOGETHER
C --------------------------------
C
      CALL TITLE(HEAD,'TMSCAN  ',1,LUN,PAGE,DAYCDC)
      WRITE(LUN,9000)
      WRITE(LUN,9021) TCARTA,ATCARTA
      WRITE(LUN,9008)
C
      DO 860 I=1,24
         TM=I-1
         WRITE(LUN,9009) TM,TM,(TIMTAB(TM,J,1),
     *         TIMTAB(TM,J,2)/CONV_UNIT,
     *         (MOD(TIMTAB(TM,J,2),CONV_UNIT)),J=1,TRET)
860   CONTINUE
C
      WRITE(LUN,9010) (TOTTIM(I,1),TOTTIM(I,2)/CONV_UNIT,
     *               (MOD(TOTTIM(I,2),CONV_UNIT)),I=1,TRET)
C
C MAKE AGENT BASED TIME INTERVAL REPORT
C -------------------------------------
C
      IF (DETCART.EQ.-1) GOTO 990
C
C SORT TABLES BY AGENT NUMBER
C ---------------------------
C
      IF(AGT_LOOKUP_CNT.LE.0) THEN
	 TYPE*,IAM(),'SYSTRPT - AGENT COMMON IS NOT UPDATED....'
	 CALL GSTOP(GEXIT_FATAL)
      ENDIF
      DO I=1,AGT_LOOKUP_CNT
         SORTTAB(1,I) = AGT_LOOKUP_TER(I)
         SORTTAB(2,I) = CARTAB(2,AGT_LOOKUP_TER(I))
      ENDDO
C
C REPORT AGENTS
C -------------
C
      CALL TITLE(HEAD,'TMSCAN  ',1,LUN,PAGE,DAYCDC)
      WRITE(LUN,9022) DETCART
C
      DO 940 T=1,NUMAGT
         TRM = SORTTAB(1,T)
	 IF(TRM.EQ.0) GOTO 940
C
         IF (CARTAB(1,TRM).NE.DETCART) GOTO 940
C
         ATOTTMT = 0
         DO 915 I = 1,TRET
            ATOTTMT = ATOTTMT + ATOTTIM(I,2,TRM)
915      CONTINUE
C
         IF (ATOTTMT.EQ.0) GOTO 940
C
         CALL TITLE(HEAD,'TMSCAN  ',1,LUN,PAGE,DAYCDC)
         WRITE(LUN,9000)
         WRITE(LUN,9023) SORTTAB(2,T)/10,
     *                     MOD(SORTTAB(2,T),10)
         WRITE(LUN,9008)
C
         DO 930 I=1,24
            TM=I-1
            WRITE(LUN,9009) TM,TM,
     *            (ATIMTAB(TM,J,1,TRM),
     *            ATIMTAB(TM,J,2,TRM)/CONV_UNIT,
     *            (MOD(ATIMTAB(TM,J,2,TRM),CONV_UNIT)),J=1,TRET)
930      CONTINUE
C
         WRITE(LUN,9010) (ATOTTIM(I,1,TRM),
     *         ATOTTIM(I,2,TRM)/CONV_UNIT,
     *         (MOD(ATOTTIM(I,2,TRM),CONV_UNIT)),I=1,TRET)
940   CONTINUE
C
C SPOOL REPORT TO THE PRINTER
C ---------------------------
C
990   CONTINUE
      CLOSE(LUN)
      ST = LIB$FREE_LUN(LUN)	
      CALL SPOOL('SYSSUM.REP',COPY,ST)
C
C WRITE FINANCE TOTALS TO BALANSFILE         V04
C ----------------------------------
C
	CALL FASTMOV(FINANCE,GAMESUMS,MAXGAM*NUMFIN*NUMTOT)
C
        DO GAM = 1, MAXGAM
           GAMESUMS(GAM,TVAL,1) = GAMESUMS(GAM,TVAL,1) + VAL_REF(GAM,1)
           GAMESUMS(GAM,TVAL,2) = GAMESUMS(GAM,TVAL,2) + VAL_REF(GAM,2)
        END DO
C
        TOTFINA(TVAL,1) = TOTFINA(TVAL,1) + TOTFINA(TREF,1)
        TOTFINA(TVAL,2) = TOTFINA(TVAL,2) + TOTFINA(TREF,2)
C
        I = 0
        DO TYP = 1,TRET
            I = I + 1
            TOTSUMS(I) = TOTFINA(TYP,1)
            I = I + 1
            TOTSUMS(I) = TOTFINA(TYP,2)
        END DO
C
        RAPCODE = 1
        CALL BALWRI(RAPCODE,GAMESUMS,TOTSUMS,TOTREAL)
C
1000  CONTINUE
      RETURN
C
C     ======================= FORMAT STATEMENTS ====================
C
8001  FORMAT('SYSTEM TRANSACTION REPORT FOR WEEK ',I2.2,'/',I4.4)
9000  FORMAT(1X,131('='),///)
9001  FORMAT(36X,10('* '),' TRANSACTION STATUS SUMARY ',10(' *'),/)
9002  FORMAT(22X,11(6X,A4),/)
9003  FORMAT(10X,A8,':',3X,11(2X,I8))
9004  FORMAT(/,10X,'TOTALS  :',3X,11(2X,I8))
9005  FORMAT(//,36X,10('* '),' WAGER STATUS TABLE ',10(' *'),/)
9006  FORMAT(//,30X,10('* '),' FINANCIAL SUMMARY TABLE ',10(' *'),//,
     *       22X,'------NET WAGERS-----',2X,'------CANCELS-------',2X,
     *       '-------DELETES------',2X,'--------CASHES------',2X,
     *       '-------RETURNS------', /,22X,
     *       'COUNT',10X,'AMOUNT',4(2X,'COUNT',9X,'AMOUNT'),/)
9007  FORMAT(2X,4A4,':',3X,I7,1X,I10,'.',I2.2,
     *       4(2X,I6,1X,I10,'.',I2.2))
9008   FORMAT(4X,'TIME INTERVAL',5X,'--------SALES--------',2X,
     *       '-------CANCELS------',2X,'-------DELETES------',2X,
     *       '-------CASHES-------',2X,'-------RETURNS------',/,
     *        22X,'COUNT',10X,'AMOUNT',4(2X,'COUNT',9X,'AMOUNT'),/)
9009  FORMAT(4X,I2.2,':00 TO ',I2.2,':59',4X,I7,1X,I10,'.',I2.2,
     *       4(2X,I6,1X,I10,'.',I2.2))
9010  FORMAT(/,10X,'TOTALS  :',2X,I8,1X,I10,'.',I2.2,
     *       4(2X,I6,1X,I10,'.',I2.2))
9011  FORMAT(/,10X,'TOTALS  :',3X,I7,1X,I10,'.',I2.2,
     *       4(2X,I6,1X,I10,'.',I2.2))
C
9020  FORMAT(//,11X,'CARTEL',I4,', ',I4,' AGENTS, ',I4,' ACTIVE',
     *       /,11X,'------------------------------------',//)
9021  FORMAT(//,11X,'ALL CARTELS, ',I4,' AGENTS, ',I4,' ACTIVE',
     *       /,11X,'-------------------------------------',//)
9022  FORMAT(//////////,50X,'C A R T E L ',I4,'  A G E N T S',
     *       /,50X,'-----------------------------')
9023  FORMAT(//,11X,'AGENT ',I6.6,'-',I1,
     *       /,11X,'--------------',//)
9024  FORMAT(2X,4A4,':',3X,11(2X,I8))
C
9030  FORMAT(//,26X,10('* '),'  GVT INSTANT SUMMARY TABLE  ',10(' *'),//,
     *         11X,'ESTABL',1X,' ISSUE',1X,
     *             '   LOT',1X,' QUOTA',1X,
     *             'INVTRY',1X,'FINACL',1X,
     *             'SPLORD',1X,'GAMNAM',1X,
     *             'BK RPT',1X,'SIGNON',1X,
     *             'SIGNOF',1X,'CONFIR',1X,
     *             'FSE SO',1X,'   VAL',1X,
     *             ' ALARM',1X,'GAMTBL',/,
     *         10X,<NUMCRS>(2X,'COUNT'),/)
9031  FORMAT(//,25X,10('* '),'  GOOD INSTANT SUMMARY TABLE  ',10(' *'),//,
     *         11X,'ESTABL',1X,' ISSUE',1X,
     *             '   LOT',1X,' QUOTA',1X,
     *             'INVTRY',1X,'FINACL',1X,
     *             'SPLORD',1X,'GAMNAM',1X,
     *             'BK RPT',1X,'SIGNON',1X,
     *             'SIGNOF',1X,'CONFIR',1X,
     *             'FSE SO',1X,'   VAL',1X,
     *             ' ALARM',1X,'GAMTBL',/,
     *         10X,<NUMCRS>(2X,'COUNT'),/)
9032  FORMAT(//,26X,10('* '),'  ALL INSTANT SUMMARY TABLE  ',10(' *'),//,
     *         11X,'ESTABL',1X,' ISSUE',1X,
     *             '   LOT',1X,' QUOTA',1X,
     *             'INVTRY',1X,'FINACL',1X,
     *             'SPLORD',1X,'GAMNAM',1X,
     *             'BK RPT',1X,'SIGNON',1X,
     *             'SIGNOF',1X,'CONFIR',1X,
     *             'FSE SO',1X,'   VAL',1X,
     *             ' ALARM',1X,'GAMTBL',/,
     *         10X,<NUMCRS>(2X,'COUNT'),/)
9033  FORMAT(1X,A8,':',<NUMCRS>(1X,I6),/)
9034  FORMAT(//,26X,10('* '),'  ACTUAL INSTANT VALIDATIONS  ',10(' *'),//,
     *         11X,'  COUNT',1X,'    AMOUNT',/)
9035  FORMAT(10X,1X,I7,1X,A10,/)
9036  FORMAT(1X,'Timed out: ',I6,1X,I7,'.',I2.2,/)
C
      END

