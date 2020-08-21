C AGTACTSUB.FOR
C
C V07 03-AUG-2011 RXK Fix for total ticket charge
C V06 11-MAR-2010 RXK Claims replaced with returns
C V05 29-NOV-2000 UXN TotoGola added.
C V04 16-OCT-2000 UXN Initial release for Portugal
C V03 13-OCT-1999 RXK World Tour added.
C V02 10-MAY-1999 UXN Today's Triple changed to Today's Trio.
C                     Super Triple added.
C V01 13-NOV-1997 UXN Initial release (produced from AGTACT.FOR)
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE AGTACTSUB
	IMPLICIT NONE
C
	INCLUDE '(LIB$ROUTINES)'
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:ASFREP.DEF'
C
        INTEGER*4  MAX_INDEX                ! Max value if index for TOT_AMT,
        PARAMETER (MAX_INDEX = MAXTYP + 7)  ! TOT_CNT etc

	INTEGER*4 ADJUST(2)		    !
	INTEGER*4 CANIND		    !
	INTEGER*4 CLMIND		    !returns
	INTEGER*4 DSCIND		    !
	INTEGER*4 SALIND		    !
	INTEGER*4 VALIND		    !
	INTEGER*4 REFIND		    !
	INTEGER*4 TKCIND		    !

	INTEGER*4 AMTDUE(2)		    !
	INTEGER*4 COMAMT(2)		    !
	INTEGER*4 NETSAL		    !
	INTEGER*4 GTYP			    !
        INTEGER*4 GIND                      !
	INTEGER*4 GAM			    !
	INTEGER*4 SCOM(2)		    !
	INTEGER*4 I			    !
	INTEGER*4 ST			    !status after call
	INTEGER*4 PAGE			    !
	INTEGER*4 LINCNT/999/		    !
	INTEGER*4 COPY			    !number of report copies
	INTEGER*4 VCOM(2)		    !
	INTEGER*4 DAT			    !
	INTEGER*4 K			    !
	INTEGER*4 TOT_ADJUST(2)		    !
	INTEGER*4 DUEDUE(2)		    !
	INTEGER*4 TCOM(2)		    !
	INTEGER*4 GAMACT(MAXGAM)	    !is game active or not
	INTEGER*4 TOT_AMT(MAX_INDEX)/MAX_INDEX*0/	    !
	INTEGER*4 TOT_CNT(MAX_INDEX)/MAX_INDEX*0/	    !
	INTEGER*4 TOT_SCOM(2)		    !
	INTEGER*4 TOT_DUE(2)		    !
	INTEGER*4 TOT_AMTDUE(2)		    !
	INTEGER*4 TOT_VCOM(2)		    !
	INTEGER*4 TOT_COM(2)		    !
	INTEGER*4 TOTGAM(GTKCHG,MAXGAM)	    !
        INTEGER*4 TOTGAM_FOR_COM(GTKCHG,MAXGAM) !
	INTEGER*4 TOTGAM_CNT/0/		    !
	INTEGER*4 TOTGAM_AMT/0/		    !
	INTEGER*4 TOTPRV(3,MAXGAM)	    !
	INTEGER*4 TOTGAM_TCOM(2)/2*0/	    !
        INTEGER*4 GAMECOMS(MAXGAM)
        INTEGER*4 TOTCOM
        INTEGER*4 TOTDUE(2)   

	INTEGER*4 CNT(MAX_INDEX)	    !
	INTEGER*4 AMT(MAX_INDEX)	    !

        INTEGER*4  IND                      !
        INTEGER*4  TTYP                     !
        INTEGER*4  GAMESUMS(MAXGAM,NUMFIN,NUMTOT) !
        INTEGER*4  TOTSUMS(NO_BALSUMS)      ! 
        INTEGER*4  RAPCODE                  !
        INTEGER*4  ODDS_TOT(2)              !
        INTEGER*4  SCR_COM(2) /2*0/
        INTEGER*4  WIT_COM(2) /2*0/
        INTEGER*4  CPL_COM(2) /2*0/
        INTEGER*4  DBL_COM(2) /2*0/
        INTEGER*4  TRP_COM(2) /2*0/
        INTEGER*4  STR_COM(2) /2*0/
        INTEGER*4  SSC_COM(2) /2*0/
        INTEGER*4  NON_COMM(MAXGAM,2)
        INTEGER*4  NON_COMM_TEMP(2)
        INTEGER*4  GAM_COMM(2,MAXGAM)

        REAL*8     TOTREAL                  !

	DATA	  TOT_VCOM/2*0/		    !
	DATA	  TOT_SCOM/2*0/		    !
	DATA	  TOT_COM/2*0/		    !

	INTEGER*4 PRECAR,CARTEL
	INTEGER*4 LUN

C
C ENTRY AGTACT_BEGIN
C
	ENTRY AGTACT_BEGIN
C
	TYPE*,IAM(),'**** AGTACT Agent Daily Summary Report ****'
C 
        CALL FASTSET(0,TOTGAM,GTKCHG*MAXGAM)
        CALL FASTSET(0,TOTGAM_FOR_COM,GTKCHG*MAXGAM)
	CALL FASTSET(0,TOTPRV,3*MAXGAM)
        CALL FASTSET(0,GAMECOMS,MAXGAM)
        SALIND=MAXTYP+1
        VALIND=MAXTYP+2
        CANIND=MAXTYP+3
        CLMIND=MAXTYP+4
	REFIND=MAXTYP+5
        DSCIND=MAXTYP+6
        TKCIND=MAXTYP+7
	DAT=DAYCDC

C       GET NUMBER OF REPORT COPIES
C       ---------------------------
C***	CALL INPNUM('Enter number of report copies ',COPY,0,20,EXT)
C***	IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
	COPY=0

C       OPEN THE REPORT FILE
C       --------------------
	ST = LIB$GET_LUN(LUN)
	IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
	CALL ROPEN('AGTACT.REP',LUN,ST)
	IF(ST.NE.0) THEN
	   TYPE*,IAM(),'AGTACT.REP Open error  st - ',ST
	   CALL USRCLOS1(LUN)
	   CALL GSTOP(GEXIT_FATAL)
	ENDIF
	RETURN
C
C ENTRY AGTACT_UPDATE
C
	ENTRY AGTACT_UPDATE()
C
C	   CLEAR ACCUMULATORS
C	   ------------------
           CALL FASTSET(0,AMT,MAX_INDEX)
           CALL FASTSET(0,CNT,MAX_INDEX)
           SCOM(1)=0
           SCOM(2)=0
	   VCOM(1)=0
	   VCOM(2)=0
           TCOM(1)=0
           TCOM(2)=0
           ADJUST(1)=0
           ADJUST(2)=0

C          SALES DATA FROM FILE
C	   --------------------
	   DO 150 GAM=1,MAXGAM
	      NETSAL=0
	      NON_COMM_TEMP(1)=0
	      NON_COMM_TEMP(2)=0
	      GTYP=GNTTAB(GAMTYP,GAM)
              GIND=GNTTAB(GAMIDX,GAM)
	      IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) GOTO 150

	      CNT(GTYP)=CNT(GTYP)+ASFDAY(GSCNT,GAM,1)-
     *	                          ASFDAY(GCCNT,GAM,1)
	      AMT(GTYP)=AMT(GTYP)+ASFDAY(GSAMT,GAM,1)-
     *	                          ASFDAY(GCAMT,GAM,1)

	      CNT(SALIND)=CNT(SALIND)+ASFDAY(GSCNT,GAM,1)  !gross sales count
	      AMT(SALIND)=AMT(SALIND)+ASFDAY(GSAMT,GAM,1)  !gross sales amount

	      CNT(VALIND)=CNT(VALIND)+ASFDAY(GVCNT,GAM,1)
	      AMT(VALIND)=AMT(VALIND)+ASFDAY(GVAMT,GAM,1)

	      CNT(CANIND)=CNT(CANIND)+ASFDAY(GCCNT,GAM,1)
	      AMT(CANIND)=AMT(CANIND)+ASFDAY(GCAMT,GAM,1)

	      CNT(CLMIND)=CNT(CLMIND)+ASFDAY(GCLCNT,GAM,1)
	      AMT(CLMIND)=AMT(CLMIND)+ASFDAY(GCLAMT,GAM,1)

              CNT(REFIND)=CNT(REFIND)+ASFDAY(GRCNT,GAM,1)
              AMT(REFIND)=AMT(REFIND)+ASFDAY(GRAMT,GAM,1)

	      CNT(DSCIND)=CNT(DSCIND)+ASFDAY(GDCNT,GAM,1)
	      AMT(DSCIND)=AMT(DSCIND)+ASFDAY(GDAMT,GAM,1)

              AMT(TKCIND)=AMT(TKCIND)+ASFDAY(GTKCHG,GAM,1)
	      NETSAL=ASFDAY(GSAMT,GAM,1)-ASFDAY(GCAMT,GAM,1)
	      CALL GETCOM(NETSAL,TWAG,GAM,COMAMT,SCOM,GTYP,GIND,XREC)

	      IF(TSBIT(AGTTAB(AGTTYP,XREC),AGTNCM).NE.0) THEN
		IF(ASFDAY(GSAMT,GAM,1).NE.0) THEN
	          NON_COMM(GAM,1)=NON_COMM(GAM,1)+COMAMT(1)
	          NON_COMM(GAM,2)=NON_COMM(GAM,2)+COMAMT(2)
		ENDIF
	      ELSE
C
C COMMISSION
C
                CALL ADDI8I8(GAM_COMM(1,GAM),COMAMT,BETUNIT)
                CALL ADDI8I8(TOTGAM_TCOM,COMAMT,BETUNIT)

                GAMECOMS(GAM) = GAMECOMS(GAM) + COMAMT(1)

                IF(GTYP.EQ.TSCR) CALL ADDI8I8(SCR_COM,COMAMT,BETUNIT)
                IF(GTYP.EQ.TWIT) CALL ADDI8I8(WIT_COM,COMAMT,BETUNIT)
                IF(GTYP.EQ.TDBL) CALL ADDI8I8(DBL_COM,COMAMT,BETUNIT)
                IF(GTYP.EQ.TCPL) CALL ADDI8I8(CPL_COM,COMAMT,BETUNIT)
                IF(GTYP.EQ.TSSC) CALL ADDI8I8(SSC_COM,COMAMT,BETUNIT)
                IF(GTYP.EQ.TTRP) CALL ADDI8I8(TRP_COM,COMAMT,BETUNIT)
                IF(GTYP.EQ.TSTR) CALL ADDI8I8(STR_COM,COMAMT,BETUNIT)

	      ENDIF

	      DO 160 I=1,GTKCHG
	          TOTGAM(I,GAM)=TOTGAM(I,GAM)+ASFDAY(I,GAM,1)
                     TOTGAM_FOR_COM(I,GAM) = TOTGAM_FOR_COM(I,GAM) + 
     *                                       ASFDAY(I,GAM,1)
160	      CONTINUE
	    
              IF(TSBIT(AGTTAB(AGTTYP,XREC),AGTNCM).NE.0) THEN
                TOTPRV(1,GAM)=TOTPRV(1,GAM)+ASFDAY(GVAMT,GAM,1)
                TOTPRV(2,GAM)=TOTPRV(2,GAM)+ASFDAY(GRAMT,GAM,1)
	      ENDIF

150	   CONTINUE

           CALL GETCOM(AMT(VALIND),TVAL,GAM,COMAMT,VCOM,GTYP,GIND,XREC)
           CALL GETCOM(AMT(REFIND),TREF,GAM,COMAMT,VCOM,GTYP,GIND,XREC)

	   DO 155 I=1,15
	      IF(ASFLGR(LGRCDC,I).NE.DAT) GOTO 155
	      CALL ADDI8I8(ADJUST,ASFLGR(LGRAMTU,I),BETUNIT)
155	   CONTINUE

	   IF(TSBIT(AGTTAB(AGTTYP,XREC),AGTNCM).NE.0) THEN
	     SCOM(1)=0
	     SCOM(2)=0
	     VCOM(1)=0
	     VCOM(2)=0
             TCOM(1)=0
             TCOM(2)=0
	   ENDIF

	   AMTDUE(1)=0
	   AMTDUE(2)=0
	   DUEDUE(1)=0
	   DUEDUE(2)=0

	   CALL ADDI8I4(AMTDUE,AMT(SALIND),BETUNIT)

           CALL SUBI8I4(AMTDUE,AMT(VALIND),VALUNIT)
	   CALL SUBI8I4(AMTDUE,AMT(REFIND),BETUNIT)
	   CALL SUBI8I4(AMTDUE,AMT(DSCIND),BETUNIT)

	   CALL ADDI8I8(DUEDUE,AMTDUE,BETUNIT)
           CALL SUBI8I8(DUEDUE,AMT(CANIND),BETUNIT)

	   CALL ADDI8I8(AMTDUE,ADJUST,BETUNIT)
C***       CALL ADDI8I4(AMTDUE,AMT(TKCIND),BETUNIT)

	   CALL SUBI8I8(AMTDUE,SCOM,BETUNIT)
	   CALL SUBI8I8(AMTDUE,VCOM,BETUNIT)
	   CALL SUBI8I4(AMTDUE,AMT(CANIND),BETUNIT)

           CALL ADDI8I8(TCOM,SCOM,BETUNIT)
	   CALL ADDI8I8(TCOM,VCOM,BETUNIT)

C   	   PROCESS CARTEL TOTALS
C 	   ---------------------
           CARTEL = 0
           CALL ASCBIN(ASFINF,SCHAN,LCHAN,CARTEL,ST)
           PRECAR = CARTEL

C          WRITE TITLE IF NEEDED
C	   ---------------------
	   IF(LINCNT.GT.LINSPP) THEN
	      CALL TITLE('AGENT ACTIVITY DAILY REPORT',
     *	                 '  AGTACT',1,LUN,PAGE,DAYCDC)
              WRITE(LUN,908)
	      WRITE(LUN,900)
	     LINCNT=8
	   ENDIF

 900  FORMAT(26X,'--GROSS SALES-- ',
     *       '   --CANCELS--        --VALIDS-- ',
C    *       '      --RETURN-- ','     --DEVOL---',
     *       '      --RETURN-- ',15X,
     *       /,3X,'AGENT CARTEL TERMI',4X,
     *       4('COUNT     AMOUNT',2X),11X,
     *       'COMMISSION    AMOUNT DUE')

	   WRITE(LUN,901) (ASFBYT(K),K=1,LAGNO),
     *			PRECAR,XREC,
     *			CNT(SALIND),CSMONY(AMT(SALIND),11,BETUNIT),

     *			CNT(CANIND),CMONY(AMT(CANIND),11,BETUNIT),
     *			CNT(VALIND),CMONY(AMT(VALIND),11,VALUNIT),
     *                  CNT(CLMIND),CMONY(AMT(CLMIND),11,BETUNIT),

     *			CMONYI8(TCOM(1),11,BETUNIT),
     *			CSMONYI8(AMTDUE(1),12,BETUNIT)

901	FORMAT(1X,<LAGNO>(A1),
     *         3X,I4.4,I6,            ! PRECAR,XREC
     *         3X,I6,A11,       ! GROSS AMT
     *         (1X,I6,A11),     ! CANCEL
     *         (1X,I6,A11),     ! VALIDATION
     *         (1X,I6,A11),     ! RETURNS
     *         11X,             ! 
     *         1X,A11,                 ! TCOM
     *         A12)                    ! AMT DUE

	   LINCNT=LINCNT+1

C          UPDATE GRAND TOTALS
C	   -------------------
	   DO 120 I = 1, MAX_INDEX
	      TOT_CNT(I) = TOT_CNT(I) + CNT(I)
	      TOT_AMT(I) = TOT_AMT(I) + AMT(I)
120	   CONTINUE

           ! salind is net sales for  TOT_
           TOT_CNT(SALIND) = TOT_CNT(SALIND) - AMT(CANIND)
           TOT_AMT(SALIND) = TOT_AMT(SALIND) - AMT(CANIND)          

	   CALL ADDI8I8(TOT_ADJUST,ADJUST,BETUNIT)
	   CALL ADDI8I8(TOT_AMTDUE,AMTDUE,BETUNIT)
	   CALL ADDI8I8(TOT_DUE,DUEDUE,BETUNIT)
	   CALL ADDI8I8(TOT_SCOM,SCOM,BETUNIT)
	   CALL ADDI8I8(TOT_VCOM,VCOM,BETUNIT)
	RETURN
C
C ENTRY AGTACT_END
C
	ENTRY AGTACT_END()
C
	CALL ADDI8I8(TOT_COM,TOT_SCOM,BETUNIT)
	CALL ADDI8I8(TOT_COM,TOT_VCOM,BETUNIT)

C       WRITE OUT TOTAL PAGE
C       --------------------
        CALL TITLE('AGENT ACTIVITY DAILY REPORT',
     *	           '  AGTACT',1,LUN,PAGE,DAYCDC)
        WRITE(LUN,908)
	WRITE(LUN,902) CSMONY(TOT_AMT(TLTO),12,BETUNIT),
     *		     CSMONY(TOT_AMT(TSPT),12,BETUNIT),
     *               CSMONY(TOT_AMT(TKIK),12,BETUNIT),
C    *               CSMONY(TOT_AMT(TNBR),12,BETUNIT),
C    *               CSMONY(TOT_AMT(TSCR),12,BETUNIT),
C    *               CSMONY(TOT_AMT(TWIT),12,BETUNIT),
C    *               CSMONY(TOT_AMT(TTSL),12,BETUNIT),
C    *               CSMONY(TOT_AMT(TBNG),12,BETUNIT),
C    *               CSMONY(TOT_AMT(TDBL),12,BETUNIT),
C    *               CSMONY(TOT_AMT(TCPL),12,BETUNIT),
C    *               CSMONY(TOT_AMT(TSSC),12,BETUNIT),
C    *               CSMONY(TOT_AMT(TTRP),12,BETUNIT),
C    *               CSMONY(TOT_AMT(TSTR),12,BETUNIT),
     *               CSMONY(TOT_AMT(TTGL),12,BETUNIT),
     *               CSMONY(TOT_AMT(TPAS),12,BETUNIT),
     *               CSMONYI8(TOT_ADJUST(1),12,BETUNIT),
     *               CMONY(TOT_AMT(SALIND),12,BETUNIT),
     *		     CMONY(TOT_AMT(CANIND),12,BETUNIT),
     *		     CMONY(TOT_AMT(VALIND),12,VALUNIT),
     *               CMONY(TOT_AMT(REFIND),12,BETUNIT),
     *		     CMONY(TOT_AMT(CLMIND),12,VALUNIT),
     *		     CSMONYI8(TOT_DUE(1),12,BETUNIT),
     *               CSMONYI8(TOT_SCOM(1),12,BETUNIT),
     *               CMONYI8(TOT_VCOM(1),12,BETUNIT),
     *		     CMONY(TOT_AMT(TKCIND),12,BETUNIT),
     *		     CSMONYI8(TOT_AMTDUE(1),12,BETUNIT)

        IF (TOT_AMTDUE(1) .GT. 0) THEN
            TOTDUE(1) = TOT_AMTDUE(1)
            TOTDUE(2) = 0 
        ELSE
            TOTDUE(1) = 0
            TOTDUE(2) = TOT_AMTDUE(1)
        END IF

C	WRITE OUT GAME TOTAL PAGE
C       -------------------------
        CALL TITLE('AGENT ACTIVITY DAILY REPORT',
     *	           '  AGTACT',1,LUN,PAGE,DAYCDC)
        WRITE(LUN,908)

C	CHECK IF A GAME IS ACTIVE BY LOOKING AT DAYHDR AND !
C	THE TOTAL FOR THAT GAME
C       ---------------------------------------------------- 
	DO 180 I=1,MAXGAM
	    IF(DAYDRW(I).GT.0)		GAMACT(I)=GAMACT(I) + 1
	    IF(TOTGAM(GSAMT,I).GT.0)	GAMACT(I)=GAMACT(I) + 1
	    IF(TOTGAM(GCAMT,I).GT.0)	GAMACT(I)=GAMACT(I) + 1
	    IF(TOTGAM(GVAMT,I).GT.0)	GAMACT(I)=GAMACT(I) + 1
	    IF(TOTGAM(GCLAMT,I).GT.0)	GAMACT(I)=GAMACT(I) + 1
	    IF(TOTGAM(GRAMT,I).GT.0)	GAMACT(I)=GAMACT(I) + 1
	    IF(TOTGAM(GTKCHG,I).GT.0)	GAMACT(I)=GAMACT(I) + 1
180	CONTINUE

C	GROSS SALES
C       -----------
	WRITE(LUN,907) ' GROSS  SALES '
	DO 200 I=1,MAXGAM
	   GTYP=GNTTAB(GAMTYP,I)
           GIND=GNTTAB(GAMIDX,I)
C	   IF(DAYDRW(I).LE.0) GOTO 190
	   IF(GAMACT(I).LE.0) GOTO 190
	   WRITE(LUN,903) (GLNAMES(K,I),K=1,4),
     *                  TOTGAM(GSCNT,I),
     *                  CSMONY(TOTGAM(GSAMT,I),12,BETUNIT)
	   TOTGAM_CNT = TOTGAM_CNT + TOTGAM(GSCNT,I)
	   TOTGAM_AMT = TOTGAM_AMT + TOTGAM(GSAMT,I)
190        CONTINUE
	   IF(GTYP.EQ.TSCR.AND.GIND.EQ.NUMSCR.OR.
     *        GTYP.EQ.TWIT.AND.GIND.EQ.NUMWIT.OR.
     *        GTYP.EQ.TCPL.AND.GIND.EQ.NUMCPL.OR.
     *        GTYP.EQ.TDBL.AND.GIND.EQ.NUMDBL.OR.
     *        GTYP.EQ.TSSC.AND.GIND.EQ.NUMSSC.OR.
     *        GTYP.EQ.TTRP.AND.GIND.EQ.NUMTRP.OR.
     *        GTYP.EQ.TSTR.AND.GIND.EQ.NUMSTR) THEN
                ODDS_TOT(1)=TOTGAM(GSCNT,I-5)+TOTGAM(GSCNT,I-4)
     *                            +TOTGAM(GSCNT,I-3)+TOTGAM(GSCNT,I-2)
     *                            +TOTGAM(GSCNT,I-1)+TOTGAM(GSCNT,I)
                ODDS_TOT(2)=TOTGAM(GSAMT,I-5)+TOTGAM(GSAMT,I-4)
     *                            +TOTGAM(GSAMT,I-3)+TOTGAM(GSAMT,I-2)
     *                            +TOTGAM(GSAMT,I-1)+TOTGAM(GSAMT,I)

                WRITE(LUN,9030) GTNAMES(GTYP),
     *                            ODDS_TOT(1),
     *                            CSMONY(ODDS_TOT(2),13,VALUNIT)
	ENDIF
200	CONTINUE

        ! store info for BALANS
        TOTSUMS(1) = TOTGAM_CNT
        TOTSUMS(2) = TOTGAM_AMT

	WRITE(LUN,904) 'TOTAL SALES',TOTGAM_CNT,
     *               CSMONY(TOTGAM_AMT,12,BETUNIT)
	TOTGAM_CNT=0
	TOTGAM_AMT=0

C       CANCELS
C       -------
	WRITE(LUN,907) '    CANCELS   '

	DO 210 I=1,MAXGAM
	   GTYP=GNTTAB(GAMTYP,I)
           GIND=GNTTAB(GAMIDX,I)
C***	   IF(DAYDRW(I).LE.0) GOTO 205
	   IF(GAMACT(I).LE.0) GOTO 205
	   WRITE(LUN,903) (GLNAMES(K,I),K=1,4),
     *                  TOTGAM(GCCNT,I),
     *                  CSMONY(TOTGAM(GCAMT,I),12,BETUNIT)
	   TOTGAM_CNT=TOTGAM_CNT+TOTGAM(GCCNT,I)
	   TOTGAM_AMT=TOTGAM_AMT+TOTGAM(GCAMT,I)
205	   CONTINUE
	   IF(GTYP.EQ.TSCR.AND.GIND.EQ.NUMSCR.OR.
     *        GTYP.EQ.TWIT.AND.GIND.EQ.NUMWIT.OR.
     *        GTYP.EQ.TCPL.AND.GIND.EQ.NUMCPL.OR.
     *        GTYP.EQ.TDBL.AND.GIND.EQ.NUMDBL.OR.
     *        GTYP.EQ.TSSC.AND.GIND.EQ.NUMSSC.OR.
     *        GTYP.EQ.TTRP.AND.GIND.EQ.NUMTRP.OR.
     *        GTYP.EQ.TSTR.AND.GIND.EQ.NUMSTR) THEN
                ODDS_TOT(1)=TOTGAM(GCCNT,I-5)+TOTGAM(GCCNT,I-4)
     *                            +TOTGAM(GCCNT,I-3)+TOTGAM(GCCNT,I-2)
     *                            +TOTGAM(GCCNT,I-1)+TOTGAM(GCCNT,I)
                ODDS_TOT(2)=TOTGAM(GCAMT,I-5)+TOTGAM(GCAMT,I-4)
     *                            +TOTGAM(GCAMT,I-3)+TOTGAM(GCAMT,I-2)
     *                            +TOTGAM(GCAMT,I-1)+TOTGAM(GCAMT,I)
                WRITE(LUN,9030) GTNAMES(GTYP),
     *                            ODDS_TOT(1),
     *                            CSMONY(ODDS_TOT(2),13,VALUNIT)
            ENDIF
210	CONTINUE

        ! store info for BALANS
        TOTSUMS(3) = TOTGAM_CNT
        TOTSUMS(4) = TOTGAM_AMT

	WRITE(LUN,904) 'TOTAL CANCELS',TOTGAM_CNT,
     *               CSMONY(TOTGAM_AMT,12,BETUNIT)
	TOTGAM_CNT=0
	TOTGAM_AMT=0

C       NET SALES
C       ---------
	WRITE(LUN,907) '  NET  SALES  '

	DO 230 I=1,MAXGAM
	   GTYP=GNTTAB(GAMTYP,I)
           GIND=GNTTAB(GAMIDX,I)
C***	   IF(DAYDRW(I).LE.0) GOTO 225
	   IF(GAMACT(I).LE.0) GOTO 225
	   WRITE(LUN,903) (GLNAMES(K,I),K=1,4),
     *                  TOTGAM(GSCNT,I)-TOTGAM(GCCNT,I),
     *                  CSMONY(TOTGAM(GSAMT,I)-TOTGAM(GCAMT,I)-
     *                         TOTGAM(GDAMT,I),12,BETUNIT)
	   TOTGAM_CNT=TOTGAM_CNT+TOTGAM(GSCNT,I)-TOTGAM(GCCNT,I)
	   TOTGAM_AMT=TOTGAM_AMT+TOTGAM(GSAMT,I)-TOTGAM(GCAMT,I)
225	   CONTINUE
	   IF(GTYP.EQ.TSCR.AND.GIND.EQ.NUMSCR.OR.
     *        GTYP.EQ.TWIT.AND.GIND.EQ.NUMWIT.OR.
     *        GTYP.EQ.TCPL.AND.GIND.EQ.NUMCPL.OR.
     *        GTYP.EQ.TDBL.AND.GIND.EQ.NUMDBL.OR.
     *        GTYP.EQ.TSSC.AND.GIND.EQ.NUMSSC.OR.
     *        GTYP.EQ.TTRP.AND.GIND.EQ.NUMTRP.OR.
     *        GTYP.EQ.TSTR.AND.GIND.EQ.NUMSTR) THEN
                ODDS_TOT(1)=TOTGAM(GSCNT,I-5)+TOTGAM(GSCNT,I-4)
     *                            +TOTGAM(GSCNT,I-3)+TOTGAM(GSCNT,I-2)
     *                            +TOTGAM(GSCNT,I-1)+TOTGAM(GSCNT,I)
     *				 -(TOTGAM(GCCNT,I-5)+TOTGAM(GSCNT,I-4)
     *                            +TOTGAM(GCCNT,I-3)+TOTGAM(GCCNT,I-2)
     *                            +TOTGAM(GCCNT,I-1)+TOTGAM(GCCNT,I))
                ODDS_TOT(2)=TOTGAM(GSAMT,I-5)+TOTGAM(GSAMT,I-4)
     *                            +TOTGAM(GSAMT,I-3)+TOTGAM(GSAMT,I-2)
     *                            +TOTGAM(GSAMT,I-1)+TOTGAM(GSAMT,I)
     *				  -(TOTGAM(GCAMT,I-5)+TOTGAM(GCAMT,I-4)
     *                            +TOTGAM(GCAMT,I-3)+TOTGAM(GCAMT,I-2)
     *                            +TOTGAM(GCAMT,I-1)+TOTGAM(GCAMT,I))
                WRITE(LUN,9030) GTNAMES(GTYP),
     *                            ODDS_TOT(1),
     *                            CSMONY(ODDS_TOT(2),13,VALUNIT)
	   ENDIF
230	CONTINUE

	WRITE(LUN,904) 'TOTAL NET SALES',TOTGAM_CNT,
     *               CSMONY(TOTGAM_AMT,12,BETUNIT)
	TOTGAM_CNT=0
	TOTGAM_AMT=0

C       WRITE OUT TITLE
C       ---------------
        CALL TITLE('AGENT ACTIVITY DAILY REPORT',
     *	           '  AGTACT',1,LUN,PAGE,DAYCDC)
        WRITE(LUN,908)

C       VALIDATION
C       ----------
	WRITE(LUN,907) ' VALIDATIONS  '
	DO 240 I=1,MAXGAM
	   GTYP=GNTTAB(GAMTYP,I)
           GIND=GNTTAB(GAMIDX,I)
C***	   IF(DAYDRW(I).LE.0) GOTO 240
	   IF(GAMACT(I).LE.0) GOTO 235
	   WRITE(LUN,903) (GLNAMES(K,I),K=1,4),
     *                  TOTGAM(GVCNT,I),
     *                  CSMONY(TOTGAM(GVAMT,I),12,VALUNIT)
	   TOTGAM_CNT=TOTGAM_CNT+TOTGAM(GVCNT,I)
	   TOTGAM_AMT=TOTGAM_AMT+TOTGAM(GVAMT,I)
235	   CONTINUE
	   IF(GTYP.EQ.TSCR.AND.GIND.EQ.NUMSCR.OR.
     *        GTYP.EQ.TWIT.AND.GIND.EQ.NUMWIT.OR.
     *        GTYP.EQ.TCPL.AND.GIND.EQ.NUMCPL.OR.
     *        GTYP.EQ.TDBL.AND.GIND.EQ.NUMDBL.OR.
     *        GTYP.EQ.TSSC.AND.GIND.EQ.NUMSSC.OR.
     *        GTYP.EQ.TTRP.AND.GIND.EQ.NUMTRP.OR.
     *        GTYP.EQ.TSTR.AND.GIND.EQ.NUMSTR) THEN
              ODDS_TOT(1)=TOTGAM(GVCNT,I-5)+TOTGAM(GVCNT,I-4)
     *                            +TOTGAM(GVCNT,I-3)+TOTGAM(GVCNT,I-2)
     *                            +TOTGAM(GVCNT,I-1)+TOTGAM(GVCNT,I)
              ODDS_TOT(2)=TOTGAM(GVAMT,I-5)+TOTGAM(GVAMT,I-4)
     *                            +TOTGAM(GVAMT,I-3)+TOTGAM(GVAMT,I-2)
     *                            +TOTGAM(GVAMT,I-1)+TOTGAM(GVAMT,I)
              WRITE(LUN,9030) GTNAMES(GTYP),
     *                            ODDS_TOT(1),
     *                            CSMONY(ODDS_TOT(2),13,VALUNIT)
	   ENDIF
240	CONTINUE

        ! store info for BALANS
        TOTSUMS(5) = TOTGAM_CNT
        TOTSUMS(6) = TOTGAM_AMT

	WRITE(LUN,904) 'TOTAL VALIDATIONS',TOTGAM_CNT,
     *               CSMONY(TOTGAM_AMT,12,VALUNIT)
	TOTGAM_CNT=0
	TOTGAM_AMT=0

C       RETURNS
C       ------
	WRITE(LUN,907) '    RETURNS   '
	DO 250 I=1,MAXGAM
	   GTYP=GNTTAB(GAMTYP,I)
           IF(GTYP.EQ.TPAS) THEN 
              GIND=GNTTAB(GAMIDX,I)
	      IF(GAMACT(I).LE.0) GOTO 250
	      WRITE(LUN,903) (GLNAMES(K,I),K=1,4),
     *                  TOTGAM(GCLCNT,I),
     *                  CSMONY(TOTGAM(GCLAMT,I),12,VALUNIT)
	      TOTGAM_CNT=TOTGAM_CNT+TOTGAM(GCLCNT,I)
	      TOTGAM_AMT=TOTGAM_AMT+TOTGAM(GCLAMT,I)
           ENDIF
250	CONTINUE

        TOTSUMS(9) = TOTGAM_CNT
        TOTSUMS(10) = TOTGAM_AMT

	WRITE(LUN,904) 'TOTAL RETURNS',TOTGAM_CNT,
     *               CSMONY(TOTGAM_AMT,12,VALUNIT)

	TOTGAM_CNT = 0
	TOTGAM_AMT = 0

C       REFUNDS
C       -------
        WRITE(LUN,907) '   REFUNDS    '
        DO 258 I=1,MAXGAM
	   GTYP=GNTTAB(GAMTYP,I)
           GIND=GNTTAB(GAMIDX,I)
C***       IF(DAYDRW(I).LE.0) GOTO 255
	   IF(GAMACT(I).LE.0) GOTO 255 
           WRITE(LUN,903) (GLNAMES(K,I),K=1,4),
     *                  TOTGAM(GRCNT,I),
     *                  CSMONY(TOTGAM(GRAMT,I),12,BETUNIT)
           TOTGAM_CNT=TOTGAM_CNT+TOTGAM(GRCNT,I)
           TOTGAM_AMT=TOTGAM_AMT+TOTGAM(GRAMT,I)
255	   CONTINUE
	   IF(GTYP.EQ.TSCR.AND.GIND.EQ.NUMSCR.OR.
     *        GTYP.EQ.TWIT.AND.GIND.EQ.NUMWIT.OR.
     *        GTYP.EQ.TCPL.AND.GIND.EQ.NUMCPL.OR.
     *        GTYP.EQ.TDBL.AND.GIND.EQ.NUMDBL.OR.
     *        GTYP.EQ.TSSC.AND.GIND.EQ.NUMSSC.OR.
     *        GTYP.EQ.TTRP.AND.GIND.EQ.NUMTRP.OR.
     *        GTYP.EQ.TSTR.AND.GIND.EQ.NUMSTR) THEN
              ODDS_TOT(1)=TOTGAM(GRCNT,I-5)+TOTGAM(GRCNT,I-4)
     *                            +TOTGAM(GRCNT,I-3)+TOTGAM(GRCNT,I-2)
     *                            +TOTGAM(GRCNT,I-1)+TOTGAM(GRCNT,I)
              ODDS_TOT(2)=TOTGAM(GRAMT,I-5)+TOTGAM(GRAMT,I-4)
     *                            +TOTGAM(GRAMT,I-3)+TOTGAM(GRAMT,I-2)
     *                            +TOTGAM(GRAMT,I-1)+TOTGAM(GRAMT,I)
              WRITE(LUN,9030) GTNAMES(GTYP),
     *                            ODDS_TOT(1),
     *                            CSMONY(ODDS_TOT(2),13,VALUNIT)
           ENDIF
258     CONTINUE

        WRITE(LUN,904) 'TOTAL REFUNDS',TOTGAM_CNT,
     *               CSMONY(TOTGAM_AMT,12,BETUNIT)

	TOTGAM_CNT = 0
	TOTGAM_AMT = 0

	CALL TITLE('AGENT ACTIVITY DAILY REPORT','  AGTACT',
     *            1,LUN,PAGE,DAYCDC)
        WRITE(LUN,908)

C       SALES COMMISION
C       ---------------
	WRITE(LUN,907) 'SALE COMMISION'

	DO 265 I=1,MAXGAM
           GTYP=GNTTAB(GAMTYP,I)
           GIND=GNTTAB(GAMIDX,I)
	   IF(GAMACT(I).LE.0) GOTO 260

	   WRITE(LUN,905) (GLNAMES(K,I),K=1,4),
     *                  CMONYI8(GAM_COMM(1,I),12,BETUNIT)

260	   CONTINUE

           IF(GTYP.EQ.TSCR.AND.GIND.EQ.NUMSCR) THEN
                WRITE(LUN,9031) GTNAMES(TSCR),
     *                            CMONYI8(SCR_COM,13,BETUNIT)
           ELSEIF(GTYP.EQ.TWIT.AND.GIND.EQ.NUMWIT) THEN
                WRITE(LUN,9031) GTNAMES(TWIT),
     *                            CMONYI8(WIT_COM,13,BETUNIT)
           ELSEIF(GTYP.EQ.TDBL.AND.GIND.EQ.NUMDBL) THEN
                WRITE(LUN,9031) GTNAMES(TDBL),
     *                            CMONYI8(DBL_COM,13,BETUNIT)
           ELSEIF(GTYP.EQ.TCPL.AND.GIND.EQ.NUMCPL) THEN
                WRITE(LUN,9031) GTNAMES(TCPL),
     *                            CMONYI8(CPL_COM,13,BETUNIT)
           ELSEIF(GTYP.EQ.TSSC.AND.GIND.EQ.NUMSSC) THEN
                WRITE(LUN,9031) GTNAMES(TSSC),
     *                            CMONYI8(SSC_COM,13,BETUNIT)
           ELSEIF(GTYP.EQ.TTRP.AND.GIND.EQ.NUMTRP) THEN
                WRITE(LUN,9031) GTNAMES(TTRP),
     *                            CMONYI8(TRP_COM,13,BETUNIT)
           ELSEIF(GTYP.EQ.TSTR.AND.GIND.EQ.NUMSTR) THEN
                WRITE(LUN,9031) GTNAMES(TSTR),
     *                            CMONYI8(STR_COM,13,BETUNIT)
           ENDIF 
265	CONTINUE

        TOTCOM = TOTGAM_TCOM(1)
	WRITE(LUN,906) 'TOTAL SALE COMM',CMONYI8(TOTGAM_TCOM,12,BETUNIT)

C       SPOOL REPORT
C       ------------
	CALL USRCLOS1(LUN)

C       GAME TOTALS AND COMISSIONS TO BALANSFILE (BALWRI)       V04       
C       -----------------------------------------------------------
        DO GAM = 1, MAXGAM                                                    
            IND = 0                                                       
            DO TTYP = 1, TRET       
                IND = IND + 1
                GAMESUMS(GAM,TTYP,1) = TOTGAM(IND,GAM)
                IF(IND.EQ.GVCNT) THEN
                   GAMESUMS(GAM,TTYP,1)=GAMESUMS(GAM,TTYP,1)+TOTGAM(GRCNT,GAM)
                ENDIF
                IND = IND + 1
                GAMESUMS(GAM,TTYP,2) = TOTGAM(IND,GAM)
                IF(IND.EQ.GVAMT) THEN
                   GAMESUMS(GAM,TTYP,2)=GAMESUMS(GAM,TTYP,2)+TOTGAM(GRAMT,GAM)
                ENDIF
            END DO
        END DO

        RAPCODE = 2          
        CALL BALWRI(RAPCODE,GAMESUMS,TOTSUMS,TOTREAL)                           
        CALL BALWRI2(RAPCODE,GAMECOMS,TOTCOM,TOTDUE)

	CALL SPOOL('AGTACT.REP',COPY,ST)
	ST = LIB$FREE_LUN(LUN)
	RETURN
C
C FORMATS DEFINITION
C

902	FORMAT(42X,'TOTAL LOTTO AMOUNT ..............    ',A12,/,
     *	       42X,'TOTAL SPORTS AMOUNT..............    ',A12,/,
     *         42X,'TOTAL JOKER AMOUNT...............    ',A12,/,
C    *         42X,'TOTAL NUMBERS AMOUNT.............    ',A12,/,
C    *         42X,'TOTAL SCORE AMOUNT...............    ',A12,/,
C    *	       42X,'TOTAL WINNERS TIP AMOUNT.........    ',A12,/,
C    *         42X,'TOTAL TOTO SELECT................    ',A12,/,
C    *         42X,'TOTAL BINGOLOTTO.................    ',A12,/,
C    *         42X,'TOTAL SUPER DOUBLE...............    ',A12,/,
C    *         42X,'TOTAL TODAY''S COUPLE.............    ',A12,/,
C    *         42X,'TOTAL SUPER SCORE................    ',A12,/,
C    *         42X,'TOTAL TODAY''S TRIO...............    ',A12,/,
C    *         42X,'TOTAL SUPER TRIPLE...............    ',A12,/,
     *         42X,'TOTAL TOTOGOLO...................    ',A12,/,
     *         42X,'TOTAL PASSIVE....................    ',A12,/,
     *         42X,'TOTAL ADJUSTMENT AMOUNT..........    ',A12,/,
     *	       42X,'TOTAL NET SALES AMOUNT...........    ',A12,/,
     *	       42X,'TOTAL CANCEL AMOUNT..............    ',A12,/,
     *         42X,'TOTAL CASH AMOUNT................    ',A12,/,
     *         42X,'TOTAL REFUND AMOUNT..............    ',A12,/,
     *         42X,'TOTAL RETURN AMOUNT..............    ',A12,/,
     *         42X,'TOTAL NET AMOUNT.................    ',A12,/,
     *         42X,'TOTAL SALES COMMISSION ..........    ',A12,/,
     *         42X,'TOTAL VALIDATION COMMISSION .....    ',A12,/,
     *         42X,'TOTAL TICKET CHARGE..............    ',A12,/,
     *         42X,'TOTAL NET DUE AMOUNT.............    ',A12,/)
903	FORMAT(47X,4A4,1X,I8,1X,A12)
904	FORMAT(/,47X,A16,1X,I8,1X,A12)
905	FORMAT(47X,4A4,10X,A12)
906	FORMAT(/,47X,A16,10X,A12)
907	FORMAT(/,47X,12('-'),A14,12('-'),/)
908     FORMAT(X, 131('='), /)

9030    FORMAT(87X,'Totals for ',A8,2X,I8,2X,A13)
9031    FORMAT(87X,'Totals for ',A8,10X,A13)
	END   ! AGTACT.FCC
