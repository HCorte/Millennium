C
C CARACT_REPRT
C
C V15 14-MAY-2010 RXK Reporting of unused refunds replaced with rep.of returns.
C V14 13-MAY-1999 UXN SUPER TRIPLE ADDED.
C V13 13-NOV-1997 UXN Logical unit for the report file added as a parameter in
C	              subroutine call. 
C V12 17-FEB-1997 WPW Marge the fix for RFSS #315 with IPS software.
C V11 29-JAN-1997 WXW RFSS 315. Totals excluding tebe cartel.
C V10 15-DEC-1995 PXB Changes for double and couple games
C V09 02-SEP-1994 HXK Merge of May,June RFSS batch 
C V08 05-JUN-1994 HXK CHANGE CMONY TO CSMONY
C V07 06-MAY-1994 JXP Corrected printing game index
C V06 26-APR-1994 JXP Print refunds as in AGTACT
C V05 05-FEB-1994 HXK FIXED BUG IN TULOS 6 NETS SALES TOTAL.
C V04 28-JAN-1994 JXP Corrected report format
C V03 26-JAN-1994 JXP Total on tulos and voitaja games
C V02 04-NOV-1993 HXK FIX FOR COMMISSIONS.
C V01 19-OCT-1993 HXK PUT IN REFUNDS.
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
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE CARACT_REPRT(REPLU,CARTEL,TOTGAM,GRDCOM,GRNTOT,
     *                          TOTCMS,DUEAGT,DUELOT,
     *                          AGCOUNT,TAGCOUNT,FINAL,
     *                          SPECIAL_CARTEL,TB_CARTEL)
C
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
C
        ! arguments
        INTEGER*4  CARTEL
        INTEGER*4  TOTGAM(MAXGAM,GRAMT)  ! BOTTOM LINE TOTALS
        INTEGER*4  GRDCOM(2,MAXGAM)      ! BOTTOM LINE TOTALS
        INTEGER*4  GRNTOT(GRAMT)         ! GRAND TOTAL LINE
        INTEGER*4  TOTCMS(2)             !
        INTEGER*4  DUEAGT(2)             !
        INTEGER*4  DUELOT(2)             !
        INTEGER*4  AGCOUNT               !
        INTEGER*4  TAGCOUNT              !
        INTEGER*4  XOFF                  !
        INTEGER*4  RNUM                  !
        INTEGER*4  PAGE
        INTEGER*4  SPECIAL_CARTEL        !
        INTEGER*4  TB_CARTEL             !
        INTEGER*4  ODDS_TOT(2)
        INTEGER*4  GAME_INDEX

        LOGICAL    FINAL                 !

        ! variables
        INTEGER*4  REPLU                 !
        INTEGER*4  GTYP                  !
        INTEGER*4  GIND                  !
        INTEGER*4  TOTSUM                !
        INTEGER*4  I                     !
        INTEGER*4  K                     !
        INTEGER*4  J                     !



        ! test if empty cartel
        IF (.NOT.FINAL) THEN
            TOTSUM = 0
            DO 400 XOFF=1,GRAMT
                TOTSUM = TOTSUM + GRNTOT(XOFF)
400         CONTINUE
            IF (TOTSUM.EQ.0) RETURN
        ENDIF

        ! report on totals per game
C
        CALL TITLE('CARTEL DAILY SUMMARY REPORT',
     *           '  CARACT',RNUM,REPLU,PAGE,DAYCDC)
        WRITE(REPLU,9000)

        IF (.NOT.FINAL) THEN
            IF (CARTEL.GT.999.AND.CARTEL.LT.1999) THEN
                WRITE(REPLU,9023) SPECIAL_CARTEL, TAGCOUNT, AGCOUNT
            ELSEIF (CARTEL.GT.1999) THEN
                WRITE(REPLU,9024) SPECIAL_CARTEL, TB_CARTEL, TAGCOUNT, AGCOUNT
            ELSE
                WRITE(REPLU,9003) CARTEL, TAGCOUNT, AGCOUNT
            ENDIF
        ELSE
            WRITE(REPLU,9013) TAGCOUNT, AGCOUNT
        ENDIF
C
        WRITE(REPLU,9004) '  GROSS SALES '
        DO 650 I=1,MAXGAM
            GTYP = GNTTAB(GAMTYP,I)
            IF (GTYP.LE.0) GOTO 650
            GIND = GNTTAB(GAMIDX,I)
	    IF((GTYP.EQ.TSCR.AND.GIND.EQ.NUMSCR).OR.
     *         (GTYP.EQ.TWIT.AND.GIND.EQ.NUMWIT).OR.
     *         (GTYP.EQ.TDBL.AND.GIND.EQ.NUMDBL).OR.
     *         (GTYP.EQ.TCPL.AND.GIND.EQ.NUMCPL).OR.
     *         (GTYP.EQ.TSSC.AND.GIND.EQ.NUMSSC).OR.
     *         (GTYP.EQ.TTRP.AND.GIND.EQ.NUMTRP).OR.
     *         (GTYP.EQ.TSTR.AND.GIND.EQ.NUMSTR)) THEN
               IF(GTYP.EQ.TSCR) THEN
                  GAME_INDEX = NUMSCR
               ELSEIF(GTYP.EQ.TWIT) THEN
                  GAME_INDEX = NUMWIT
               ELSEIF(GTYP.EQ.TDBL) THEN
                  GAME_INDEX = NUMDBL
               ELSEIF(GTYP.EQ.TCPL) THEN
                  GAME_INDEX = NUMCPL
               ELSEIF(GTYP.EQ.TSSC) THEN
                  GAME_INDEX = NUMSSC
               ELSEIF(GTYP.EQ.TTRP) THEN
                  GAME_INDEX = NUMTRP
               ELSEIF(GTYP.EQ.TSTR) THEN
                  GAME_INDEX = NUMSTR
               ENDIF
               ODDS_TOT(1) = 0
               ODDS_TOT(2) = 0
               DO J =  1,GAME_INDEX  !assume games of same gtyp are together
                  ODDS_TOT(1) = ODDS_TOT(1) + TOTGAM(I-J+1,GSCNT)
                  ODDS_TOT(2) = ODDS_TOT(2) + TOTGAM(I-J+1,GSAMT)
               ENDDO
               WRITE(REPLU,9030) (GLNAMES(K,I),K=1,4),GIND,TOTGAM(I,GSCNT),
     *	 			 CSMONY(TOTGAM(I,GSAMT),13,VALUNIT),
     *	 			 GTNAMES(GTYP),
     *				 ODDS_TOT(1),
     *				 CSMONY(ODDS_TOT(2),13,VALUNIT)
	    ELSE
               WRITE(REPLU,9005) (GLNAMES(K,I),K=1,4),GIND,TOTGAM(I,GSCNT),
     *                           CSMONY(TOTGAM(I,GSAMT),13,VALUNIT)
	    ENDIF
650     CONTINUE

        IF (FINAL) THEN
            WRITE(REPLU,9016) 'SALES      ',GRNTOT(GSCNT),
     *                         CSMONY(GRNTOT(GSAMT),14,VALUNIT)
        ELSE
            WRITE(REPLU,9006) 'SALES      ',GRNTOT(GSCNT),
     *                         CSMONY(GRNTOT(GSAMT),14,VALUNIT)
        ENDIF
C
        WRITE(REPLU,9004) '    CANCELS   '
        DO 651 I=1,MAXGAM
            GTYP = GNTTAB(GAMTYP,I)
            IF(GTYP.LE.0) GOTO 651
            GIND = GNTTAB(GAMIDX,I)
	    IF((GTYP.EQ.TSCR.AND.GIND.EQ.NUMSCR).OR.
     *         (GTYP.EQ.TWIT.AND.GIND.EQ.NUMWIT).OR.
     *         (GTYP.EQ.TDBL.AND.GIND.EQ.NUMDBL).OR.
     *         (GTYP.EQ.TCPL.AND.GIND.EQ.NUMCPL).OR.
     *         (GTYP.EQ.TSSC.AND.GIND.EQ.NUMSSC).OR.
     *         (GTYP.EQ.TSTR.AND.GIND.EQ.NUMSTR).OR.
     *         (GTYP.EQ.TTRP.AND.GIND.EQ.NUMTRP)) THEN
		ODDS_TOT(1)=TOTGAM(I-5,GCCNT)+TOTGAM(I-4,GCCNT)
     *				  +TOTGAM(I-3,GCCNT)+TOTGAM(I-2,GCCNT)
     *				  +TOTGAM(I-1,GCCNT)+TOTGAM(I,GCCNT) 
		ODDS_TOT(2)=TOTGAM(I-5,GCAMT)+TOTGAM(I-4,GCAMT)
     *				  +TOTGAM(I-3,GCAMT)+TOTGAM(I-2,GCAMT)
     *				  +TOTGAM(I-1,GCAMT)+TOTGAM(I,GCAMT) 
                WRITE(REPLU,9030) (GLNAMES(K,I),K=1,4),GIND,TOTGAM(I,GCCNT),
     *				  CSMONY(TOTGAM(I,GCAMT),13,VALUNIT),
     *				  GTNAMES(GTYP),
     *				  ODDS_TOT(1),
     *				  CSMONY(ODDS_TOT(2),13,VALUNIT)
	    ELSE
                WRITE(REPLU,9005) (GLNAMES(K,I),K=1,4),GIND,TOTGAM(I,GCCNT),
     *                         CSMONY(TOTGAM(I,GCAMT),13,VALUNIT)
	    ENDIF
651     CONTINUE

        IF (FINAL) THEN
            WRITE(REPLU,9016) 'CANCELS    ',GRNTOT(GCCNT),
     *                         CSMONY(GRNTOT(GCAMT),14,VALUNIT)
        ELSE
            WRITE(REPLU,9006) 'CANCELS    ',GRNTOT(GCCNT),
     *                         CSMONY(GRNTOT(GCAMT),14,VALUNIT)
        ENDIF
C
        WRITE(REPLU,9004) '   NET SALES  '
        DO 652 I = 1, MAXGAM
            GTYP = GNTTAB(GAMTYP,I)
            IF(GTYP.LE.0) GOTO 652
            GIND = GNTTAB(GAMIDX,I)
	    IF((GTYP.EQ.TSCR.AND.GIND.EQ.NUMSCR).OR.
     *         (GTYP.EQ.TWIT.AND.GIND.EQ.NUMWIT).OR.
     *         (GTYP.EQ.TDBL.AND.GIND.EQ.NUMDBL).OR.
     *         (GTYP.EQ.TCPL.AND.GIND.EQ.NUMCPL).OR.
     *         (GTYP.EQ.TSSC.AND.GIND.EQ.NUMSSC).OR.
     *         (GTYP.EQ.TSTR.AND.GIND.EQ.NUMSTR).OR.
     *         (GTYP.EQ.TTRP.AND.GIND.EQ.NUMTRP)) THEN
		ODDS_TOT(1)=TOTGAM(I-5,GSCNT)+TOTGAM(I-4,GSCNT)
     *				  +TOTGAM(I-3,GSCNT)+TOTGAM(I-2,GSCNT)
     *				  +TOTGAM(I-1,GSCNT)+TOTGAM(I,GSCNT) 
     *				-(TOTGAM(I-5,GCCNT)+TOTGAM(I-4,GCCNT)
     *				  +TOTGAM(I-3,GCCNT)+TOTGAM(I-2,GCCNT)
     *				  +TOTGAM(I-1,GCCNT)+TOTGAM(I,GCCNT)) 

		ODDS_TOT(2)=TOTGAM(I-5,GSAMT)+TOTGAM(I-4,GSAMT)
     *				  +TOTGAM(I-3,GSAMT)+TOTGAM(I-2,GSAMT)
     *				  +TOTGAM(I-1,GSAMT)+TOTGAM(I,GSAMT) 
     *	      			 -(TOTGAM(I-5,GCAMT)+TOTGAM(I-4,GCAMT)
     *				  +TOTGAM(I-3,GCAMT)+TOTGAM(I-2,GCAMT)
     *				  +TOTGAM(I-1,GCAMT)+TOTGAM(I,GCAMT))
                WRITE(REPLU,9030) (GLNAMES(K,I),K=1,4),GIND,
     *                            TOTGAM(I,GSCNT)-TOTGAM(I,GCCNT),
     *                CSMONY(TOTGAM(I,GSAMT)-TOTGAM(I,GCAMT),13,VALUNIT),
     *				  GTNAMES(GTYP),
     *				  ODDS_TOT(1),
     *				  CSMONY(ODDS_TOT(2),13,VALUNIT)
	    ELSE
            WRITE(REPLU,9005) (GLNAMES(K,I),K=1,4),GIND,TOTGAM(I,GSCNT)-
     *                         TOTGAM(I,GCCNT),
     *                    CSMONY((TOTGAM(I,GSAMT)-TOTGAM(I,GCAMT)),13,VALUNIT)
	    ENDIF
652     CONTINUE

        IF (FINAL) THEN
            WRITE(REPLU,9016) 'NET SALES  ',
     *                        GRNTOT(GSCNT)-GRNTOT(GCCNT),
     *                        CSMONY((GRNTOT(GSAMT)-GRNTOT(GCAMT)),14,VALUNIT)
        ELSE
            WRITE(REPLU,9006) 'NET SALES  ',
     *                         GRNTOT(GSCNT)-GRNTOT(GCCNT),
     *                         CSMONY((GRNTOT(GSAMT)-GRNTOT(GCAMT)),14,VALUNIT)
        ENDIF
C
        WRITE(REPLU,9004) '  VALIDATIONS '
        DO 653 I = 1, MAXGAM
            GTYP = GNTTAB(GAMTYP,I)
            IF(GTYP.LE.0) GOTO 653
            GIND = GNTTAB(GAMIDX,I)
	    IF((GTYP.EQ.TSCR.AND.GIND.EQ.NUMSCR).OR.
     *         (GTYP.EQ.TWIT.AND.GIND.EQ.NUMWIT).OR.
     *         (GTYP.EQ.TDBL.AND.GIND.EQ.NUMDBL).OR.
     *         (GTYP.EQ.TCPL.AND.GIND.EQ.NUMCPL).OR.
     *         (GTYP.EQ.TSSC.AND.GIND.EQ.NUMSSC).OR.
     *         (GTYP.EQ.TSTR.AND.GIND.EQ.NUMSTR).OR.
     *         (GTYP.EQ.TTRP.AND.GIND.EQ.NUMTRP)) THEN
		ODDS_TOT(1)=TOTGAM(I-5,GVCNT)+TOTGAM(I-4,GVCNT)
     *				  +TOTGAM(I-3,GVCNT)+TOTGAM(I-2,GVCNT)
     *				  +TOTGAM(I-1,GVCNT)+TOTGAM(I,GVCNT) 
     *				  +TOTGAM(I-5,GRCNT)+TOTGAM(I-4,GRCNT)
     *				  +TOTGAM(I-3,GRCNT)+TOTGAM(I-2,GRCNT)
     *				  +TOTGAM(I-1,GRCNT)+TOTGAM(I,GRCNT)

		ODDS_TOT(2)=TOTGAM(I-5,GVAMT)+TOTGAM(I-4,GVAMT)
     *				  +TOTGAM(I-3,GVAMT)+TOTGAM(I-2,GVAMT)
     *				  +TOTGAM(I-1,GVAMT)+TOTGAM(I,GVAMT) 
     *				  +TOTGAM(I-5,GRAMT)+TOTGAM(I-4,GRAMT)
     *				  +TOTGAM(I-3,GRAMT)+TOTGAM(I-2,GRAMT)
     *				  +TOTGAM(I-1,GRAMT)+TOTGAM(I,GRAMT)
	       WRITE(REPLU,9030) (GLNAMES(K,I),K=1,4),GIND,TOTGAM(I,GVCNT)+
     *                         TOTGAM(I,GRCNT),           
     *                         CSMONY(TOTGAM(I,GVAMT)+
     *                               TOTGAM(I,GRAMT),13,VALUNIT),
     *				  GTNAMES(GTYP),
     *				  ODDS_TOT(1),
     *				  CSMONY(ODDS_TOT(2),13,VALUNIT)
	      ELSE
	       WRITE(REPLU,9005) (GLNAMES(K,I),K=1,4),GIND,TOTGAM(I,GVCNT)+
     *                         TOTGAM(I,GRCNT),
     *                         CSMONY(TOTGAM(I,GVAMT)+
     *                               TOTGAM(I,GRAMT),13,VALUNIT)
	      ENDIF
653     CONTINUE

        IF (FINAL) THEN
            WRITE(REPLU,9016) 'VALIDATIONS',GRNTOT(GVCNT)+
     *                                      GRNTOT(GRCNT),
     *                         CSMONY(GRNTOT(GVAMT)+
     *                               GRNTOT(GRAMT),14,VALUNIT)
        ELSE
            WRITE(REPLU,9006) 'VALIDATIONS',GRNTOT(GVCNT)+GRNTOT(GRCNT),
     *                         CSMONY(GRNTOT(GVAMT)+GRNTOT(GRAMT),14,VALUNIT)
        ENDIF
C
        WRITE(REPLU,9004) '  COMMISSIONS '
        DO 654 I = 1, MAXGAM
            GTYP = GNTTAB(GAMTYP,I)
            IF(GTYP.LE.0) GOTO 654
            GIND = GNTTAB(GAMIDX,I)
	    IF((GTYP.EQ.TSCR.AND.GIND.EQ.NUMSCR).OR.
     *         (GTYP.EQ.TWIT.AND.GIND.EQ.NUMWIT).OR.
     *         (GTYP.EQ.TDBL.AND.GIND.EQ.NUMDBL).OR.
     *         (GTYP.EQ.TCPL.AND.GIND.EQ.NUMCPL).OR.
     *         (GTYP.EQ.TSSC.AND.GIND.EQ.NUMSSC).OR.
     *         (GTYP.EQ.TSTR.AND.GIND.EQ.NUMSTR).OR.
     *         (GTYP.EQ.TTRP.AND.GIND.EQ.NUMTRP)) THEN
		ODDS_TOT(1) = 0
		ODDS_TOT(2) = 0
		DO J=0,5
		  CALL ADDI8I8(ODDS_TOT,GRDCOM(1,I-J),VALUNIT)
	        ENDDO
	           WRITE(REPLU,9031) (GLNAMES(K,I),K=1,4),GIND,
     *                         CSMONYI8(GRDCOM(1,I),14,VALUNIT),
     *				  GTNAMES(GTYP),
     *                         CSMONYI8(ODDS_TOT,14,VALUNIT) 
	    ELSE
 	           WRITE(REPLU,9008) (GLNAMES(K,I),K=1,4),GIND,
     *                         CSMONYI8(GRDCOM(1,I),14,VALUNIT)
	    ENDIF
654     CONTINUE

        IF (FINAL) THEN
            WRITE(REPLU,9019) 'COMMISSIONS',CSMONYI8(TOTCMS,14,VALUNIT)
        ELSE
            WRITE(REPLU,9009) 'COMMISSIONS',CSMONYI8(TOTCMS,14,VALUNIT)
        ENDIF
C
C RETURNS
C
        WRITE(REPLU,9004) '   RETURNS    '
        DO 258 I=1,MAXGAM
           GTYP = GNTTAB(GAMTYP,I)
           IF(GTYP.LE.0) GOTO 258
           IF(GTYP.NE.TPAS) GOTO 258
           GIND = GNTTAB(GAMIDX,I)
255        CONTINUE
	       WRITE(REPLU,9005) (GLNAMES(K,I),K=1,4),GIND,
     *                         TOTGAM(I,GCLCNT),
     *                         CSMONY(TOTGAM(I,GCLAMT),13,VALUNIT)
258	CONTINUE

        IF (FINAL) THEN
		WRITE(REPLU,9016) 'RETURNS',GRNTOT(GCLCNT),
     *                         CSMONY(GRNTOT(GCLAMT),14,VALUNIT)
        ELSE
	        WRITE(REPLU,9006) 'RETURNS',GRNTOT(GCLCNT),
     *                         CSMONY(GRNTOT(GCLAMT),14,VALUNIT)
        ENDIF

        WRITE(REPLU,9010)
        IF (FINAL) THEN
            WRITE(REPLU,9017) CSMONYI8(DUEAGT,14,VALUNIT),
     *                        CSMONYI8(DUELOT,14,VALUNIT)
        ELSE
            WRITE(REPLU,9007) CSMONYI8(DUEAGT,14,VALUNIT),
     *                        CSMONYI8(DUELOT,14,VALUNIT)
        ENDIF
        RETURN
C
C       ================= Format Statements ====================
C
9000    FORMAT(1X,131('='),//)
9003    FORMAT(1X,'CARTEL ',I4,' TOTALS: ',I4,' AGENTS, ',
     *         I4,' ACTIVE')
9004    FORMAT(/,30X,21('-'),A14,18('-'),/)
9005    FORMAT(30X,4A4,1X,I1,10X,I8,4X,A13)
9006    FORMAT(/,30X,A11,17X,I8,3X,A14)
9007    FORMAT(/,30X,'DUE AGENTS  ',27X,A14,/,
     *           30X,'DUE LOTTERY ',27X,A14)
9008    FORMAT(30X,4A4,1X,I1,21X,A14)
9009    FORMAT(/,30X,A11,28X,A14)
9010    FORMAT(/,30X,53('-'))
C
9013    FORMAT(1X,'TOTALS: ',I4,' AGENTS, ',I4, ' ACTIVE')
9016    FORMAT(/,30X,'TOTAL ',A11,11X,I8,3X,A14)
9017    FORMAT(/,30X,'TOTAL DUE AGENTS  ',12X,A14,/,
     *         30X,'TOTAL DUE LOTTERY ',12X,A14)
9019    FORMAT(/,30X,'TOTAL ',A11,22X,A14)
C
9023    FORMAT(1X,'OTHER THAN CARTEL ',I2,' TOTALS: ',
     *         I4,' AGENTS, ',I4, ' ACTIVE')
9024    FORMAT(1X,'OTHER THAN CARTEL ',I2,' AND OTHER THAN CARTEL ',I2,/,
     *         1X,'TOTALS (TELEBETTING AND CARTEL 5 EXCLUDED): '
     *            ,I4,' AGENTS, ',I4,' ACTIVE')
C
9030    FORMAT(30X,4A4,1X,I1,10X,I8,4X,A13,2X,'Totals for ',A8,2X,I8,2X,A13)
9031    FORMAT(30X,4A4,1X,I1,21X,A14,2X,'Totals for ',A8,2X,A14)
        END
