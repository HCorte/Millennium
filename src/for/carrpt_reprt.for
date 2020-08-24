C CARRPT_REPRT.FOR
C
C V13 13-MAY-1999 UXN Super Triple added.
C V12 09-JAN-1998 UXN Super Score and Todays Triple added.
C V11 13-NOV-1997 UXN REPLU ADDED AS A PARAMETER.
C V10 17-FEB-1997 WPW Marge the fix for RFSS #315 with IPS software.
C V09 29-JAN-1997 WXW RFSS 315. Totals excluding tebe cartel.
C V08 15-DEC-1995 PXB Changes for double and couple games
C V07 05-FEB-1994 HXK corrected TULOS 6 net sales.
C V06 26-JAN-1994 JXP Print format change
C V05 26-JAN-1994 JXP Totals on tulos and voittaja 
C V04 27-OCT-1993 HXK SIGNED DUE AGENTS., ETC.
C V03 19-OCT-1993 HXK PUT IN REFUNDS.
C V02 08-OCT-1993 HXK FURTHER FIXES FOR RENT.
C V01 07-OCT-1993 HXK Fixed FORMAT bugs.
C
C ======================================================================
C
C       REPORT SUBROUTINE
C       =================
C       
C=======OPTIONS/CHECK=NOOVERFLOW/EXT
        SUBROUTINE CARRPT_REPRT (REPLU,CARTEL, TOTRNT, SPECIAL_CARTEL,
     *                   TOTADJ,AGCOUNT,TAGCOUNT,FINAL,TB_CARTEL )
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:CARRPT.DEF'
C
        ! arguments
        INTEGER*4  CARTEL                 !
        INTEGER*4  TOTRNT                 !
        INTEGER*4  TOTADJ(2)              !
        INTEGER*4  AGCOUNT                !
        INTEGER*4  TAGCOUNT               !
        INTEGER*4  SPECIAL_CARTEL         !
        INTEGER*4  TB_CARTEL              !

        LOGICAL    FINAL                  ! 

        ! variables
        INTEGER*4  ZTOTGAM(MAXGAM,GRAMT)  ! BOTTOM LINE TOTALS
        INTEGER*4  ZGRDCOM(2,MAXGAM)      ! BOTTOM LINE TOTALS
        INTEGER*4  ZGRNTOT(GRAMT)         ! GRAND TOTAL LINE
        INTEGER*4  ZDUEAGT(2)             !
        INTEGER*4  ZDUELOT(2)             !
        INTEGER*4  ZTOTCMS(2)             !

        INTEGER*4  I                      !
        INTEGER*4  J                      !
        INTEGER*4  TOTSUM                 !
        INTEGER*4  XOFF                   !
        INTEGER*4  RNUM                   !
        INTEGER*4  REPLU                  ! REPORT LOGICAL UNIT
        INTEGER*4  PAGE                   !
        INTEGER*4  GTYP                   !
        INTEGER*4  GIND                   !
        INTEGER*4  ODDS_TOT(2)		  !
C
        CALL FASTSET(0, ZGRDCOM, 2*MAXGAM)
        ZTOTCMS(1) = 0
        ZTOTCMS(2) = 0
        ZDUELOT(1) = 0
        ZDUELOT(2) = 0
        ZDUEAGT(1) = 0
        ZDUEAGT(2) = 0
C
        IF(REPTYP.EQ.1) THEN
            DO 211 I = 1, MAXGAM
                DO 212 J = 1, GRAMT
                    ZTOTGAM(I,J) = CTOTGAM(I,J)
212             CONTINUE
211         CONTINUE
C
            DO 213 I = 1, MAXGAM
                CALL ADDI8I8(ZGRDCOM(1,I), CGRDCOM(1,I), BETUNIT)
213         CONTINUE
C
            DO 214 I = 1, GRAMT
                ZGRNTOT(I) = CGRNTOT(I)
214         CONTINUE
C
            CALL ADDI8I8(ZDUELOT, CDUELOT, BETUNIT)
            CALL ADDI8I8(ZDUEAGT, CDUEAGT, BETUNIT)
            CALL ADDI8I8(ZTOTCMS, CTOTCMS, BETUNIT)
        ENDIF
C
        IF(REPTYP.EQ.2) THEN
            DO 311 I = 1, MAXGAM
                DO 312 J =1, GRAMT
                    ZTOTGAM(I,J) = OTOTGAM(I,J)
312             CONTINUE
311         CONTINUE
C
            DO 313 I = 1, MAXGAM
                CALL ADDI8I8(ZGRDCOM(1,I), OGRDCOM(1,I), BETUNIT)
313         CONTINUE
C                                              
            DO 314 I = 1, GRAMT
                ZGRNTOT(I) = OGRNTOT(I)
314         CONTINUE
C
            CALL ADDI8I8(ZDUELOT, ODUELOT, BETUNIT)
            CALL ADDI8I8(ZDUEAGT, ODUEAGT, BETUNIT)
            CALL ADDI8I8(ZTOTCMS, OTOTCMS, BETUNIT)
        ENDIF
C
        IF(REPTYP.EQ.3) THEN
            DO 411 I = 1, MAXGAM
                DO 412 J =1, GRAMT
                   ZTOTGAM(I,J) = TB_TOTGAM(I,J)
412             CONTINUE
411         CONTINUE
C
            DO 413 I = 1, MAXGAM
                CALL ADDI8I8(ZGRDCOM(1,I), TB_GRDCOM(1,I), BETUNIT)
413         CONTINUE
C                                              
            DO 414 I = 1, GRAMT
                ZGRNTOT(I) = TB_GRNTOT(I)
414         CONTINUE
C
            CALL ADDI8I8(ZDUELOT, TB_DUELOT, BETUNIT)
            CALL ADDI8I8(ZDUEAGT, TB_DUEAGT, BETUNIT)
            CALL ADDI8I8(ZTOTCMS, TB_TOTCMS, BETUNIT)
        ENDIF
C
        IF(REPTYP.EQ.4) THEN
            DO 511 I = 1, MAXGAM
                DO 512 J = 1, GRAMT
                    ZTOTGAM(I,J) = TOTGAM(I,J)
512             CONTINUE
511         CONTINUE
C
            DO 513 I = 1, MAXGAM
                CALL ADDI8I8(ZGRDCOM(1,I), GRDCOM(1,I), BETUNIT)
513         CONTINUE
C
            DO 514 I = 1, GRAMT
                ZGRNTOT(I) = GRNTOT(I)
514         CONTINUE
C
            CALL ADDI8I8(ZDUELOT, DUELOT, BETUNIT)
            CALL ADDI8I8(ZDUEAGT, DUEAGT, BETUNIT)
            CALL ADDI8I8(ZTOTCMS, TOTCMS, BETUNIT)
        ENDIF
C
C ==================================================
C
C       TEST IF EMPTY CARTEL
C
C
        IF (.NOT.FINAL) THEN
            TOTSUM = 0
            DO 400 XOFF = 1, GRAMT
                TOTSUM = TOTSUM + ZGRNTOT(XOFF)
400         CONTINUE
            IF (TOTSUM.EQ.0) RETURN
        ENDIF
C
        CALL TITLE('CARTEL INVOICE SUMMARY REPORT',
     *             '  CARRPT',RNUM,REPLU,PAGE,DAYCDC)
        WRITE(REPLU,9000)
C
        IF (.NOT.FINAL) THEN
            IF (CARTEL.GT.999.AND.CARTEL.LT.1999) THEN
                WRITE(REPLU,9023) SPECIAL_CARTEL,TAGCOUNT, AGCOUNT
            ELSEIF (CARTEL.GT.1999) THEN
                WRITE(REPLU,9024) SPECIAL_CARTEL,TB_CARTEL,TAGCOUNT, AGCOUNT
            ELSE
                WRITE(REPLU,9003) CARTEL, TAGCOUNT, AGCOUNT
            ENDIF
        ELSE
            WRITE(REPLU,9013) TAGCOUNT, AGCOUNT
        ENDIF
C
        WRITE(REPLU,9004) '  GROSS SALES '
        DO 650 I = 1, MAXGAM
            GTYP = GNTTAB(GAMTYP,I)
            IF (GTYP.LE.0) GOTO 650
            GIND = GNTTAB(GAMIDX,I)
	   IF(GTYP.EQ.TSCR.AND.GIND.EQ.NUMSCR.OR.
     *        GTYP.EQ.TWIT.AND.GIND.EQ.NUMWIT.OR.
     *        GTYP.EQ.TCPL.AND.GIND.EQ.NUMCPL.OR.
     *        GTYP.EQ.TDBL.AND.GIND.EQ.NUMDBL.OR.
     *        GTYP.EQ.TSSC.AND.GIND.EQ.NUMSSC.OR.
     *        GTYP.EQ.TSTR.AND.GIND.EQ.NUMSTR.OR.
     *        GTYP.EQ.TTRP.AND.GIND.EQ.NUMTRP) THEN
                ODDS_TOT(1)=ZTOTGAM(I-5,GSCNT)+ZTOTGAM(I-4,GSCNT)
     *                            +ZTOTGAM(I-3,GSCNT)+ZTOTGAM(I-2,GSCNT)
     *                            +ZTOTGAM(I-1,GSCNT)+ZTOTGAM(I,GSCNT)
                ODDS_TOT(2)=ZTOTGAM(I-5,GSAMT)+ZTOTGAM(I-4,GSAMT)
     *                            +ZTOTGAM(I-3,GSAMT)+ZTOTGAM(I-2,GSAMT)
     *                            +ZTOTGAM(I-1,GSAMT)+ZTOTGAM(I,GSAMT)
 		WRITE(REPLU,9030) GTNAMES(GTYP),
     *                        GIND,
     *                        ZTOTGAM(I,GSCNT),
     *                        CMONY(ZTOTGAM(I,GSAMT),14,BETUNIT),
     *			      GTNAMES(GTYP),
     *                        ODDS_TOT(1),
     *                        CMONY(ODDS_TOT(2),14,BETUNIT)
            ELSE
               WRITE(REPLU,9005) GTNAMES(GTYP),
     *                        GIND,
     *                        ZTOTGAM(I,GSCNT),
     *                        CMONY(ZTOTGAM(I,GSAMT),14,BETUNIT)
	    ENDIF
650     CONTINUE
C
        IF (FINAL) THEN
            WRITE(REPLU,9016) 'SALES      ',
     *                        ZGRNTOT(GSCNT),
     *                        CMONY(ZGRNTOT(GSAMT),14,BETUNIT)
        ELSE
            WRITE(REPLU,9006) 'SALES      ',
     *                        ZGRNTOT(GSCNT),
     *                        CMONY(ZGRNTOT(GSAMT),14,BETUNIT)
        ENDIF
C
        WRITE(REPLU,9004) '    CANCELS   '
        DO 651 I = 1, MAXGAM
            GTYP = GNTTAB(GAMTYP,I)
            IF(GTYP.LE.0) GOTO 651
            GIND = GNTTAB(GAMIDX,I)
	    IF(GTYP.EQ.TSCR.AND.GIND.EQ.NUMSCR.OR.
     *         GTYP.EQ.TWIT.AND.GIND.EQ.NUMWIT.OR.
     *         GTYP.EQ.TCPL.AND.GIND.EQ.NUMCPL.OR.
     *         GTYP.EQ.TDBL.AND.GIND.EQ.NUMDBL.OR.
     *         GTYP.EQ.TSSC.AND.GIND.EQ.NUMSSC.OR.
     *         GTYP.EQ.TSTR.AND.GIND.EQ.NUMSTR.OR.
     *         GTYP.EQ.TTRP.AND.GIND.EQ.NUMTRP) THEN
                ODDS_TOT(1)=ZTOTGAM(I-5,GCCNT)+ZTOTGAM(I-4,GCCNT)
     *                            +ZTOTGAM(I-3,GCCNT)+ZTOTGAM(I-2,GCCNT)
     *                            +ZTOTGAM(I-1,GCCNT)+ZTOTGAM(I,GCCNT)
                ODDS_TOT(2)=ZTOTGAM(I-5,GCAMT)+ZTOTGAM(I-4,GCAMT)
     *                            +ZTOTGAM(I-3,GCAMT)+ZTOTGAM(I-2,GCAMT)
     *                            +ZTOTGAM(I-1,GCAMT)+ZTOTGAM(I,GCAMT)
		WRITE(REPLU,9030) GTNAMES(GTYP),
     *                        GIND,
     *                        ZTOTGAM(I,GCCNT),
     *                        CMONY(ZTOTGAM(I,GCAMT),14,BETUNIT),
     *			      GTNAMES(GTYP),
     *                        ODDS_TOT(1),
     *                        CMONY(ODDS_TOT(2),14,BETUNIT)
	    ELSE
                WRITE(REPLU,9005) GTNAMES(GTYP),
     *                        GIND,
     *                        ZTOTGAM(I,GCCNT),
     *                        CMONY(ZTOTGAM(I,GCAMT),14,BETUNIT)
           ENDIF
651     CONTINUE
        IF (FINAL) THEN
            WRITE(REPLU,9016) 'CANCELS    ',
     *                        ZGRNTOT(GCCNT),
     *                        CMONY(ZGRNTOT(GCAMT),14,BETUNIT)
        ELSE
            WRITE(REPLU,9006) 'CANCELS    ',
     *                        ZGRNTOT(GCCNT),
     *                        CMONY(ZGRNTOT(GCAMT),14,BETUNIT)
        ENDIF
C
        WRITE(REPLU,9004) '   NET SALES  '
        DO 652 I = 1, MAXGAM
            GTYP = GNTTAB(GAMTYP,I)
            IF(GTYP.LE.0) GOTO 652
            GIND = GNTTAB(GAMIDX,I)
	    IF(GTYP.EQ.TSCR.AND.GIND.EQ.NUMSCR.OR.
     *         GTYP.EQ.TWIT.AND.GIND.EQ.NUMWIT.OR.
     *         GTYP.EQ.TCPL.AND.GIND.EQ.NUMCPL.OR.
     *         GTYP.EQ.TDBL.AND.GIND.EQ.NUMDBL.OR.
     *         GTYP.EQ.TSSC.AND.GIND.EQ.NUMSSC.OR.
     *         GTYP.EQ.TSTR.AND.GIND.EQ.NUMSTR.OR.
     *         GTYP.EQ.TTRP.AND.GIND.EQ.NUMTRP) THEN
                ODDS_TOT(1)=ZTOTGAM(I-5,GSCNT)+ZTOTGAM(I-4,GSCNT)
     *                            +ZTOTGAM(I-3,GSCNT)+ZTOTGAM(I-2,GSCNT)
     *                            +ZTOTGAM(I-1,GSCNT)+ZTOTGAM(I,GSCNT)
     *				 -(ZTOTGAM(I-5,GCCNT)+ZTOTGAM(I-4,GCCNT)
     *                            +ZTOTGAM(I-3,GCCNT)+ZTOTGAM(I-2,GCCNT)
     *                            +ZTOTGAM(I-1,GCCNT)+ZTOTGAM(I,GCCNT))
                ODDS_TOT(2)=ZTOTGAM(I-5,GSAMT)+ZTOTGAM(I-4,GSAMT)
     *                            +ZTOTGAM(I-3,GSAMT)+ZTOTGAM(I-2,GSAMT)
     *                            +ZTOTGAM(I-1,GSAMT)+ZTOTGAM(I,GSAMT)
     *				  -(ZTOTGAM(I-5,GCAMT)+ZTOTGAM(I-4,GCAMT)
     *                            +ZTOTGAM(I-3,GCAMT)+ZTOTGAM(I-2,GCAMT)
     *                            +ZTOTGAM(I-1,GCAMT)+ZTOTGAM(I,GCAMT))
		WRITE(REPLU,9030) GTNAMES(GTYP),
     *                        GIND,
     *                        ZTOTGAM(I,GSCNT)-ZTOTGAM(I,GCCNT),
     *              CMONY(ZTOTGAM(I,GSAMT)-ZTOTGAM(I,GCAMT),14,BETUNIT),
     *			      GTNAMES(GTYP),
     *                        ODDS_TOT(1),
     *                        CMONY(ODDS_TOT(2),14,BETUNIT)
	   ELSE
               WRITE(REPLU,9005) GTNAMES(GTYP),
     *                           GIND,
     *                           ZTOTGAM(I,GSCNT)-ZTOTGAM(I,GCCNT),
     *                 CMONY((ZTOTGAM(I,GSAMT)-ZTOTGAM(I,GCAMT)),14,BETUNIT)
	   ENDIF
652     CONTINUE
        IF (FINAL) THEN
            WRITE(REPLU,9016) 'NET SALES  ',
     *                         ZGRNTOT(GSCNT)-ZGRNTOT(GCCNT),
     *                  CMONY((ZGRNTOT(GSAMT)-ZGRNTOT(GCAMT)),14,BETUNIT)
        ELSE
            WRITE(REPLU,9006) 'NET SALES  ',
     *                         ZGRNTOT(GSCNT)-ZGRNTOT(GCCNT),
     *                  CMONY((ZGRNTOT(GSAMT)-ZGRNTOT(GCAMT)),14,BETUNIT)
        ENDIF
C
        WRITE(REPLU,9004) '  VALIDATIONS '
        DO 653 I = 1, MAXGAM
            GTYP = GNTTAB(GAMTYP,I)
            IF(GTYP.LE.0) GOTO 653
            GIND = GNTTAB(GAMIDX,I)
	    IF(GTYP.EQ.TSCR.AND.GIND.EQ.NUMSCR.OR.
     *         GTYP.EQ.TWIT.AND.GIND.EQ.NUMWIT.OR.
     *         GTYP.EQ.TCPL.AND.GIND.EQ.NUMCPL.OR.
     *         GTYP.EQ.TDBL.AND.GIND.EQ.NUMDBL.OR.
     *         GTYP.EQ.TSSC.AND.GIND.EQ.NUMSSC.OR.
     *         GTYP.EQ.TSTR.AND.GIND.EQ.NUMSTR.OR.
     *         GTYP.EQ.TTRP.AND.GIND.EQ.NUMTRP) THEN
                ODDS_TOT(1)=ZTOTGAM(I-5,GVCNT)+ZTOTGAM(I-4,GVCNT)
     *                            +ZTOTGAM(I-3,GVCNT)+ZTOTGAM(I-2,GVCNT)
     *                            +ZTOTGAM(I-1,GVCNT)+ZTOTGAM(I,GVCNT)
     *                            +ZTOTGAM(I-5,GRCNT)+ZTOTGAM(I-4,GRCNT)
     *                            +ZTOTGAM(I-3,GRCNT)+ZTOTGAM(I-2,GRCNT)
     *                            +ZTOTGAM(I-1,GRCNT)+ZTOTGAM(I,GRCNT)
                ODDS_TOT(2)=ZTOTGAM(I-5,GVAMT)+ZTOTGAM(I-4,GVAMT)
     *                            +ZTOTGAM(I-3,GVAMT)+ZTOTGAM(I-2,GVAMT)
     *                            +ZTOTGAM(I-1,GVAMT)+ZTOTGAM(I,GVAMT)
     *                            +ZTOTGAM(I-5,GRAMT)+ZTOTGAM(I-4,GRAMT)
     *                            +ZTOTGAM(I-3,GRAMT)+ZTOTGAM(I-2,GRAMT)
     *                            +ZTOTGAM(I-1,GRAMT)+ZTOTGAM(I,GRAMT)
		WRITE(REPLU,9030) GTNAMES(GTYP),
     *                        GIND,
     *                        ZTOTGAM(I,GVCNT)+ZTOTGAM(I,GRCNT),
     *                        CMONY(ZTOTGAM(I,GVAMT)+ZTOTGAM(I,GRAMT),
     *                              14,BETUNIT),
     *			      GTNAMES(GTYP),
     *                        ODDS_TOT(1),
     *                        CMONY(ODDS_TOT(2),14,BETUNIT)
	   ELSE
                WRITE(REPLU,9005) GTNAMES(GTYP),
     *                        GIND,
     *                        ZTOTGAM(I,GVCNT)+ZTOTGAM(I,GRCNT),
     *                        CMONY(ZTOTGAM(I,GVAMT)+ZTOTGAM(I,GRAMT),
     *                              14,BETUNIT)
           ENDIF
653     CONTINUE
        IF (FINAL) THEN
            WRITE(REPLU,9016) 'VALIDATIONS',
     *                         ZGRNTOT(GVCNT)+ZGRNTOT(GRCNT),
     *                         CMONY(ZGRNTOT(GVAMT)+ZGRNTOT(GRAMT),14,BETUNIT)
        ELSE
            WRITE(REPLU,9006) 'VALIDATIONS',
     *                         ZGRNTOT(GVCNT)+ZGRNTOT(GRCNT),
     *                         CMONY(ZGRNTOT(GVAMT)+ZGRNTOT(GRAMT),14,BETUNIT)
        ENDIF
C
        WRITE(REPLU,9004) '  COMMISSIONS '
        DO 654 I = 1, MAXGAM
            GTYP = GNTTAB(GAMTYP,I)
            IF(GTYP.LE.0) GOTO 654
            GIND = GNTTAB(GAMIDX,I)
	    IF(GTYP.EQ.TSCR.AND.GIND.EQ.NUMSCR.OR.
     *         GTYP.EQ.TWIT.AND.GIND.EQ.NUMWIT.OR.
     *         GTYP.EQ.TCPL.AND.GIND.EQ.NUMCPL.OR.
     *         GTYP.EQ.TDBL.AND.GIND.EQ.NUMDBL.OR.
     *         GTYP.EQ.TSSC.AND.GIND.EQ.NUMSSC.OR.
     *         GTYP.EQ.TSTR.AND.GIND.EQ.NUMSTR.OR.
     *         GTYP.EQ.TTRP.AND.GIND.EQ.NUMTRP) THEN
	        ODDS_TOT(1) = 0
	        ODDS_TOT(2) = 0
		DO J=0,5
		    CALL ADDI8I8(ODDS_TOT,ZGRDCOM(1,I-J),VALUNIT)
	        ENDDO
		WRITE(REPLU,9031) GTNAMES(GTYP),
     *                        GIND,
     *                        CMONYI8(ZGRDCOM(1,I),14,BETUNIT),
     *			      GTNAMES(GTYP),
     *                        CMONYI8(ODDS_TOT,14,VALUNIT)
	    ELSE
		WRITE(REPLU,9008) GTNAMES(GTYP),
     *                        GIND,
     *                        CMONYI8(ZGRDCOM(1,I),14,BETUNIT) 
	    ENDIF
654     CONTINUE
        IF (FINAL) THEN
            WRITE(REPLU,9019) 'COMMISSIONS',
     *                        CMONYI8(ZTOTCMS,14,BETUNIT)
        ELSE
            WRITE(REPLU,9009) 'COMMISSIONS',
     *                        CMONYI8(ZTOTCMS,14,BETUNIT)
        ENDIF
        IF (FINAL) THEN
            WRITE(REPLU,9017) CMONY(TOTRNT,14,BETUNIT),
     *                        CSMONYI8(TOTADJ,14,BETUNIT),
     *                        CMONYI8(ZTOTCMS,14,BETUNIT),
     *                        CSMONYI8(ZDUEAGT,14,BETUNIT),
     *                        CSMONYI8(ZDUELOT,14,BETUNIT)
        ELSE
           WRITE(REPLU,9007) CMONY(TOTRNT,14,BETUNIT),
     *                       CSMONYI8(TOTADJ,14,BETUNIT),
     *                       CMONYI8(ZTOTCMS,14,BETUNIT),
     *                       CSMONYI8(ZDUEAGT,14,BETUNIT),
     *                       CSMONYI8(ZDUELOT,14,BETUNIT)
        ENDIF
C
        RETURN
C
C       ================= Format Statements ====================
C
9000    FORMAT(1X,131('='),/)
9003    FORMAT(1X,'CARTEL ',I4,' TOTALS: ',
     *         I4,' AGENTS, ',I4,' ACTIVE')
9004    FORMAT(/,30X,21('-'),A14,9('-'),/)
9005    FORMAT(30X,A8,I1,10X,I8,3X,A14)
9006    FORMAT(/,30X,A11,8X,I8,3X,A14)
9007    FORMAT(/,30X,44('='),/,
     *         30X,'RENT              ',12X,A14,/,
     *         30X,'ADJUSTMENTS       ',12X,A14,/,
     *         30X,'COMMISSION        ',12X,A14,/,
     *         30X,'DUE AGENTS        ',12X,A14,/,
     *         30X,'DUE LOTTERY       ',12X,A14)
9008    FORMAT(30X,A8,I1,21X,A14)
9009    FORMAT(/,30X,A11,19X,A14)
C
9013    FORMAT(1X,'TOTALS: ',I4,' AGENTS',I4,' ACTIVE')
9016    FORMAT(/,30X,'TOTAL ',A11,2X,I8,3X,A14)
9017    FORMAT(/,30X,44('='),/,
     *         30X,'TOTAL RENT        ',12X,A14,/,
     *         30X,'TOTAL ADJUSTMENTS ',12X,A14,/,
     *         30X,'TOTAL COMMISSION  ',12X,A14,/,
     *         30X,'TOTAL DUE AGENTS  ',12X,A14,/,
     *         30X,'TOTAL DUE LOTTERY ',12X,A14)
9019    FORMAT(/,30X,' TOTAL ',A11,13X,A14)
C
9023    FORMAT(1X,'OTHER THAN CARTEL ',I3,' TOTALS: ',
     *         I4,' AGENTS, ',I4, ' ACTIVE')
9024    FORMAT(1X,'OTHER THAN CARTEL ',I3,' AND OTHER THAN CARTEL ',I3,/,
     *         1X,'TOTALS (TELEBETTING AND CARTEL 5 EXCLUDED): '
     *            ,I4,' AGENTS, ',I4,' ACTIVE')
C
9030    FORMAT(30X,A8,I1,10X,I8,3X,A14,2X,'Totals for ',A8,2X,I8,2X,A14)
9031    FORMAT(30X,A8,I1,21X,A14,2X,'Totals for ',A8,2X,A14)
        END
