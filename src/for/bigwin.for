C
C SUBROUTINE BIGWIN
C
C BIGWIN.FOR
C
C V19 29-NOV-2000 UXN TOTOGOLA ADDED.
C V18 13-JAN-2000 RXK Def-file for Bingo division names added
C V17 13-OCT-1999 RXK World Tour game added.
C V16 13-MAY-1999 UXN Super Triple added.
C V15 02-OCT-1997 UXN BINGO LOTTO CHANGES.
C V14 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting, 
C                     Instant Pass Thru Phase 1
C V13 18-JAN-1996 RXK Today's Couple and Super Double added 
C V12 14-SEP-1995 RXK Change of RAVI report names
C V11 27-JUL-1995 PXB ravi game not initialized bug fix. 
C V10 15-DEC-1994 PXB Added bingo game.
C V09 12-OCT-1993 HXK TOOK OUT CANCELLED WINNERS.
C V08 02-SEP-1993 SXH AGENT # 8 CHARS
C V07 13-JUL-1993 SXH Relesaed for Finland
C V06 19-JUN-1993 HXK brought LU values up to MAXGAM
C V05 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V04 07-APR-1992 GCAN CHECK FOR AMOUNT ON ODDSET GAMES.
C V03 22-FEB-1992 GCAN ALSO DISPLAY INTERNAL SERIAL NUMBER.
C V02 12-NOV-1991 MTK  INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 MTK  INITIAL RELEASE FOR MARYLAND
C
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE BIGWIN(VALREC,GAM,GTYP,GIND,DRAW,COPY,OPT,AMOUNT)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:NBRCOM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:TGLCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:DBNREC.DEF'
        INCLUDE 'INCLIB:BNGDNAM.DEF'
C
        ! arguments
        INTEGER*4  GAM                        !
        INTEGER*4  GTYP                       !
        INTEGER*4  GIND                       !
        INTEGER*4  DRAW                       !
        INTEGER*4  COPY                       !
        INTEGER*4  OPT                        !
        INTEGER*4  AMOUNT                     !

        ! variables
        INTEGER*4  ST                         !
        INTEGER*4  I                          !
        INTEGER*4  DIV                        !
        INTEGER*4  SHR                        !
        INTEGER*4  SSER                       !
        INTEGER*4  SCHK                       !
        INTEGER*4  DRW                        !
        INTEGER*4  SUB                        ! Subdivisions for bingo.
        INTEGER*4  TSHARES(20)                !
        INTEGER*4  NUMDIV(MAXGAM)             !
        INTEGER*4  TOTWON,BNS                 !
        INTEGER*4  LINCNT(MAXGAM)             !
        INTEGER*4  TKTCNT(MAXGAM)             !
        INTEGER*4  LU(MAXGAM)                 !
        INTEGER*4  PAGE(MAXGAM)               !
        CHARACTER*132 TEMPLINE
        LOGICAL    FIRST_REP

        INTEGER*2  DATE(LDATE_LEN)            !

        CHARACTER  REPHDR(MAXGAM)*47          !
        CHARACTER  REPNAM(MAXGAM)*13          !

        LOGICAL    ALLFLG                     !

        CHARACTER*2   PREFIX(MAXTYP)        !Game Prefix Names.
C
        DATA        PREFIX/'LO','SC','NB','JO','SK','TU',
     *                     'VO','PI','VX','SP','MV','BI','PP','ST','TR','IN',
     *                     'MY','  '/
 
        DATA PAGE/MAXGAM*0/,LINCNT/MAXGAM*0/,TKTCNT/MAXGAM*0/

        LOGICAL FIRST/.TRUE./
        INTEGER*4   FDB(7),LUN
C
C Read Bingo divisions from the game file.
C
        IF(FIRST) THEN
          FIRST = .FALSE.
	  DO I=1,MAXGAM
	     LU(I) = 30+I
	  ENDDO
          IF(GTYP.EQ.TBNG) THEN
            LUN = 7
            CALL GETLUN(LUN)
            CALL OPENW(LUN,GFNAMES(1,GTNTAB(TBNG,GIND)),4,0,0,ST)
            IF(ST.NE.0) THEN
              CALL FILERR(GFNAMES(1,GTNTAB(TBNG,GIND)),1,ST,0)
              CALL GSTOP(GEXIT_FATAL)
            ENDIF
            CALL IOINIT(FDB,LUN,DBNSEC*256)
            CALL READW(FDB,DRAW,DBNREC,ST)
            IF(ST.NE.0) THEN
               CALL FILERR(GFNAMES(1,GTNTAB(TBNG,GIND)),1,ST,0)
               CALL GSTOP(GEXIT_FATAL)
            ENDIF
            CALL CLOSEFIL(FDB)
          ENDIF
        ENDIF   
C
        ALLFLG = .FALSE.
        IF(GAM.LT.0) THEN
            ALLFLG = .TRUE.
            GAM = ABS(GAM)
        ENDIF
C
        IF(OPT.EQ.1) GOTO 1000
        IF(OPT.EQ.2) GOTO 2000
        IF(OPT.EQ.3) GOTO 3000

        RETURN
C
C INITIALIZE REPORT
C
1000    CONTINUE

        IF(GTYP.EQ.TLTO) NUMDIV(GAM)=LTODIV(GIND)
        IF(GTYP.EQ.TSPT) NUMDIV(GAM)=SPTDIV(GIND)
        IF(GTYP.EQ.TTGL) NUMDIV(GAM)=TGLDIV(GIND)
        IF(GTYP.EQ.TKIK) NUMDIV(GAM)=KIKDIV(GIND)
        IF(GTYP.EQ.TBNG) NUMDIV(GAM)=9

        WRITE(REPNAM(GAM),800) PREFIX(GTYP),GIND


        DATE(5)=DAYCDC
        CALL LCDATE(DATE)
        WRITE(REPHDR(GAM),801) GSNAMES(GAM),(DATE(I),I=7,13)
        CALL ROPEN(REPNAM(GAM),LU(GAM),ST)
        CALL TITLE(REPHDR(GAM),'BIGWIN  ',GAM,
     *             LU(GAM),PAGE(GAM),DAYCDC)

        IF(GTYP.EQ.TLTO .OR. GTYP.EQ.TSPT .OR. GTYP.EQ.TKIK .OR.
     *     GTYP.EQ.TBNG .OR. GTYP.EQ.TTGL) THEN
          IF (GTYP .NE. TBNG) THEN
            WRITE(LU(GAM),901),(I,I=1,NUMDIV(GAM))
          ELSE
            WRITE(LU(GAM),906)
          END IF
        ELSE
            WRITE(LU(GAM),902)
        ENDIF

        LINCNT(GAM)=7

        RETURN
C
C PRINT DETAIL LINE
C
2000    CONTINUE
        IF(VALREC(VSTAT).EQ.VCXL.OR.VALREC(VSTAT).EQ.VDEL) RETURN
        IF(LINCNT(GAM).GT.LINSPP) THEN
            LINCNT(GAM)=7
            CALL TITLE(REPHDR(GAM),'BIGWIN  ',GAM,
     *                 LU(GAM),PAGE(GAM),DAYCDC)

            IF(GTYP.EQ.TLTO .OR. GTYP.EQ.TSPT .OR. GTYP.EQ.TKIK .OR.
     *         GTYP.EQ.TBNG .OR. GTYP.EQ.TTGL) THEN
              IF (GTYP .NE. TBNG) THEN
                WRITE(LU(GAM),901),(I,I=1,NUMDIV(GAM))
              ELSE
                WRITE(LU(GAM),906)
              END IF
            ELSE
                WRITE(LU(GAM),902)
            ENDIF
        ENDIF
C
C
        CALL DLOGVAL(VALREC,VDETAIL)
        IF(GTYP.EQ.TLTO .OR. GTYP.EQ.TSPT .OR. GTYP.EQ.TKIK .OR.
     *     GTYP.EQ.TBNG .OR. GTYP.EQ.TTGL) THEN

            CALL FASTSET(0,TSHARES,20)
            DO 2010 I=1,VALREC(VPZOFF)
                DIV = VDETAIL(VDIV,I)
                SHR = VDETAIL(VSHR,I)
                DRW = VDETAIL(VDRW,I)
                SUB = VDETAIL(VSUB,I)
                IF(DRW.NE.DRAW) GOTO 2010
                TSHARES(DIV)=TSHARES(DIV)+SHR
2010        CONTINUE
            IF(.NOT.ALLFLG) THEN
                IF(TSHARES(1).EQ.0.AND.TSHARES(2).EQ.0) RETURN
            ENDIF

            DATE(VCDC)=VALREC(VSCDC)
            CALL LCDATE(DATE)
            CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SSER,SCHK)
            IF (GTYP .NE. TBNG) THEN
              WRITE(LU(GAM),903) AGTTAB(AGTNUM,VALREC(VSTER)),
     *                           VALREC(VSTER),
     *                           DATE(VJUL),
     *                           SSER,
     *                           VALREC(VSCDC),
     *                           VALST(VALREC(VSTAT)),
     *                           VALREC(VEXP),
     *                           (TSHARES(DIV),DIV=1,NUMDIV(GAM)),
     *                           VALREC(VSSER)
              LINCNT(GAM)=LINCNT(GAM)+1
              TKTCNT(GAM)=TKTCNT(GAM)+1
            ELSE
              TEMPLINE = ' '
              WRITE(TEMPLINE,907) AGTTAB(AGTNUM,VALREC(VSTER)),
     *                           VALREC(VSTER),
     *                           DATE(VJUL),
     *                           SSER,
     *                           VALREC(VSCDC),
     *                           VALST(VALREC(VSTAT)),
     *                           VALREC(VEXP),
     *                           VALREC(VSSER)
              FIRST_REP = .TRUE.
              DO DIV=1,BGODIV
                IF(TSHARES(DIV).GT.0) THEN  
                  IF(FIRST_REP) THEN
                    FIRST_REP = .FALSE.
                    WRITE(TEMPLINE(90:),908) TSHARES(DIV),
     *                                       BNGDNAMES(DBNDNR(DIV)),DIV
                    WRITE(LU(GAM),'(A132)') TEMPLINE            
                    LINCNT(GAM)=LINCNT(GAM)+1
                  ELSE  
                    WRITE(LU(GAM),909) TSHARES(DIV),BNGDNAMES(DBNDNR(DIV)),DIV
                    LINCNT(GAM)=LINCNT(GAM)+1
                  ENDIF
                ENDIF
              ENDDO  
              TKTCNT(GAM)=TKTCNT(GAM)+1
            END IF
        ELSE
            TOTWON=0
            DO 2020 I=1,VALREC(VPZOFF)
                DIV = VDETAIL(VDIV,I)
                SHR = VDETAIL(VSHR,I)
                BNS = VDETAIL(VBDR,I)+1
                DRW = VDETAIL(VDRW,I)
                IF(DRW.NE.DRAW) GOTO 2020

                IF(GTYP.EQ.TSCR .OR. GTYP.EQ.TWIT .OR.GTYP.EQ.TTSL .OR.
     *             GTYP.EQ.TBNG .OR.
     *             GTYP.EQ.TCPL .OR. GTYP.EQ.TDBL .OR. GTYP.EQ.TSSC .OR.
     *             GTYP.EQ.TTRP .OR. GTYP.EQ.TSTR) THEN
                    TOTWON=TOTWON+SHR
                END IF
                IF(GTYP.EQ.TNBR) 
     *            TOTWON=TOTWON+SHR*NBRPRZ(DIV,BNS,GIND)
2020        CONTINUE

            IF(TOTWON.LT.AMOUNT.AND.TOTWON.NE.0) RETURN
            DATE(VCDC)=VALREC(VSCDC)
            CALL LCDATE(DATE)
            CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SSER,SCHK)
            WRITE(LU(GAM),904) AGTTAB(AGTNUM,VALREC(VSTER)),
     *                         VALREC(VSTER),
     *                         DATE(VJUL),
     *                         SSER,
     *                         VALREC(VSCDC),
     *                         VALST(VALREC(VSTAT)),
     *                         VALREC(VEXP),
     *                         CMONY(VALREC(VPAMT),11,VALUNIT),
     *                         CMONY(VALREC(VRAMT),11,BETUNIT),
     *                         VALREC(VSSER)
            LINCNT(GAM)=LINCNT(GAM)+1
            TKTCNT(GAM)=TKTCNT(GAM)+1
        ENDIF

        RETURN
C
C PRINT REPORTS
C
3000    CONTINUE
        IF(TKTCNT(GAM).EQ.0) WRITE(LU(GAM),905)

        CALL USRCLOS1(LU(GAM))
        CALL SPOOL(REPNAM(GAM),COPY,ST)

        RETURN
C
C
800     FORMAT(A2,I1,'BIGWIN.REP')
801     FORMAT(A4,' BIG WINNERS REPORT FOR ',7A2)
901     FORMAT(/,2X,' SELLING     SELLING',T43,'CDC   TICKET  ',
     *       2X,'DRAW',/,4X,' AGENT    TERMINAL      TICKET SERIAL  SOLD',
     *         '   STATUS  EXPIRE    ',
     *         <NUMDIV(GAM)>(' DIV ',I2),' INTERNAL SERIAL',/)
902     FORMAT(/,2X,' SELLING     SELLING',T43,'CDC   TICKET  ',
     *       2X,'DRAW',/,4X,' AGENT    TERMINAL      TICKET SERIAL  SOLD',
     *         '   STATUS  EXPIRE   PRIZE AMT  REFUND AMT',/)
903     FORMAT(4X,I8.8,5X,I5,3X,I3.3,'-',I8.8,'-***',
     *         1X,I5,5X,A4,3X,I5,4X,<NUMDIV(GAM)>(I7),6X,I10)
904     FORMAT(4X,I8.8,5X,I5,3X,I3.3,'-',I8.8,'-***',
     *         1X,I5,5X,A4,3X,I5,1X,A11,1X,A11,2X,I10)
905     FORMAT(//,1X,' NO WINNERS FOUND')
906     FORMAT (/,2X,' SELLING     SELLING',T43,'CDC   TICKET  ',
     *       2X,'DRAW',/,4X,' AGENT    TERMINAL      TICKET SERIAL  SOLD',
     *         '   STATUS  EXPIRE',3X,'INTERNAL SERIAL',13X,
     *         'D I V I S I O N S',/)

907     FORMAT (4X,I8.8,5X,I5,3X,I3.3,'-',I8.8,'-***',
     *         1X,I5,5X,A4,3X,I5,2X,I10)

908     FORMAT (I2,' SHARES IN ',A8,1X,'(DIV ',I2,')')
909     FORMAT (89X,I2,' SHARES IN ',A8,1X,'(DIV ',I2,')')



        END
