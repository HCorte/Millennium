C
C SUBROUTINE CANWIN
C
C CANWIN.FOR
C
C V16 29-NOV-2000 UXN TOTOGOLA ADDED.
C V15 13-JAN-2000 RXK Def-file for Bingo division names added
C V14 13-OCT-1999 RXK World Tour game added
C V13 21-MAY-1999 UXN MAXGAM changes.
C V12 18-JAN-1996 RXK Today's Couple and Super Double added 
C V11 14-SEP-1995 RXK Change of RAVI report names
C V10 26-JUL-1995 PXB Hardcoded two divisions for ravi came only if 
C                     game not active.
C V09 14-DEC-1994 PXB Added bingo cancelled winners report.
C V08 22-FEB-1994 HXK REMOVED BUG.
C V07 16-SEP-1993 SXH Added bank id, number and draw won to report
C V06 02-SEP-1993 SXH AGENT # 8 CHARS
C V05 13-JUL-1993 SXH Released for Finland
C V04 19-JUL-1993 HXK brought LU values up to MAXGAM
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 12-NOV-1991 MTK INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 MTK INITIAL RELEASE FOR MARYLAND
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
        SUBROUTINE CANWIN(VALREC,GAM,GTYP,GIND,DRAW,COPY,OPT)
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
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:TGLCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:BNGDNAM.DEF'
C
        INCLUDE 'INCLIB:DBNREC.DEF'
C
        ! arguments
        INTEGER*4  GAM                          !
        INTEGER*4  GTYP                         !
        INTEGER*4  GIND                         !
        INTEGER*4  DRAW                         !
        INTEGER*4  COPY                         !
        INTEGER*4  OPT                          !

        ! arguments
        INTEGER*4  ST                           !
        INTEGER*4  I                            !
        INTEGER*4  DIV                          !
        INTEGER*4  SHR                          !
        INTEGER*4  SSER                         !
        INTEGER*4  SCHK                         !
        INTEGER*4  DRW                          !
        INTEGER*4  SUB                          !
        INTEGER*4  PFLAG                        !
        INTEGER*4  LINCNT(MAXGAM)               !
        INTEGER*4  TKTCNT(MAXGAM)               !
        INTEGER*4  LU(MAXGAM)                   !
        INTEGER*4  PAGE(MAXGAM)                 !
        INTEGER*4  TSHARES(20)                  !
        INTEGER*4  NUMDIV(MAXGAM)               !
        CHARACTER*132 TEMPLINE
        LOGICAL    FIRST_REP

        INTEGER*2  DATE(LDATE_LEN)              !

        CHARACTER  REPHDR(MAXGAM)*53            !
        CHARACTER  REPNAM(MAXGAM)*13            !
        CHARACTER*2   PREFIX(MAXTYP)        !Game Prefix Names.
C
      DATA        PREFIX/'LO','VA','NB','JO','SK','TU',
     *                   'VO','PI','VX','SP','MV','BI','PP','ST','TR','IN',
     *                   'MY','  '/
C
        DATA PAGE/MAXGAM*0/,LINCNT/MAXGAM*0/,TKTCNT/MAXGAM*0/

        LOGICAL FIRST/.TRUE./
        INTEGER*4   FDB(7),LUN
C
C Read Bingo divisions from the game file.
C
        IF(FIRST) THEN
          FIRST = .FALSE.
	  DO I=1,MAXGAM
	     LU(I) = 10+I
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
C
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
        IF(GTYP.EQ.TBNG) NUMDIV(GAM) = 9
        WRITE(REPNAM(GAM),800) PREFIX(GTYP),GIND


        DATE(5)=DAYCDC
        CALL LCDATE(DATE)
        WRITE(REPHDR(GAM),801) GSNAMES(GAM),(DATE(I),I=9,13)
        CALL ROPEN(REPNAM(GAM),LU(GAM),ST)
        CALL TITLE(REPHDR(GAM),'CANWIN  ',GAM,
     *             LU(GAM),PAGE(GAM),DAYCDC)

        IF(GTYP.EQ.TLTO .OR. GTYP.EQ.TSPT .OR. GTYP.EQ.TTGL .OR. 
     *     GTYP.EQ.TKIK) THEN
           WRITE(LU(GAM),901),(I,I=1,NUMDIV(GAM))
        ELSEIF(GTYP .EQ. TBNG) THEN
           WRITE(LU(GAM),908)
        ELSE
            WRITE(LU(GAM),902)
        ENDIF
        LINCNT(GAM)=7

        RETURN
C
C PRINT DETAIL LINE
C
2000    CONTINUE
        IF(VALREC(VSTAT).NE.VCXL.AND.VALREC(VSTAT).NE.VDEL) RETURN
        PFLAG=0
        IF(LINCNT(GAM).GT.LINSPP) THEN
            LINCNT(GAM)=7
            CALL TITLE(REPHDR(GAM),'CANWIN  ',GAM,
     *                 LU(GAM),PAGE(GAM),DAYCDC)
            IF(GTYP.EQ.TLTO .OR. GTYP.EQ.TSPT .OR. GTYP.EQ.TKIK.OR.
     *         GTYP.EQ.TTGL) THEN
               WRITE(LU(GAM),901),(I,I=1,NUMDIV(GAM))
            ELSEIF(GTYP .EQ. TBNG) THEN
               WRITE(LU(GAM),908)
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
            DO 2010 I = 1, VALREC(VPZOFF)
                DIV = VDETAIL(VDIV,I)
                SHR = VDETAIL(VSHR,I)
                DRW = VDETAIL(VDRW,I)
                SUB = VDETAIL(VSUB,I)
                IF(DRAW.NE.DRW) GOTO 2010
                TSHARES(DIV) = TSHARES(DIV)+SHR
                PFLAG=1
2010        CONTINUE

            IF(PFLAG.EQ.0) RETURN
            DATE(VCDC)=VALREC(VSCDC)
            CALL LCDATE(DATE)
            CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SSER,SCHK)

            IF(GTYP.EQ.TLTO .OR. GTYP.EQ.TSPT .OR. GTYP.EQ.TKIK .OR.
     *         GTYP.EQ.TTGL) THEN
               WRITE(LU(GAM),903) AGTTAB(AGTNUM,VALREC(VSTER)),
     *                            VALREC(VSTER),
     *                            DATE(VJUL),
     *                            SSER,
     *                            SCHK/10,
     *                            VALREC(VSCDC),
     *                            VALST(VALREC(VSTAT)),
     *                            VALREC(VBNKID),
     *                            VALREC(VBNKNUM),
     *                            VDETAIL(VDRW,1),
     *                            VALREC(VEXP),
     *                            (TSHARES(DIV),DIV=1,NUMDIV(GAM))

               LINCNT(GAM) = LINCNT(GAM) + 1
               TKTCNT(GAM) = TKTCNT(GAM) + 1
            ELSEIF(GTYP.EQ.TBNG) THEN
               TEMPLINE = ' '
               WRITE(TEMPLINE,906) AGTTAB(AGTNUM,VALREC(VSTER)),
     *                            VALREC(VSTER),
     *                            DATE(VJUL),
     *                            SSER,
     *                            SCHK/10,
     *                            VALREC(VSCDC),
     *                            VALST(VALREC(VSTAT)),
     *                            VALREC(VBNKID),
     *                            VALREC(VBNKNUM),
     *                            VDETAIL(VDRW,1),
     *                            VALREC(VEXP)
              FIRST_REP = .TRUE.
              DO I=1,BGODIV
                IF(TSHARES(I).GT.0) THEN
                  IF(FIRST_REP) THEN
                    FIRST_REP = .FALSE.
                    WRITE(TEMPLINE(100:),907) TSHARES(I),BNGDNAMES(DBNDNR(I)),I
                    WRITE(LU(GAM),'(A132)') TEMPLINE
                    LINCNT(GAM) = LINCNT(GAM) + 1
                  ELSE
                    WRITE(LU(GAM),909)  TSHARES(I),BNGDNAMES(DBNDNR(I))
                    LINCNT(GAM) = LINCNT(GAM) + 1
                  ENDIF
                ENDIF
              ENDDO
              TKTCNT(GAM) = TKTCNT(GAM) + 1
            END IF
        ELSE
            DO 2020 I=1,VALREC(VPZOFF)
                DRW=VDETAIL(VDRW,I)
                IF(DRAW.NE.DRW) GOTO 2020
                PFLAG=1
2020        CONTINUE

            IF(PFLAG.EQ.0) RETURN
            DATE(VCDC)=VALREC(VSCDC)
            CALL LCDATE(DATE)
            CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SSER,SCHK)
            WRITE(LU(GAM),904)  AGTTAB(AGTNUM,VALREC(VSTER)),
     *                          VALREC(VSTER),
     *                          DATE(VJUL),
     *                          SSER,
     *                          SCHK/10,
     *                          VALREC(VSCDC),
     *                          VALST(VALREC(VSTAT)),
     *                          VALREC(VBNKID),
     *                          VALREC(VBNKNUM),
     *                          VDETAIL(VDRW,1),
     *                          VALREC(VEXP),
     *                          CMONY(VALREC(VPAMT),11,VALUNIT),
     *                          CMONY(VALREC(VRAMT),11,BETUNIT)
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
800     FORMAT(A2,I1,'CANWIN.REP')
801     FORMAT(A4,' CANCELLED WINNERS REPORT FOR ',5A2)
901     FORMAT(/,2X,' SELLING     SELLING',T43,'CDC   TICKET  ',
     *       '   BANK   BANK',
     *       4X,'     DRAW',/,4X,' AGENT    TERMINAL      TICKET SERIAL  SOLD',
     *         '   STATUS     ID   ACCOUNT     WON/EXP     ',
     *         <NUMDIV(GAM)>(' DIV ',I2),/)
9011    FORMAT(/,2X,' SELLING     SELLING',T43,'CDC   TICKET  ',
     *       '   BANK   BANK',
     *       4X,'     DRAW',/,4X,' AGENT    TERMINAL      TICKET SERIAL  SOLD',
     *         '   STATUS     ID   ACCOUNT     WON/EXP  ',
     *         <NUMDIV(GAM)>(' DIV',I1),/)
902     FORMAT(/,2X,' SELLING     SELLING',T43,'CDC   TICKET  ',
     *       '   BANK   BANK',
     *       4X,'     DRAW',/,4X,' AGENT    TERMINAL      TICKET SERIAL  SOLD',
     *       '   STATUS     ID   ACCOUNT     WON/EXP  PRIZE AMT  REFUND AMT',/)
903     FORMAT(4X,I8.8,5X,I5,3X,I3.3,'-',I8.8,'-',I2.2,'*',
     *         1X,I5,5X,A4,3X,I6,1X,I8,1X,I5,'/',I5,4X,9(I7))
9031    FORMAT(4X,I8.8,5X,I5,3X,I3.3,'-',I8.8,'-',I2.2,'*',
     *         1X,I5,5X,A4,3X,I6,1X,I8,1X,I5,'/',I5,1X,9(I5))
904     FORMAT(4X,I8.8,5X,I5,3X,I3.3,'-',I8.8,'-',I2.2,'*',
     *         1X,I5,5X,A4,3X,I6,1X,I8,1X,I5,'/',I5,1X,A11,1X,A11)
905     FORMAT(//,1X,' NO WINNERS FOUND')

906     FORMAT(4X,I8.8,5X,I5,3X,I3.3,'-',I8.8,'-',I2.2,'*',
     *         1X,I5,5X,A4,3X,I6,1X,I8,1X,I5,'/',I5,20X)

907     FORMAT(I2,' SHARES IN ',A8,1X,'(DIV ',I2,')')

908     FORMAT(/,2X,' SELLING     SELLING',T43,' CDC    TICKET  ',
     *       '   BANK   BANK',
     *       4X,'   DRAW',/,4X,' AGENT    TERMINAL      TICKET SERIAL  SOLD',
     *         '   STATUS      ID   ACCOUNT     WON/EXP     ',
     *         10X,'D I V I S I O N S',/)

909     FORMAT(91X,I2,' SHARES IN ',A8,1X,'(DIV ',I2,')')
        END
