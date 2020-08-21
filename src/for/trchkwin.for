C
C SUBROUTINE TRCHKWIN
C
C V03 03-JUL-2000 UXN Refund too late played tickets.
C V02 14-JAN-1999 UXN GET_TEBEID ADDED. 
C V01 ??-???-1998 RXK INITIAL RELEASE. 
C  
C SUBROUTINE TO CHECK IF TODAYS COUPLE TICKET IS A WINNER
C AND UPDATE VALIDATION RECORDS
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE TRCHKWIN(TRABUF,V4BUF,WIN)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'

        INTEGER*4 WINTAB(65)
        INTEGER*4 REFTAB(65)
        INTEGER*4 WIN
        INTEGER*4 ST
        INTEGER*4 GIND
        INTEGER*4 AMTWON
        INTEGER*4 TOTWON
        INTEGER*4 COUNT
        INTEGER*4 I
        INTEGER*4 PRZIND
        INTEGER*4 TAXAMT
        INTEGER*4 NETAMT
        INTEGER*4 NAMT
        INTEGER*4 TAMT
        INTEGER*4 REFROWS

        LOGICAL CXLED
        LOGICAL PRVFLG
        LOGICAL HLDFLG

        INTEGER*4 AWNTAB(2,NUMAGT)
        COMMON/BIGWIN/ AWNTAB
	LOGICAL*4 LATEFLG

C--------------------------- Start of code -----------------------------

        WIN    = 0
        COUNT  = 0
        TOTWON = 0
        NETAMT = 0
        TAXAMT = 0
        PRVFLG = .FALSE.
        CXLED  = .FALSE.
        HLDFLG = .FALSE.
        
        CALL FASTSET(0,WINTAB,65)
        CALL FASTSET(0,REFTAB,65)

        ST     = TRABUF(TSTAT)
        GIND   = TRABUF(TGAMIND)

        IF (LTRDRW(GIND) .LT. 0) RETURN
        IF (ST .NE. GOOD .AND. ST .NE. VOID .AND.
     *      ST .NE. INCA .AND. ST .NE. EXCH) RETURN
        IF (TRABUF(TWBEG) .GT. LTRDRW(GIND)) RETURN
        IF (TRABUF(TWEND) .LT. LTRDRW(GIND)) RETURN
        IF (TRABUF(TSTAT) .EQ. VOID .OR. 
     *      TRABUF(TSTAT) .EQ. INCA) CXLED = .TRUE.


C---- Check for cancelled game/row.

	LATEFLG = .FALSE.
        IF (TRREFUND(GIND)) THEN
          IF (.NOT.CXLED) THEN
	     LATEFLG = LTRLAT(LATCDC,GIND).GT.0.AND.
     *                (LTRLAT(LATCDC,GIND).LT.TRABUF(TCDC).OR.
     *                 (LTRLAT(LATCDC,GIND).EQ.TRABUF(TCDC).AND.
     *                  LTRLAT(LATTIM,GIND).LT.TRABUF(TTIM)))
	    CALL TRP_GETREF (TRABUF,WINTAB,REFTAB,WIN,REFROWS,LATEFLG)
	  ENDIF
        ENDIF

C---- Check for win

        IF (LTRSTS(GIND).GE.GAMENV .AND. LTRSTS(GIND).NE.GAMCAN .AND.
     *      .NOT. LATEFLG) THEN
          CALL TRWINLOS(TRABUF,WINTAB,WIN,CXLED)
        END IF

C---- Update validation record.

        IF (WIN .EQ. 0) RETURN

        IF (V4BUF(VFSSER) .NE. 0) THEN
          CALL LOGVAL(VALREC,V4BUF)
          CALL DLOGVAL(VALREC,VDETAIL)
        ELSE
          CALL FASTSET(0,VALREC,VALLEN)
          CALL FASTSET(0,VDETAIL,VPLEN*VMAX)
        END IF

        DO 1000 I = 1,WIN
           AMTWON = WINTAB(I)
           IF (REFTAB(I).NE.0) THEN
              LTRREF(GIND) = LTRREF(GIND) + AMTWON
              LTRWRO(1,PRWON,GIND) = LTRWRO(1,PRWON,GIND) + REFROWS
              LTRWRO(2,PRWON,GIND) = LTRWRO(2,PRWON,GIND) + AMTWON
              LTRWRA(1,PRWON,GIND) = LTRWRA(1,PRWON,GIND) + REFROWS
              LTRWRA(2,PRWON,GIND) = LTRWRA(2,PRWON,GIND) + AMTWON
              LTRWON(GIND) = LTRWON(GIND) + AMTWON 
              LTRWPR(1,PRWON,GIND) = LTRWPR(1,PRWON,GIND) + REFROWS
              LTRWPR(2,PRWON,GIND) = LTRWPR(2,PRWON,GIND) + AMTWON
           ENDIF

          VALREC(VPZOFF) = VALREC(VPZOFF) + 1
          PRZIND = VALREC(VPZOFF)
          IF (PRZIND .GT. VMAX) THEN
            TYPE*,IAM(),' Prize table overflow ',TRABUF(TCDC),TRABUF(TSER)
            CALL GPAUSE
            HLDFLG = .TRUE.
            VALREC(VPZOFF) = VMAX
            GOTO 20
          END IF

          VDETAIL(VKIK,PRZIND) = 0
          VDETAIL(VKI2,PRZIND) = 0
          VDETAIL(VPRG,PRZIND) = 0
          VDETAIL(VREF,PRZIND) = 0
          VDETAIL(VBDR,PRZIND) = 0
          VDETAIL(VDIV,PRZIND) = 0
          VDETAIL(VUPD,PRZIND) = 1
          VDETAIL(VSHR,PRZIND) = AMTWON
          IF (REFTAB(I).NE.0) VDETAIL(VREF,PRZIND) = 1
          VDETAIL(VDRW,PRZIND) = LTRDRW(GIND)

20        CONTINUE

          IF (REFTAB(I).NE.0) THEN
            VALREC(VRAMT) = VALREC(VRAMT) + AMTWON
          ELSE
            TOTWON = TOTWON + AMTWON
          END IF

1000    CONTINUE

C---- Update validation header if new winner.

        IF (VALREC(VSTAT) .EQ. VNOWIN) THEN
          VALREC(VSCDC) = TRABUF(TCDC)
          VALREC(VSTER) = TRABUF(TTER)
          VALREC(VSSER) = TRABUF(TSER)
          VALREC(VEXP ) = TRABUF(TWEND)
          VALREC(VKEXP) = TRABUF(TWKEND)
          VALREC(VGAM ) = TRABUF(TGAM)
          VALREC(VKGME) = TRABUF(TWKGME)
          VALREC(VGTYP) = TRABUF(TGAMTYP)
          VALREC(VGIND) = TRABUF(TGAMIND)
          VALREC(VFRAC) = TRABUF(TFRAC)
          VALREC(VBNKID) = TRABUF(TWBNKID)
          VALREC(VBNKNUM) = TRABUF(TWBNKNM)
        ENDIF

C---- Get taxes and set priv pay flag
C---- and update big winner commission table.

        IF (.NOT. CXLED .AND. TOTWON .NE. 0) THEN
          LTRWON(GIND) = LTRWON(GIND) + TOTWON
 
          DO 2000 I = 1,WIN
             AMTWON = WINTAB(I)
             IF (REFTAB(I).EQ.0) THEN
               LTRWPR(1,PRWON,GIND) = LTRWPR(1,PRWON,GIND) + 1
               LTRWPR(2,PRWON,GIND) = LTRWPR(2,PRWON,GIND) + AMTWON
               LTRWPO(1,PRWON,GIND) = LTRWPO(1,PRWON,GIND) + 1
               LTRWPO(2,PRWON,GIND) = LTRWPO(2,PRWON,GIND) + AMTWON
               CALL GETTAX(AMTWON,TAMT,NAMT)
               NETAMT = NETAMT + NAMT
               TAXAMT = TAXAMT + TAMT
             END IF
2000      CONTINUE

          IF (NETAMT .GT. REDMAX(TRABUF(TGAM))) PRVFLG = .TRUE.

          LTRTAX(GIND) = LTRTAX(GIND) + TAXAMT

          IF (HVCLVL .NE. 0) THEN
            IF (NETAMT .GE. HVCLVL) THEN
              AWNTAB(1,TRABUF(TTER)) = AWNTAB(1,TRABUF(TTER)) + 1
              AWNTAB(2,TRABUF(TTER)) = AWNTAB(2,TRABUF(TTER)) + NETAMT
            END IF
          END IF
          VALREC(VPAMT) = VALREC(VPAMT) + TOTWON
          VALREC(VTAMT) = VALREC(VTAMT) + TAXAMT
        END IF

        VALREC(VWCDC) = DAYCDC
        VALREC(VSTAT) = VUNCSH
        IF (PRVFLG) VALREC(VSTAT) = VPRPAY
        IF (HLDFLG) VALREC(VSTAT) = VHOLD
        IF (TRABUF(TSTAT) .EQ. VOID) VALREC(VSTAT) = VCXL
        IF (TRABUF(TSTAT) .EQ. INCA) VALREC(VSTAT) = VDEL
        CALL DVALLOG(VALREC,VDETAIL)
        CALL VALLOG (VALREC,V4BUF  )

        RETURN

        END
