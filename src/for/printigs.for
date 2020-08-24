C SUBROUTINE PRINTIGS
C
C V03 23-APR-2018 SCML QR CODE + NEW MARKET TYPES PROJECT
C V02 13-APR-2016 SCML M16 PROJECT
C V01 10-MAR-2014 SCML Placard Project
C
C SUBROUTINE TO PRINT IGS TRANSACTIONS IN TMIR FORMAT
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE PRINTIGS(TRABUF,PUNIT,DETAIL,LINCNT,ODS_JUSTONE,SCRAM,IGSTOTAL)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:NAMCMD.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:IGSTNAMES.DEF'
        INCLUDE 'INCLIB:HASF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C

        ! arguments
        LOGICAL    DETAIL                  !
        INTEGER*4  PUNIT
        INTEGER*4  IGSTOTAL(0:NUMIGSTTYP,2) 
C
        LOGICAL    ODS_JUSTONE             !
        LOGICAL    SCRAM                   !
        LOGICAL    DUMMY                   ! Avoid warning

        ! variables

        BYTE OUTTAB(500)
        INTEGER*4 I,IND
!        CHARACTER*12 TRANSTYPE
        INTEGER*4  CHECK                   !
        INTEGER*4  SERIAL                  !
        INTEGER*4  PAGE                    !
        INTEGER*4  LINCNT,ST,TRABUFERROR                  !
        INTEGER*4 I4TEMP
        INTEGER*2 I2TEMP(2)
        BYTE      I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
        INTEGER*4  LINES
!       REAL*8        COMMAND     ! Avoid Warning
!        DATA LINCNT/100/,LINES/0/ ! Avoid Warning COMMAND/0/
        
        INTEGER*8  I8TMP
        INTEGER*4  I4TMP(2)
        EQUIVALENCE (I8TMP,I4TMP)
        
        INTEGER*8  WEXSER ! WAGER EXTERNAL SERIAL
        INTEGER*8  CEXSER ! CANCEL EXTERNAL SERIAL
C        INTEGER*8  VEXSER ! VALIDATION EXTERNAL SERIAL
        INTEGER*8  PEXSER ! PAYMENT EXTERNAL SERIAL
        INTEGER*8  TMSGID ! TERMINAL MESSAGE ID
        INTEGER*8  TIGSR_MVR ! MEDIA VERSION

        CHARACTER*10 C10MONY

        INTEGER*4  NIB(6)
        CHARACTER*24 CNIB
        EQUIVALENCE (NIB,CNIB)
        INTEGER*4  BLANK
        
        INTEGER*4 CXERR_LEN
        PARAMETER (CXERR_LEN = 9) ! XERR HAS 9 CHARACTERS
        CHARACTER*12 CXERR ! XERR_I4LEN * 4
        
        CHARACTER*8 IAGT_NO !FUNCTION
        CHARACTER*9 WAGTYPDESC
C
        CHARACTER*13 BETEMODE(0:255) !BET ENTRY MODE DESCRIPTION                !V03
        DATA BETEMODE /                                                         !V03
     *                   '             '                                        !x00    : N.A.
     *              ,    'QR Web (iRdr)'                                        !x01    : QR CODE WEB READ USING INTERNAL CCD READER
     *              ,    'QR App (iRdr)'                                        !x02    : QR CODE APP MOBILE PLACARD READ USING INTERNAL CCD READER
     *              ,    'QR Slf (iRdr)'                                        !x03    : QR CODE SELF-CARE READ USING INTERNAL CCD READER
     *              ,    'QR Web       '                                        !x04    : QR CODE WEB READ USING EXTERNAL CCD READER
     *              ,    'QR App       '                                        !x05    : QR CODE APP MOBILE PLACARD READ USING EXTERNAL CCD READER
     *              ,    'QR Slf       '                                        !x06    : QR CODE SELF-CARE READ USING EXTERNAL CCD READER
     *              ,  9*'             '                                        !x07-x0F: N.D.
     *              ,    'Manual       '                                        !x10    : MANUAL ENTRY (IN TERMINAL SCREEN)
     *              , 47*'             '                                        !x11-x3F: N.D.
     *              ,    'Coupon       '                                        !x40    : COUPON READ USING READER (BEFORE AND FROM V03)
     *              ,  3*'             '                                        !x41-x43: N.D.
     *              ,    'Coupon       '                                        !x44    : COUPON READ USING READER (UP TO V02)
     *              ,187*'             '                                        !x45-xFF: N.D.
     *                /
C
        REAL*8 ODSGTNAM
C
          DATA BLANK /'    '/
C
        DUMMY = ODS_JUSTONE ! Avoid Warning
C
        IF(LINCNT .GT. LINSPP) THEN
            CALL TITLE('TRANSACTION FILE REPORT','    TMIR',1,
     *               PUNIT,PAGE,DAYCDC)
            WRITE(PUNIT,900)
            LINCNT=7
        ENDIF 
C
        SERIAL = TRABUF(TSER)
        IF(SCRAM) CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),SERIAL,CHECK)
        IF(TRABUF(TIGS_TTYP) .EQ. IGSWAG) THEN
          IF (TRABUF(TGAMTYP) .EQ. TODS .AND. TRABUF(TGAMIND) .EQ. 1) GOTO 1000
        ENDIF
        IF(TRABUF(TIGS_TTYP) .EQ. IGSCAN) THEN
          IF (TRABUF(TGAMTYP) .EQ. TODS .AND. TRABUF(TGAMIND) .EQ. 1) GOTO 2000
        ENDIF
        IF(TRABUF(TIGS_TTYP) .EQ. IGSVAL) THEN
          IF (TRABUF(TGAMTYP) .EQ. TODS .AND. TRABUF(TGAMIND) .EQ. 1) GOTO 3000
        ENDIF
        IF(TRABUF(TIGS_TTYP) .EQ. IGSPAY) THEN
          IF (TRABUF(TGAMTYP) .EQ. TODS .AND. TRABUF(TGAMIND) .EQ. 1) GOTO 4000
        ENDIF
        IF(TRABUF(TIGS_TTYP) .EQ. IGSREP) THEN
          IF (TRABUF(TGAMTYP) .EQ. TODS .AND. TRABUF(TGAMIND) .EQ. 1) GOTO 5000
        ENDIF
! UNKNOWN TRANSACTION
        WRITE(PUNIT,1903) STAT(TRABUF(TSTAT)),
     *                   ERROR(TRABUF(TERR)),
     *                   TTYPE(TRABUF(TTYP)),
     *                   SERIAL,
     *                   DISTIM(TRABUF(TTIM)),
     *                   TRABUF(TTER),
     *                   TRABUF(TTRN),
     *                   TRABUF(TCDC),
     *                   TRABUF(TGAM),
     *                   TRABUF(TGAMTYP),
     *                   TRABUF(TGAMIND),
     *                   TRABUF(TSIZE),
     *                   TRABUF(TIGS_TTYP),
     *                   IAGT_NO(TRABUF(TAGT))
        RETURN
C
C PRINT WAGERS
C
1000    CONTINUE
!        TRANSTYPE = 'APOSTA'
        LINCNT = LINCNT+1
        IF ((TRABUF(TSTAT) .EQ. GOOD).AND.(TRABUF(TERR) .EQ. NOER)) THEN
          IGSTOTAL(TRABUF(TIGS_TTYP),1) = IGSTOTAL(TRABUF(TIGS_TTYP),1) + 1
          IGSTOTAL(TRABUF(TIGS_TTYP),2) = IGSTOTAL(TRABUF(TIGS_TTYP),2) + TRABUF(TIGSW_TSTK)
          IGSTOTAL(NUMIGSTTYP,1)        = IGSTOTAL(NUMIGSTTYP,1) + 1
        ENDIF
        
        IF (TRABUF(TIGSW_STID) .EQ. 1) THEN
          WAGTYPDESC = 'SIMPLES  '
        ELSEIF (TRABUF(TIGSW_STID) .GT. 1) THEN
          IF (TRABUF(TIGSW_TBET) .EQ. TRABUF(TIGSW_STID)) THEN
            WAGTYPDESC = 'COMBINADA'
          ELSEIF (TRABUF(TIGSW_TBET) .GT. TRABUF(TIGSW_STID)) THEN
            WAGTYPDESC = 'MULTIPLA '
          ENDIF
        ENDIF
        
        WRITE(PUNIT,1902) STAT(TRABUF(TSTAT)),
     *                   ERROR(TRABUF(TERR)),
     *                   TTYPE(TRABUF(TTYP)),
     *                   SERIAL,
     *                   DISTIM(TRABUF(TTIM)),
     *                   TRABUF(TTER),
     *                   TRABUF(TTRN),
     *                   TRABUF(TCDC),
     *                   TRABUF(TGAM),
     *                   GTNAMES(TRABUF(TGAMTYP)),
     *                   TRABUF(TGAMIND),
     *                   TRABUF(TSIZE),
     *                   TRIM(IGSTTYPE(TRABUF(TIGS_TTYP))),
     *                   WAGTYPDESC,
     *                   IAGT_NO(TRABUF(TAGT)),
     *                   TRABUF(TTSTCS)
        IF (DETAIL) THEN   
          I4TMP(2) = TRABUF(TIGSW_MIDH)
          I4TMP(1) = TRABUF(TIGSW_MIDL)
          TMSGID = I8TMP
          
          I4TMP(2) = TRABUF(TIGSW_WRSH)
          I4TMP(1) = TRABUF(TIGSW_WRSL)
          WEXSER = I8TMP
          
          WRITE(PUNIT,902) TMSGID ! MESSAGE ID
          
          !PORTUGUESE PLAYER VAT IDENTIFICATION NUMBER (9 DIGITS)
          WRITE(PUNIT,935) TRABUF(TIGSW_PNIF)
          
          WRITE(PUNIT,903) TRABUF(TIGSW_XGID) ! ABP GAME ID
          WRITE(PUNIT,904) TRABUF(TIGSW_STID) ! SUBTYPE ID
          
          ! UNIT STAKE OF THE BET
          WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSW_USTK),10,BETUNIT)
          WRITE(PUNIT,905) TRIM(ADJUSTL(C10MONY)) 
          
          WRITE(PUNIT,906) TRABUF(TIGSW_TBET) ! TOTAL BETS/NUMBER OF SELECTIONS (MAX = 8)
C
          !BET ENTRY MODE DESCRIPTION                                           !V03
          WRITE(PUNIT,914) TRIM(BETEMODE(TRABUF(TTSTCS)))                       !V03
C
          WRITE(PUNIT,907) TRABUF(TIGS_XREF) ! MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
          
          ! IGS ERROR CODE DESCRIPTION
C          IF((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUF(TERR) .EQ. NOER)) THEN
C            WRITE(PUNIT,937)
C            type*, 'IF'
C          ELSE
            WRITE(CXERR,'(<XERR_I4LEN>A4)') (TRABUF(TIGS_XERR+I),I=0,XERR_I4LEN-1)
            IF (TRABUF(TIGS_SERR) .GE. SEC_LIDX .AND. TRABUF(TIGS_SERR) .LE. SEC_RIDX) THEN
              WRITE(PUNIT,908) SYSTEMERRORCODE(TRABUF(TIGS_SERR)), CXERR
            ELSE
              WRITE(PUNIT,9081) TRABUF(TIGS_SERR), CXERR, ' (UNKNOWN SYSTEM) '
            ENDIF
C          WRITE(PUNIT,908) SYSTEMERRORCODE(TRABUF(TIGS_SERR)), (TRABUF(TIGS_XERR+I),I=0,XERR_I4LEN-1) ! IGS ERROR CODE DESCRIPTION
C            type*, 'ELSE'
C          ENDIF
          
          ! WAGER BET REFERENCE
          IF (SCRAM .EQ. .TRUE.) THEN
            WRITE(PUNIT,9091) TRABUF(TIGSW_WRDY),
     *                        TRABUF(TIGSW_WRDM),
     *                        TRABUF(TIGSW_WRDD),
     *                        TRABUF(TIGSW_WRGM),
     *                        WEXSER,
     *                        TRABUF(TIGSW_WRCD)
          ELSE
            WRITE(PUNIT,909) TRABUF(TIGSW_WRDY),
     *                       TRABUF(TIGSW_WRDM),
     *                       TRABUF(TIGSW_WRDD),
     *                       TRABUF(TIGSW_WRGM),
     *                       WEXSER,
     *                       '***'
          ENDIF
          
C TESTES          ! WAGER BET REFERENCE  !!! REMOVER ESTA INSTRUCAO TAL COMO A SUA RESPECTIVA FORMATACAO!!!
C          WRITE(PUNIT,939) TRABUF(TIGSW_WRDY),
C     *                     TRABUF(TIGSW_WRDM),
C     *                     TRABUF(TIGSW_WRDD),
C     *                     TRABUF(TIGSW_WRGM),
C     *                     WEXSER,
C     *                     TRABUF(TIGSW_WRCD)
          
          ! BET CREATION DATETIME
          WRITE(PUNIT,910) TRABUF(TIGSW_WCDY),
     *                     TRABUF(TIGSW_WCDM),
     *                     TRABUF(TIGSW_WCDD),
     *                     TRABUF(TIGSW_WCTH),
     *                     TRABUF(TIGSW_WCTM),
     *                     TRABUF(TIGSW_WCTS)
          
          ! BET LAST EVENT DATE
          WRITE(PUNIT,911) TRABUF(TIGSW_LEDY),
     *                     TRABUF(TIGSW_LEDM),
     *                     TRABUF(TIGSW_LEDD)
          
          ! BET TOTAL STAKE
          WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSW_TSTK),10,BETUNIT)
          WRITE(PUNIT,912) TRIM(ADJUSTL(C10MONY))
          
          ! BET MAXIMUM POSSIBLE RETURNS
          WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSW_MAXR),10,BETUNIT)
          WRITE(PUNIT,913) TRIM(ADJUSTL(C10MONY)) ! 
          
          WRITE(PUNIT,*)
          
          LINCNT = LINCNT+14
        ENDIF
        RETURN
C
C PRINT CANCEL
C
2000    CONTINUE
!        TRANSTYPE = 'CANCELAMENTO'
        LINCNT = LINCNT+1
        TRABUFERROR = TRABUF(TERR)
        IF ((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUFERROR .EQ. NOER)) THEN
          IGSTOTAL(TRABUF(TIGS_TTYP),1) = IGSTOTAL(TRABUF(TIGS_TTYP),1) + 1
          IGSTOTAL(TRABUF(TIGS_TTYP),2) = IGSTOTAL(TRABUF(TIGS_TTYP),2) + TRABUF(TIGSC_CAMT)
          IGSTOTAL(NUMIGSTTYP,1)        = IGSTOTAL(NUMIGSTTYP,1) + 1
        ENDIF
        WRITE(PUNIT,1901) STAT(TRABUF(TSTAT)),
     *                   ERROR(TRABUFERROR),
     *                   TTYPE(TRABUF(TTYP)),
     *                   SERIAL,
     *                   DISTIM(TRABUF(TTIM)),
     *                   TRABUF(TTER),
     *                   TRABUF(TTRN),
     *                   TRABUF(TCDC),
     *                   TRABUF(TGAM),
     *                   GTNAMES(TRABUF(TGAMTYP)),
     *                   TRABUF(TGAMIND),
     *                   TRABUF(TSIZE),
     *                   IGSTTYPE(TRABUF(TIGS_TTYP)),
     *                   IAGT_NO(TRABUF(TAGT))
     
        IF (DETAIL) THEN
          I4TMP(2) = TRABUF(TIGSC_MIDH)
          I4TMP(1) = TRABUF(TIGSC_MIDL)
          TMSGID = I8TMP
          
          I4TMP(2) = TRABUF(TIGSC_WRSH)
          I4TMP(1) = TRABUF(TIGSC_WRSL)
          WEXSER = I8TMP
          
          WRITE(PUNIT,902) TMSGID ! MESSAGE ID
          
          ! CANCEL BET REFERENCE
          IF (SCRAM .EQ. .TRUE.) THEN
            WRITE(PUNIT,9091) TRABUF(TIGSC_WRDY),
     *                        TRABUF(TIGSC_WRDM),
     *                        TRABUF(TIGSC_WRDD),
     *                        TRABUF(TIGSC_WRGM),
     *                        WEXSER,
     *                        TRABUF(TIGSC_WRCD)
          ELSE
            WRITE(PUNIT,909) TRABUF(TIGSC_WRDY),
     *                       TRABUF(TIGSC_WRDM),
     *                       TRABUF(TIGSC_WRDD),
     *                       TRABUF(TIGSC_WRGM),
     *                       WEXSER,
     *                       '***'
          ENDIF
          
C TESTES          ! CANCEL BET REFERENCE  !!! REMOVER ESTA INSTRUCAO TAL COMO A SUA RESPECTIVA FORMATACAO!!!
C          WRITE(PUNIT,939) TRABUF(TIGSC_WRDY),
C     *                     TRABUF(TIGSC_WRDM),
C     *                     TRABUF(TIGSC_WRDD),
C     *                     TRABUF(TIGSC_WRGM),
C     *                     WEXSER,
C     *                     TRABUF(TIGSC_WRCD)
          
          WRITE(PUNIT,907) TRABUF(TIGS_XREF) ! MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
          
          ! IGS ERROR CODE DESCRIPTION
C          IF((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUFERROR .EQ. NOER)) THEN
C            WRITE(PUNIT,937)
C          ELSE
            WRITE(CXERR,'(<XERR_I4LEN>A4)') (TRABUF(TIGS_XERR+I),I=0,XERR_I4LEN-1)
            IF (TRABUF(TIGS_SERR) .GE. SEC_LIDX .AND. TRABUF(TIGS_SERR) .LE. SEC_RIDX) THEN
              WRITE(PUNIT,908) SYSTEMERRORCODE(TRABUF(TIGS_SERR)), CXERR
            ELSE
              WRITE(PUNIT,9081) TRABUF(TIGS_SERR), CXERR, ' (UNKNOWN SYSTEM) '
            ENDIF
C          WRITE(PUNIT,908) SYSTEMERRORCODE(TRABUF(TIGS_SERR)), (TRABUF(TIGS_XERR+I),I=0,XERR_I4LEN-1) ! IGS ERROR CODE DESCRIPTION
C          ENDIF
          
          I4TMP(2) = TRABUF(TIGSC_CRSH)
          I4TMP(1) = TRABUF(TIGSC_CRSL)
          CEXSER = I8TMP
          
          ! CANCEL REFERENCE
          IF (SCRAM .EQ. .TRUE.) THEN
            WRITE(PUNIT,9151) TRABUF(TIGSC_CRDY),
     *                        TRABUF(TIGSC_CRDM),
     *                        TRABUF(TIGSC_CRDD),
     *                        TRABUF(TIGSC_CRGM),
     *                        CEXSER,
     *                        TRABUF(TIGSC_CRCD)
          ELSE
            WRITE(PUNIT,915) TRABUF(TIGSC_CRDY),
     *                       TRABUF(TIGSC_CRDM),
     *                       TRABUF(TIGSC_CRDD),
     *                       TRABUF(TIGSC_CRGM),
     *                       CEXSER,
     *                       '***'
          ENDIF
          
C TESTES          ! CANCEL CANCEL REFERENCE  !!! REMOVER ESTA INSTRUCAO TAL COMO A SUA RESPECTIVA FORMATACAO!!!
C          WRITE(PUNIT,938) TRABUF(TIGSC_CRDY),
C     *                     TRABUF(TIGSC_CRDM),
C     *                     TRABUF(TIGSC_CRDD),
C     *                     TRABUF(TIGSC_CRGM),
C     *                     CEXSER,
C     *                     TRABUF(TIGSC_CRCD)
          
          ! CANCEL CREATION DATETIME
          WRITE(PUNIT,916) TRABUF(TIGSC_WCDY),
     *                     TRABUF(TIGSC_WCDM),
     *                     TRABUF(TIGSC_WCDD),
     *                     TRABUF(TIGSC_WCTH),
     *                     TRABUF(TIGSC_WCTM),
     *                     TRABUF(TIGSC_WCTS)
          
          ! CANCEL AMOUNT (WAGER UNITS)
          WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSC_CAMT),10,BETUNIT)
          WRITE(PUNIT,917) TRIM(ADJUSTL(C10MONY))
          
          WRITE(PUNIT,*)
          
          LINCNT = LINCNT+8
        ENDIF
        RETURN 
C
C PRINT VALIDATION (INQUIRY)
C
3000    CONTINUE
!        TRANSTYPE = 'VALIDACAO'
        LINCNT = LINCNT+1

        TRABUFERROR = TRABUF(TERR)

        IF ((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUFERROR .EQ. NOER)) THEN
          IGSTOTAL(TRABUF(TIGS_TTYP),1) = IGSTOTAL(TRABUF(TIGS_TTYP),1) + 1
          IGSTOTAL(TRABUF(TIGS_TTYP),2) = IGSTOTAL(TRABUF(TIGS_TTYP),2) + TRABUF(TIGSV_TPRZ)
          IGSTOTAL(NUMIGSTTYP,1)        = IGSTOTAL(NUMIGSTTYP,1) + 1
        ENDIF

        WRITE(PUNIT,1901) STAT(TRABUF(TSTAT)),
     *                   ERROR(TRABUFERROR),
     *                   TTYPE(TRABUF(TTYP)),
     *                   SERIAL,
     *                   DISTIM(TRABUF(TTIM)),
     *                   TRABUF(TTER),
     *                   TRABUF(TTRN),
     *                   TRABUF(TCDC),
     *                   TRABUF(TGAM),
     *                   GTNAMES(TRABUF(TGAMTYP)),
     *                   TRABUF(TGAMIND),
     *                   TRABUF(TSIZE),
     *                   IGSTTYPE(TRABUF(TIGS_TTYP)),
     *                   IAGT_NO(TRABUF(TAGT))
     
        IF (DETAIL) THEN
          I4TMP(2) = TRABUF(TIGSV_MIDH)
          I4TMP(1) = TRABUF(TIGSV_MIDL)
          TMSGID = I8TMP
          
          I4TMP(2) = TRABUF(TIGSV_WRSH)
          I4TMP(1) = TRABUF(TIGSV_WRSL)
          WEXSER = I8TMP
          
          WRITE(PUNIT,902) TMSGID ! MESSAGE ID
          
          ! VALIDATION BET REFERENCE
          IF (SCRAM .EQ. .TRUE.) THEN
            WRITE(PUNIT,9091) TRABUF(TIGSV_WRDY),
     *                        TRABUF(TIGSV_WRDM),
     *                        TRABUF(TIGSV_WRDD),
     *                        TRABUF(TIGSV_WRGM),
     *                        WEXSER,
     *                        TRABUF(TIGSV_WRCD)
          ELSE
            WRITE(PUNIT,909) TRABUF(TIGSV_WRDY),
     *                       TRABUF(TIGSV_WRDM),
     *                       TRABUF(TIGSV_WRDD),
     *                       TRABUF(TIGSV_WRGM),
     *                       WEXSER,
     *                       '***'
          ENDIF
          
          WRITE(PUNIT,907) TRABUF(TIGS_XREF) ! MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
          
          ! IGS ERROR CODE DESCRIPTION
C          IF((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUFERROR .EQ. NOER)) THEN
C            WRITE(PUNIT,937)
C          ELSE
            WRITE(CXERR,'(<XERR_I4LEN>A4)') (TRABUF(TIGS_XERR+I),I=0,XERR_I4LEN-1)
            IF (TRABUF(TIGS_SERR) .GE. SEC_LIDX .AND. TRABUF(TIGS_SERR) .LE. SEC_RIDX) THEN
              WRITE(PUNIT,908) SYSTEMERRORCODE(TRABUF(TIGS_SERR)), CXERR
            ELSE
              WRITE(PUNIT,9081) TRABUF(TIGS_SERR), CXERR, ' (UNKNOWN SYSTEM) '
            ENDIF
C          WRITE(PUNIT,908) SYSTEMERRORCODE(TRABUF(TIGS_SERR)), (TRABUF(TIGS_XERR+I),I=0,XERR_I4LEN-1) ! IGS ERROR CODE DESCRIPTION
C          ENDIF
          
          ! WAGER VALIDATION DATETIME
          WRITE(PUNIT,919) TRABUF(TIGSV_WVDY),
     *                     TRABUF(TIGSV_WVDM),
     *                     TRABUF(TIGSV_WVDD),
     *                     TRABUF(TIGSV_WVTH),
     *                     TRABUF(TIGSV_WVTM),
     *                     TRABUF(TIGSV_WVTS)
          
          ! TOTAL PRIZE AMOUNT
          WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSV_TPRZ),10,VALUNIT)
          WRITE(PUNIT,920) TRIM(ADJUSTL(C10MONY))
          
          ! TOTAL TAX AMOUNT
          WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSV_TTAX),10,VALUNIT)
          WRITE(PUNIT,921) TRIM(ADJUSTL(C10MONY))
          
          ! NET PRIZE AMOUNT
          WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSV_NPRZ),10,VALUNIT)
          WRITE(PUNIT,922) TRIM(ADJUSTL(C10MONY))
          
          ! PAYMENT MODE
          IF (TRABUF(TIGSV_PMOD) .GE. VPM_LIDX .AND. TRABUF(TIGSV_PMOD) .LE. VPM_RIDX) THEN
            WRITE(PUNIT,923) VALPAYMODE(TRABUF(TIGSV_PMOD))
          ELSE
            WRITE(PUNIT,9231) TRABUF(TIGSV_PMOD), ' (UNKNOWN)'
          ENDIF
          
          !PORTUGUESE PLAYER VAT IDENTIFICATION NUMBER (9 DIGITS)
          IF (TRABUF(TIGSV_FNIF) .GE. VFN_LIDX .AND. TRABUF(TIGSV_FNIF) .LE. VFN_RIDX) THEN
            WRITE(PUNIT,9351) TRABUF(TIGSV_PNIF), VALFLAGNIF(TRABUF(TIGSV_FNIF))
          ELSE
            WRITE(PUNIT,9352) TRABUF(TIGSV_PNIF), TRABUF(TIGSV_FNIF), ' (UNKNOWN CONFIRMATION NEEDED VALUE)' 
          ENDIF
          
          WRITE(PUNIT,*)
          
          LINCNT = LINCNT+11
        ENDIF
        RETURN 
C
C PRINT PAYMENT
C
4000    CONTINUE
!        TRANSTYPE = 'PAGAMENTO'
        LINCNT = LINCNT+1

        TRABUFERROR = TRABUF(TERR)

        IF ((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUFERROR .EQ. NOER)) THEN
          IGSTOTAL(TRABUF(TIGS_TTYP),1) = IGSTOTAL(TRABUF(TIGS_TTYP),1) + 1
          IGSTOTAL(TRABUF(TIGS_TTYP),2) = IGSTOTAL(TRABUF(TIGS_TTYP),2) + TRABUF(TIGSP_TPRZ)
          IGSTOTAL(NUMIGSTTYP,1)        = IGSTOTAL(NUMIGSTTYP,1) + 1
        ENDIF

        WRITE(PUNIT,1901) STAT(TRABUF(TSTAT)),
     *                   ERROR(TRABUFERROR),
     *                   TTYPE(TRABUF(TTYP)),
     *                   SERIAL,
     *                   DISTIM(TRABUF(TTIM)),
     *                   TRABUF(TTER),
     *                   TRABUF(TTRN),
     *                   TRABUF(TCDC),
     *                   TRABUF(TGAM),
     *                   GTNAMES(TRABUF(TGAMTYP)),
     *                   TRABUF(TGAMIND),
     *                   TRABUF(TSIZE),
     *                   IGSTTYPE(TRABUF(TIGS_TTYP)),
     *                   IAGT_NO(TRABUF(TAGT))
     
        IF (DETAIL) THEN
          I4TMP(2) = TRABUF(TIGSP_MIDH)
          I4TMP(1) = TRABUF(TIGSP_MIDL)
          TMSGID = I8TMP
          
          I4TMP(2) = TRABUF(TIGSP_WRSH)
          I4TMP(1) = TRABUF(TIGSP_WRSL)
          WEXSER = I8TMP
          
          WRITE(PUNIT,902) TMSGID ! MESSAGE ID
          
          ! PAYMENT BET REFERENCE
          IF (SCRAM .EQ. .TRUE.) THEN
            WRITE(PUNIT,9091) TRABUF(TIGSP_WRDY),
     *                        TRABUF(TIGSP_WRDM),
     *                        TRABUF(TIGSP_WRDD),
     *                        TRABUF(TIGSP_WRGM),
     *                        WEXSER,
     *                        TRABUF(TIGSP_WRCD)
          ELSE
            WRITE(PUNIT,909) TRABUF(TIGSP_WRDY),
     *                       TRABUF(TIGSP_WRDM),
     *                       TRABUF(TIGSP_WRDD),
     *                       TRABUF(TIGSP_WRGM),
     *                       WEXSER,
     *                       '***'
          ENDIF
          
          IF (TRABUF(TIGSP_PMOD) .GE. VPM_LIDX .AND. TRABUF(TIGSP_PMOD) .LE. VPM_RIDX) THEN
            WRITE(PUNIT,923) VALPAYMODE(TRABUF(TIGSP_PMOD))
          ELSE
            WRITE(PUNIT,9231) TRABUF(TIGSP_PMOD), ' (UNKNOWN)'
          ENDIF
          
          IF(TRABUF(TIGSP_PMOD).EQ.IGS_PMBNK) THEN
            CALL FASTSET(BLANK,NIB,6)
            ! PLAYER NIB
            WRITE(CNIB,926) TRABUF(TIGSP_NIBB),
     *                      TRABUF(TIGSP_NIBO),
     *                      TRABUF(TIGSP_NIA1),
     *                      TRABUF(TIGSP_NIA2),
     *                      TRABUF(TIGSP_NICD)
            IF(TRABUF(TIGSP_IDTY).EQ.IGS_PTYPPHN) THEN 
              ! PLAYER ID IS PHONE NUMBER
              WRITE(PUNIT,927) TRABUF(TIGSP_PYID)
              WRITE(PUNIT,9271) (NIB(I),I=1,6)
            ELSEIF(TRABUF(TIGSP_IDTY).EQ.IGS_PTYPCRD) THEN
              ! PLAYER ID IS CARD NUMBER
              WRITE(PUNIT,928) TRABUF(TIGSP_PYID)
              WRITE(PUNIT,9271) (NIB(I),I=1,6)
            ELSE
              WRITE(PUNIT,9272) TRABUF(TIGSP_PYID)
              WRITE(PUNIT,9271) (NIB(I),I=1,6)
            ENDIF
          ENDIF
          
          WRITE(PUNIT,907) TRABUF(TIGS_XREF) ! MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
          
          ! IGS ERROR CODE DESCRIPTION
C          IF((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUFERROR .EQ. NOER)) THEN
C            WRITE(PUNIT,937)
C          ELSE
            WRITE(CXERR,'(<XERR_I4LEN>A4)') (TRABUF(TIGS_XERR+I),I=0,XERR_I4LEN-1)
            IF (TRABUF(TIGS_SERR) .GE. SEC_LIDX .AND. TRABUF(TIGS_SERR) .LE. SEC_RIDX) THEN
              WRITE(PUNIT,908) SYSTEMERRORCODE(TRABUF(TIGS_SERR)), CXERR
            ELSE
              WRITE(PUNIT,9081) TRABUF(TIGS_SERR), CXERR, ' (UNKNOWN SYSTEM) '
            ENDIF
C          WRITE(PUNIT,908) SYSTEMERRORCODE(TRABUF(TIGS_SERR)), (TRABUF(TIGS_XERR+I),I=0,XERR_I4LEN-1) ! IGS ERROR CODE DESCRIPTION
C          ENDIF
          
          I4TMP(2) = TRABUF(TIGSP_PRSH)
          I4TMP(1) = TRABUF(TIGSP_PRSL)
          PEXSER = I8TMP
          
          ! PAYMENT REFERENCE
          IF (SCRAM .EQ. .TRUE.) THEN
            WRITE(PUNIT,9251) TRABUF(TIGSP_PRDY),
     *                        TRABUF(TIGSP_PRDM),
     *                        TRABUF(TIGSP_PRDD),
     *                        TRABUF(TIGSP_PRGM),
     *                        PEXSER,
     *                        TRABUF(TIGSP_PRCD)
          ELSE
            WRITE(PUNIT,925) TRABUF(TIGSP_PRDY),
     *                       TRABUF(TIGSP_PRDM),
     *                       TRABUF(TIGSP_PRDD),
     *                       TRABUF(TIGSP_PRGM),
     *                       PEXSER,
     *                       '***'
          ENDIF
          
          ! PRIZE PAYMENT DATETIME
          WRITE(PUNIT,929) TRABUF(TIGSP_PPDY),
     *                     TRABUF(TIGSP_PPDM),
     *                     TRABUF(TIGSP_PPDD),
     *                     TRABUF(TIGSP_PPTH),
     *                     TRABUF(TIGSP_PPTM),
     *                     TRABUF(TIGSP_PPTS)
          
          ! TOTAL PRIZE AMOUNT
          WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSP_TPRZ),10,VALUNIT)
          WRITE(PUNIT,920) TRIM(ADJUSTL(C10MONY))
          
          ! TOTAL TAX AMOUNT
          WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSP_TTAX),10,VALUNIT)
          WRITE(PUNIT,921) TRIM(ADJUSTL(C10MONY))
          
          ! NET PRIZE AMOUNT
          WRITE(C10MONY,'(A10)') CMONY(TRABUF(TIGSP_NPRZ),10,VALUNIT)
          WRITE(PUNIT,922) TRIM(ADJUSTL(C10MONY))
          
          !PORTUGUESE PLAYER VAT IDENTIFICATION NUMBER (9 DIGITS)
          WRITE(PUNIT,935) TRABUF(TIGSP_PNIF)
          
          WRITE(PUNIT,*)
          
          LINCNT = LINCNT+12
        ENDIF
        RETURN 
C
C PRINT GAME PROGRAMME REPORT
C
5000    CONTINUE
!        TRANSTYPE = 'REPORT'
        LINCNT = LINCNT+1

        TRABUFERROR = TRABUF(TERR)

        IF ((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUFERROR .EQ. NOER)) THEN
          IGSTOTAL(TRABUF(TIGS_TTYP),1) = IGSTOTAL(TRABUF(TIGS_TTYP),1) + 1
          IGSTOTAL(NUMIGSTTYP,1)        = IGSTOTAL(NUMIGSTTYP,1) + 1
        ENDIF

        WRITE(PUNIT,1901) STAT(TRABUF(TSTAT)),
     *                   ERROR(TRABUFERROR),
     *                   TTYPE(TRABUF(TTYP)),
     *                   SERIAL,
     *                   DISTIM(TRABUF(TTIM)),
     *                   TRABUF(TTER),
     *                   TRABUF(TTRN),
     *                   TRABUF(TCDC),
     *                   TRABUF(TGAM),
     *                   GTNAMES(TRABUF(TGAMTYP)),
     *                   TRABUF(TGAMIND),
     *                   TRABUF(TSIZE),
     *                   IGSTTYPE(TRABUF(TIGS_TTYP)),
     *                   IAGT_NO(TRABUF(TAGT))
     
        IF (DETAIL) THEN
          I4TMP(2) = TRABUF(TIGSV_MIDH)
          I4TMP(1) = TRABUF(TIGSV_MIDL)
          TMSGID = I8TMP
          
          WRITE(PUNIT,902) TMSGID ! MESSAGE ID
          
          WRITE(PUNIT,930) TRABUF(TIGSR_SEGN) ! SEGMENT NUMBER REQUESTED
          
          WRITE(PUNIT,931) TRABUF(TIGSR_MEID) ! MEDIA ID
          
          WRITE(PUNIT,932) TRABUF(TIGSR_PTID) ! PROGRAMME TEMPLATE ID
          
          I4TMP(2) = TRABUF(TIGSR_MVRH)
          I4TMP(1) = TRABUF(TIGSR_MVRL)
          TIGSR_MVR = I8TMP
          WRITE(PUNIT,936) TIGSR_MVR ! MEDIA VERSION
          
          WRITE(PUNIT,907) TRABUF(TIGS_XREF) ! MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
          
          ! IGS ERROR CODE DESCRIPTION
C          IF((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUFERROR .EQ. NOER)) THEN
C            WRITE(PUNIT,937)
C          ELSE
            WRITE(CXERR,'(<XERR_I4LEN>A4)') (TRABUF(TIGS_XERR+I),I=0,XERR_I4LEN-1)
            IF (TRABUF(TIGS_SERR) .GE. SEC_LIDX .AND. TRABUF(TIGS_SERR) .LE. SEC_RIDX) THEN
              WRITE(PUNIT,908) SYSTEMERRORCODE(TRABUF(TIGS_SERR)), CXERR
            ELSE
              WRITE(PUNIT,9081) TRABUF(TIGS_SERR), CXERR, ' (UNKNOWN SYSTEM) '
            ENDIF
C          WRITE(PUNIT,908) SYSTEMERRORCODE(TRABUF(TIGS_SERR)), (TRABUF(TIGS_XERR+I),I=0,XERR_I4LEN-1) ! IGS ERROR CODE DESCRIPTION
C          ENDIF
          
C          type*, 'TRABUF(TIGSR_PRDY)[', TRABUF(TIGSR_PRDY), ']TRABUF(TIGSR_PRDM)[', TRABUF(TIGSR_PRDM)
C     *          ,'TRABUF(TIGSR_PRDD)[', TRABUF(TIGSR_PRDD), ']TRABUF(TIGSR_PRTH)[', TRABUF(TIGSR_PRTH)
C     *          ,'TRABUF(TIGSR_PRTM)[', TRABUF(TIGSR_PRTM), ']TRABUF(TIGSR_PRTS)[', TRABUF(TIGSR_PRTS)
          ! PROGRAMME REPORT DATETIME
          WRITE(PUNIT,933) TRABUF(TIGSR_PRDY),
     *                     TRABUF(TIGSR_PRDM),
     *                     TRABUF(TIGSR_PRDD),
     *                     TRABUF(TIGSR_PRTH),
     *                     TRABUF(TIGSR_PRTM),
     *                     TRABUF(TIGSR_PRTS)
          
          WRITE(PUNIT,934) TRABUF(TIGSR_TSEG) ! TOTAL SEGMENTS
          
          WRITE(PUNIT,*)
          
          LINCNT = LINCNT+10
        ENDIF
        RETURN 
C
C===========
C
CCC     FORMAT statements
CV02900     FORMAT(/,' STATUS ERROR  TYPE',4X,'SERIAL',5X,
CV02     *         'TIME  TERM SEQ  DATE GAME GAMETYP  GIND ',
CV02     *         'SIZE  BEG        END     JOKER ',11X,
CV02     *          'FRACTION','  X BET',
CV02     *         /,1X,131('='),/)
900     FORMAT(/,' STATUS ERROR  TYPE',4X,'SERIAL',5X,                          !V02
     *         'TIME  TERM SEQ  DATE GAME GAMETYP  GIND ',
     *         'SIZE  BEG        END     JOKER CMIL  M1LH',1X,
     *          'FRACTION','  X BET',
     *         /,1X,131('='),/)
1901     FORMAT(1X,3(2X,A4),I10,1X,A8,I6,Z4,I6,I5,1X,A8,
     *         I5,I5,1X,A12,6X,'AGT> ',A)
CV031902     FORMAT(1X,3(2X,A4),I10,1X,A8,I6,Z4,I6,I5,1X,A8,
CV03     *         I5,I5,1X,A,1X,A9,2X,'AGT> ',A)
1902     FORMAT(1X,3(2X,A4),I10,1X,A8,I6,Z4,I6,I5,1X,A8,                        !V03
     *         I5,I5,1X,A,1X,A9,2X,'AGT> ',A,2X,'TSTCS> ',Z2.2)

1903     FORMAT(1X,3(2X,A4),I10,1X,A8,I6,Z4,I6,I5,1X,I8,
     *         I5,I5,1X,I0,17X,'AGT> ',A)
C
902     FORMAT(10X,'Terminal Message ID             ',I0)
903     FORMAT(10X,'ABP Game ID                     ',I0)
904     FORMAT(10X,'Subtype ID                      ',I0)
905     FORMAT(10X,'Unit Stake of the Bet           ',A)
906     FORMAT(10X,'Number of Bets                  ',I0)
907     FORMAT(10X,'MsgQ Reference #                ',I0)
C908     FORMAT(10X,'Error Code                      ',A4,' - ',<XERR_I4LEN>A4)
908     FORMAT(10X,'Error Code                      ',A,A<CXERR_LEN>)
9081    FORMAT(10X,'Error Code                      ',I0,A<CXERR_LEN>,A)
909     FORMAT(10X,'Bet External Reference Serial # ',I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',A3)
9091    FORMAT(10X,'Bet External Reference Serial # ',I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',I3.3)
C909     FORMAT(10X,'Bet External Reference Serial # ',I0,'/',I0,'/',I0,'-',I0,'-',I0,'-',A3)
C939     FORMAT(10X,'Bet External Reference Serial # ',I0,'/',I0,'/',I0,'-',I0,'-',I0,'-',I0) !!! REMOVER ESTA FORMATACAO TAL COMO A SUA RESPECTIVA CHAMADA !!!
910     FORMAT(10X,'Bet Creation Datetime           ',I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2)
911     FORMAT(10X,'Bet Last Event Date             ',I4.4,'/',I2.2,'/',I2.2)
912     FORMAT(10X,'Bet Total Stake                 ',A)
913     FORMAT(10X,'Bet Maximum Possible Returns    ',A)
914     FORMAT(10X,'Bet Entry Mode                  ',A)                        !V03
C
915     FORMAT(10X,'Cancel External Ref Serial #    ',I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',A3)
9151    FORMAT(10X,'Cancel External Ref Serial #    ',I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',I3.3)
C938     FORMAT(10X,'Cancel External Ref Serial #    ',I0,'/',I0,'/',I0,'-',I0,'-',I0,'-',I0) !!! REMOVER ESTA FORMATACAO TAL COMO A SUA RESPECTIVA CHAMADA !!!
916     FORMAT(10X,'Cancel Datetime                 ',I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2)
917     FORMAT(10X,'Cancel Amount                   ',A)
C
919     FORMAT(10X,'Validation Datetime             ',I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2)
920     FORMAT(10X,'Total Prize Amount              ',A)
921     FORMAT(10X,'Total Tax Amount                ',A)
922     FORMAT(10X,'Net Prize Amount                ',A)
923     FORMAT(10X,'Payment Mode                    ',A)
9231    FORMAT(10X,'Payment Mode                    ',I0,A)
C924     FORMAT(10X,'Player Id                       ',I0)
C
925     FORMAT(10X,'Payment External Ref Serial #   ',I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',A3)
9251    FORMAT(10X,'Payment External Ref Serial #   ',I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',I3.3)
C925     FORMAT(10X,'Payment External Ref Serial #   ',I0,'/',I0,'/',I0,'-',I0,'-',I0,'-',I0) !!! REMOVER ESTA FORMATACAO TAL COMO A SUA RESPECTIVA CHAMADA !!!
C926     FORMAT(10X,'NIB                             'I4.4,1X,I4.4,I9.9,I2.2,1X,I2.2)
926     FORMAT(I4.4,1X,I4.4,I9.9,I2.2,1X,I2.2)
927     FORMAT(10X,'Phone Number                    ',I0)
9271    FORMAT(10X,'NIB                             ',6A4)
928     FORMAT(10X,'Player Card                     ',I0)
9272    FORMAT(10X,'Player Id (Type Unknown)        ',I0)
929     FORMAT(10X,'Payment Datetime                ',I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2)
C
930     FORMAT(10X,'Segment Number                  ',I0)
931     FORMAT(10X,'Media Id                        ',I0)
932     FORMAT(10X,'Programme Template Id           ',I0)
936     FORMAT(10X,'Media Version                   ',I0)
933     FORMAT(10X,'Programme Report Datetime       ',I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2)
934     FORMAT(10X,'Total Segments                  ',I0)
C
!936     FORMAT(10X,'Payment Mode')
C937     FORMAT(10X,'Error Code')
C
935     FORMAT(10X,'NIF                             ',I9)
9351    FORMAT(10X,'NIF / Confirmation Needed       ',I9,' / ',A)
9352    FORMAT(10X,'NIF / Confirmation Needed       ',I9,' / ',I, A)
C
     
        END
