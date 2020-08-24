C SUBROUTINE PRINTEURO
C
C V07 02-APR.2016 SCML M16 PROJECT
C V06 01-AUG-2014 SCML Added LINCNT and DETAIL arguments to the subroutine
C V05 10-OCT-2013 SCML New Validation Messages
C V04 12-APR-2011 FJG ACCENTURE MERGE FOR EM2
C V03 26-JAN-2011 FJG More Out of Bounds problems
C V02 11-NOV-2010 FJG EUROMILLIONS VALIDATION PRIZE OVER 42M
C     18-NOV-2010 FJG Batch2: Fix NOBOUNDS checking errors
C
C SUBROUTINE TO PRINT EURO MIL TRANSACTIONS IN TMIR FORMAT
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
CV07        SUBROUTINE PRINTEURO(TRABUF,PUNIT,DETAIL,LINCNT,EM_JUSTONE,SCRAM,
CV07     *                       SUMEUROVALID,SUMEUROWAGER,SUMEUROCANCEL)
        SUBROUTINE PRINTEURO(TRABUF,PUNIT,DETAIL,LINCNT,EM_JUSTONE,SCRAM)
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:PRINTEURO.DEF'                                          !V07
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       ARGUMENTS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        INTEGER*4  PUNIT
        INTEGER*4  LINCNT
CV07        INTEGER*4  SUMEUROVALID, SUMEUROWAGER, SUMEUROCANCEL
        LOGICAL    DETAIL                                                       !V06
        LOGICAL    EM_JUSTONE
        LOGICAL    SCRAM
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       LOCAL VARIABLES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        LOGICAL      DUMMY                                                      !AVOID WARNING
        CHARACTER*6  PQFLAG 
        BYTE         OUTTAB(500)
        INTEGER*4    I, IND, JUL
        INTEGER*2    DBUF(12)
CV07        CHARACTER*21 VALSUBTYP(16)
CV07        DATA VALSUBTYP /
CV07     *                   'Regular                '                              !0x10
CV07     *               ,   'Mid-Tier Cash          '                              !0x11
CV07     *               ,   'Claim                  '                              !0x12
CV07     *               ,   'Validation Detail      '                              !0x13
CV07     *               ,   ' '                                                    !0x14
CV07     *               ,   ' '                                                    !0x15
CV07     *               ,   'NV Inq.Regular         '                              !0x16
CV07     *               ,   'NV Inq.Mid-Tier        '                              !0x17
CV07     *               ,   'NV Inq.Cash Accepted   '                              !0x18
CV07     *               ,   'NV Bank Transf.Accepted'                              !0x19
CV07     *               ,   'NV Inq.Mid-Tier BT Only'                              !0x1A
CV07     *               ,   '                       '                              !0x1B
CV07     *               ,   '                       '                              !0x1C
CV07     *               ,   '                       '                              !0x1D
CV07     *               ,   '                       '                              !0x1E
CV07     *               ,   'Validation Error       '                              !0x1F
CV07     *                 /
CV07        CHARACTER*31 VALSTATUS(0:18)
CV07        CHARACTER*31 VALSTATUSE(0:30)                                           
        CHARACTER*12 TRANSTYPE
CV07        CHARACTER*30 CANSTATUS(0:5)
        INTEGER*4  CHECK
        INTEGER*4  SERIAL
        INTEGER*4  PAGE
        INTEGER*4  ST, TRABUFERROR
        INTEGER*4 I4TEMP
        INTEGER*2 I2TEMP(2)
        BYTE      I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
        INTEGER*4  LINES
!       REAL*8        COMMAND     ! Avoid Warning
C        DATA LINCNT/100/,LINES/0/ ! Avoid Warning COMMAND/0/
        DATA LINES/0/ ! V06
C
CV07        INTEGER*4  EMVALAMT(2)
CV07        INTEGER*8  EMVALAMTI8
CV07        REAL*8     EMVALAMTR8
CV07        EQUIVALENCE (EMVALAMT,EMVALAMTI8)
C
        INTEGER*4 KIFLAG, SMFLAG, SHFLAG, NIFFLAG, NCRFLAG                      !V07
        LOGICAL*4 PRZTXD                                                        !PRIZE TAXED FLAG !V07
        CHARACTER*10 CRAFCODE                                                   !FUNCTION !V07
C
C----+------------------------------------------------------------------
C V05| New Validation Messages
C----+------------------------------------------------------------------
CV07        INTEGER*4  NIB(6)
CV07        CHARACTER*24 CNIB
CV07        EQUIVALENCE (NIB,CNIB)
CV07        INTEGER*4  BLANK
CV07        DATA BLANK /'    '/                                                     !V07
C----+------------------------------------------------------------------
C V05| New Validation Messages
C----+------------------------------------------------------------------
        
C        DATA VALSTATUS /'Not A Winner','Not Cashed','Cashed',
C     *                  'Cashed With Exchange','Deleted Winner',
C     *                  'Cancelled Winner','Validation On Hold','Cant Pay Yet',
C     *                  'Prize Values Not Set','Host Game Postponed','No Exchange Ticket',
C     *                  'Cashed With Exchange','Priv Pay','No Prize Priv Pay',
C     *                  'Priv Pay Postponed','Payment Issued To Bank',
C     *                  'Set Banking Info','Set Banking Info/Multi-Draw'/
C        
C----+------------------------------------------------------------------
C V05| New Validation Messages
C----+------------------------------------------------------------------
C         DATA VALSUBTYP /'Regular','Mid-Tier Cash','Claim','Validation Detail',' ',' ',' ',' ',' ',' ',
C    *                  ' ',' ',' ',' ',' ','Validation Error'/
CV07          DATA VALSUBTYP /
CV07     *                'Regular'                 ! x10
CV07     *              , 'Mid-Tier Cash'           ! x11
CV07     *              , 'Claim'                   ! x12
CV07     *              , 'Validation Detail'       ! x13
CV07     *              , ' '                       ! x14
CV07     *              , ' '                       ! x15
CV07     *              , 'NV Inq.Regular'          ! x16
CV07     *              , 'NV Inq.Mid-Tier'         ! x17
CV07     *              , 'NV Inq.Cash Accepted'    ! x18
CV07     *              , 'NV Bank Transf.Accepted' ! x19
CV07     *              , 'NV Inq.Mid-Tier BT Only' ! x1A
CV07     *              , ' '                       ! x1B
CV07     *              , ' '                       ! x1C
CV07     *              , ' '                       ! x1D
CV07     *              , ' '                       ! x1E
CV07     *              , 'Validation Error'        ! x1F
CV07     *                   /
CV07          DATA BLANK /'    '/
C----+------------------------------------------------------------------
C V05| New Validation Messages
C----+------------------------------------------------------------------
CV07        VALSTATUSE(0)  = 'No Results Yet Or Not A Winner'
CV07        VALSTATUSE(1)  = 'Results Not Confirmed'
CV07        VALSTATUSE(2)  = 'No Such Ticket'
CV07        VALSTATUSE(3)  = 'Cant Pay Yet'
CV07        VALSTATUSE(4)  = 'Already Cashed'
CV07        VALSTATUSE(5)  = '---------------'
CV07        VALSTATUSE(6)  = 'Prize Expired'
CV07        VALSTATUSE(9)  = 'Cash At Lottery'
CV07        VALSTATUSE(18) = 'No Details Available'
CV07        VALSTATUSE(30) = 'Winner Holding Limit'
CV07        VALSTATUS(11) = 'Cashed With Exchange'
CV07        VALSTATUS(10) = 'No Exchange Ticket'
CV07        VALSTATUS(5) = '---------------'
C
CV07        CANSTATUS(0) = 'Good Cancel'
CV07        CANSTATUS(1) = 'Time Limit Exceeded'
CV07        CANSTATUS(2) = 'Invalid Cancel'
CV07        CANSTATUS(3) = 'Already Cancel'
CV07        CANSTATUS(4) = 'Wrong Terminal'
CV07        CANSTATUS(5) = '---------------'
C
        DUMMY = EM_JUSTONE ! Avoid Warning
C
        IF(LINCNT.GT.LINSPP) THEN
          CALL TITLE('TRANSACTION FILE REPORT','    TMIR',1,
     *               PUNIT,PAGE,DAYCDC)
          WRITE(PUNIT,900)
          LINCNT=7
        ENDIF 
C
        SERIAL = TRABUF(TSER)
        IF(SCRAM) CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),SERIAL,CHECK)
        IF(TRABUF(TEUTYP) .EQ. TWAG) GOTO 1000
        IF(TRABUF(TEUTYP) .EQ. TCAN) GOTO 2000
        IF(TRABUF(TEUTYP) .EQ. TVAL) GOTO 3000
        RETURN
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       PRINT WAGER TRANSACTION
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
1000    CONTINUE
        TRANSTYPE = 'APOSTA'
        KIFLAG = NV                                                             !NO VALUE !V07
        SMFLAG = NV                                                             !NO VALUE !V07
        SHFLAG = NV                                                             !NO VALUE !V07
CV07        LINCNT = LINCNT+1
        IF(TRABUF(TSTAT).EQ.GOOD .AND. (TRABUF(TERR).EQ.NOER .OR.
     *                                  TRABUF(TERR).EQ.RETY)
     *    ) THEN
CV07          SUMEUROWAGER = SUMEUROWAGER + 1
C
          IF(TRABUF(TEUW_SHWFL).NE.0) THEN                                      !V07
            SHFLAG = YES
          ELSE
            SHFLAG = NO
          ENDIF
          IF(TRABUF(TEUW_SMWFL).NE.0) THEN                                      !V07
            SMFLAG = YES
          ELSE
            SMFLAG = NO
          ENDIF
        ENDIF
C
        IF(TRABUF(TEUW_KIWFL).NE.0) THEN                                        !V07
          KIFLAG = YES
        ELSE
          KIFLAG = NO
        ENDIF
C
        CALL UPDEURSTAT(TRABUF)                                                 !V07
C
        WRITE(PUNIT,9000) STAT(TRABUF(TSTAT)),
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
CV07     *                   TRANSTYPE
     *                   TRANSTYPE,
     *                   YNDESC(KIFLAG),                                        !V07
     *                   YNDESC(SHFLAG),                                        !V07
     *                   YNDESC(SMFLAG),                                        !V07
     *                   IAGT_NO(TRABUF(TAGT))                                  !V07
C
        LINCNT = LINCNT + 1
        IF(DETAIL) THEN
          IF(TRABUF(TEUWQP).EQ.0) THEN
            PQFLAG = 'MANUAL'
          ELSE
            PQFLAG = 'AUTO'
          ENDIF
          DBUF(VCDC) = TRABUF(TCDC)
          CALL CDATE(DBUF)
          JUL = DBUF(VJUL) + 500
C----+---+-------------+------------------------------------------------
C V07|BEG| M16 PROJECT | WAGER TRANSACTION DETAIL
C----+---+-------------+------------------------------------------------
CV07          WRITE(PUNIT,902) JUL,
CV07     *                     TRABUF(TEUSER),
CV07C     *                     TRABUF(TEUCHK),
CV07     *                     '***',
CV07     *                     TRABUF(TEUWBEGW),
CV07     *                     TRABUF(TEUWBEGY),
CV07     *                     TRABUF(TEUWENDW),
CV07     *                     TRABUF(TEUWENDY),
CV07     *                     TRABUF(TEUWDUR),
CV07     *                     TRABUF(TEUWDRWIND)
CV07          WRITE(PUNIT,903) TRABUF(TEUWNBET),
CV07     *                     PQFLAG,
CV07     *                     TRABUF(TEUWNMK),
CV07     *                     TRABUF(TEUWNST),
CV07     *                     TRABUF(TEUWTIMEH),
CV07     *                     TRABUF(TEUWTIMEM),
CV07     *                     TRABUF(TEUWTIMES)
CV07          LINCNT = LINCNT+2
CV07          IND = 1
CV07          DO I=TEUWBOARD,120
CV07            IF(TRABUF(I) .NE. 0) THEN
CV07               CALL MOVBYT(TRABUF(I),1,OUTTAB,IND,4) 
CV07               IND=IND+4
CV07            ENDIF
CV07          ENDDO
CV07          CALL TRANSFBOARD(OUTTAB,TRABUF(TEUWNBET),TRABUF(TEUWNMK),TRABUF(TEUWNST),50,11,ST,PUNIT)
CV07          LINES = TRABUF(TEUWNBET)
CV07          WRITE(PUNIT,*)
CV07          LINCNT = LINCNT+LINES+1
CV07        ENDIF
CV07        RETURN
          IF(TRABUF(TEUW_PLNIF).GT.0) THEN                                      !PLAYER NIF IS SET
            WRITE(PUNIT,9100) '*********',                                      !PLAYER NIF
     *                        TRIM(WCHDESC(TRABUF(TEUW_EUWCH))),                !EM SALES CHANNEL
     *                        TRIM(DRWINDDESC(TRABUF(TEUWDRWIND))),             !EM DRAW INDICATOR
     *                        TRABUF(TEUWDUR),                                  !EM DURATION
     *                        TRIM(PQFLAG),                                     !EM QUICK PICK FLAG
     *                        TRABUF(TEUWNMK),TRABUF(TEUWNST)                   !EM # MARKS + # STARS (SYSTEM NUMBER)
C
          ELSE                                                                  !PLAYER NIF IS NOT SET
            WRITE(PUNIT,9101) TRIM(WCHDESC(TRABUF(TEUW_EUWCH))),                !EM SALES CHANNEL
     *                        TRIM(DRWINDDESC(TRABUF(TEUWDRWIND))),             !EM DRAW INDICATOR
     *                        TRABUF(TEUWDUR),                                  !EM DURATION
     *                        TRIM(PQFLAG),                                     !EM QUICK PICK FLAG
     *                        TRABUF(TEUWNMK),TRABUF(TEUWNST)                   !EM # MARKS + # STARS (SYSTEM NUMBER)
          ENDIF
          IND = 1
          DO I=TEUWBOARD,TEUWBEND
            IF(TRABUF(I) .NE. 0) THEN
               CALL MOVBYT(TRABUF(I),1,OUTTAB,IND,4) 
               IND=IND+4
            ENDIF
          ENDDO
          CALL TRANSFBOARD(OUTTAB,TRABUF(TEUWNBET),
     *                            TRABUF(TEUWNMK),
     *                            TRABUF(TEUWNST),50,12,ST,PUNIT)
          LINCNT = LINCNT + TRABUF(TEUWNBET)
C
          WRITE(PUNIT,9110) 
     *        TRABUF(TEUMESSQ),                                                 !MESSAGE QUEUE SEQUENCE NUMBER
     *        TRABUF(TEUWTIMEH), TRABUF(TEUWTIMEM), TRABUF(TEUWTIMES),          !BET CREATION TIME (HH24) IN EUROMILLIONS
     *        JUL, TRABUF(TEUSER), '***',                                       !EM EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *        TRABUF(TEUWBEGW), 2000 + TRABUF(TEUWBEGY),                        !EM BEGIN DRAW NUMBER CCC/YYYY
     *        TRABUF(TEUWENDW), 2000 + TRABUF(TEUWENDY)                         !EM END   DRAW NUMBER CCC/YYYY
          LINCNT = LINCNT + 4
C
          IF(SHFLAG .EQ. YES) THEN
            IF(TRABUF(TEUW_SHWTB).EQ.1) THEN
              WRITE(PUNIT,9120) 
     *          TRABUF(TEUW_SHWDN), 2000 + TRABUF(TEUW_SHWDY),                  !SoM DRAW NUMBER CCC/YYYY
     *          TRIM(CRAFCODE(TRABUF(TEUW_SHWB1), TRABUF(TEUW_SHWB2))),         !SoM RAFFLE FIRST NUMBER
     *          TRIM(CRAFCODE(TRABUF(TEUW_SHWE1), TRABUF(TEUW_SHWE2))),         !SoM RAFFLE LAST NUMBER
     *          TRABUF(TEUW_SHWTB)                                              !SoM TOTAL CODES
              LINCNT = LINCNT + 2
            ELSE
              WRITE(PUNIT,9130) 
     *          TRABUF(TEUW_SHWDN), 2000 + TRABUF(TEUW_SHWDY),                  !SoM DRAW NUMBER CCC/YYYY
     *          TRIM(CRAFCODE(TRABUF(TEUW_SHWB1), TRABUF(TEUW_SHWB2))),         !SoM RAFFLE FIRST NUMBER
     *          TRIM(CRAFCODE(TRABUF(TEUW_SHWE1), TRABUF(TEUW_SHWE2))),         !SoM RAFFLE LAST NUMBER
     *          TRABUF(TEUW_SHWTB)                                              !SoM TOTAL CODES
              LINCNT = LINCNT + 2
            ENDIF
          ENDIF
C
          IF(SMFLAG .EQ. YES) THEN
            IF(TRABUF(TEUW_SMWTB).EQ.1) THEN
              WRITE(PUNIT,9140) 
     *          JUL, TRABUF(TEUW_SMWSN), '***',                                 !SM EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *          TRABUF(TEUW_SMWDN), 2000 + TRABUF(TEUW_SMWDY),                  !SM DRAW NUMBER CCC/YYYY
     *          TRIM(CRAFCODE(TRABUF(TEUW_SMWB1), TRABUF(TEUW_SMWB2))),         !SM RAFFLE CODE RANGE: X TO Y
     *          TRIM(CRAFCODE(TRABUF(TEUW_SMWE1), TRABUF(TEUW_SMWE2))),         !SM RAFFLE CODE RANGE: X TO Y
     *          TRABUF(TEUW_SMWTB)
              LINCNT = LINCNT + 3
            ELSE
              WRITE(PUNIT,9150) 
     *          JUL, TRABUF(TEUW_SMWSN), '***',                                 !SM EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *          TRABUF(TEUW_SMWDN), 2000 + TRABUF(TEUW_SMWDY),                  !SM DRAW NUMBER CCC/YYYY
     *          TRIM(CRAFCODE(TRABUF(TEUW_SMWB1), TRABUF(TEUW_SMWB2))),         !SM RAFFLE CODE RANGE: X TO Y
     *          TRIM(CRAFCODE(TRABUF(TEUW_SMWE1), TRABUF(TEUW_SMWE2))),         !SM RAFFLE CODE RANGE: X TO Y
     *          TRABUF(TEUW_SMWTB)
             LINCNT = LINCNT + 3
            ENDIF
          ENDIF
C
          WRITE(PUNIT,*)                                                        !INSERT NEW LINE
          LINCNT = LINCNT + 1
        ENDIF
C
        RETURN
C----+---+-------------+------------------------------------------------
C V07|END| M16 PROJECT | WAGER TRANSACTION DETAIL
C----+---+-------------+------------------------------------------------
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       PRINT CANCEL TRANSACTION
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
2000    CONTINUE
        TRANSTYPE = 'CANCELAMENTO'
        KIFLAG = NV                                                             !NO VALUE !V07
        SMFLAG = NV                                                             !NO VALUE !V07
        SHFLAG = NV                                                             !NO VALUE !V07
CV07        LINCNT = LINCNT + 1
        TRABUFERROR = TRABUF(TERR)
CV07        IF ((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUFERROR .EQ. NOER)) SUMEUROCANCEL = SUMEUROCANCEL + 1 
C
        CALL UPDEURSTAT(TRABUF)                                                 !V07
C
        IF(TRABUF(TEUCST).NE.0) THEN                                            !CANCEL WITH ERROR
          TRABUF(TEUCAM) = 0
          TRABUF(TEUC_SMWCA) = 0
        ELSE
          IF(TRABUF(TEUCST).EQ.0 .AND. TRABUF(TSTAT).EQ.GOOD) THEN              !GOOD CANCEL
            IF(TRABUF(TEUC_SHCFL).NE.0) THEN                                    !V07
              SHFLAG = YES
            ELSE
              SHFLAG = NO
            ENDIF
            IF(TRABUF(TEUC_SMCFL).NE.0) THEN                                    !V07
              SMFLAG = YES
            ELSE
              SMFLAG = NO
            ENDIF
          ENDIF
        ENDIF
C
        WRITE(PUNIT,9000) STAT(TRABUF(TSTAT)),
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
CV07     *                   TRANSTYPE
     *                   TRANSTYPE,
     *                   YNDESC(KIFLAG),                                        !V07
     *                   YNDESC(SHFLAG),                                        !V07
     *                   YNDESC(SMFLAG),                                        !V07
     *                   IAGT_NO(TRABUF(TAGT))                                  !V07
C
        LINCNT = LINCNT + 1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       CANCEL TRANSACTION DETAIL
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        IF(DETAIL) THEN
          DBUF(VCDC) = TRABUF(TCDC)
          CALL CDATE(DBUF)
          JUL = DBUF(VJUL) + 500
C
C----+---+-------------+------------------------------------------------
C V07|BEG| M16 PROJECT | CANCEL TRANSACTION DETAIL
C----+---+-------------+------------------------------------------------
CV07          IF(TRABUF(TEUCST).NE.0) TRABUF(TEUCAM) = 0
CV07
CV07          WRITE(PUNIT,2902) JUL,
CV07     *                      TRABUF(TEUSER),
CV07     *                      TRABUF(TEUCHK),
CV07     *                      CANSTATUS(TRABUF(TEUCST))
CV07        
CV07          WRITE(PUNIT,2903) TRABUF(TEUCWJUL),
CV07     *                      TRABUF(TEUCWSER),
CV07     *                      TRABUF(TEUCWCKD),
CV07     *                      CMONY(TRABUF(TEUCAM),11,VALUNIT)
CV07  
CV07          WRITE(PUNIT,*)
CV07          LINCNT = LINCNT+3
C
          IF(TRABUF(TEUCST).EQ.0 .AND. TRABUF(TSTAT).EQ.GOOD) THEN              !GOOD CANCEL
            IF(SMFLAG.EQ.YES) THEN
              WRITE(C11MONY(1),'(A11)') CMONY(TRABUF(TEUCAM),11,BETUNIT)
              WRITE(C11MONY(2),'(A11)') CMONY(TRABUF(TEUC_SMWCA),11,BETUNIT)
              WRITE(PUNIT,9200) 
     *            TRABUF(TEUCWJUL), TRABUF(TEUCWSER), TRABUF(TEUCWCKD),         !EM BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *            TRABUF(TEUMESSQ),                                             !MESSAGE QUEUE SEQUENCE NUMBER
     *            TRIM(EURCANSTATUS(TRABUF(TEUCST))),                           !CANCEL STATUS
     *            TRABUF(TEUCTIMEH), TRABUF(TEUCTIMEM), TRABUF(TEUCTIMES),      !CANCEL TIME (HH24:MI:SS) IN EUROMILLIONS
     *            JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                          !EM CANCEL EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *            TRIM(ADJUSTL(C11MONY(1))),                                    !EM CANCEL AMOUNT (BET UNITS)
     *            TRABUF(TEUCWJUL), TRABUF(TEUC_SMWSN), TRABUF(TEUC_SMWCD),     !SM BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *            JUL, TRABUF(TEUC_SMCSN), TRABUF(TEUC_SMCCD),                  !SM CANCEL EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *            TRIM(ADJUSTL(C11MONY(2)))                                     !SM CANCEL AMOUNT (BET UNITS)
              LINCNT = LINCNT + 10
            ELSE                                                                !EM DO NOT HAVE SM
              WRITE(C11MONY(1),'(A11)') CMONY(TRABUF(TEUCAM),11,BETUNIT)
              WRITE(PUNIT,9210) 
     *            TRABUF(TEUCWJUL), TRABUF(TEUCWSER), TRABUF(TEUCWCKD),         !EM BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *            TRABUF(TEUMESSQ),                                             !MESSAGE QUEUE SEQUENCE NUMBER
     *            TRIM(EURCANSTATUS(TRABUF(TEUCST))),                           !CANCEL STATUS
     *            TRABUF(TEUCTIMEH), TRABUF(TEUCTIMEM), TRABUF(TEUCTIMES),      !CANCEL TIME (HH24:MI:SS) IN EUROMILLIONS
     *            JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                          !EM CANCEL EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *            TRIM(ADJUSTL(C11MONY(1)))                                     !EM CANCEL AMOUNT (BET UNITS)
              LINCNT = LINCNT + 7
            ENDIF
C
          ELSEIF(TRABUF(TEUCST).NE.0 .AND. TRABUF(TSTAT).EQ.GOOD) THEN          !CANCELLATION ERROR MESSAGE
            WRITE(PUNIT,9220)
     *            TRABUF(TEUCWJUL), TRABUF(TEUCWSER), TRABUF(TEUCWCKD),         !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *            TRABUF(TEUMESSQ),                                             !MESSAGE QUEUE SEQUENCE NUMBER
     *            TRIM(EURCANSTATUS(TRABUF(TEUCST))),                           !CANCEL STATUS
     *            TRABUF(TEUCTIMEH), TRABUF(TEUCTIMEM), TRABUF(TEUCTIMES),      !CANCEL TIME (HH24:MI:SS) IN EUROMILLIONS
     *            JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                          !CANCEL EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *            '0.00'                                                        !CANCEL AMOUNT (BET UNITS)
            LINCNT = LINCNT + 7
C
          ELSEIF(TRABUF(TSTAT).NE.GOOD) THEN                                    !ERROR MESSAGE
            WRITE(PUNIT,9230)
     *            TRABUF(TEUCWJUL), TRABUF(TEUCWSER), TRABUF(TEUCWCKD),         !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *            TRABUF(TEUMESSQ),                                             !MESSAGE QUEUE SEQUENCE NUMBER
     *            '---------------',                                            !CANCEL STATUS
     *            TRABUF(TEUCTIMEH), TRABUF(TEUCTIMEM), TRABUF(TEUCTIMES),      !CANCEL TIME (HH24:MI:SS) IN EUROMILLIONS
     *            JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                          !CANCEL EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *            '0.00'                                                        !CANCEL AMOUNT (BET UNITS)
C
            LINCNT = LINCNT + 7
          ENDIF
C----+---+-------------+------------------------------------------------
C V07|END| M16 PROJECT | CANCEL TRANSACTION DETAIL
C----+---+-------------+------------------------------------------------
        ENDIF
C
        RETURN
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       PRINT VALIDATION TRANSACTION
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
3000    CONTINUE
        TRANSTYPE = 'VALIDACAO'
CV07        LINCNT = LINCNT+1
        SHFLAG  = NV                                                            !NO VALUE !V07
        NIFFLAG = NV                                                            !NO VALUE !V07
        NCRFLAG = NV                                                            !NO VALUE !V07
CV07        PQFLAG = ' AUTO'
        TRABUFERROR = TRABUF(TERR)

C----+------------------------------------------------------------------
C V05| Adding New Validation Messages and clarifying variables
C----+------------------------------------------------------------------
C        IF (TRABUF(TEUVSBT) .EQ. 1) TRABUFERROR = 16
C        IF ((TRABUF(TSTAT) .EQ. GOOD).AND.(TRABUF(TEUVSBT) .EQ. 0).AND.((TRABUF(TEUVCAM) .NE. 0).OR.(TRABUF(TEUVCAMH) .NE. 0))) THEN
C           SUMEUROVALID = SUMEUROVALID + 1 
C        ENDIF
        IF ( TRABUF(TEUVSBT) .EQ. VMID
     *  .OR. TRABUF(TEUVSBT) .EQ. VNREG
     *  .OR. TRABUF(TEUVSBT) .EQ. VNINQ
     *  .OR. TRABUF(TEUVSBT) .EQ. VNIBO
     *  ) THEN
           TRABUFERROR = VINQ
        ENDIF
CV07        IF (  (TRABUF(TSTAT) .EQ. GOOD)
CV07     *  .AND. (     TRABUF(TEUVSBT) .EQ. VREG
CV07     *         .OR. TRABUF(TEUVSBT) .EQ. VNDON
CV07     *         .OR. TRABUF(TEUVSBT) .EQ. VNBNK
CV07     *        )
CV07     *  .AND. (    (TRABUF(TEUVCAM) .NE. 0)
CV07     *         .OR.(TRABUF(TEUVCAMH) .NE. 0)
CV07     *        )
CV07     *  ) THEN
CV07           SUMEUROVALID = SUMEUROVALID + 1 
CV07        ENDIF
C
        CALL UPDEURSTAT(TRABUF)                                                 !V07
C
C----+------------------------------------------------------------------
C V05| Adding New Validation Messages and clarifying variables
C----+------------------------------------------------------------------
C
        IF(TRABUF(TEUVST).EQ.10) THEN                                           !VALIDATION STATUS = NO EXCHANGE TICKET
          IF(TRABUF(TEUV_SHVFL).NE.0) THEN
            SHFLAG  = YES                                                       !V07
          ELSE
            SHFLAG  = NO                                                        !V07
          ENDIF
          IF(TRABUF(TEUV_NIFFL).NE.0) THEN
            NIFFLAG = YES                                                       !V07
          ELSE
            NIFFLAG = NO                                                        !V07
          ENDIF
          IF(TRABUF(TEUV_NIFCF).NE.0) THEN
            NCRFLAG = YES                                                       !V07
          ELSE
            NCRFLAG = NO                                                        !V07
          ENDIF
        ENDIF
C
        WRITE(PUNIT,9000) STAT(TRABUF(TSTAT)),
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
CV07     *                   TRANSTYPE
     *                   TRANSTYPE,
     *                   ' ',                                                   !V07
     *                   YNDESC(SHFLAG),                                        !V07
     *                   ' ',                                                   !V07
     *                   IAGT_NO(TRABUF(TAGT))                                  !V07
C
        LINCNT = LINCNT + 1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       VALIDATION TRANSACTION DETAIL
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        IF(DETAIL) THEN
          DBUF(VCDC) = TRABUF(TCDC)
          CALL CDATE(DBUF)
          JUL = DBUF(VJUL) + 500
          I4PRZAMT(2) = TRABUF(TEUVCAMH)
          I4PRZAMT(1) = TRABUF(TEUVCAM)
          R8PRZAMT    = DFLOAT(I8PRZAMT)/100.0D0
C----+---+-------------+------------------------------------------------
C V07|BEG| M16 PROJECT | VALIDATION TRANSACTION DETAIL
C----+---+-------------+------------------------------------------------
C
          IF(TRABUF(TSTAT) .EQ. GOOD) THEN
            I8NPZAMT = 0
            I8TAXAMT = 0
            PRZTXD = .FALSE.
            I4NPZAMT(2) = TRABUF(TEUVRAMH)
            I4NPZAMT(1) = TRABUF(TEUVRAM)
            IF(I8NPZAMT.GT.0) THEN                                              !IF DIFFERENT FROM ZERO THEN REFUND AMOUNT IS THE NET PRIZE AMOUNT AND PRIZE AMOUNT HAS BEEN TAXED
              PRZTXD = .TRUE.
              I8TAXAMT = I8PRZAMT - I8NPZAMT
              R8TAXAMT = DFLOAT(I8TAXAMT)/100.0D0
              R8NPZAMT = DFLOAT(I8NPZAMT)/100.0D0
            ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C           VALIDATION ERROR SUBTYPE (APPLICABLE TO VIRTUAL AND PHYSICAL
C           TERMINALS OF WEB AND RETAILER NETWORKS, RESPECTIVELY)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            IF(TRABUF(TEUVSBT).EQ.15) THEN                                      !VALIDATION ERROR (IN RESPONSE OF AN INQUIRY OR PAYMENT REQUEST)
              IF(TRABUF(TEUVPLIDTYP).EQ.0 .AND.
     *           TRABUF(TEUVNIBBB) .EQ.0 .AND. TRABUF(TEUVNIBBO) .EQ.0 .AND.
     *           TRABUF(TEUVNIBBA1).EQ.0 .AND. TRABUF(TEUVNIBBA2).EQ.0 .AND.
     *           TRABUF(TEUVNIBCD) .EQ.0) THEN                                  !DO NOT PRINT PLAYER ID AND NIB IF BOTH ARE ZERO
                WRITE(PUNIT,9250)
     *                TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),     !BET EXTERNAL SERIAL #
     *                TRABUF(TEUMESSQ),                                         !MESSAGE QUEUE SEQUENCE NUMBER
     *                TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                      !VALIDATION SUBTYPE DESCRIPTION
     *                TRIM(EURVALSTATUSE(TRABUF(TEUVST))),                      !VALIDATION STATUS ERROR DESCRIPTION
     *                TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),  !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *                JUL, TRABUF(TEUSER), TRABUF(TEUCHK)                       !VALIDATION EXTERNAL SERIAL # + NEW LINE
                LINCNT = LINCNT + 7
              ELSE                                                              !PRINT PLAYER ID AND NIB IF AT LEAST ONE IS NOT ZERO
                IF(TRABUF(TEUVPLIDTYP).EQ.PHONNBR) THEN
                  PTID = PTPHON
                ELSEIF(TRABUF(TEUVPLIDTYP).EQ.PLAYCRD) THEN
                  PTID = PTCARD
                ELSE
                  PTID = PTUNKN
                ENDIF
                CALL FASTSET(BLANK,NIB,6)
                WRITE(CNIB,9500) TRABUF(TEUVNIBBB)                              !FORMAT NIB
     *                         , TRABUF(TEUVNIBBO)
     *                         , TRABUF(TEUVNIBBA1)
     *                         , TRABUF(TEUVNIBBA2)
     *                         , TRABUF(TEUVNIBCD)
                WRITE(PUNIT,9255)
     *                TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),     !BET EXTERNAL SERIAL #
     *                TRABUF(TEUVPLCARD), TRIM(PLIDTYPDESC(PTID)),              !PLAYER ID # AND PLAYER TYPE ID DESCRIPTION
     *                (NIB(I),I=1,6),                                           !PLAYER NIB
     *                TRABUF(TEUMESSQ),                                         !MESSAGE QUEUE SEQUENCE NUMBER
     *                TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                      !VALIDATION SUBTYPE DESCRIPTION
     *                TRIM(EURVALSTATUSE(TRABUF(TEUVST))),                      !VALIDATION STATUS ERROR DESCRIPTION
     *                TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),  !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *                JUL, TRABUF(TEUSER), TRABUF(TEUCHK)                       !VALIDATION EXTERNAL SERIAL # + NEW LINE
                LINCNT = LINCNT + 9
              ENDIF
C
              RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C           NEW VALIDATION INQUIRY SUBTYPES (APPLICABLE TO PHYSICAL
C           TERMINALS OF RETAILER NETWORK)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            ELSEIF(TRABUF(TEUVSBT).EQ.VNREG .OR.
     *         TRABUF(TEUVSBT).EQ.VNINQ .OR.
     *         TRABUF(TEUVSBT).EQ.VNIBO) THEN
              IF(TRABUF(TEUVST).EQ.10) THEN                                     !VALIDATION STATUS = NO EXCHANGE TICKET
                C17MONY(1) = BLANK17MONY
                WRITE(C17MONY(1),'(F14.2)') R8PRZAMT
                IF(NIFFLAG.EQ.NO) THEN                                          !NIF NOT PRESENT
                  IF(PRZTXD) THEN                                               !PRIZE HAS TAX
                    C17MONY(2) = BLANK17MONY
                    C17MONY(3) = BLANK17MONY
                    WRITE(C17MONY(2),'(F14.2)') R8NPZAMT                        !NET PRIZE AMOUNT
                    WRITE(C17MONY(3),'(F14.2)') R8TAXAMT                        !TAX PRIZE AMOUNT
                    WRITE(PUNIT,9300)
     *                TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),     !BET EXTERNAL SERIAL #
     *                TRABUF(TEUMESSQ),                                         !MESSAGE QUEUE SEQUENCE NUMBER
     *                TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                      !VALIDATION SUBTYPE DESCRIPTION
     *                TRIM(EURVALSTATUS(TRABUF(TEUVST))),                       !VALIDATION STATUS ERROR DESCRIPTION
     *                TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),  !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *                JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                      !VALIDATION EXTERNAL SERIAL #
     *                TRIM(ADJUSTL(C17MONY(1))),                                !PRIZE AMOUNT
     *                TRIM(ADJUSTL(C17MONY(2))),                                !NET PRIZE AMOUNT
     *                TRIM(ADJUSTL(C17MONY(3)))                                 !TAX PRIZE AMOUNT + NEW LINE
                    LINCNT = LINCNT + 10
                  ELSE                                                          !PRIZE HAS NO TAX
                    WRITE(PUNIT,9305)
     *                TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),     !BET EXTERNAL SERIAL #
     *                TRABUF(TEUMESSQ),                                         !MESSAGE QUEUE SEQUENCE NUMBER
     *                TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                      !VALIDATION SUBTYPE DESCRIPTION
     *                TRIM(EURVALSTATUS(TRABUF(TEUVST))),                       !VALIDATION STATUS ERROR DESCRIPTION
     *                TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),  !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *                JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                      !VALIDATION EXTERNAL SERIAL #
     *                TRIM(ADJUSTL(C17MONY(1)))                                 !PRIZE AMOUNT + NEW LINE
                    LINCNT = LINCNT + 8
                  ENDIF
                ELSEIF(NIFFLAG.EQ.YES) THEN                                     !NIF IS PRESENT
                  IF(PRZTXD) THEN                                               !PRIZE HAS TAX
                    C17MONY(2) = BLANK17MONY
                    C17MONY(3) = BLANK17MONY
                    WRITE(C17MONY(2),'(F14.2)') R8NPZAMT                        !NET PRIZE AMOUNT
                    WRITE(C17MONY(3),'(F14.2)') R8TAXAMT                        !TAX PRIZE AMOUNT
                    WRITE(PUNIT,9310)
     *                TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),     !BET EXTERNAL SERIAL #
     *                TRABUF(TEUMESSQ),                                         !MESSAGE QUEUE SEQUENCE NUMBER
     *                TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                      !VALIDATION SUBTYPE DESCRIPTION
     *                TRIM(EURVALSTATUS(TRABUF(TEUVST))),                       !VALIDATION STATUS ERROR DESCRIPTION
     *                TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),  !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *                JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                      !VALIDATION EXTERNAL SERIAL #
     *                TRIM(ADJUSTL(C17MONY(1))),                                !PRIZE AMOUNT
     *                TRIM(ADJUSTL(C17MONY(2))),                                !NET PRIZE AMOUNT
     *                TRIM(ADJUSTL(C17MONY(3))),                                !TAX PRIZE AMOUNT + NEW LINE
     *                '*********',                                              !DO NOT PRINT PLAYER NIF
     *                TRIM(YESNODESC(NCRFLAG))                                  !NIF CONFIRMATION REQUIRED FLAG + NEW LINE
                    LINCNT = LINCNT + 12
                  ELSE
                    WRITE(PUNIT,9315)
     *                TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),     !BET EXTERNAL SERIAL #
     *                TRABUF(TEUMESSQ),                                         !MESSAGE QUEUE SEQUENCE NUMBER
     *                TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                      !VALIDATION SUBTYPE DESCRIPTION
     *                TRIM(EURVALSTATUS(TRABUF(TEUVST))),                       !VALIDATION STATUS ERROR DESCRIPTION
     *                TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),  !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *                JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                      !VALIDATION EXTERNAL SERIAL #
     *                TRIM(ADJUSTL(C17MONY(1))),                                !PRIZE AMOUNT
     *                '*********',                                              !DO NOT PRINT PLAYER NIF
     *                TRIM(YESNODESC(NCRFLAG))                                  !NIF CONFIRMATION REQUIRED FLAG + NEW LINE
                    LINCNT = LINCNT + 10
                  ENDIF
                ENDIF
              ENDIF
C
              RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C           NEW VALIDATION CASH PAYMENT ACCEPTED SUBTYPE (APPLICABLE TO 
C           PHYSICAL TERMINALS OF RETAILER NETWORK)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            ELSEIF(TRABUF(TEUVSBT).EQ.VNDON) THEN
              IF(TRABUF(TEUVST).EQ.10) THEN                                     !VALIDATION STATUS = NO EXCHANGE TICKET
                C17MONY(1) = BLANK17MONY
                WRITE(C17MONY(1),'(F14.2)') R8PRZAMT                            !PRIZE AMOUNT
                IF(NIFFLAG.EQ.NO) THEN                                          !NIF NOT PRESENT
                  IF(PRZTXD) THEN                                               !PRIZE HAS TAX
                    C17MONY(2) = BLANK17MONY
                    C17MONY(3) = BLANK17MONY
                    WRITE(C17MONY(2),'(F14.2)') R8NPZAMT                        !NET PRIZE AMOUNT
                    WRITE(C17MONY(3),'(F14.2)') R8TAXAMT                        !TAX PRIZE AMOUNT
                    WRITE(PUNIT,9320)
     *                TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),     !BET EXTERNAL SERIAL #
     *                TRABUF(TEUMESSQ),                                         !MESSAGE QUEUE SEQUENCE NUMBER
     *                TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                      !VALIDATION SUBTYPE DESCRIPTION
     *                TRIM(EURVALSTATUS(TRABUF(TEUVST))),                       !VALIDATION STATUS ERROR DESCRIPTION
     *                TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),  !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *                JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                      !VALIDATION EXTERNAL SERIAL #
     *                TRIM(ADJUSTL(C17MONY(1))),                                !PRIZE AMOUNT
     *                TRIM(ADJUSTL(C17MONY(2))),                                !NET PRIZE AMOUNT
     *                TRIM(ADJUSTL(C17MONY(3)))                                 !TAX PRIZE AMOUNT + NEW LINE
                    LINCNT = LINCNT + 10
                  ELSE                                                          !PRIZE HAS NOT TAX
                    WRITE(PUNIT,9325)
     *                TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),     !BET EXTERNAL SERIAL #
     *                TRABUF(TEUMESSQ),                                         !MESSAGE QUEUE SEQUENCE NUMBER
     *                TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                      !VALIDATION SUBTYPE DESCRIPTION
     *                TRIM(EURVALSTATUS(TRABUF(TEUVST))),                       !VALIDATION STATUS ERROR DESCRIPTION
     *                TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),  !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *                JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                      !VALIDATION EXTERNAL SERIAL #
     *                TRIM(ADJUSTL(C17MONY(1)))                                 !PRIZE AMOUNT + NEW LINE
                    LINCNT = LINCNT + 8
                  ENDIF
                ELSEIF(NIFFLAG.EQ.YES) THEN                                     !NIF IS PRESENT
                  IF(PRZTXD) THEN
                    C17MONY(2) = BLANK17MONY
                    C17MONY(3) = BLANK17MONY
                    WRITE(C17MONY(2),'(F14.2)') R8NPZAMT                        !NET PRIZE AMOUNT
                    WRITE(C17MONY(3),'(F14.2)') R8TAXAMT                        !TAX PRIZE AMOUNT
                    WRITE(PUNIT,9330)
     *                TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),     !BET EXTERNAL SERIAL #
     *                TRABUF(TEUMESSQ),                                         !MESSAGE QUEUE SEQUENCE NUMBER
     *                TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                      !VALIDATION SUBTYPE DESCRIPTION
     *                TRIM(EURVALSTATUS(TRABUF(TEUVST))),                       !VALIDATION STATUS ERROR DESCRIPTION
     *                TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),  !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *                JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                      !VALIDATION EXTERNAL SERIAL #
     *                TRIM(ADJUSTL(C17MONY(1))),                                !PRIZE AMOUNT
     *                TRIM(ADJUSTL(C17MONY(2))),                                !NET PRIZE AMOUNT
     *                TRIM(ADJUSTL(C17MONY(3))),                                !TAX PRIZE AMOUNT
     *                '*********'                                               !DO NOT PRINT PLAYER NIF + NEW LINE
                    LINCNT = LINCNT + 11
                  ELSE
                    WRITE(PUNIT,9335)
     *                TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),     !BET EXTERNAL SERIAL #
     *                TRABUF(TEUMESSQ),                                         !MESSAGE QUEUE SEQUENCE NUMBER
     *                TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                      !VALIDATION SUBTYPE DESCRIPTION
     *                TRIM(EURVALSTATUS(TRABUF(TEUVST))),                       !VALIDATION STATUS ERROR DESCRIPTION
     *                TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),  !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *                JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                      !VALIDATION EXTERNAL SERIAL #
     *                TRIM(ADJUSTL(C17MONY(1))),                                !PRIZE AMOUNT
     *                '*********'                                               !DO NOT PRINT PLAYER NIF + NEW LINE
                    LINCNT = LINCNT + 9
                  ENDIF
                ENDIF
              ENDIF
C
              RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C           NEW VALIDATION BANK TRANSFER ACCEPTED SUBTYPE (APPLICABLE TO
C           PHYSICAL TERMINALS OF RETAILER NETWORK)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            ELSEIF(TRABUF(TEUVSBT).EQ.VNBNK) THEN   
              IF(TRABUF(TEUVST).EQ.10) THEN                                     !VALIDATION STATUS = NO EXCHANGE TICKET
                IF(TRABUF(TEUVPLIDTYP).EQ.PHONNBR) THEN
                  PTID = PTPHON
                ELSEIF(TRABUF(TEUVPLIDTYP).EQ.PLAYCRD) THEN
                  PTID = PTCARD
                ELSE
                  PTID = PTUNKN
                ENDIF
                CALL FASTSET(BLANK,NIB,6)
                WRITE(CNIB,9500) TRABUF(TEUVNIBBB)
     *                         , TRABUF(TEUVNIBBO)
     *                         , TRABUF(TEUVNIBBA1)
     *                         , TRABUF(TEUVNIBBA2)
     *                         , TRABUF(TEUVNIBCD)
C
                C17MONY(1) = BLANK17MONY
                WRITE(C17MONY(1),'(F14.2)') R8PRZAMT
                IF(NIFFLAG.EQ.NO) THEN                                          !NO NIF PRESENT IN BANK TRANSFER
                  IF(PRZTXD) THEN                                               !PRIZE HAS TAX
                    C17MONY(2) = BLANK17MONY
                    C17MONY(3) = BLANK17MONY
                    WRITE(C17MONY(2),'(F14.2)') R8NPZAMT                        !NET PRIZE AMOUNT
                    WRITE(C17MONY(3),'(F14.2)') R8TAXAMT                        !TAX PRIZE AMOUNT
                    WRITE(PUNIT,9340)
     *                TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),     !BET EXTERNAL SERIAL #
     *                TRABUF(TEUVPLCARD), TRIM(PLIDTYPDESC(PTID)),              !PLAYER ID # AND PLAYER TYPE ID DESCRIPTION
     *                (NIB(I),I=1,6),                                           !PLAYER NIB
     *                TRABUF(TEUMESSQ),                                         !MESSAGE QUEUE SEQUENCE NUMBER
     *                TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                      !VALIDATION SUBTYPE DESCRIPTION
     *                TRIM(EURVALSTATUS(TRABUF(TEUVST))),                       !VALIDATION STATUS ERROR DESCRIPTION
     *                TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),  !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *                JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                      !VALIDATION EXTERNAL SERIAL #
     *                TRIM(ADJUSTL(C17MONY(1)))                                 !PRIZE AMOUNT + NEW LINE
                    LINCNT = LINCNT + 12
                  ELSE                                                          !PRIZE HAS NOT TAX
                    WRITE(PUNIT,9345)
     *                TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),     !BET EXTERNAL SERIAL #
     *                TRABUF(TEUVPLCARD), TRIM(PLIDTYPDESC(PTID)),              !PLAYER ID # AND PLAYER TYPE ID DESCRIPTION
     *                (NIB(I),I=1,6),                                           !PLAYER NIB
     *                TRABUF(TEUMESSQ),                                         !MESSAGE QUEUE SEQUENCE NUMBER
     *                TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                      !VALIDATION SUBTYPE DESCRIPTION
     *                TRIM(EURVALSTATUS(TRABUF(TEUVST))),                       !VALIDATION STATUS ERROR DESCRIPTION
     *                TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),  !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *                JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                      !VALIDATION EXTERNAL SERIAL #
     *                TRIM(ADJUSTL(C17MONY(1)))                                 !PRIZE AMOUNT + NEW LINE
                    LINCNT = LINCNT + 10
                  ENDIF
                ELSEIF(NIFFLAG.EQ.YES) THEN                                     !NIF IS PRESENT
                  IF(PRZTXD) THEN                                               !PRIZE HAS TAX
                    C17MONY(2) = BLANK17MONY
                    C17MONY(3) = BLANK17MONY
                    WRITE(C17MONY(2),'(F14.2)') R8NPZAMT                        !NET PRIZE AMOUNT
                    WRITE(C17MONY(3),'(F14.2)') R8TAXAMT                        !TAX PRIZE AMOUNT
                    WRITE(PUNIT,9350)
     *                TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),     !BET EXTERNAL SERIAL #
     *                TRABUF(TEUVPLCARD), TRIM(PLIDTYPDESC(PTID)),              !PLAYER ID # AND PLAYER TYPE ID DESCRIPTION
     *                (NIB(I),I=1,6),                                           !PLAYER NIB
     *                TRABUF(TEUMESSQ),                                         !MESSAGE QUEUE SEQUENCE NUMBER
     *                TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                      !VALIDATION SUBTYPE DESCRIPTION
     *                TRIM(EURVALSTATUS(TRABUF(TEUVST))),                       !VALIDATION STATUS ERROR DESCRIPTION
     *                TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),  !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *                JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                      !VALIDATION EXTERNAL SERIAL #
     *                TRIM(ADJUSTL(C17MONY(1))),                                !PRIZE AMOUNT
     *                TRIM(ADJUSTL(C17MONY(2))),                                !NET PRIZE AMOUNT
     *                TRIM(ADJUSTL(C17MONY(3))),                                !TAX PRIZE AMOUNT
     *                '*********',                                              !DO NOT PRINT PLAYER NIF
     *                TRIM(YESNODESC(NCRFLAG))                                  !NIF CONFIRMATION REQUIRED FLAG + NEW LINE
                    LINCNT = LINCNT + 14
                  ELSE                                                          !PRIZE HAS NOT TAX
                    WRITE(PUNIT,9355)
     *                TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),     !BET EXTERNAL SERIAL #
     *                TRABUF(TEUVPLCARD), TRIM(PLIDTYPDESC(PTID)),              !PLAYER ID # AND PLAYER TYPE ID DESCRIPTION
     *                (NIB(I),I=1,6),                                           !PLAYER NIB
     *                TRABUF(TEUMESSQ),                                         !MESSAGE QUEUE SEQUENCE NUMBER
     *                TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                      !VALIDATION SUBTYPE DESCRIPTION
     *                TRIM(EURVALSTATUS(TRABUF(TEUVST))),                       !VALIDATION STATUS ERROR DESCRIPTION
     *                TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),  !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *                JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                      !VALIDATION EXTERNAL SERIAL #
     *                TRIM(ADJUSTL(C17MONY(1))),                                !PRIZE AMOUNT
     *                '*********',                                              !DO NOT PRINT PLAYER NIF
     *                TRIM(YESNODESC(NCRFLAG))                                  !NIF CONFIRMATION REQUIRED FLAG + NEW LINE
                    LINCNT = LINCNT + 12
                  ENDIF
                ENDIF
              ENDIF
C           
              RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C           VALIDATION SUBTYPES USED BY VIRTUAL TERMINALS OF WEB NETWORK
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            ELSEIF(TRABUF(TEUVSBT).EQ.VREG .OR. 
     *             TRABUF(TEUVSBT).EQ. VMID) THEN
              IF(TRABUF(TEUVST).EQ.10) THEN                                     !VALIDATION STATUS = NO EXCHANGE TICKET
                WRITE(C17MONY(1),'(F14.2)') R8PRZAMT
                IF(PRZTXD) THEN                                                 !PRIZE HAS TAX
                  WRITE(C17MONY(2),'(F14.2)') R8NPZAMT                          !NET PRIZE AMOUNT
                  WRITE(C17MONY(3),'(F14.2)') R8TAXAMT                          !TAX PRIZE AMOUNT
                  WRITE(PUNIT,9360)
     *              TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),       !BET EXTERNAL SERIAL #
     *              TRABUF(TEUMESSQ),                                           !MESSAGE QUEUE SEQUENCE NUMBER
     *              TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                        !VALIDATION SUBTYPE DESCRIPTION
     *              TRIM(EURVALSTATUS(TRABUF(TEUVST))),                         !VALIDATION STATUS ERROR DESCRIPTION
     *              TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),    !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *              JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                        !VALIDATION EXTERNAL SERIAL #
     *              TRIM(ADJUSTL(C17MONY(1))),                                  !PRIZE AMOUNT
     *              TRIM(ADJUSTL(C17MONY(2))),                                  !NET PRIZE AMOUNT
     *              TRIM(ADJUSTL(C17MONY(3)))                                   !TAX PRIZE AMOUNT + NEW LINE
                  LINCNT = LINCNT + 10
                ELSE                                                            !PRIZE HAS NO TAX
                  WRITE(PUNIT,9365)
     *              TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),       !BET EXTERNAL SERIAL #
     *              TRABUF(TEUMESSQ),                                           !MESSAGE QUEUE SEQUENCE NUMBER
     *              TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                        !VALIDATION SUBTYPE DESCRIPTION
     *              TRIM(EURVALSTATUS(TRABUF(TEUVST))),                         !VALIDATION STATUS ERROR DESCRIPTION
     *              TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),    !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *              JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                        !VALIDATION EXTERNAL SERIAL #
     *              TRIM(ADJUSTL(C17MONY(1)))                                   !PRIZE AMOUNT
                  LINCNT = LINCNT + 8
                ENDIF
              ENDIF
C           
              RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C           UNKNOWN VALIDATION SUBTYPES (IT SHOULD NOT HAPPEN)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            ELSE
              WRITE(PUNIT,9365)
     *          TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),           !BET EXTERNAL SERIAL #
     *          TRABUF(TEUMESSQ),                                               !MESSAGE QUEUE SEQUENCE NUMBER
     *          TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                            !VALIDATION SUBTYPE DESCRIPTION
     *          TRIM(EURVALSTATUS(TRABUF(TEUVST))),                             !VALIDATION STATUS ERROR DESCRIPTION
     *          TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),        !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *          JUL, TRABUF(TEUSER), TRABUF(TEUCHK),                            !VALIDATION EXTERNAL SERIAL #
     *          TRIM(ADJUSTL(C17MONY(1)))                                       !PRIZE AMOUNT
              LINCNT = LINCNT + 8
            ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C         TRANSACTION STATUS IS NOT GOOD (E.G. FUNCTION SUPRESSED)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          ELSE
            IF(TRABUF(TEUVPLIDTYP).EQ.0 .AND.
     *         TRABUF(TEUVNIBBB) .EQ.0 .AND. TRABUF(TEUVNIBBO) .EQ.0 .AND.
     *         TRABUF(TEUVNIBBA1).EQ.0 .AND. TRABUF(TEUVNIBBA2).EQ.0 .AND.
     *         TRABUF(TEUVNIBCD) .EQ.0) THEN                                    !DO NOT PRINT PLAYER ID AND NIB IF BOTH ARE ZERO
              WRITE(PUNIT,9370)
     *              TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),       !BET EXTERNAL SERIAL #
     *              TRABUF(TEUMESSQ),                                           !MESSAGE QUEUE SEQUENCE NUMBER
     *              TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                        !VALIDATION SUBTYPE DESCRIPTION
     *              TRIM(EURVALSTATUSE(TRABUF(TEUVST))),                        !VALIDATION STATUS ERROR DESCRIPTION
     *              TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),    !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *              JUL, TRABUF(TEUSER), TRABUF(TEUCHK)                         !VALIDATION EXTERNAL SERIAL # + NEW LINE
              LINCNT = LINCNT + 7
            ELSE                                                                !PRINT PLAYER ID AND NIB IF AT LEAST ONE IS NOT ZERO
              IF(TRABUF(TEUVPLIDTYP).EQ.PHONNBR) THEN
                PTID = PTPHON
              ELSEIF(TRABUF(TEUVPLIDTYP).EQ.PLAYCRD) THEN
                PTID = PTCARD
              ELSE
                PTID = PTUNKN
              ENDIF
              CALL FASTSET(BLANK,NIB,6)
              WRITE(CNIB,9500) TRABUF(TEUVNIBBB)                                !FORMAT NIB
     *                       , TRABUF(TEUVNIBBO)
     *                       , TRABUF(TEUVNIBBA1)
     *                       , TRABUF(TEUVNIBBA2)
     *                       , TRABUF(TEUVNIBCD)
              WRITE(PUNIT,9375)
     *              TRABUF(TEUVWJUL), TRABUF(TEUVWSER), TRABUF(TEUVWCKD),       !BET EXTERNAL SERIAL #
     *              TRABUF(TEUVPLCARD), TRIM(PLIDTYPDESC(PTID)),                !PLAYER ID # AND PLAYER TYPE ID DESCRIPTION
     *              (NIB(I),I=1,6),                                             !PLAYER NIB
     *              TRABUF(TEUMESSQ),                                           !MESSAGE QUEUE SEQUENCE NUMBER
     *              TRIM(EURVALSUBTYP(TRABUF(TEUVSBT))),                        !VALIDATION SUBTYPE DESCRIPTION
     *              TRIM(EURVALSTATUSE(TRABUF(TEUVST))),                        !VALIDATION STATUS ERROR DESCRIPTION
     *              TRABUF(TEUVTIMEH), TRABUF(TEUVTIMEM), TRABUF(TEUVTIMES),    !VALIDATION TIME (H24:MI:SS) IN EUROMILIIONS SYSTEM
     *              JUL, TRABUF(TEUSER), TRABUF(TEUCHK)                         !VALIDATION EXTERNAL SERIAL # + NEW LINE
              LINCNT = LINCNT + 9
            ENDIF
          ENDIF
        ENDIF
C
CV07          IF (TRABUF(TEUVSBT) .EQ. 15) WRITE(PUNIT,3901)
CV07     *                     VALSUBTYP(TRABUF(TEUVSBT)+1),
CV07     *                     VALSTATUSE(TRABUF(TEUVST)),
CV07     *                     '              ' ! If Validation Error then do not print the Cash Amount
CV07C    *                     CMONYI8(EMVALAMT,14,VALUNIT)
CV07C    *                     CMONY(TRABUF(TEUVCAM),11,VALUNIT)
CV07        
CV07          IF (TRABUF(TEUVSBT) .NE. 15) WRITE(PUNIT,3902)
CV07     *                     VALSUBTYP(TRABUF(TEUVSBT)+1),
CV07     *                     VALSTATUS(TRABUF(TEUVST)),
CV07     *                     EMVALAMTR8
CV07C    *                     CMONY(TRABUF(TEUVCAM),11,VALUNIT)
CV07          WRITE(PUNIT,3903) JUL, 
CV07     *                     TRABUF(TEUSER),
CV07     *                     TRABUF(TEUCHK),
CV07     *                     TRABUF(TEUVWJUL),
CV07     *                     TRABUF(TEUVWSER),
CV07     *                     TRABUF(TEUVWCKD),
CV07     *                     TRABUF(TEUVTIMEH),
CV07     *                     TRABUF(TEUVTIMEM),
CV07     *                     TRABUF(TEUVTIMES)
CV07          LINCNT = LINCNT+1
CV07C----+------------------------------------------------------------------          
CV07C V05| New Validation Messages
CV07C----+------------------------------------------------------------------
CV07          IF(TRABUF(TEUVSBT) .EQ. VNBNK) THEN
CV07             CALL FASTSET(BLANK,NIB,6)
CV07             WRITE(CNIB,3914) TRABUF(TEUVNIBBB)
CV07     *                      , TRABUF(TEUVNIBBO)
CV07     *                      , TRABUF(TEUVNIBBA1)
CV07     *                      , TRABUF(TEUVNIBBA2)
CV07     *                      , TRABUF(TEUVNIBCD)
CV07             IF(TRABUF(TEUVPLIDTYP).EQ.PHONNBR) THEN
CV07               WRITE(PUNIT,3915) TRABUF(TEUVPLCARD),(NIB(I),I=1,6)
CV07               LINCNT = LINCNT+1 
CV07             ELSEIF(TRABUF(TEUVPLIDTYP).EQ.PLAYCRD) THEN
CV07C07               WRITE(PUNIT,3916) TRABUF(TEUVPLCARD),(NIB(I),I=1,6)
CV07               WRITE(PUNIT,3916) TRABUF(TEUVPLCARD),(NIB(I),I=1,6),
CV07     *                           TRABUF(TEUV_PLNIF)
CV07               LINCNT = LINCNT+1 
CV07             ENDIF
CV07          ENDIF
CV07C----+------------------------------------------------------------------          
CV07C V05| New Validation Messages
CV07C----+------------------------------------------------------------------          
CV07          IF (TRABUF(TEUVST) .EQ. 11) THEN
CV07             IF (TRABUF(TEUVEQP) .EQ. 0) PQFLAG = 'MANUAL'
CV07             I4TEMP = TRABUF(TEUEVWCKD)
CV07                      
CV07             WRITE(PUNIT,3904) JUL,
CV07     *                        TRABUF(TEUEVWSER),
CV07     *                        ZEXT(I1TEMP(1)),
CV07C     *                        TRABUF(TEUEVWCKD),
CV07     *                        TRABUF(TEUVEBEGW),
CV07     *                        TRABUF(TEUVEBEGY),
CV07     *                        TRABUF(TEUVEENDW),
CV07     *                        TRABUF(TEUVEENDY),
CV07     *                        TRABUF(TEUVEDUR) 
CV07          
CV07             WRITE(PUNIT,903) TRABUF(TEUVENBET),
CV07     *                        PQFLAG,
CV07     *                        TRABUF(TEUVENMK),
CV07     *                        TRABUF(TEUVENST),
CV07     *                        TRABUF(TEUVETIMEH),
CV07     *                        TRABUF(TEUVETIMEM),
CV07     *                        TRABUF(TEUVETIMES)
CV07             LINCNT = LINCNT+2
CV07             IND = 1
CV07             DO I=TEUVEBOARD,120
CV07               IF(TRABUF(I) .NE. 0) THEN
CV07                 CALL MOVBYT(TRABUF(I),1,OUTTAB,IND,4) 
CV07	               IND=IND+4
CV07               ENDIF
CV07             ENDDO
CV07!
CV07! EUROMILLIONS EVOLUTION PROJECT
CV07!           CALL TRANSFBOARD(OUTTAB,TRABUF(TEUVENBET),TRABUF(TEUVENMK),TRABUF(TEUVENST),50,9,ST,PUNIT) 
CV07CV07             CALL TRANSFBOARD(OUTTAB,TRABUF(TEUVENBET),TRABUF(TEUVENMK),TRABUF(TEUVENST),50,11,ST,PUNIT) 
CV07             CALL TRANSFBOARD(OUTTAB,TRABUF(TEUVENBET),
CV07     *                               TRABUF(TEUVENMK),
CV07     *                               TRABUF(TEUVENST),50,12,ST,PUNIT)           !V07
CV07!
CV07             LINES = TRABUF(TEUVENBET)
CV07             LINCNT = LINCNT+LINES
CV07          ENDIF
CV07          WRITE(PUNIT,*)
CV07          LINCNT = LINCNT+1
CV07        ENDIF
C----+---+-------------+------------------------------------------------
C V07|END| M16 PROJECT | VALIDATION TRANSACTION DETAIL
C----+---+-------------+------------------------------------------------
C
        RETURN
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       FORMAT STATEMENTS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       TRANSACTION HEADER FIELDS DESCRIPTION
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
900     FORMAT(/,1X,'STATUS'
     *           1X,'ERROR',
     *           2X,'TYPE',
     *           4X,'SERIAL',
     *           5X,'TIME',
     *           2X,'TERM',
     *           1X,'SEQ',
     *           2X,'DATE',
     *           1X,'GAME',
     *           1X,'GAMETYP',
     *           2X,'GIND',
     *           1X,'SIZE',
     *           2X,'BEG',
     *           8X,'END',
     *           5X,'JOKER',
     *           1X,'CMIL',
     *           2X,'M1LH',
     *           1X,'FRACTION',
     *           2X,'X',
     *           1X,'BET',/,
     *           1X,131('='),/)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       TRANSACTION HEADER FIELDS FORMAT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
9000    FORMAT(1X,
     *         2X,A4,                                                           !TRANSACTION STATUS DESCRIPTION
     *         2X,A4,                                                           !TRANSACTION ERROR DESCRIPTION
     *         2X,A4,                                                           !TRANSACTION TYPE
     *            I10,                                                          !TRANSACION SERIAL
     *         1X,A8,                                                           !TRANSACTION TIME
     *            I6,                                                           !TERMINAL NUMBER
     *            Z4,                                                           !TRANSACTION SEQUENCE NUMBER
     *            I6,                                                           !CDC DATE
     *            I5,                                                           !GAME NUMBER
     *         1X,A8,                                                           !GAME TYPE DESCRIPTION
     *            I5,                                                           !GAME INDEX
     *            I5,                                                           !TRANSACTION SIZE (# OF LOG RECORDS)
     *         2X,A12,                                                          !TRANSACTION SUBTYPE DESCRIPTION
     *         7X,A1,                                                           !JOKER FLAG
     *         5X,A1,                                                           !SoM FLAG
     *         5X,A1,                                                           !SM FLAG
     *         4X,'AGT> ',A)                                                    !AGENT NUMBER NN-NNNNN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       WAGER TRANSACTION FIELDS FORMAT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
9100    FORMAT(10X,'Player NIF'       ,22X,A,/,                                 !PLAYER NIF
     *         10X,'EM Sales Channel' ,16X,A,/,                                 !EM SALES CHANNEL
     *         10X,'EM Draw Indicator',15X,A,/,                                 !EM DRAW INDICATOR
     *         10X,'EM Draw Duration' ,16X,I0,/,                                !EM DURATION
     *         10X,'EM Quick Pick'    ,19X,A,/,                                 !EM QUICK PICK FLAG
     *         10X,'EM Bet Marks'     ,20X,I0,1X,'+',1X,I0)                     !EM # MARKS + # STARS (SYSTEM NUMBER)
9101    FORMAT(10X,'EM Sales Channel' ,16X,A,/,                                 !EM SALES CHANNEL
     *         10X,'EM Draw Indicator',15X,A,/,                                 !EM DRAW INDICATOR
     *         10X,'EM Draw Duration' ,16X,I0,/,                                !EM DURATION
     *         10X,'EM Quick Pick'    ,19X,A,/,                                 !EM QUICK PICK FLAG
     *         10X,'EM Bet Marks'     ,20X,I0,1X,'+',1X,I0)                     !EM # MARKS + # STARS (SYSTEM NUMBER)
C
9110    FORMAT(10X,'MsgQ Reference #',16X,I0,/,                                 !MESSAGE QUEUE SEQUENCE NUMBER
     *         10X,'Bet Creation Time in Euromil',4X,I2.2,':',I2.2,':',I2.2,/,  !EM BET CREATION TIME (HH24:MI:SS)
     *         10X,'EM Bet External Serial #',8X,I3.3,'-',I8.8,'-',A3,/,        !EM EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *         10X,'EM Draw #',23X,I3.3,'/',I4.4,1X,'to',1X,I3.3,'/',I4.4)      !EM BEGIN DRAW NUMBER CCC/YYYY
C                                                                               
9120    FORMAT(10X,'CMIL Draw #',21X,I3.3,'/',I4.4,/,                           !SoM DRAW NUMBER CCC/YYYY
     *         10X,'CMIL Code #s',20X,A,1X,'to',1X,A,1X,'(',I0,1X,'code)')      !SoM RAFFLE CODE RANGE (1 CODE)
9130    FORMAT(10X,'CMIL Draw #',21X,I3.3,'/',I4.4,/,                           !SoM DRAW NUMBER CCC/YYYY
     *         10X,'CMIL Code #s',20X,A,1X,'to',1X,A,1X,'(',I0,1X,'codes)')     !SoM RAFFLE CODE RANGE (MORE THAN 1 CODE)
C
9140    FORMAT(10X,'M1LHAO Bet External Serial #',4X,I3.3,'-',I8.8,'-',A3,/,    !SM EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *         10X,'M1LHAO Draw #',19X,I3.3,'/',I4.4,/,                         !SM DRAW NUMBER CCC/YYYY
     *         10X,'M1LHAO Code #s',18X,A,1X,'to',1X,A,1X,'(',I0,1X,'code)')    !SM RAFFLE CODE RANGE (1 CODE)
9150    FORMAT(10X,'M1LHAO Bet External Serial #',4X,I3.3,'-',I8.8,'-',A3,/,    !SM EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *         10X,'M1LHAO Draw #',19X,I3.3,'/',I4.4,/,                         !SM DRAW NUMBER CCC/YYYY
     *         10X,'M1LHAO Code #s',18X,A,1X,'to',1X,A,1X,'(',I0,1X,'codes)')   !SM RAFFLE CODE RANGE (MORE THAN 1 CODE)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       CANCEL TRANSACTION FIELDS FORMAT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
9200    FORMAT(10X,'EM Bet External Serial #',8X,I3.3,'-',I8.8,'-',I3.3,/,      !EM EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Cancel Status',19X,A,/,                                        !CANCEL STATUS DESCRIPTION
     *      10X,'Cancel Time in Euromil',10X,I2.2,':',I2.2,':',I2.2,/,          !CANCEL TIME (HH24:MI:SS)
     *      10X,'EM Cancel External Serial #',5X,I3.3,'-',I8.8,'-',I3.3,/,      !EM CANCEL EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'EM Cancel Amount' ,16X,A,/,                                    !EM CANCEL AMOUNT
     *      10X,'M1LHAO Bet External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/,     !SM EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'M1LHAO Cancel External Serial #',1X,I3.3,'-',I8.8,'-',I3.3,/,  !SM CANCEL EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'M1LHAO Cancel Amount' ,12X,A,/)                                !SM CANCEL AMOUNT
C
9210    FORMAT(10X,'EM Bet External Serial #',8X,I3.3,'-',I8.8,'-',I3.3,/,      !EM EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Cancel Status',19X,A,/,                                        !CANCEL STATUS DESCRIPTION
     *      10X,'Cancel Time in Euromil',10X,I2.2,':',I2.2,':',I2.2,/,          !CANCEL TIME (HH24:MI:SS)
     *      10X,'EM Cancel External Serial #',5X,I3.3,'-',I8.8,'-',I3.3,/,      !EM CANCEL EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'EM Cancel Amount' ,16X,A,/)                                    !EM CANCEL AMOUNT
C
9220    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Cancel Status',19X,A,/,                                        !CANCEL STATUS DESCRIPTION
     *      10X,'Cancel Time in Euromil',10X,I2.2,':',I2.2,':',I2.2,/,          !CANCEL TIME (HH24:MI:SS)
     *      10X,'Cancel External Serial #',8X,I3.3,'-',I8.8,'-',I3.3,/,         !CANCEL EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Cancel Amount' ,19X,A,/)                                       !CANCEL AMOUNT
C
9230    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Cancel Status',19X,A,/,                                        !CANCEL STATUS DESCRIPTION
     *      10X,'Cancel Time in Euromil',10X,I2.2,':',I2.2,':',I2.2,/,          !CANCEL TIME (HH24:MI:SS)
     *      10X,'Cancel External Serial #',8X,I3.3,'-',I8.8,'-',I3.3,/,         !CANCEL EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Cancel Amount' ,19X,A,/)                                       !CANCEL AMOUNT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       VALIDATION TRANSACTION FIELDS FORMAT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       VALIDATION ERROR SUBTYPE (PLAYER ID AND NIB ARE FILLED WITH 0'S)
C
9250    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/)     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC + NEW LINE
C
C       VALIDATION ERROR SUBTYPE (AT LEAST PLAYER ID OR NIB IS NOT FILLED
C       WITH 0'S)
C
9255    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Player Id',23X,I0,1X,A/,                                       !PLAYER ID NUMBER AND PLAYER ID TYPE DESCRIPTION
     *      10X,'NIB',29X,6A4,/,                                                !PLAYER NIB
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/)     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC + NEW LINE
C
C       INQUIRY SUBTYPES - NIF NOT PRESENT, PRIZE HAS TAX
C
9300    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/,     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Prize Amount' ,20X,A,/,                                        !PRIZE AMOUNT
     *      10X,'Net Prize Amount' ,16X,A,/,                                    !NET PRIZE AMOUNT
     *      10X,'Tax Prize Amount' ,16X,A,/)                                    !TAX PRIZE AMOUNT + NEW LINE
C
C       INQUIRY SUBTYPES - NIF NOT PRESENT, PRIZE HAS NO TAX
C
9305    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/,     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Prize Amount' ,20X,A,/)                                        !PRIZE AMOUNT + NEW LINE
C
C       INQUIRY SUBTYPES - NIF PRESENT, PRIZE HAS TAX
C
9310    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/,     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Prize Amount' ,20X,A,/,                                        !PRIZE AMOUNT
     *      10X,'Net Prize Amount' ,16X,A,/,                                    !NET PRIZE AMOUNT
     *      10X,'Tax Prize Amount' ,16X,A,/,                                    !TAX PRIZE AMOUNT
     *      10X,'Player NIF',22X,A,/,                                           !PLAYER NIF
     *      10X,'NIF Confirmation Needed',9X,A,/)                               !NIF CONFIRMATION NEEDED FLAG + NEW LINE
C
C       INQUIRY SUBTYPES - NIF PRESENT, PRIZE HAS NO TAX
C
9315    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/,     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Prize Amount' ,20X,A,/,                                        !PRIZE AMOUNT
     *      10X,'Player NIF',22X,A,/,                                           !PLAYER NIF
     *      10X,'NIF Confirmation Needed',9X,A,/)                               !NIF CONFIRMATION NEEDED FLAG + NEW LINE
C
C       CASH PAYMENT - NIF NOT PRESENT, PRIZE HAS TAX
C
9320    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/,     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Prize Amount' ,20X,A,/,                                        !PRIZE AMOUNT
     *      10X,'Net Prize Amount' ,16X,A,/,                                    !NET PRIZE AMOUNT
     *      10X,'Tax Prize Amount' ,16X,A,/)                                    !TAX PRIZE AMOUNT + NEW LINE
C
C       CASH PAYMENT - NIF NOT PRESENT, PRIZE HAS NO TAX
C
9325    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/,     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Prize Amount' ,20X,A,/)                                        !PRIZE AMOUNT + NEW LINE
C
C       CASH PAYMENT - NIF PRESENT, PRIZE HAS TAX
C
9330    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/,     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Prize Amount' ,20X,A,/,                                        !PRIZE AMOUNT
     *      10X,'Net Prize Amount' ,16X,A,/,                                    !NET PRIZE AMOUNT
     *      10X,'Tax Prize Amount' ,16X,A,/,                                    !TAX PRIZE AMOUNT
     *      10X,'Player NIF',22X,A,/)                                           !PLAYER NIF + NEW LINE
C
C       CASH PAYMENT - NIF PRESENT, PRIZE HAS NO TAX
C
9335    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/,     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Prize Amount' ,20X,A,/,                                        !PRIZE AMOUNT
     *      10X,'Player NIF',22X,A,/)                                           !PLAYER NIF + NEW LINE
C
C       BANK TRANSFER - NIF NOT PRESENT, PRIZE HAS TAX
C
9340    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Player Id',23X,I0,1X,A/,                                       !PLAYER ID NUMBER AND PLAYER ID TYPE DESCRIPTION
     *      10X,'NIB',29X,6A4,/,                                                !PLAYER NIB
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/,     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Prize Amount' ,20X,A,/,                                        !PRIZE AMOUNT + NEW LINE
     *      10X,'Net Prize Amount' ,16X,A,/,                                    !NET PRIZE AMOUNT
     *      10X,'Tax Prize Amount' ,16X,A,/)                                    !TAX PRIZE AMOUNT
C
C       BANK TRANSFER - NIF NOT PRESENT, PRIZE HAS NO TAX
C
9345    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Player Id',23X,I0,1X,A/,                                       !PLAYER ID NUMBER AND PLAYER ID TYPE DESCRIPTION
     *      10X,'NIB',29X,6A4,/,                                                !PLAYER NIB
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/,     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Prize Amount' ,20X,A,/)                                        !PRIZE AMOUNT + NEW LINE
C
C       BANK TRANSFER - NIF PRESENT, PRIZE HAS TAX
C
9350    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Player Id',23X,I0,1X,A/,                                       !PLAYER ID NUMBER AND PLAYER ID TYPE DESCRIPTION
     *      10X,'NIB',29X,6A4,/,                                                !PLAYER NIB
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/,     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Prize Amount' ,20X,A,/,                                        !PRIZE AMOUNT
     *      10X,'Net Prize Amount' ,16X,A,/,                                    !NET PRIZE AMOUNT
     *      10X,'Tax Prize Amount' ,16X,A,/,                                    !TAX PRIZE AMOUNT
     *      10X,'Player NIF',22X,A,/,                                           !PLAYER NIF
     *      10X,'NIF Confirmation Needed',9X,A,/)                               !NIF CONFIRMATION NEEDED FLAG + NEW LINE
C
C       BANK TRANSFER - NIF PRESENT, PRIZE HAS NO TAX
C
9355    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Player Id',23X,I0,1X,A/,                                       !PLAYER ID NUMBER AND PLAYER ID TYPE DESCRIPTION
     *      10X,'NIB',29X,6A4,/,                                                !PLAYER NIB
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/,     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Prize Amount' ,20X,A,/,                                        !PRIZE AMOUNT
     *      10X,'Player NIF',22X,A,/,                                           !PLAYER NIF
     *      10X,'NIF Confirmation Needed',9X,A,/)                               !NIF CONFIRMATION NEEDED FLAG + NEW LINE
C
C       INQUIRY/CASH SUBTYPES - VIRTUAL TERMINALS - PRIZE HAS TAX
C
9360    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/,     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Prize Amount' ,20X,A,/,                                        !PRIZE AMOUNT
     *      10X,'Net Prize Amount' ,16X,A,/,                                    !NET PRIZE AMOUNT
     *      10X,'Tax Prize Amount' ,16X,A,/)                                    !TAX PRIZE AMOUNT + NEW LINE
C
C       INQUIRY/CASH SUBTYPES - VIRTUAL TERMINALS - PRIZE HAS NO TAX
C
9365    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/,     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Prize Amount' ,20X,A,/)                                        !PRIZE AMOUNT + NEW LINE
C
C       TRANSACTION STATUS IS NOT GOOD (PLAYER ID AND NIB ARE FILLED 
C       WITH 0'S)
C
9370    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/)     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC + NEW LINE
C
C       TRANSACTION STATUS IS NOT GOOD (AT LEAST PLAYER ID OR NIB IS NOT
C       FILLED WITH 0'S)
9375    FORMAT(10X,'Bet External Serial #',11X,I3.3,'-',I8.8,'-',I3.3,/,        !BET EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC
     *      10X,'Player Id',23X,I0,1X,A/,                                       !PLAYER ID NUMBER AND PLAYER ID TYPE DESCRIPTION
     *      10X,'NIB',29X,6A4,/,                                                !PLAYER NIB
     *      10X,'MsgQ Reference #',16X,I0,/,                                    !MESSAGE QUEUE SEQUENCE NUMBER
     *      10X,'Validation Subtype',14X,A,/,                                   !VALIDATION SUBTYPE
     *      10X,'Validation Status',15X,A,/,                                    !VALIDATION STATUS DESCRIPTION
     *      10X,'Validation Time in Euromil',6X,I2.2,':',I2.2,':',I2.2,/,       !VALIDATION TIME (HH24:MI:SS)
     *      10X,'Validation External Serial #',4X,I3.3,'-',I8.8,'-',I3.3,/)     !VALIDATION EXTERNAL SERIAL NUMBER JJJ-SSSSSSSS-CCC + NEW LINE
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
9500    FORMAT(I4.4,1X,I4.4,I9.9,I2.2,1X,I2.2)                                  !NIB PRINT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
CV07800     FORMAT(A2,I7.7,A2)
CV07900     FORMAT(/,' STATUS ERROR  TYPE',4X,'SERIAL',5X,
CV07     *         'TIME  TERM SEQ  DATE GAME GAMETYP  GIND ',
CV07     *         'SIZE  BEG        END     JOKER ',11X,
CV07     *          'FRACTION','  X BET',
CV07     *         /,1X,131('='),/)
CV071901     FORMAT(1X,3(2X,A4),I10,1X,A8,I6,Z4,I6,I5,1X,A8,
CV07     *         I5,I5,2X,A12)
C
CV07901     FORMAT(1X,3(2X,A4),I10,1X,A8,I6,Z4,I6,I5,1X,A8,
CV07     *         I5,I5,I5,1X,A4,1X,I5,A2,2X,A11,A10,1X,I4,1X,I3)     
C902     FORMAT('   EXTSER: < ',I3.3,'-',I8.8,'-',I3.3,' >    Draw Beg: <',I3.2,'/',I2.2,
C
C EM EVOLUTION PROJECT
C
!902     FORMAT('   EXTSER: < ',I3.3,'-',I8.8,'-',A3,' >    Draw Beg: <',I3.2,'/',I2.2,
!     *         ' >   Draw End: <',I3.2,'/',I2.2,' > Duration: <',I2.2,'>')
!903     FORMAT('   # Of Boards: <',I2,'>     Qp: <',A6,'>     # Of Marks: <',I2,
!     *         '>     # Of Stars: <',I1,'>     Euromil Time: <',I2.2,':',I2.2,':',I2.2,'>')
CV07902     FORMAT('   EXTSER: < ',I3.3,'-',I8.8,'-',A3,' >    Draw Beg: <',I4.3,'/',I2.2,
CV07     *         ' >   Draw End: <',I4.3,'/',I2.2,' > Duration: <',I2.2,'>',' Draw Indicator: <',I1,'>')
C
CV07903     FORMAT('   # Of Boards: <',I2,'>     Qp: <',A6,'>     # Of Marks: <',I2,
CV07     *         '>     # Of Stars: <',I2,'>     Euromil Time: <',I2.2,':',I2.2,':',I2.2,'>')
C
C
C----+------------------------------------------------------------------          
C V05| New Validation Messages
C----+------------------------------------------------------------------
C3901     FORMAT('   Val Subtyp: < ',A16,
C     *          ' > Val Status: < ',A31,' > Cash Amount: < ',A14,' >')     
CV073901     FORMAT('   Val Subtyp: < ',A23,
CV07     *          ' > Val Status: < ',A31,' > Cash Amount: < ',A14,' >')     
C----+------------------------------------------------------------------          
C V05| New Validation Messages
C----+------------------------------------------------------------------
C3902     FORMAT('   EXTSER: < ',I3.3,'-',I8.8,'-',I3.3,' > Val Subtyp: <',A21,
CV073902     FORMAT('   Val Subtyp: < ',A16,
CV07     *          ' > Val Status: < ',A31,' > Cash Amount: < ',F14.2,' >')
CV073903     FORMAT('   EXTSER: < ',I3.3,'-',I8.8,'-',I3.3,
CV07     *          ' > Wager External Serial: < ',I3.3,'-',I8.8,'-',I3.3,
CV07     *          ' >       Euromil Time: <',I2.2,':',I2.2,':',I2.2,'>')
C----+------------------------------------------------------------------          
C V05| New Validation Messages
C----+------------------------------------------------------------------
CV073914     FORMAT(I4.4,1X,I4.4,I9.9,I2.2,1X,I2.2)        ! NIB Print
C073915     FORMAT(3X,'PHONE NUMBER: < ',I10,' > NIB: < ',6A4, ' >' ) ! Phone Nr
C073916     FORMAT(3X,' PLAYER CARD: < ',I10,' > NIB: < ',6A4, ' >') ! Player Card
CV073915     FORMAT(3X,'PHONE NUMBER: < ',I10,' > NIB: < ',6A4, ' >'
CV07     *          ' NIF: < ',I9.9, ' >') ! Phone Nr
CV073916     FORMAT(3X,' PLAYER CARD: < ',I10,' > NIB: < ',6A4, ' >'
CV07     *          ' NIF: < ',I9.9, ' >') ! Player Card
C----+------------------------------------------------------------------          
C V05| New Validation Messages
C----+------------------------------------------------------------------
     
C
C EM EVOLUTION PROJECT
C         
!3904     FORMAT('   EXTSER (Exchange Ticket): < ',I3.3,'-',I8.8,'-',I3.3,
!     *          ' >    Draw Beg: <',I3.2,'/',I2.2,
!     *          ' >   Draw End: <',I3.2,'/',I2.2,' > Duration: <',I2.2,'>')
CV073904     FORMAT('   EXTSER (Exchange Ticket): < ',I3.3,'-',I8.8,'-',I3.3,
CV07     *          ' >    Draw Beg: <',I4.3,'/',I2.2,
CV07     *          ' >   Draw End: <',I4.3,'/',I2.2,' > Duration: <',I2.2,'>')
C
CV072902     FORMAT('   EXTSER: < ',I3.3,'-',I8.8,'-',I3.3,
CV07     *          ' > Can Status: < ',A51,' >')
CV072903     FORMAT('    Wager External Serial: <',I3.3,'-',I8.8,'-',I3.3,'>',
CV07     *          '    Cancel Amount: < ',A11,' >')
     
        END
C***********************************************************************
C       SUBROUTINE TRANSFBOARD
C***********************************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TRANSFBOARD(TERMES,NBOARD,SYSTEMMARK,SYSTEMSTAR,
     *                         MAXNUM,MAXSTAR,ST,PUNIT)
C
        IMPLICIT NONE
C
        BYTE     TERMES(*)
        INTEGER*4   NBOARD
        INTEGER*4   SYSTEMMARK
        INTEGER*4   MAXNUM
        INTEGER*4   ST,PUNIT,SYSTEMSTAR,MAXSTAR
        INTEGER*4 MARKS(20)
        INTEGER*4 STARS(20)
C
        INTEGER*4   BRD,PNT,VAL,CNT,XBYT,NIB,SVAL,SCNT,J,I
        LOGICAL     LEFT
C
        PNT = 0
        DO J=1, 20 
          MARKS(J) = 0
          STARS(J) = 0
        ENDDO
C
C START DECODE SYSTEM BET
C
        LEFT = .TRUE.
        DO 2900 BRD = 1, NBOARD
C
C MARKS NUMBERS
C
        VAL = 0
        CNT = 0
2100    CONTINUE
        IF(LEFT)THEN
          PNT  = PNT+1
          XBYT = TERMES(PNT)
          XBYT = IAND(XBYT, '000000FF'X)
          NIB  = ISHFT(XBYT,-4)
          LEFT = .FALSE.
        ELSE
          NIB  = XBYT
          LEFT = .TRUE.
        ENDIF
        NIB = IAND (NIB, '0F'X)
        IF(NIB .EQ. 0) THEN
          VAL = VAL+15
          IF(VAL .GT. MAXNUM)THEN
            ST = -2
            GOTO 9000
          ENDIF
          GOTO 2100
        ENDIF
C
        VAL = VAL + NIB 
C          WRITE(MARKS,100) VAL
        CNT = CNT+1
        MARKS(CNT) = VAL
        IF(CNT .LT. SYSTEMMARK) GOTO 2100
C
C STAR NUMBERS 
C	  
        SVAL = 0
        SCNT = 0
2200    CONTINUE
        IF(LEFT)THEN
          PNT  = PNT+1
          XBYT = TERMES(PNT)
          XBYT = IAND(XBYT, '000000FF'X)
          NIB  = ISHFT(XBYT,-4)
          LEFT = .FALSE.
        ELSE
          NIB  = XBYT
          LEFT = .TRUE.
        ENDIF
        NIB = IAND (NIB, '0F'X)
        IF(NIB .EQ. 0)THEN
          SVAL = SVAL+15
          IF(SVAL .GT. MAXSTAR)THEN
            ST = -2
            GOTO 9000
          ENDIF
          GOTO 2200
        ENDIF
C
        SVAL = SVAL + NIB
        IF(SVAL .GT. MAXSTAR)THEN
          ST = -3
          GOTO 9000
        ENDIF
C
        SCNT = SCNT+1
        STARS(SCNT) = SVAL
        IF(SCNT .LT. SYSTEMSTAR)GOTO 2200
C
C WRITE BET'S TO REPORT FILE
C
CV07	  WRITE(PUNIT,100) BRD,(MARKS(J+1),J=0,SYSTEMMARK-1),(STARS(I+1),I=0,SYSTEMSTAR-1)
        IF(BRD .EQ. 1) THEN
          WRITE(PUNIT,100) BRD,
     *                     NBOARD, 
     *                    (MARKS(J+1),J=0,SYSTEMMARK-1),
     *                    (STARS(I+1),I=0,SYSTEMSTAR-1)
        ELSE
          WRITE(PUNIT,102) BRD,
     *                     NBOARD,
     *                    (MARKS(J+1),J=0,SYSTEMMARK-1),
     *                    (STARS(I+1),I=0,SYSTEMSTAR-1)
        ENDIF
c	  WRITE(PUNIT,101) '             ', 	
C
2900    CONTINUE
        ST = 0
C
9000    CONTINUE
        IF (ST .NE. 0) WRITE(PUNIT,*) 'ERROR ',ST 
        RETURN
CV07100     FORMAT('   Board ',I2,'    Numbers: <',<SYSTEMMARK>I3.2,' > Stars: <',<SYSTEMSTAR>I3.2,' >')
100     FORMAT(10X,'EM Board ',I0,1X,'of',1X,I0,16X,
     *             <SYSTEMMARK>I3.2,1X,'+',<SYSTEMSTAR>I3.2)
102     FORMAT(10X,'   Board ',I0,1X,'of',1X,I0,16X,
     *             <SYSTEMMARK>I3.2,1X,'+',<SYSTEMSTAR>I3.2)
101     FORMAT(A12,A10,<SYSTEMSTAR>I3)
        END
C
C***********************************************************************
C       SUBROUTINE UPDEURSTAT
C***********************************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE UPDEURSTAT(TRABUF)
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:PRINTEURO.DEF'
C
        INTEGER*4 SHWAG, SMWAG, KIWAG, SHVAL, SMVAL, SHCAN, SMCAN
        INTEGER*4 EGTYP, EGIND, EGNUM
C
        IF(TRABUF(TEUTYP) .EQ. TWAG) GOTO 1000
        IF(TRABUF(TEUTYP) .EQ. TVAL) GOTO 2000
        IF(TRABUF(TEUTYP) .EQ. TCAN) GOTO 3000
C
        RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       WAGERS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
1000    CONTINUE
C
        IF(TRABUF(TSTAT).EQ.GOOD .AND. TRABUF(TERR).EQ.NOER) THEN
          EGTYP = TRABUF(TGAMTYP)
          EGIND = TRABUF(TGAMIND)
          IF(EGTYP.EQ.TEUM .AND. EGIND.EQ.EUM1GI) THEN
            EGNUM = EUM1GN
            SHWAG = SHNO
            SMWAG = SMNO
            KIWAG = KINO
            IF(TRABUF(TEUW_SHWFL).NE.0) SHWAG = SHYES
            IF(TRABUF(TEUW_SMWFL).NE.0) SMWAG = SMYES
            IF(TRABUF(TEUW_KIWFL).NE.0) KIWAG = KIYES
            EURGWAG(EGNUM,SHWAG,SMWAG,KIWAG,GTCNT) = 
     *            EURGWAG(EGNUM,SHWAG,SMWAG,KIWAG,GTCNT) + 1                    !INCREMENT GAME TOTAL WAGER TRANSACTIONS
C
            EURTOT(TOTWAG,GTCNT) = 
     *            EURTOT(TOTWAG,GTCNT) + 1                                      !INCREMENT TOTAL WAGER TRANSACTIONS
C
            IF(TRABUF(TEUWNMK).EQ.NUMLO.AND.TRABUF(TEUWNST).EQ.STRLO) THEN      !EM SIMPLE BET TRANSACTIONS
              IF(TRABUF(TEUWNBET).GE.SIMLO.AND.TRABUF(TEUWNBET).LE.SIMHI) THEN
                EUM1SIMTOT(TRABUF(TEUWNBET)) = EUM1SIMTOT(TRABUF(TEUWNBET)) + 1
                EUM1SIMTOT(SIMHI+1) = EUM1SIMTOT(SIMHI+1) + 1                   !TOTAL COUNT OF EM SIMPLE BET TRANSACTIONS
              ENDIF
            ELSE                                                                !EM MULTIPLE BET TRANSACTIONS
              IF(TRABUF(TEUWNMK).GE.NUMLO.AND.TRABUF(TEUWNMK).LE.NUMHI.AND.
     *           TRABUF(TEUWNST).GE.STRLO.AND.TRABUF(TEUWNST).LE.STRHI) THEN
C
                EUM1MULTOT(TRABUF(TEUWNMK),TRABUF(TEUWNST)) =                   !TOTAL TRANSACTIONS WITH TRABUF(TEUWNMK) NUMBERS + TRABUF(TEUWNST) STARS
     *           EUM1MULTOT(TRABUF(TEUWNMK),TRABUF(TEUWNST)) + 1
C             
                EUM1MULTOT(TRABUF(TEUWNMK),STRHI+1) =                           !TOTAL TRANSACTIONS WITH TRABUF(TEUWNMK) NUMBERS
     *           EUM1MULTOT(TRABUF(TEUWNMK),STRHI+1) + 1
C             
                EUM1MULTOT(NUMHI+1,TRABUF(TEUWNST)) =                           !TOTAL TRANSACTIONS WITH TRABUF(TEUWNST) STARS
     *           EUM1MULTOT(NUMHI+1,TRABUF(TEUWNST)) + 1
C
                EUM1MULTOT(NUMHI+1,STRHI+1) = EUM1MULTOT(NUMHI+1,STRHI+1) + 1   !TOTAL COUNT OF EM MULTIPLE BET TRANSACTIONS
              ENDIF
            ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C           TOTAL BETS STATISTICS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
            IF(TRABUF(TEUWNMK).GE.NUMLO.AND.TRABUF(TEUWNMK).LE.NUMHI.AND.
     *         TRABUF(TEUWNST).GE.STRLO.AND.TRABUF(TEUWNST).LE.STRHI) THEN
C           
              EURGBET(EGNUM,SHWAG,SMWAG,KIWAG,GBCNT) =                          !EM + SoM + SM TOTAL BETS BY (MAIN) GAME
     *         EURGBET(EGNUM,SHWAG,SMWAG,KIWAG,GBCNT) +
     *         EUM1MSTAB(TRABUF(TEUWNMK),TRABUF(TEUWNST))*TRABUF(TEUWNBET) +
     *         TRABUF(TEUW_SHWTB) +
     *         TRABUF(TEUW_SMWTB)
C           
              EURGBET(EGNUM,SHWAG,SMWAG,KIWAG,EUM1BCNT) =                       !EM TOTAL BETS
     *         EURGBET(EGNUM,SHWAG,SMWAG,KIWAG,EUM1BCNT) +
     *         EUM1MSTAB(TRABUF(TEUWNMK),TRABUF(TEUWNST))*TRABUF(TEUWNBET)
C           
              EURGBET(EGNUM,SHWAG,SMWAG,KIWAG,RAF1BCNT) =                       !SoM TOTAL BETS
     *         EURGBET(EGNUM,SHWAG,SMWAG,KIWAG,RAF1BCNT) +
     *         TRABUF(TEUW_SHWTB)
C           
              EURGBET(EGNUM,SHWAG,SMWAG,KIWAG,RAF2BCNT) =                       !SM TOTAL BETS
     *         EURGBET(EGNUM,SHWAG,SMWAG,KIWAG,RAF2BCNT) +
     *         TRABUF(TEUW_SMWTB)
C           
              EURTBET(GBCNT) = EURTBET(GBCNT) +                                 !EM + SoM + SM TOTAL BETS
     *         EUM1MSTAB(TRABUF(TEUWNMK),TRABUF(TEUWNST))*TRABUF(TEUWNBET) +
     *         TRABUF(TEUW_SHWTB) +
     *         TRABUF(TEUW_SMWTB)
C           
              EURTBET(EUM1BCNT) = EURTBET(EUM1BCNT) +                           !EM TOTAL BETS
     *         EUM1MSTAB(TRABUF(TEUWNMK),TRABUF(TEUWNST))*TRABUF(TEUWNBET)
C           
              EURTBET(RAF1BCNT) = EURTBET(RAF1BCNT) +                           !SoM TOTAL BETS
     *         TRABUF(TEUW_SHWTB)
C           
              EURTBET(RAF2BCNT) = EURTBET(RAF2BCNT) +                           !SM TOTAL BETS
     *         TRABUF(TEUW_SMWTB)
            ENDIF
          ENDIF
        ENDIF
C
        RETURN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       VALIDATIONS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
2000    CONTINUE
C
        IF (  (TRABUF(TSTAT) .EQ. GOOD .AND. TRABUF(TERR).EQ.NOER)
     *  .AND. (     TRABUF(TEUVSBT) .EQ. VREG
     *         .OR. TRABUF(TEUVSBT) .EQ. VNDON
     *         .OR. TRABUF(TEUVSBT) .EQ. VNBNK
     *        )
     *  .AND. (    (TRABUF(TEUVCAM) .NE. 0)
     *         .OR.(TRABUF(TEUVCAMH) .NE. 0)
     *        )
     *  ) THEN
C
          IF(TRABUF(TEUVST).EQ.10) THEN                                         !VALIDATION STATUS = NO EXCHANGE TICKET
            I4PRZAMT(2) = TRABUF(TEUVCAMH)
            I4PRZAMT(1) = TRABUF(TEUVCAM)
            EGTYP = TRABUF(TGAMTYP)
            EGIND = TRABUF(TGAMIND)
            IF(EGTYP.EQ.TEUM .AND. EGIND.EQ.EUM1GI) THEN                        !EUROMILLIONS GAME
              EGNUM = EUM1GN
              SHVAL = SHNO
              SMVAL = SMNO
              IF(TRABUF(TEUV_SHVFL).NE.0) SHVAL = 1 
C
              EURGVAL(EGNUM,SHVAL,SMVAL,GTCNT) = 
     *              EURGVAL(EGNUM,SHVAL,SMVAL,GTCNT) + 1                        !INCREMENT SM GAME TOTAL VALIDATION TRANSACTIONS
C
              EURGVAL(EGNUM,SHVAL,SMVAL,GTAMT) = 
     *              EURGVAL(EGNUM,SHVAL,SMVAL,GTAMT) + I8PRZAMT                 !ADD EM GAME VALIDATION AMOUNT TO TOTAL VALIDATION AMOUNT
C
              EURGVAL(EGNUM,SHVAL,SMVAL,EUM1TAMT) = 
     *              EURGVAL(EGNUM,SHVAL,SMVAL,EUM1TAMT) + I8PRZAMT              !ADD EM GAME VALIDATION AMOUNT TO SM GAME TOTAL VALIDATION AMOUNT
C
              EURTOT(TOTVAL,GTCNT) = 
     *              EURTOT(TOTVAL,GTCNT) + 1                                    !INCREMENT TOTAL VALIDATION TRANSACTIONS
C
              EURTOT(TOTVAL,GTAMT) = 
     *              EURTOT(TOTVAL,GTAMT) + I8PRZAMT                             !ADD EM GAME VALIDATION AMOUNT TO TOTAL VALIDATION AMOUNT
C
              EURTOT(TOTVAL,EUM1TAMT) = 
     *              EURTOT(TOTVAL,EUM1TAMT) + I8PRZAMT                          !ADD EM GAME VALIDATION AMOUNT TO EM GAME TOTAL VALIDATION AMOUNT
C
            ELSEIF(EGTYP.EQ.TRAF .AND. EGIND.EQ.RAF2GI) THEN                    !SM GAME
              EGNUM = RAF2GN
              SHVAL = SHYES
              SMVAL = SMNO
C
              EURGVAL(EGNUM,SHVAL,SMVAL,GTCNT) = 
     *              EURGVAL(EGNUM,SHVAL,SMVAL,GTCNT) + 1                        !INCREMENT SM GAME TOTAL VALIDATION TRANSACTIONS
C
              EURGVAL(EGNUM,SHVAL,SMVAL,GTAMT) = 
     *              EURGVAL(EGNUM,SHVAL,SMVAL,GTAMT) + I8PRZAMT                 !ADD SM GAME VALIDATION AMOUNT TO TOTAL VALIDATION AMOUNT
              EURGVAL(EGNUM,SHVAL,SMVAL,RAF2TAMT) = 
     *              EURGVAL(EGNUM,SHVAL,SMVAL,RAF2TAMT) + I8PRZAMT              !ADD SM GAME VALIDATION AMOUNT TO SM GAME TOTAL VALIDATION AMOUNT
C
              EURTOT(TOTVAL,GTCNT) = 
     *              EURTOT(TOTVAL,GTCNT) + 1                                    !INCREMENT TOTAL VALIDATION TRANSACTIONS
C
              EURTOT(TOTVAL,GTAMT) = 
     *              EURTOT(TOTVAL,GTAMT) + I8PRZAMT                             !ADD SM GAME VALIDATION AMOUNT TO TOTAL VALIDATION AMOUNT
C
              EURTOT(TOTVAL,RAF2TAMT) = 
     *              EURTOT(TOTVAL,RAF2TAMT) + I8PRZAMT                          !ADD SM GAME VALIDATION AMOUNT TO SM GAME TOTAL VALIDATION AMOUNT
C
            ENDIF
          ENDIF
C
          RETURN
        ENDIF
C
        RETURN
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       CANCELS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
3000    CONTINUE
C
        IF(TRABUF(TSTAT).EQ.GOOD .AND. TRABUF(TERR).EQ.NOER) THEN
          IF(TRABUF(TEUCST).EQ.0) THEN                                          !GOOD CANCEL
            EGTYP = TRABUF(TGAMTYP)
            EGIND = TRABUF(TGAMIND)
            IF(EGTYP.EQ.TEUM .AND. EGIND.EQ.EUM1GI) THEN                        !EM GAME
              EGNUM = EUM1GN
              SHCAN = SHNO
              SMCAN = SMNO
              IF(TRABUF(TEUC_SHCFL).NE.0) SHCAN = SHYES
              IF(TRABUF(TEUC_SMCFL).NE.0) SMCAN = SMYES
C
              EURGCAN(EGNUM,SHCAN,SMCAN,GTCNT) = 
     *              EURGCAN(EGNUM,SHCAN,SMCAN,GTCNT) + 1                        !INCREMENT EM GAME TOTAL CANCEL TRANSACTIONS
C
              EURGCAN(EGNUM,SHCAN,SMCAN,GTAMT) = 
     *              EURGCAN(EGNUM,SHCAN,SMCAN,GTAMT) + 
     *                                     TRABUF(TEUCAM) +                     !ADD EM GAME CANCEL AMOUNT TO TOTAL CANCEL AMOUNT
     *                                     TRABUF(TEUC_SMWCA)                   !ADD SM GAME CANCEL AMOUNT TO TOTAL CANCEL AMOUNT
C
              EURGCAN(EGNUM,SHCAN,SMCAN,EUM1TAMT) = 
     *              EURGCAN(EGNUM,SHCAN,SMCAN,EUM1TAMT) + TRABUF(TEUCAM)        !ADD EM GAME CANCEL AMOUNT TO EM GAME TOTAL CANCEL AMOUNT
C
              EURGCAN(EGNUM,SHCAN,SMCAN,RAF2TAMT) = 
     *              EURGCAN(EGNUM,SHCAN,SMCAN,RAF2TAMT) + TRABUF(TEUC_SMWCA)    !ADD SM GAME CANCEL AMOUNT TO SM GAME TOTAL CANCEL AMOUNT
C
              EURTOT(TOTCAN,GTCNT) = 
     *              EURTOT(TOTCAN,GTCNT) + 1                                    !INCREMENT TOTAL CANCEL TRANSACTIONS
C
              I4AMT(2) = 0
              I4AMT(1) = TRABUF(TEUCAM)
              EURTOT(TOTCAN,GTAMT) = 
     *              EURTOT(TOTCAN,GTAMT) + I8AMT                                !ADD EM GAME CANCEL AMOUNT TO TOTAL CANCEL AMOUNT
C
              EURTOT(TOTCAN,EUM1TAMT) = 
     *              EURTOT(TOTCAN,EUM1TAMT) + I8AMT                             !ADD EM GAME CANCEL AMOUNT TO EM GAME TOTAL CANCEL AMOUNT
C
              I4AMT(2) = 0
              I4AMT(1) = TRABUF(TEUC_SMWCA)
              EURTOT(TOTCAN,GTAMT) = 
     *              EURTOT(TOTCAN,GTAMT) + I8AMT                                !ADD SM GAME CANCEL AMOUNT TO TOTAL CANCEL AMOUNT
C
              EURTOT(TOTCAN,RAF2TAMT) = 
     *              EURTOT(TOTCAN,RAF2TAMT) + I8AMT                             !ADD EM GAME CANCEL AMOUNT TO EM GAME TOTAL CANCEL AMOUNT
            ENDIF
          ENDIF
C
          RETURN
        ENDIF
C
        RETURN
        END
C
C***********************************************************************
C       SUBROUTINE PRTEURSTAT
C***********************************************************************
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PRTEURSTAT(PUNIT)
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRINTEURO.DEF'
C
        INTEGER*4 PUNIT
C
        INTEGER*4 I, J, K, L
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       PRINT EUROMILLIONS TRANSACTION HEADER
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        WRITE(PUNIT,9000)
        WRITE(PUNIT,9010)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       PRINT WAGER TRANSACTION STATISTICS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        WRITE(PUNIT,9100)
        DO I=1,EMAXGAM
          IF(I.GT.0) THEN
            DO J = SHNO,SHYES
              DO K = SMNO, SMYES
                DO L = KINO, KIYES
                  IF(EURGWAG(I,J,K,L,GTCNT).GT.0) THEN
                    WRITE(PUNIT,9110) EURGNAM(I,J,K,L),
     *                         EURGWAG(I,J,K,L,GTCNT)
                  ENDIF
                ENDDO
              ENDDO
            ENDDO
          ENDIF
        ENDDO
C
        WRITE(PUNIT,9020)
        WRITE(PUNIT,9120) EURTOT(TOTWAG,GTCNT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       PRINT TOTAL COUNT OF EM WAGER TRANSACTIONS BY MULTIPLE BET TYPE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        WRITE(PUNIT,9130)
        WRITE(PUNIT,9131) (J,J=NUMLO,NUMHI)
        WRITE(PUNIT,9132)
        WRITE(PUNIT,9133) STRLO,
     *                   (EUM1MULTOT(J,STRLO),J=NUMLO+1,EUM1NUMHI(STRLO)),
     *                    EUM1MULTOT(NUMHI+1,STRLO)
        DO I=STRLO+1,STRHI
          WRITE(PUNIT,9134) I,
     *                     (EUM1MULTOT(J,I),J=NUMLO,EUM1NUMHI(I)),
     *                      EUM1MULTOT(NUMHI+1,I)
        ENDDO
        WRITE(PUNIT,9132)
        WRITE(PUNIT,9135) (EUM1MULTOT(J,STRHI+1),J=NUMLO,NUMHI),
     *                     EUM1MULTOT(NUMHI+1,STRHI+1)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       PRINT TOTAL COUNT OF EM WAGER TRANSACTIONS WITH 1, 2, 3, 4 AND 5
C       SIMPLE BETS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        WRITE(PUNIT,9140)
        DO I=SIMLO,SIMHI
          WRITE(PUNIT,9141) I, EUM1SIMTOT(I)
        ENDDO
        WRITE(PUNIT,9142)
        WRITE(PUNIT,9143) EUM1SIMTOT(SIMHI+1)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       PRINT CANCEL TRANSACTION STATISTICS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        WRITE(PUNIT,9200)
        WRITE(PUNIT,9210)
        DO I=1,EMAXGAM
          IF(I.GT.0) THEN
            DO J = SHNO,SHYES
              DO K = SMNO, SMYES
                IF(EURGCAN(I,J,K,GTCNT).GT.0) THEN
                  WRITE(PUNIT,9220) EURGNAM(I,J,K,0),
     *                       EURGCAN(I,J,K,GTCNT),
     *                       CMONY(EURGCAN(I,J,K,GTAMT),11,BETUNIT),
     *                       CMONY(EURGCAN(I,J,K,EUM1TAMT),11,BETUNIT),
     *                       CMONY(EURGCAN(I,J,K,RAF2TAMT),11,BETUNIT)
                ENDIF
              ENDDO
            ENDDO
          ENDIF
        ENDDO
C
        WRITE(PUNIT,9230)
        WRITE(PUNIT,9240) EURTOT(TOTCAN,GTCNT),
     *                    DFLOAT(EURTOT(TOTCAN,GTAMT))/100.0D0,
     *                    DFLOAT(EURTOT(TOTCAN,EUM1TAMT))/100.0D0,
     *                    DFLOAT(EURTOT(TOTCAN,RAF2TAMT))/100.0D0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       PRINT VALIDATION TRANSACTION STATISTICS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        WRITE(PUNIT,9300)
        WRITE(PUNIT,9310)
        DO I=1,EMAXGAM
          IF(I.GT.0) THEN
            DO J = SHNO,SHYES
              DO K = SMNO, SMYES
                IF(EURGVAL(I,J,K,GTCNT).GT.0) THEN
                  WRITE(PUNIT,9320) EURGNAM(I,J,K,0),
     *                       EURGVAL(I,J,K,GTCNT),
     *                       DFLOAT(EURGVAL(I,J,K,GTAMT))/100.0D0,
     *                       DFLOAT(EURGVAL(I,J,K,EUM1TAMT))/100.0D0,
     *                       DFLOAT(EURGVAL(I,J,K,RAF2TAMT))/100.0D0
                ENDIF
              ENDDO
            ENDDO
          ENDIF
        ENDDO
C
        WRITE(PUNIT,9330)
        WRITE(PUNIT,9340) EURTOT(TOTVAL,GTCNT),
     *                    DFLOAT(EURTOT(TOTVAL,GTAMT))/100.0D0,
     *                    DFLOAT(EURTOT(TOTVAL,EUM1TAMT))/100.0D0,
     *                    DFLOAT(EURTOT(TOTVAL,RAF2TAMT))/100.0D0
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       PRINT EUROMILLIONS SYSTEM BETS HEADER
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        WRITE(PUNIT,9500)
        WRITE(PUNIT,9510)
        WRITE(PUNIT,9520)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        DO I=1,EMAXGAM
          IF(I.GT.0) THEN
            DO J = SHNO,SHYES
              DO K = SMNO, SMYES
                DO L = KINO, KIYES
                  IF(EURGBET(I,J,K,L,GBCNT).GT.0) THEN
                  WRITE(PUNIT,9530) EURGNAM(I,J,K,L),
     *                       EURGBET(I,J,K,L,GBCNT),
     *                       EURGBET(I,J,K,L,EUM1BCNT),
     *                       EURGBET(I,J,K,L,RAF1BCNT),
     *                       EURGBET(I,J,K,L,RAF2BCNT)
                  ENDIF
                ENDDO
              ENDDO
            ENDDO
          ENDIF
        ENDDO
C
        WRITE(PUNIT,9540)
        WRITE(PUNIT,9550) EURTBET(GBCNT),
     *                    EURTBET(EUM1BCNT),
     *                    EURTBET(RAF1BCNT),
     *                    EURTBET(RAF2BCNT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       FORMAT STATEMENTS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
9000    FORMAT(/,' Euro Milhoes transactions')
9010    FORMAT(31X,'COUNT')
9020    FORMAT(26X,10('-'))
C
9100    FORMAT(2X,'Wagers',18X,10('-'))
9110    FORMAT(3X,A20,3X,I10)
9120    FORMAT(3X,'TOTAL wagers',11X,I10)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
9130    FORMAT(/,3X,'TOTAL count of EM wager transactions by multiple bet type')
9131    FORMAT(/,12X,<NUMHI-NUMLO+1>(1X,I8,'N'),3X,'SUBTOT S')
9132    FORMAT(13X,69('-'),2X,9('-'))
9133    FORMAT(9X,I2,'S',9X,'-',<EUM1NUMHI(STRLO)-(NUMLO+1)+1>(1X,I9),
     *                          2X,I9)
9134    FORMAT(9X,I2,'S',<EUM1NUMHI(I)-NUMLO+1>(1X,I9),
     *                   <NUMHI-EUM1NUMHI(I)>(10X),
     *         2X,I9)
9135    FORMAT(4X,'SUBTOT N',<NUMHI-NUMLO+1>(1X,I9),2X,I9,1X,'TOTAL')
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
9140    FORMAT(/,3X,'TOTAL count of EM wager transactions with 1, 2, 3, 4 and 5 simple bets',
     *         //,28X,'COUNT',
     *         /,8X,'# Simple Bets',3X,9('-'))
9141    FORMAT(9X,I2,13X,I9)
9142    FORMAT(24X,9('-'))
9143    FORMAT(8X,'TOTAL',11X,I9)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
9200    FORMAT(/,31X,'COUNT       TOT AMT        EM AMT    M1LHAO AMT')
9210    FORMAT(2X,'Cancels',17X,52('-'))
9220    FORMAT(3X,A20,3X,I10,3X,A11,3X,A11,3X,A11)
9230    FORMAT(26X,52('-'))
9240    FORMAT(3X,'TOTAL cancels',10X,I10,2X,F12.2,2X,F12.2,2X,F12.2)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
9300    FORMAT(/,31X,'COUNT       TOT AMT        EM AMT    M1LHAO AMT')
9310    FORMAT(2X,'Validations',13X,52('-'))
9320    FORMAT(3X,A20,3X,I10,2X,F12.2,2X,F12.2,2X,F12.2)
9330    FORMAT(26X,52('-'))
9340    FORMAT(3X,'TOTAL validations',6X,I10,2X,F12.2,2X,F12.2,2X,F12.2)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
9500    FORMAT(/,' Euro Milhoes bets (gross)')
9510    FORMAT(28X,'TOT BETS       EM BETS     CMIL BETS   M1LHAO BETS')
9520    FORMAT(2X,'Bets',20X,52('-'))
9530    FORMAT(3X,A20,3X,I10,4X,I10,4X,I10,4X,I10)
9540    FORMAT(26X,52('-'))
9550    FORMAT(3X,'TOTAL bets',13X,I10,4X,I10,4X,I10,4X,I10)
C
        RETURN
C
        END
C
C*******************************************************************************
C       FUNCTION CRAFCODE
C*******************************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW/EXT  
        CHARACTER*10 FUNCTION CRAFCODE(CODEP1, CODEP2)
C
        IMPLICIT NONE
C
        INTEGER*4 CODEP1                                                        !INPUT: RAFFLE CODE PART 1 (ALPHA PART)
        INTEGER*4 CODEP2                                                        !INPUT: RAFFLE CODE PART 2 (NUMERIC PART)
C
        INTEGER*4    I4CODEP1
        CHARACTER*1  C1CODEP1(4)                                                !RAFFLE CODE: ALPHA PART
        EQUIVALENCE (I4CODEP1, C1CODEP1(1))
C
        CHARACTER*10 C10TEMP
        CHARACTER*1  C1TEMP10(10)
        EQUIVALENCE (C10TEMP,C1TEMP10(1))
C
        INTEGER*4    I
        BYTE         BEOSC /'FF'X/
        CHARACTER*1  EOSC                                                       !END OF CODE CHARACTER DELIMITATOR FOR ALPHA PART
        EQUIVALENCE (BEOSC,EOSC)
C
        C10TEMP = '          '
        I4CODEP1 = CODEP1
C
        I = 1
        DO WHILE(C1CODEP1(I).NE.EOSC .AND. I.LE.4)
          C1TEMP10(I) = C1CODEP1(I)
          I = I + 1
        ENDDO
        WRITE(C10TEMP(I:I+4),'(I5.5)') CODEP2
C
        CRAFCODE = C10TEMP
C
        END
C
C END PRINTEURO.FOR
C
