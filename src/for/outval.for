C
C SUBROUTINE OUTVAL
C  
C V20 23-DEC-2013 SCML Handling Low Tier prizes for privileged terminals
C V19 20-DEC-2013 SCML CALL OPS removed.
C V18 08-OCT-2013 SCML New Validation Messages
C V17 12-MAR-2010 RXK  Claims commented out.  
C V16 15-MAY-2001 EPH  Use REFUND space in message format to send VALUE IN OPS
C V15 29-NOV-2000 UXN  TOTOGOLO ADDED.
C V14 13-OCT-1999 RXK  World Tour added.
C V13 18-MAR-1999 RXK  Game type/game index change. Hack for V5 removed.
C                      Temp.change for old Viking bonus numbers removed as well.
C V12 09-SEP-1998 RXK  Changed for new Kicker
C V11 22-FRB-1995 HXK  TWAS BRILLIG AND THE BOROGOVES DID
C V10 23-SEP-1993 GXA  Enabled breakdown of Refunds and cashes flag.
C V09 13-SEP-1993 HXK  Changed fraction definition for terminal
C V08 10-SEP-1993 GXA  Corrected typo in storing of bank validations.
C V07 01-SEP-1993 HXK  Changed Validation error check so that VINQ is specifically
C                      checked
C V06 31-AUG-1993 GXA  Added Game Indext to Exchange ticket (Host to Terminal msg.)
C V05 22-AUG-1993 GXA  Released for Finland Dec Conversion / Oddset.
C V04 21-JAN-1993 DAB  Initial Release Based on Netherlands Bible,
C                      DEC Baseline
C V03 02-FEB-1992 GCAN FIXED LOTTO 4/5 EXCHAGES AND JOKER Y/N FLAG.
C V02 12-NOV-1991 MTK  INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C OUTVAL.FOR
C
C SUBROUTINE TO BUILD VALIDATION OUTPUT MESSAGES.
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
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE OUTVAL(TRABUF,OUTTAB,OUTLEN,WRKBUF,VALREC,RETRY_FLAG)

        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:TGLCOM.DEF'
        INCLUDE 'INCLIB:NBRCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
C----+------------------------------------------------------------------
C V18| New Validation Messages
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:AGTCOM.DEF'
C----+------------------------------------------------------------------
C V18| New Validation Messages
C----+------------------------------------------------------------------
C
        BYTE      OUTTAB(*)                     !Terminal Output Message.
C
        INTEGER*2 OUTLEN                        !Output Message Length.
C
        INTEGER*4 WRKBUF(TRALEN)                !Exchange TKT Trans. Record.
        INTEGER*4 MYCHKSUM                      !Checksum I Calculate.
        INTEGER*4 CHKLEN                        !Length to Checksum Message.
        INTEGER*4 I                             !Loop Variable.
        INTEGER*4 OFFEND                        !Wager Starting CDC Offset.
        INTEGER*4 OFFBEG                        !Wager Ending CDC Offset.
        INTEGER*4 KOFFBEG                       !Kicker Starting CDC Offset.
        INTEGER*4 KOFFEND                       !Kicker Ending CDC Offset.
C       INTEGER*4 CLMAMT                        !Total Claim Amount.
        INTEGER*4 PAYAMT                        !Total Winning Amount.
C       INTEGER*4 REFAMT                        !Total Refund Amount             !V16
	INTEGER*4 OPSAMT                        !Total in OPS for the ticket     !V16
        INTEGER*4 CHECK                         !Check Digits for Val. Serial#
        INTEGER*4 IND                           !Index into Output Message.
        INTEGER*4 ERRTYP                        !Error Type/Subtype
        INTEGER*4 CONTRL                        !Control/Sequence
        INTEGER*4 VALERR                        !Validation Error Type/Subtype
        INTEGER*4 VALINQ                        !Validation Inquiry Type/Subtype
        INTEGER*4 VALTYP                        !Regular Validation Type/Subtype
        INTEGER*4 VALBNK                        !Bank Validation Type/Subtype.
C----+------------------------------------------------------------------
C V18| New Validation Messages
C----+------------------------------------------------------------------
        INTEGER*4 VAL_VNREG                     !NEW VALIDATION INQUIRY REGULAR !V70              
        INTEGER*4 VAL_VNDON                     !NEW VALIDATION INQUIRY MID-TIER !V70             
        INTEGER*4 VAL_VNIBO                     !NEW VALIDATION INQUIRY CASH ACCEPTED !V70        
        INTEGER*4 VAL_VNBNK                     !NEW VALIDATION TO BANK !V70                      
        INTEGER*4 VAL_VNINQ                     !NEW VALIDATION INQUIRY MID-TIER BANK ONLY !V70
        
        INTEGER*4 OUT_SUBTYPE                     !OUT_SUBTYPE
        INTEGER*4 IN_SUBTYPE                      !IN_SUBTYPE   
        
        INTEGER*4 VAL_INQ_REG                   !New Validation Inquiry Regular
        INTEGER*4 VAL_INQ_MID_TIER              !New Validation Inquiry Mid Tier
        INTEGER*4 VAL_INQ_CSH_ACC               !New Validation Inquiry Cash Accepted
        INTEGER*4 VAL_BNK_TFR_ACC               !New Validation Bank Transfer Accepted
        INTEGER*4 VAL_INQ_MID_TIER_BTO          !New Validation Inquiry Mid Tier (Bank Transfer only)
        LOGICAL   PRIV_TERM                     !Is Privileged Terminal ?
        LOGICAL   LOW_TIER                      !Is Low Tier Prize (<= 150 EUR) ?
        LOGICAL   LOW_TIER_PRIV                 !Is Low Tier Prize for privileged terminal (<= 150 EUR) ? !V20
        LOGICAL   MID_TIER                      !Is Mid Tier Prize (150 EUR < Prize < 5000 EUR) ?
        LOGICAL   MID_TIER_PRIV                 !Is Mid Tier Prize for privileged terminal ( Prize < 5000 EUR) ?
        LOGICAL   HIGH_TIER_PRIV                !Is High Tier Prize for privileged terminal ( Prize >= 5000 EUR) ? !V20
        LOGICAL   HAS_OP                        !Has OP (Payment Order) ?
C----+------------------------------------------------------------------
C V18| New Validation Messages
C----+------------------------------------------------------------------
        INTEGER*4 POL                           !Pool Bet on for games.
        INTEGER*4 AMT                           !Amount Bet.
        INTEGER*4 OPTION                        !Wager Options In/Out
        INTEGER*4 N3POL(10)                     !
        INTEGER*4 N4POL(16)                     !
        INTEGER*4 N3MULT(10)                    !
        INTEGER*4 N4MULT(16)                    !
C
        LOGICAL   BNKINF                        !Banking Info Present Flag.
        LOGICAL   EXPREF/.TRUE./                !Refunds Expanded in Message.
        LOGICAL   RETRY_FLAG                    !To Skip Val Detailes on Retry
C
        BYTE      I1TEMP(4)                     !Temp. Variable.
        INTEGER*4 TEMP                          !Temp. Variable.
        EQUIVALENCE (TEMP,I1TEMP)
C
        DATA N3POL/1,1,1,17,18,18,19,19,16,16/
        DATA N4POL/1,1,1,17,18,18,18,18,19,19,19,19,16,16,16,16/
        DATA N3MULT/1,1,1,1,1,1,1,1,6,3/
        DATA N4MULT/1,1,1,1,1,1,1,1,1,1,1,1,24,12,6,4/
        DATA VALTYP/Z10/
        DATA VALINQ/Z11/
        DATA VALBNK/Z15/
        DATA VALERR/Z1F/
        DATA CONTRL/Z20/
        DATA ERRTYP/Z90/
        
        DATA VAL_VNREG/Z16/
        DATA VAL_VNDON/Z18/
        DATA VAL_VNIBO/Z1A/
        DATA VAL_VNBNK/Z19/
        DATA VAL_VNINQ/Z17/
C----+------------------------------------------------------------------
C V18| New Validation Messages
C----+------------------------------------------------------------------
        DATA VAL_INQ_REG            /Z16/
        DATA VAL_INQ_MID_TIER       /Z17/
        DATA VAL_INQ_CSH_ACC        /Z18/  
        DATA VAL_BNK_TFR_ACC        /Z19/   
        DATA VAL_INQ_MID_TIER_BTO   /Z1A/
 
        LOW_TIER = .FALSE.
        LOW_TIER_PRIV = .FALSE. !V20
        MID_TIER = .FALSE.
        MID_TIER_PRIV = .FALSE.
        HIGH_TIER_PRIV = .FALSE. !V20
        HAS_OP = .FALSE.
        PRIV_TERM = .FALSE.
        IN_SUBTYPE = TRABUF(TVTYPE)
C----+------------------------------------------------------------------
C V18| New Validation Messages
C----+------------------------------------------------------------------
C
        IND = 5
C
        TEMP = CONTRL + TRABUF(TTRN)
        OUTTAB(1) = I1TEMP(1)
C
C GENERAL ERROR
C
        IF(TRABUF(TSTAT).NE.GOOD.AND.TRABUF(TERR).NE.INVL.AND.
     *     TRABUF(TERR).NE.VINQ) THEN
           OUTTAB(2) = ERRTYP
           OUTTAB(5) = TRABUF(TERR)
           OUTTAB(6) = TRABUF(TSUBERR)
           OUTLEN = 6
           GOTO 3000
        ENDIF
C
C CHECK FOR VALIDATION DETAILES REQUEST
C
        IF(TRABUF(TVTYPE).EQ.VDET) THEN
           IF(.NOT.RETRY_FLAG) THEN
             CALL DETREP(TRABUF,VALREC,OUTTAB,OUTLEN)
              IF(TRABUF(TSTAT).NE.REJT) GOTO 3000
           ELSE
              TRABUF(TVCODE) = VNODET
              TRABUF(TSTAT)  = REJT             !Let Validation Error Kick in.
           ENDIF
        ENDIF
C
C VALIDATION ERROR
C
        IF(TRABUF(TSTAT).NE.GOOD.AND.TRABUF(TERR).NE.VINQ) THEN
           OUTTAB(2) = VALERR
C
           IND = 5
           TEMP = TRABUF(TTIM)
           CALL PUTIME(TEMP,OUTTAB,IND)
C
           CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),TEMP,CHECK)
           OUTTAB(IND+0) = I1TEMP(3)
           OUTTAB(IND+1) = I1TEMP(2)
           OUTTAB(IND+2) = I1TEMP(1)
           OUTTAB(IND+3) = CHECK
           IND = IND + 4
C
           OUTTAB(IND+0) = TRABUF(TGAMTYP)
           OUTTAB(IND+1) = TRABUF(TGAMIND)
           IND = IND + 2
C
           OUTTAB(IND+0) = TRABUF(TVCODE)
           IND = IND + 1
C
           OUTLEN = IND - 1
           GOTO 3000
        ENDIF
C
C BUILD GOOD VALIDATION/INQUIRY MESSAGE
C
        BNKINF = .FALSE.
        TEMP = VALTYP
        IF(TRABUF(TERR).EQ.VINQ) TEMP = VALINQ
        IF(TRABUF(TVSTS).EQ.VSBNK.OR.TRABUF(TVSTS).EQ.VSBNKM) THEN
           TEMP = VALBNK
           BNKINF = .TRUE.
        ENDIF
C----+------------------------------------------------------------------
C V18| New Validation Messages
C----+------------------------------------------------------------------
        ! Is privileged terminal ?
        IF(TSBIT(agttab(AGTTYP,trabuf(TTER)),AGTPRV)) THEN
            PRIV_TERM = .TRUE.
        ENDIF
        
        ! Is low-tiered prize (<= 150 EUR) on non-privileged terminal ?
        IF((TRABUF(TVPAY)+TRABUF(TVKPAY)) .LT. P(VALORDER)
     *       .AND. PRIV_TERM .EQ. .FALSE.
     *  ) THEN 
            LOW_TIER = .TRUE.
        ENDIF
        ! Is low-tiered prize (<= 150 EUR) on non-privileged terminal ? !V20
        IF((TRABUF(TVPAY)+TRABUF(TVKPAY)) .LT. P(VALORDER)
     *       .AND. PRIV_TERM .EQ. .TRUE.
     *  ) THEN 
            LOW_TIER_PRIV = .TRUE.
        ENDIF
        ! Is mid-tiered prize (150 EUR < prize < 5000 EUR) on non-privileged terminal ?
C       ELSE 
        IF(  (TRABUF(TVPAY)+TRABUF(TVKPAY)) .GE. P(VALORDER)
     *       .AND. (TRABUF(TVPAY)+TRABUF(TVKPAY)) .LT. P(VALPRZHI) 
C     *       .AND. PRIV_TERM .EQ. .FALSE.
     *  ) THEN
            MID_TIER = .TRUE.
        ENDIF
        ! Is mid-tiered prize ( prize < 5000 EUR) on privileged terminal ?
C        ELSE 
        IF(  (TRABUF(TVPAY)+TRABUF(TVKPAY)) .LT. P(VALPRZHI) 
     *       .AND.PRIV_TERM .EQ. .TRUE. ) THEN
            MID_TIER_PRIV = .TRUE.
        ENDIF
        ! Is high-tiered prize ( prize >= 5000 EUR) on privileged terminal ? !V20
C        ELSE 
        IF(  (TRABUF(TVPAY)+TRABUF(TVKPAY)) .GE. P(VALPRZHI) 
     *       .AND.PRIV_TERM .EQ. .TRUE. ) THEN
            HIGH_TIER_PRIV = .TRUE.
        ENDIF
        
        
        ! Has OP (Payment Order) ?
        IF(VALREC(VOPSCNT) .GT. 0) THEN
            HAS_OP = .TRUE.
        ENDIF
        
        ! Handles only new messages
        IF(HAS_OP .EQ. .FALSE.) THEN
            ! Handling non privileged terminals
            IF(PRIV_TERM .EQ. .FALSE.) THEN
                ! For prizes below EUR 150 
                IF(LOW_TIER .EQ. .TRUE.) THEN
                    ! Regular inquiry: phase 1
                    IF(TRABUF(TVTYPE) .EQ. VREG) THEN
                            TEMP = VAL_VNREG
                    ! Regular inquiry: phase 2
                    ELSE IF(TRABUF(TVTYPE) .EQ. VNDON) THEN
                        TEMP = VAL_VNDON
                    ENDIF
                ! For prizes EUR 150 < prize <= EUR 5000 
                ELSE IF(MID_TIER .EQ. .TRUE.) THEN
                    ! Mid-tier inquiry: phase 1
                    IF(TRABUF(TVTYPE) .EQ. VREG) THEN
                        TEMP = VAL_VNIBO
                    ! Mid-tier inquiry: phase 2
                    ELSE IF(TRABUF(TVTYPE) .EQ. VNBNK) THEN
                        TEMP = VAL_VNBNK
                    ENDIF
                ENDIF
            ! Handling privileged terminals    
            ELSE IF(MID_TIER_PRIV .EQ. .TRUE.) THEN
                ! Mid-tier inquiry: phase 1
                IF(TRABUF(TVTYPE) .EQ. VREG) THEN
                    TEMP = VAL_VNINQ
                ! Cash payment
                ELSE IF(TRABUF(TVTYPE) .EQ. VNDON) THEN
                    TEMP = VAL_VNDON
                ! Bank transfer payment
                ELSE IF(TRABUF(TVTYPE) .EQ. VNBNK) THEN
                    TEMP = VAL_VNBNK
                ENDIF
            ! Handling privileged terminals (high tier prizes) !V20
            ELSE IF(HIGH_TIER_PRIV .EQ. .TRUE.) THEN
                    ! Regular inquiry: phase 1
                    IF(TRABUF(TVTYPE) .EQ. VREG) THEN
                            TEMP = VAL_VNREG
                    ! Regular inquiry: phase 2
                    ELSE IF(TRABUF(TVTYPE) .EQ. VNDON) THEN
                        TEMP = VAL_VNDON
                    ENDIF
C                ! Mid-tier inquiry: phase 1
C                IF(TRABUF(TVTYPE) .EQ. VREG) THEN
C                    TEMP = VAL_VNINQ
C                ! Cash payment
C                ELSE IF(TRABUF(TVTYPE) .EQ. VNDON) THEN
C                    TEMP = VAL_VNDON
C                ! Bank transfer payment
C                ELSE IF(TRABUF(TVTYPE) .EQ. VNBNK) THEN
C                    TEMP = VAL_VNBNK
C                ENDIF
            ENDIF
        ENDIF
        OUT_SUBTYPE = TEMP
        
C        CALL OPS('VALREC(VOPSCNT)',VALREC(VOPSCNT),VALREC(VOPSCNT))
C        CALL OPS('OUT_SUBTYPE',OUT_SUBTYPE,OUT_SUBTYPE)
C        CALL OPS('HAS_OP',HAS_OP,HAS_OP)
C        CALL OPS('PRIV_TERM',PRIV_TERM,PRIV_TERM)
C        CALL OPS('LOW_TIER',LOW_TIER,LOW_TIER)
C        CALL OPS('TRABUF(TVTYPE)',TRABUF(TVTYPE),TRABUF(TVTYPE))
C        CALL OPS('MID_TIER',MID_TIER,MID_TIER)
C        CALL OPS('MID_TIER_PRIV',MID_TIER_PRIV,MID_TIER_PRIV)
C        CALL OPS('HIGH_TIER_PRIV',HIGH_TIER_PRIV,HIGH_TIER_PRIV)
C        CALL OPS('TRABUF(TVPAY)+TRABUF(TVKPAY)',TRABUF(TVPAY)+TRABUF(TVKPAY),TRABUF(TVPAY)+TRABUF(TVKPAY))
C        CALL OPS('P(VALORDER)',P(VALORDER),P(VALORDER))
C        CALL OPS('P(VALPRZHI)',P(VALPRZHI),P(VALPRZHI))
C----+------------------------------------------------------------------
C V18| New Validation Messages
C----+------------------------------------------------------------------        
        OUTTAB(2) = TEMP
C
        TEMP=TRABUF(TTIM)
        CALL PUTIME(TEMP,OUTTAB,IND)
C
        CALL OUTGEN(TRABUF(TCDC),TRABUF(TSER),TEMP,CHECK)
        OUTTAB(IND+0) = I1TEMP(3)
        OUTTAB(IND+1) = I1TEMP(2)
        OUTTAB(IND+2) = I1TEMP(1)
        OUTTAB(IND+3) = CHECK
        IND = IND + 4
C
        OUTTAB(IND+0) = TRABUF(TGAMTYP)
        OUTTAB(IND+1) = TRABUF(TGAMIND)
        IND = IND + 2
C
        TEMP=10
        IF(TRABUF(TVSTS).EQ.VCASHX.OR.TRABUF(TVSTS).EQ.VCLAMX) TEMP=11
        IF(BNKINF) TEMP = 12
        OUTTAB(IND+0) = TEMP
        IND = IND + 1
C
C       REFAMT=TRABUF(TVREF)                     !V16
	OPSAMT=TRABUF(TVOPPAY)+TRABUF(TVKOPPAY)  !V16
        PAYAMT=0
C       CLMAMT=0
C       IF(TRABUF(TTYP).EQ.TCLM) CLMAMT=TRABUF(TVPAY)+TRABUF(TVKPAY) 
        IF(TRABUF(TTYP).EQ.TVAL) PAYAMT=TRABUF(TVPAY)+TRABUF(TVKPAY)
C
C***    TEMP=CLMAMT
C***    OUTTAB(IND+0) = I1TEMP(4)
C***    OUTTAB(IND+1) = I1TEMP(3)
C***    OUTTAB(IND+2) = I1TEMP(2)
C***    OUTTAB(IND+3) = I1TEMP(1)
C***    IND = IND + 4
C
        IF(EXPREF) THEN
           TEMP = PAYAMT              
           OUTTAB(IND+0) = I1TEMP(4)
           OUTTAB(IND+1) = I1TEMP(3)
           OUTTAB(IND+2) = I1TEMP(2)
           OUTTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
C
C          TEMP = REFAMT               !V16
C----+------------------------------------------------------------------
C V18| New Validation Messages
C----+------------------------------------------------------------------
           TEMP = OPSAMT               !V16
C----+------------------------------------------------------------------
C V18| New Validation Messages: clearing refund amount if in_subtype is 
C    | regular
C----+------------------------------------------------------------------
           IF(  (OUT_SUBTYPE .EQ. VAL_VNDON 
     *     .OR.  OUT_SUBTYPE .EQ. VAL_VNREG
     *     .OR.  OUT_SUBTYPE .EQ. VAL_VNBNK
     *     .OR.  OUT_SUBTYPE .EQ. VAL_VNIBO) 
     *     .AND. IN_SUBTYPE .EQ. VREG) THEN
              IF(HIGH_TIER_PRIV .NE. .TRUE.) THEN
                 TEMP = 0
              ENDIF
           ENDIF

C----+------------------------------------------------------------------
C V18| New Validation Messages: clearing refund amount if in_subtype is 
C    | mid-tier and prize is low tier
C----+------------------------------------------------------------------
           IF(   IN_SUBTYPE    .EQ. VMID
     *     .AND.(LOW_TIER      .EQ. .TRUE.
     *     .OR.  LOW_TIER_PRIV .EQ. .TRUE.)) THEN !V20
              TEMP = 0
           ENDIF

C----+------------------------------------------------------------------
C V18| New Validation Messages: clearing refund amount if in_subtype is  
C    | NEW VALIDATION INQUIRY CASH ACCEPTED or NEW VALIDATION TO BANK
C----+------------------------------------------------------------------
           IF(  (OUT_SUBTYPE .EQ. VAL_VNDON 
     *     .OR.  OUT_SUBTYPE .EQ. VAL_VNBNK
     *     .OR.  OUT_SUBTYPE .EQ. VAL_VNIBO) 
     *     .AND.(TRABUF(TVPAY)+TRABUF(TVKPAY)) .LE. P(VALPRZHI)
     *     .AND. (IN_SUBTYPE .EQ. VNDON
     *     .OR.   IN_SUBTYPE .EQ. VNBNK)
C     *     .AND. PRIV_TERM .EQ. .FALSE. ! doing it for all terminals
     *     ) THEN
              TEMP = 0
           ENDIF
C           CALL OPS('TEMP',TEMP,TEMP) !V19
C----+------------------------------------------------------------------
C V18| New Validation Messages
C----+------------------------------------------------------------------
           OUTTAB(IND+0) = I1TEMP(4)
           OUTTAB(IND+1) = I1TEMP(3)
           OUTTAB(IND+2) = I1TEMP(2)
           OUTTAB(IND+3) = I1TEMP(1)
           IND =IND + 4
        ELSE
C          TEMP = PAYAMT + REFAMT               !Add up if not expanded.      !V16
           TEMP = PAYAMT                                                      !V16
           OUTTAB(IND+0) = I1TEMP(4)
           OUTTAB(IND+1) = I1TEMP(3)
           OUTTAB(IND+2) = I1TEMP(2)
           OUTTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
        ENDIF
C
C***    TEMP = TRABUF(TVWCDC)
C***    OUTTAB(IND+0) = I1TEMP(2)
C***    OUTTAB(IND+1) = I1TEMP(1)
C***    IND = IND + 2
C
        IF(BNKINF) THEN                         
           TEMP = TRABUF(TVBNKID)
           OUTTAB(IND+0) = I1TEMP(4)
           OUTTAB(IND+1) = I1TEMP(3)
           OUTTAB(IND+2) = I1TEMP(2)
           OUTTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
C
           TEMP = TRABUF(TVBNKNUM)
           OUTTAB(IND+0) = I1TEMP(4)
           OUTTAB(IND+1) = I1TEMP(3)
           OUTTAB(IND+2) = I1TEMP(2)
           OUTTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
        ENDIF
C          
        IF(TRABUF(TVSTS).NE.VCASHX.AND.TRABUF(TVSTS).NE.VCLAMX) THEN
           OUTLEN = IND - 1
           GOTO 3000
        ENDIF
C
C EXCHANGE TICKET
C
C
        OUTTAB(IND+0) = WRKBUF(TGAMTYP)
        OUTTAB(IND+1) = WRKBUF(TGAMIND)
        IND=IND+2

C
        CALL OUTGEN(WRKBUF(TCDC),WRKBUF(TSER),TEMP,CHECK)
        OUTTAB(IND+0) = I1TEMP(3)
        OUTTAB(IND+1) = I1TEMP(2)
        OUTTAB(IND+2) = I1TEMP(1)
        OUTTAB(IND+3) = CHECK
        IND = IND + 4
C
        CALL KOFFGET(WRKBUF,KOFFBEG,KOFFEND)
        CALL GETOFF(WRKBUF,OFFBEG,OFFEND)
        TEMP = OFFBEG
        OUTTAB(IND+0) = I1TEMP(2)
        OUTTAB(IND+1) = I1TEMP(1)
        IND = IND + 2
C
        TEMP = OFFEND
        OUTTAB(IND+0) = I1TEMP(2)
        OUTTAB(IND+1) = I1TEMP(1)
        IND = IND + 2
C
C SET OFFSETS TO FIRST - SECOND WEEK / YEAR DATES
C
        CALL PUT_WEEK_YEAR_DRAWS(WRKBUF, OUTTAB, IND)
C
        CALL PUTIME(WRKBUF(TTIM),OUTTAB,IND)
C
C GET OUTPUT MESSAGE OPTIONS
C IF KICKER AND GAME OFFSETS ARE THE SAME, DO NOT SEND KICKER OFFSETS, ALSO
C ADJUST THE OPTION FLAGS.
C
        CALL OGETOPT(WRKBUF,OPTION)
C
        IF(OFFBEG.EQ.KOFFBEG.AND.OFFEND.EQ.KOFFEND)   !Additional Kicker offsets
     *     OPTION = IAND(OPTION,'7F'X)                !are not needed.
 
        OUTTAB(IND+0) = OPTION
        IND = IND + 1
C
        IF(IAND(OPTION,'80'X).NE.0) THEN        !KICKER OFFSETS     
           TEMP = KOFFBEG
           OUTTAB(IND+0) = I1TEMP(1)

           TEMP = KOFFEND
           OUTTAB(IND+1) = I1TEMP(1)
C
           OUTTAB(IND+2) = WRKBUF(TWKDUR)
           IND = IND + 3
        ENDIF
C
        IF(IAND(OPTION,'40'X).NE.0) THEN       !KICKER 1 NUMBER
              TEMP = WRKBUF(TWKICK)
              OUTTAB(IND+0) = I1TEMP(4)
              OUTTAB(IND+1) = I1TEMP(3)
              OUTTAB(IND+2) = I1TEMP(2)
              OUTTAB(IND+3) = I1TEMP(1)
              IND = IND + 4
        ENDIF
C
        IF(IAND(OPTION,'20'X).NE.0) THEN       !KICKER 2 NUMBER
              TEMP = WRKBUF(TWKICK2)
              OUTTAB(IND+0) = I1TEMP(4)
              OUTTAB(IND+1) = I1TEMP(3)
              OUTTAB(IND+2) = I1TEMP(2)
              OUTTAB(IND+3) = I1TEMP(1)
              IND=IND+4
        ENDIF
C
        IF(IAND(OPTION,'10'X).NE.0) THEN
            OUTTAB(IND+0) = MAXFRC(WRKBUF(TGAM))/WRKBUF(TFRAC) !# FRACTIONS
            IND = IND + 1
        ENDIF
C
        TEMP = ISHFT(WRKBUF(TGAMIND),4)         !Game Index / System Type
        TEMP = TEMP + WRKBUF(TWSYST)    
        OUTTAB(IND+0) = TEMP
        IND = IND + 1
C
        TEMP = ISHFT(WRKBUF(TWDUR),4) + WRKBUF(TWNBET)
        OUTTAB(IND+0) = TEMP
        IND = IND + 1
C
        CALL IGETOPT(WRKBUF,OPTION)             !Input Message Options.
        IF(WRKBUF(TGAMTYP).EQ.TKIK) THEN
           IF(WRKBUF(TWKFLG) .NE.0) OPTION=OPTION+'0080'X
           IF(WRKBUF(TWKFLG2).NE.0) OPTION=OPTION+'0040'X
        ENDIF
        TEMP = OPTION
        OUTTAB(IND+0) = I1TEMP(2)
        OUTTAB(IND+1) = I1TEMP(1)
        IND = IND + 2
C
        IF(IAND(OPTION,'0200'X).NE.0) THEN      !Quick Pick Flags.
           TEMP = WRKBUF(TWQPF)
           OUTTAB(IND+0) = I1TEMP(2)
           OUTTAB(IND+1) = I1TEMP(1)
           IND = IND + 2
        ENDIF
C
        IF(IAND(OPTION,'0100'X).NE.0) THEN      !System #
           TEMP = WRKBUF(TWSYSN)
           OUTTAB(IND+0) = I1TEMP(2)
           OUTTAB(IND+1) = I1TEMP(1)
           IND = IND + 2
        ENDIF
C
        IF(IAND(OPTION,'0008'X).NE.0) THEN
           TEMP = WRKBUF(TWBNKID)
           OUTTAB(IND+0) = I1TEMP(4)
           OUTTAB(IND+1) = I1TEMP(3)
           OUTTAB(IND+2) = I1TEMP(2)
           OUTTAB(IND+3) = I1TEMP(1)
           IND = IND + 4

          TEMP = WRKBUF(TWBNKNM)
          OUTTAB(IND+0) = I1TEMP(4)
          OUTTAB(IND+1) = I1TEMP(3)
          OUTTAB(IND+2) = I1TEMP(2)
          OUTTAB(IND+3) = I1TEMP(1)
          IND = IND + 4
        ENDIF

        IF(IAND(OPTION,'0001'X).NE.0) THEN     !Sports WQp table revision #
          IND=IND+4
        ENDIF
C
C SET MONDAY FLAG INDICATOR ( ONLY FOR LOTTO )
C
        IF(WRKBUF(TGAMTYP) .EQ. TLTO) THEN
          TEMP = WRKBUF(TWLMFI)
          OUTTAB(IND) = I1TEMP(1)
          IND = IND + 1
        ENDIF
C
C LOTTO/SPORTS BET DATA
C
        IF(WRKBUF(TGAMTYP).EQ.TLTO.OR.WRKBUF(TGAMTYP).EQ.TSPT.OR.
     *     WRKBUF(TGAMTYP).EQ.TTGL) THEN
           DO I = TWBORD,TWBEND
              IF(WRKBUF(I).EQ.0) GOTO 100
              CALL MOVBYT(WRKBUF(I),1,OUTTAB,IND,4)
              IND = IND + 4
           END DO
100        CONTINUE
           OUTLEN = IND - 1
           GOTO 3000
        ENDIF
C
C NUMBERS BET DATA
C
        IF(WRKBUF(TGAMTYP).EQ.TNBR) THEN
          DO 200 I=0,WRKBUF(TWNBET)-1
          POL=WRKBUF(TWNPOL1+I*3)
          AMT=WRKBUF(TWNAMT1+I*3)/NBRPRC(WRKBUF(TGAMIND))
          IF(WRKBUF(TWNTYP).EQ.NB3TYP) THEN
            AMT=AMT/N3MULT(POL)
            TEMP=N3POL(POL)
          ELSE
            AMT=AMT/N4MULT(POL)
            TEMP=N4POL(POL)
          ENDIF
          OUTTAB(IND)=TEMP
          OUTTAB(IND+1)=AMT
          IND=IND+2
          CALL NUMBCD(WRKBUF(TWNTYP),POL,WRKBUF(TWNNUM1+I*3),TEMP)
          OUTTAB(IND+0)=I1TEMP(2)
          OUTTAB(IND+1)=I1TEMP(1)
          IND=IND+2
200       CONTINUE
          OUTLEN=IND-1
          GOTO 3000
        ENDIF
C
C KICKER BET DETAILS (THERE ARE NONE, KICKER #'S ARE STORED IN THE HEADER)
C
        IF(WRKBUF(TGAMTYP).EQ.TKIK) THEN
           OUTLEN = IND - 1
           GOTO 3000
        ENDIF
C
C CALCULATE CHECKSUM
C
3000    CONTINUE
        I4CCITT   = TRABUF(TCHK)
        OUTTAB(3) = I1CCITT(2)
        OUTTAB(4) = I1CCITT(1)
        CHKLEN = OUTLEN - 1
        CALL GETCCITT(OUTTAB,1,CHKLEN,MYCHKSUM)
        I4CCITT   = MYCHKSUM
        OUTTAB(3) = I1CCITT(2)
        OUTTAB(4) = I1CCITT(1)
        RETURN
        END
