C  GXSRC:DCVAL.FOR
C
C V15 18-MAR-2014 SCML Batch Validation: error code 104 removed.
C V14 16-JAN-2014 SCML Batch Validation: for validation mode = Pay (0) return
C                      error code 104 (Failed Batch Validation) if and only if all
C                      the transactions in the batch failed.
C                      BVFAILED removed.
C                      CNPB_FOUND added.
C V13 14-JAN-2014 SCML Envelope Validation fix.
C V12 30-DEC-2013 SCML Added IINQR error code.
C                      Added BVFAILED flag.
C                      Update of TRABUF(TIERR) changed.
C V11 13-DEC-2013 SCML Batch Validation logic updated.
C                      Added INFBV error code.
C                      Update of TRABUF(TIERR) changed.
C V10 21-OCT-2013 SCML New bank validation mode.
C V09 19-FEB-2001 UXN Portugal changes.
C V08 05-OCT-2000 UXN AlphaIPS release.
C V07 23-JUN-2000 JNS IPS BATCH 15 CASH FOR LIFE CHANGE - 
C                     VAL CODE 16 SHOULD RETURN TIERR = 0 (NOT 16)
C V06 21-MAR-1997 DXA ADDED PLAY-AT-HOME IPS RESULT CODE 38 (GAME OVER)
C V05 14-JAN-1997 DXA ADDED ACTIVATION CUTOFF (IPS RESULT CODES 36 & 37)
C V04 29-NOV-1996 DXA PLAY AT HOME SHOW DATE & NUMBER ADDED
C V03 30-Dec-1992 ceb Removed TIATH as it is no longer needed.
C                     RFSS TX1100-59
C V02 11-FEB-1992 JPJ ADDED (GVT)
C V01 13-NOV-1991 JPJ RELEASED FOR VAX (INSTANTS)
C
C SUBROUTINE TO DECODE CROSS SYSTEM VALIDATION MESSAGE.
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
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE DCVAL(TRABUF,OUTTAB)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:NON_CASH_PRIZE.DEF'
C
        INTEGER*4 IND, LENGTH, I
        INTEGER*4 VALMTYP
        BYTE      OUTTAB(*)
C
        INTEGER*4 I4TEMP
        INTEGER*2 I2TEMP(2)
        BYTE	    I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
        LOGICAL   CNPB_FOUND             !CAN NOT PAY IN BATCH STATUS FOUND FLAG !V14
C
        INTEGER*4 ICNPB                  !V12
        PARAMETER (ICNPB=103)            !CAN NOT PAY IN BATCH
C
        INTEGER*4 INFBV                  !V11
        PARAMETER  (INFBV=104)           !FAILED BATCH VALIDATION
C
        INTEGER*4 IINQR                  !V12
        PARAMETER  (IINQR=89)            !SUCCESSFUL INQUIRY
C
C GET LENGTH (BYTES 2-3)
C
        IND=2
        I4TEMP=0
        I1TEMP(1)=OUTTAB(IND+0)
        I1TEMP(2)=OUTTAB(IND+1)
        LENGTH=I4TEMP
C
C GET THE VALIDATION TYPE (BYTES 16-17)
C
!        IND=14
        IND=16 !V10
        I4TEMP=0
        I1TEMP(1)=OUTTAB(IND+0)
        I1TEMP(2)=OUTTAB(IND+1)
        TRABUF(TIVTYP)=I4TEMP
        IND=IND+2

C-------->>V11 ---------------------------------------------------------
!C
!C CHECK ERROR CODE (BYTES 18-19)
!C
!        IND=16
!        IND=18 !V10
!        I4TEMP=0
!        I1TEMP(1)=OUTTAB(IND+0)
!        I1TEMP(2)=OUTTAB(IND+1)
!        TRABUF(TIERR)=I4TEMP
!C
!C ERRORS LISTED WILL BE BY VALIDATION
!C
!        IF(TRABUF(TIERR).EQ.34.OR.TRABUF(TIERR).EQ.52.OR.
!     *    TRABUF(TIERR).EQ.95.OR.TRABUF(TIERR).EQ.97 ) THEN
!!                Invalid Pack Status           34
!!                Invalid Game Number           52
!!                Host Error 1                  95
!!                Record Lock                   97
!          TRABUF(TIERR)=INOER
!        ELSEIF(TRABUF(TIERR).GE.01.AND.TRABUF(TIERR).LE.11) THEN
!!                Agent exceeds redeem max      01
!!                Pack not Active               02
!!                Invalid Prize Amount          03
!!                Virn Unscramble Failed        04
!!                Previously Paid               05
!!                Game Pack Ticket Not Found    06
!!                Pack Not Retailer Status      07
!!                Game Not Active               08
!!                Previously Paid By You        09
!!                Previously Paid By Other      10
!!                Not a Winner                  11
!          TRABUF(TIERR)=INOER
!        ELSEIF(TRABUF(TIERR).EQ.36.OR.TRABUF(TIERR).EQ.38) THEN
!!                Game Closed,File Clm@Lott Off 36
!!                Game Closed                   38
!          TRABUF(TIERR)=INOER
!        ELSEIF(TRABUF(TIERR).GE.57.AND.TRABUF(TIERR).LE.58) THEN ! P-A-H
!!                Not a home play winner        57
!!                Studio play winner            58
!          TRABUF(TIERR)=INOER
!        ELSEIF(TRABUF(TIERR).GE.62.AND.TRABUF(TIERR).LE.63) THEN ! P-A-H
!!                Results not in yet            62
!!                Not a winner yet              63
!          TRABUF(TIERR)=INOER
!        ELSEIF(TRABUF(TIERR).EQ.103) THEN
!!                Cannot pay in batch          103
!          TRABUF(TIERR)=INOER
!        ENDIF
!        IF(TRABUF(TIERR).GT.INOER) GOTO 8000
C-------- V11<<---------------------------------------------------------
C
        IF(TRABUF(TIVMT) .EQ. IRVMT) THEN ! OLD VALIDATION MODE
          IF(TRABUF(TIVTYP).EQ.IVTP_NCP) THEN
C
C-------->>V11 ---------------------------------------------------------
            IND=18
            I4TEMP=0
            I1TEMP(1)=OUTTAB(IND+0)
            I1TEMP(2)=OUTTAB(IND+1)
            TRABUF(TIERR)=I4TEMP
            CALL UPDIERR(TRABUF(TIERR))
            IF (TRABUF(TIERR).GT.INOER) GOTO 8000
C-------- V11<<---------------------------------------------------------
C
C MESSAGE IS FOR LOW-TIER NON-CASH PRIZE VALIDATIONS
C
            IF(TRABUF(TIBCH).NE.1) THEN
C
C ONLY ONE TICKET IN BATCH ALLOWED
C
              TRABUF(TSTAT)=REJT
              TRABUF(TERR) =SYNT
              SYNTERRCOD   = 86
            ELSEIF(LENGTH.NE.10+TRABUF(TIBCH)*30) THEN
C
C MESSAGE LENGTH MUST BE CORRECT
C
              TRABUF(TSTAT)=REJT
              TRABUF(TIERR)=INLTH
            ELSE
C
C GET TICKET RESULT CODE (BYTES 18-19)
C
              I4TEMP=0
              I1TEMP(1)=OUTTAB(IND+0)
              I1TEMP(2)=OUTTAB(IND+1)
              TRABUF(TISTS1)=I4TEMP
              IND=IND+2
C
C GET PACK STATUS FOR PRIV TERMINAL (BYTES 20-21)
C
              I4TEMP = 0
              I1TEMP(1) = OUTTAB(IND+0)
              I1TEMP(2) = OUTTAB(IND+1)
              TRABUF(TIPCKSTS1) = I4TEMP
              IND=IND+2
C
C PRIZE DESCRIPTION (BYTES 22-51)
C
              CALL MOVBYT(OUTTAB,IND,TRABUF(TIVDESCR),1,20)
              IND=IND+20
            ENDIF
          ELSE
C
C MESSAGE IS FOR "NORMAL" VALIDATIONS
C
            IF(LENGTH.NE.16+TRABUF(TIBCH)*8) THEN
C
C MESSAGE LENGTH MUST BE CORRECT
C
              TRABUF(TSTAT)=REJT
              TRABUF(TIERR)=INLTH
            ELSE
C
C-------->>V13 ---------------------------------------------------------
              IF(TRABUF(TIBCH).EQ.1) THEN
                IND=18
                I4TEMP=0
                I1TEMP(1)=OUTTAB(IND+0)
                I1TEMP(2)=OUTTAB(IND+1)
                IND=IND+2
                TRABUF(TIERR)=I4TEMP
                CALL UPDIERR(TRABUF(TIERR))
                IF (TRABUF(TIERR).GT.INOER) GOTO 8000
C
C GET TICKET RESULT CODE
C
                TRABUF(TISTS1)=I4TEMP
C
C GET PRIZE AMOUNT
C
                I4TEMP=0
                I1TEMP(1)=OUTTAB(IND+0)
                I1TEMP(2)=OUTTAB(IND+1)
                I1TEMP(3)=OUTTAB(IND+2)
                I1TEMP(4)=OUTTAB(IND+3)
                TRABUF(TIPRZ1)=I4TEMP
                IND=IND+4
C
C GET PACK STATUS FOR PRIV TERMINAL
C
                I4TEMP = 0
                I1TEMP(1) = OUTTAB(IND+0)
                I1TEMP(2) = OUTTAB(IND+1)
                TRABUF(TIPCKSTS1) = I4TEMP
	              IND=IND+2
C-------- V13<<---------------------------------------------------------
              ELSE !MORE THAN ONE TICKET IN BATCH
C-------->>V15 ---------------------------------------------------------
!               CHECK TICKET RESULT CODE OF FIRST TICKET:
!                  IF GREATER THAN 99 THEN GOTO 8000
!                    100 - Could no send
!                    101 - Time out
!                    102 - Bad length
                IND=18
                I4TEMP=0
                I1TEMP(1)=OUTTAB(IND+0)
                I1TEMP(2)=OUTTAB(IND+1)
                TRABUF(TIERR)=I4TEMP
                CALL UPDIERR(TRABUF(TIERR))
                IF (TRABUF(TIERR).GT.INOER) GOTO 8000
C-------- V15<<---------------------------------------------------------
                IF(TRABUF(TIVALM).EQ.1) THEN !VALIDATION MODE = INQUIRY !V14
C-------->>V15 ---------------------------------------------------------
!                  CNPB_FOUND = .FALSE.       !CANNOT PAY IN BATCH TICKET STATUS FLAG
!                  TRABUF(TIERR)=INFBV        !INIT TIERR WITH FAILED BATCH VALIDATION
                  TRABUF(TIERR)=INOER         !INIT TIERR WITH NO ERROR
C-------- V15<<---------------------------------------------------------
                  IND=18
                  DO I=0,TRABUF(TIBCH)-1
C
C GET TICKET RESULT CODE
C
                    I4TEMP=0
                    I1TEMP(1)=OUTTAB(IND+0)
                    I1TEMP(2)=OUTTAB(IND+1)
                    TRABUF(TISTS1+I)=I4TEMP
                    IND=IND+2
C
C-------->>V15 ---------------------------------------------------------
!                    IF(.NOT. CNPB_FOUND .AND. TRABUF(TISTS1+I).EQ.IINQR) THEN
!                      TRABUF(TIERR)=IINQR !PRIZES GREATER THAN € 150,00 CAN NOT BE PAID IN BATCH. THEY MUST BE PAID INDIVIDUALLY.
!                    ELSEIF(TRABUF(TISTS1+I).EQ.ICNPB) THEN
!                      CNPB_FOUND = .TRUE. !PRIZES GREATER THAN € 150,00 CAN NOT BE PAID IN BATCH. THEY MUST BE PAID INDIVIDUALLY.
!                      TRABUF(TIERR)=INFBV
!                    ENDIF
                    IF(TRABUF(TISTS1+I).EQ.IINQR) TRABUF(TIERR)=IINQR
C-------- V15<<---------------------------------------------------------
C
C GET PRIZE AMOUNT
C
                    I4TEMP=0
                    I1TEMP(1)=OUTTAB(IND+0)
                    I1TEMP(2)=OUTTAB(IND+1)
                    I1TEMP(3)=OUTTAB(IND+2)
                    I1TEMP(4)=OUTTAB(IND+3)
                    TRABUF(TIPRZ1+I)=I4TEMP
                    IND=IND+4
C
C GET PACK STATUS FOR PRIV TERMINAL
C
                    I4TEMP = 0
                    I1TEMP(1) = OUTTAB(IND+0)
                    I1TEMP(2) = OUTTAB(IND+1)
                    TRABUF(TIPCKSTS1+I) = I4TEMP
	                  IND=IND+2
                  ENDDO
C
                ELSEIF(TRABUF(TIVALM).EQ.0) THEN !VALIDATION MODE = PAY !V14
C-------->>V15 ---------------------------------------------------------
!                  TRABUF(TIERR)=INFBV            !INIT TIERR WITH FAILED BATCH VALIDATION
                  TRABUF(TIERR)=INOER            !INIT TIERR WITH NO ERROR
C-------- V15<<---------------------------------------------------------
                  IND=18
                  DO I=0,TRABUF(TIBCH)-1
C
C GET TICKET RESULT CODE
C
                    I4TEMP=0
                    I1TEMP(1)=OUTTAB(IND+0)
                    I1TEMP(2)=OUTTAB(IND+1)
                    TRABUF(TISTS1+I)=I4TEMP
                    IND=IND+2
C
C-------->>V15 ---------------------------------------------------------
!                    IF(TRABUF(TISTS1+I).EQ.INOER) TRABUF(TIERR)=INOER
C-------- V15<<---------------------------------------------------------
C
C GET PRIZE AMOUNT
C
                    I4TEMP=0
                    I1TEMP(1)=OUTTAB(IND+0)
                    I1TEMP(2)=OUTTAB(IND+1)
                    I1TEMP(3)=OUTTAB(IND+2)
                    I1TEMP(4)=OUTTAB(IND+3)
                    TRABUF(TIPRZ1+I)=I4TEMP
                    IND=IND+4
C
C GET PACK STATUS FOR PRIV TERMINAL
C
                    I4TEMP = 0
                    I1TEMP(1) = OUTTAB(IND+0)
                    I1TEMP(2) = OUTTAB(IND+1)
                    TRABUF(TIPCKSTS1+I) = I4TEMP
	                  IND=IND+2
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ELSEIF(TRABUF(TIVMT) .EQ. IBVMT) THEN
C
C         NEW BANK VALIDATION MODE !V10
C
C-------->>V11 ---------------------------------------------------------
          IND=18
          I4TEMP=0
          I1TEMP(1)=OUTTAB(IND+0)
          I1TEMP(2)=OUTTAB(IND+1)
          TRABUF(TIERR)=I4TEMP
          CALL UPDIERR(TRABUF(TIERR))
          IF (TRABUF(TIERR).GT.INOER) GOTO 8000
C-------- V11<<---------------------------------------------------------
C
          IF(TRABUF(TIBCH).NE.1) THEN
C
C ONLY ONE TICKET IN BATCH ALLOWED FOR NEW BANK VALIDATION MODE
C
            TRABUF(TSTAT)=REJT
            TRABUF(TERR) =SYNT
            SYNTERRCOD   = 86
          ELSEIF(LENGTH.NE.10+TRABUF(TIBCH)*38) THEN
C
C MESSAGE LENGTH MUST BE CORRECT (48 = 10 (HEADER SIZE) + 1 (TICKET) * 38 (BODY SIZE))
C
              TRABUF(TSTAT)=REJT
              TRABUF(TIERR)=INLTH
          ELSE
C
C GET TICKET RESULT CODE (BYTES 20-21)
C
            I4TEMP=0
            I1TEMP(1)=OUTTAB(IND+0)
            I1TEMP(2)=OUTTAB(IND+1)
            TRABUF(TISTS1)=I4TEMP
            IND=IND+2
C
C GET PRIZE AMOUNT (BYTES 22-25)
C
            I4TEMP=0
            I1TEMP(1)=OUTTAB(IND+0)
            I1TEMP(2)=OUTTAB(IND+1)
            I1TEMP(3)=OUTTAB(IND+2)
            I1TEMP(4)=OUTTAB(IND+3)
            TRABUF(TIPRZ1)=I4TEMP
            IND=IND+4
C
C GET NET PRIZE AMOUNT (BYTES 26-29)
C
            I4TEMP=0
            I1TEMP(1)=OUTTAB(IND+0)
            I1TEMP(2)=OUTTAB(IND+1)
            I1TEMP(3)=OUTTAB(IND+2)
            I1TEMP(4)=OUTTAB(IND+3)
            TRABUF(TINETPRZ)=I4TEMP
            IND=IND+4
C
C GET PACK STATUS FOR PRIV TERMINAL (BYTES 30-31)
C
            I4TEMP=0
            I1TEMP(1)=OUTTAB(IND+0)
            I1TEMP(2)=OUTTAB(IND+1)
            TRABUF(TIPCKSTS1)=I4TEMP
	          IND=IND+2
C
C PRIZE DESCRIPTION (BYTES 32-51)
C
            CALL MOVBYT(OUTTAB,IND,TRABUF(TIVDESCR),1,20)
            IND=IND+20
          ENDIF
        ENDIF
C
8000	 CONTINUE
       RETURN
       END

C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SUBROUTINE UPDIERR(TCKRC)
C
C       UPDATES THE TICKET RESULT CODE FROM IPS (since version V11)
C
C       INPUTS:
C        TCKRC           TICKET RESULT CODE FROM IPS
C
C       OUTPUTS:
C        TCKRC           TICKET RESULT CODE UPDATED
C
C=======================================================================
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE UPDIERR(TCKRC)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
        INTEGER*4 TCKRC
C
        INTEGER*4 ICNPB
        PARAMETER (ICNPB=103)   !CANNOT PAY IN BATCH
C
       IF(TCKRC.EQ.34.OR.TCKRC.EQ.52.OR.
     *    TCKRC.EQ.95.OR.TCKRC.EQ.97 ) THEN
!                Invalid Pack Status           34
!                Invalid Game Number           52
!                Host Error 1                  95
!                Record Lock                   97
          TCKRC=INOER
        ELSEIF(TCKRC.GE.01.AND.TCKRC.LE.11) THEN
!                Agent exceeds redeem max      01
!                Pack not Active               02
!                Invalid Prize Amount          03
!                Virn Unscramble Failed        04
!                Previously Paid               05
!                Game Pack Ticket Not Found    06
!                Pack Not Retailer Status      07
!                Game Not Active               08
!                Previously Paid By You        09
!                Previously Paid By Other      10
!                Not a Winner                  11
          TCKRC=INOER
        ELSEIF(TCKRC.EQ.36.OR.TCKRC.EQ.38) THEN
!                Game Closed,File Clm@Lott Off 36
!                Game Closed                   38
          TCKRC=INOER
        ELSEIF(TCKRC.GE.57.AND.TCKRC.LE.58) THEN ! P-A-H
!                Not a home play winner        57
!                Studio play winner            58
          TCKRC=INOER
        ELSEIF(TCKRC.GE.62.AND.TCKRC.LE.63) THEN ! P-A-H
!                Results not in yet            62
!                Not a winner yet              63
          TCKRC=INOER
        ELSEIF(TCKRC.EQ.ICNPB) THEN
!                Cannot pay in batch          103
          TCKRC=INOER
        ENDIF

        RETURN
        END
