C SUBROUTINE REPRINT
C
C REPRINT.FOR
C
C V65 09-MAR-2015 SCML M16 PROJECT
C V64 27-MAR-2014 SCML Added support to PLACARD Project - IGS
C V63 13-JAN-2014 SCML Included new message subtype for new validations' reprint
C V62 19-NOV-2013 SCML Last Bank Validation reprint added for Euromillions
C V61 08-OCT-2013 SCML Fix for Passive Game reprint.
C                      IPS Validation Terminal to Central: amount in cents changed from 2 to 4 bytes long
C V60 24-OCT-2013 SCML Added new bank validation mode to IPS validation reprint
C V59 30-SEP-2013 SCML Last Bank Validation reprint added for online games (subtype 10)
C                      Player id type added to Last Passive Ticket Bank Validation (subtype 6)
C V58 01-MAR-2013 FRP Fix for NIB number in Passive reprint
C V57 12-APR-2011 FJG ACCENTURE MERGE FOR EM2
C V56 30-MAR-2011 ACN Euromillions Validation Prize over 42M
C V55 14-APR-2010 RXK Changes for ePassive
C V54 25-APR-2006 FRP Modify for IPS Distribution.
C V53 10-JAN-2001 CS  INCLUDED PASSIVE FOR PORTUGAL
C V52 08-JUL-1999 UXN Duplicated code replaced by WAGER_BODY() subroutine.
C                     WAGER_BODY() is also used in OUTFRA().
C V51 01-JUN-1999 UXN Bet amount is now 4 bytes for oddset games.
C V50 14-MAY-1999 UXN Super Triple added.
C V49 16-MAR-1999 RXK Gtyp/gind change, hack for V5 removed.
C V48 09-SEP-1998 RXK Changed due new kicker game
C V47 21-JAN-1998 UXN Super Score and Todays Triple added.
C V46 14-FEB-1997 RXK Bankid and banknum revbyted for instant
C V45 05-FEB-1997 RXK Instant ticket validation reprint added 
C V44 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V43 01-FEB-1996 HXK Fix for Double/Couple system bets  
C V42 11-JAN-1996 HXK Fix for simple Double / Couple reprints 
C V41 23-NOV-1995 PXB Couple and Double games added 
C V40 17-JUL-1995 HXK Various bug fixes, etc. for Ravi batch    
C V39 26-JUN-1995 HXK Changes for RAVI modification, mainly QPs,alternatives,
C                     screening and prognosis     
C V38 05-MAY-1995 HXK V5 entered into database again!!!! 
C V37 22-FEB-1995 HXK Twas brillig and the borogoves did 
C V36 23-NOV-1994 HXK Fixed for Bingo 
C V35 15-OCT-1994 HXK Adding /developing Bingo   
C V34 20-JAN-1994 HXK Allow #homes .ne. #aways for system bets 
C V33 17-DEC-1993 HXK Fix for bug with system homes & aways counts for 
c                     tulosveto (score). 
C V32 17-DEC-1993 HXK Changed for Score (tulosveto / matchen).
C V31 18-OCT-1993 HXK Fix for refund reprint. 
C V30 15-OCT-1993 HXK Fix for bank reprint. 
C V29 15-OCT-1993 HXK Fix for bank validations.   
C V28 15-OCT-1993 HXK Fix for bank reprints. 
C V27 15-OCT-1993 HXK Change for bank reprints. 
C V26 14-OCT-1993 HXK Fix for Bank Validation.  
C V25 24-SEP-1993 GXA Separate validation amount and refund amount in
C                     validation reprints. 
C V24 13-SEP-1993 HXK Changed fraction definition for terminal
C V23 10-SEP-1993 GXA Changed reprint of validation to use TVBNK instaed of 
C                     TWBNK 
C V22 31-AUG-1993 GXA Added Game Index to Reprint of Exchange 
C                     (Host to Terminal). 
C V21 17-AUG-1993 HXK Removed weirdo char(18) from PVCS comment for previous
C                     revision 
C V20 17-AUG-1993 HXK sports twsysn is stored as 1 for all full systems 
C                     (for pools),terminal sees twsimp (row equivalent) as sys 
C                     number 
C V19 17-AUG-1993 HXN Updated Validation reprint part.
C V18 13-AUG-1993 GXA Changed error lengths to really by 6. 
C V17 11-AUG-1993 HXK Allow group bet copy 
C V16 05-AUG-1993 CXK Reduced system reprints for sports send back number of 
C                     rows equivalent instead of a reduced system number. 
C V15 05-AUG-1993 SXH Commented out debugging statements  
C V14 05-AUG-1993 SXH Fixed bug with V65 wager reprints 
C V13 03-AUG-1993 GXA Added Kicker duration byte to wager / exchange reprint
C                     message. This is needed by the terminal when the main and
C                     companion games durations are different, 
C                     like Viking Lotto.
C V12 30-JUL-1993 HXN Let there some tracing for debugging... 
C V11 28-JUL-1993 SXH Put back HXN's mod 
C V10 01-JUL-1993 HXN First adaptation to Finland System. Many changes according
C                     to new messages.Add Ravi game.  
C V09 28-JUN-1993 HXK Changed err message length from 5 to 6. 
C V08 21-JAN-1993 DAB Initial Release 
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V07 05-AUG-1992 GCAN Fixed TOTO SELECT, SCORE  reprints.
C V06 20-APR-1992 GCAN Fixed LOTTO 4/5 reprint with exchange.
C V05 13-FEB-1992 GCAN Set VCODE if reprint of exchanges.
C V04 21-JAN-1992 GCAN Fixed data offsets (start in byte 5).
C V03 20-NOV-1991 GCAN Added TOTO SELECT,SCORE and WIN TIP 
C V02 12-NOV-1991 MTK  Initial release for Netherlands
C V01 01-AUG-1990 XXX  Released for VAX
C
C SUBROUTINE TO PROCESS REPRINT OF TRANSACTIONS
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE REPRINT(TRABUF,OUTTAB,OUTLEN,EUROMILSEND,IGSSEND)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
C----+------------------------------------------------------------------
C V64| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:IGSDEBUG.DEF'
C----+------------------------------------------------------------------
C V64| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C
        ! arguments
        BYTE       OUTTAB(*)     !
        INTEGER*2  OUTLEN        !

        INTEGER*4  MYCHKSUM      !
        INTEGER*4  CHKLEN        !
        INTEGER*4  AMTWON        !
        INTEGER*4  VCODE         !
        INTEGER*4  I             !
        INTEGER*4  CHECK         !
        INTEGER*4  IND           !
        INTEGER*4  ST            !
        INTEGER*4  I4TEMP        !
        INTEGER*4  LAST          !
        INTEGER*4  TER           !
        INTEGER*4  SUB           !
        INTEGER*4  GRPTYP        !group bet copy
        INTEGER*4  VALTYP        !
        INTEGER*4  CANTYP        !
        INTEGER*4  WAGTYP        !
C***    INTEGER*4  AMTREF        !
        INTEGER*4  OPSAMT        !Total in OPS for the ticket     !V16
        INTEGER*4  ERRTYP        !
        INTEGER*4  INSTYP        !
        INTEGER*4  VPATYP        !VALIDATION PASSIVE TYPE
        INTEGER*4  BPATYP        !BANK VALIDATION 
        INTEGER*4  RPATYP        !RETURN PASSIVE TYPE
        INTEGER*4  ORDTYP        !INSTANT SUPPLY ORDER TYPE (IMNU)
        INTEGER*4  VBKTYP        !BANK VALIDATION TYPE
C----+------------------------------------------------------------------
C V63| Included new message subtype for new validations' reprint
C----+------------------------------------------------------------------
        INTEGER*4  VNVTYP        !New validation types
C----+------------------------------------------------------------------
C V63| Included new message subtype for new validations' reprint
C----+------------------------------------------------------------------
        INTEGER*4  CONTRL        !
        INTEGER*4  WRKBUF(TRALEN)!
        INTEGER*4  LOGREC(LREC*3)!
        INTEGER*4  OPTION        !
        INTEGER*4  TOTIND        !
        INTEGER*4  TOTVAL        !
        INTEGER*4  TCKS          !
        INTEGER*4  WEEK          !
        INTEGER*4  YEAR          !
        INTEGER*4  OFF_AUXEMIS   !
        INTEGER*4  QTDFRAC       !
        INTEGER*4  GIND

        INTEGER*8   NIB
        BYTE        I1NIB(8)
        EQUIVALENCE (NIB,I1NIB)

        INTEGER*8   NIBBA1,NIBBA2

        BYTE       I1TEMP(4)     !

        INTEGER*2 DATE(12)       !

        EQUIVALENCE(I4TEMP,I1TEMP)
C
C EURO MIL PROJECT
C       
C INCLUDE EUROMILSEND LOGICAL VAR (TRUE = SEND TO EURO MIL / FALSE = DO NOT SEND TO EURO MIL)
        LOGICAL EUROMILSEND
        BYTE EUROQP
        BYTE EUROSN
        PARAMETER (EUROQP = '02'X)
        PARAMETER (EUROSN = '01'X)
        BYTE EUROOPTIONS
        INTEGER*4 LAST_E
C
        BYTE PVOFLG               !PASSIVE VALIDATION OPTION FLAGS             !V59
        BYTE VPNCA                !V59
        PARAMETER (VPNCA = '80'X) !TOTAL NET CASH AMOUNT MASK FOR PASSIVE GAMES!V59
        INTEGER*4  PVOFLG_IND     !PASSIVE VALIDATION OPTION FLAGS INDEX       !V59
C
        BYTE OVOFLG               !ONLINE GAMES VALIDATION OPTION FLAGS        !V59
        BYTE VONCA                !V59
        PARAMETER (VONCA = '80'X) !TOTAL NET CASH AMOUNT MASK FOR ONLINE GAMES !V59
        INTEGER*4 OVOFLG_IND      !ONLINE GAMES VALIDATION OPTION FLAGS INDEX  !V59
C
C       IPS VALIDATION REPRINT, TERMINAL TO CENTRAL (T2C)
C
        INTEGER*2 IVOFLG_T2C            !OPTION FLAGS             !V60
        INTEGER*2 IVBVM_T2C             !NEW BANK VALIDATION MODE !V60
        INTEGER*2 IVBNK_T2C             !BANK DATA MASK           !V60
        PARAMETER (IVBVM_T2C = '0100'X) !V60
        PARAMETER (IVBNK_T2C = '0200'X) !V60
C
C       IPS VALIDATION REPRINT, CENTRAL TO TERMINAL (C2T)
C
        BYTE IVOFLG_C2T                 !OPTION FLAGS             !V60
        BYTE IVBVM_C2T                  !NEW BANK VALIDATION MODE !V60
        BYTE IVNCA_C2T                  !NET CASH AMOUNT          !V60
        PARAMETER (IVBVM_C2T = '01'X)   !V60
        PARAMETER (IVNCA_C2T = '02'X)   !V60
C
        INTEGER*4 IVOFLG_IND            !OPTION FLAG INDEX        !V60
C
        DATA CONTRL/Z20/
        DATA ERRTYP/Z90/
        DATA WAGTYP/Z81/
        DATA CANTYP/Z82/
        DATA VALTYP/Z83/
        DATA INSTYP/Z84/
        DATA GRPTYP/Z85/
        DATA BPATYP/Z86/
        DATA VPATYP/Z87/
        DATA RPATYP/Z88/
        DATA ORDTYP/Z89/
        DATA VBKTYP/Z8A/ !V59
C----+------------------------------------------------------------------
C V63| Included new message subtype for new validations' reprint
C----+------------------------------------------------------------------
        DATA VNVTYP/Z8B/ 
C----+------------------------------------------------------------------
C V63| Included new message subtype for new validations' reprint
C----+------------------------------------------------------------------

        LOGICAL   SEPARATED_REFUND /.TRUE./ !if true, then store AMT and REFUND separately.
        LOGICAL   FOUND

C----+------------------------------------------------------------------
C V64| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        LOGICAL   IGSSEND 
        INTEGER*4 LAST_IGS
        
        INTEGER*4 GTYP,GINDX

        INTEGER*4 J
        INTEGER*8 I8_MSG_ID
        INTEGER*4 I4_MSG_ID(2)
        INTEGER*1 I1_MSG_ID(8)
        EQUIVALENCE(I8_MSG_ID,I4_MSG_ID)
        EQUIVALENCE(I8_MSG_ID,I1_MSG_ID)

        CHARACTER*255 LINE
        
        LAST_IGS = 0
        IGSSEND = .FALSE.
C----+------------------------------------------------------------------
C V64| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------

C       INTEGER*4 IND1,IND2    !FOR TEST ONLY
C Begin code --------------------------------------

C GET REPRINT TYPE
C ----------------
        SUB=ZEXT(OUTTAB(2))
        SUB=IAND(SUB,15)
        TER=TRABUF(TTER)
        LAST=AGTTAB(ALSTRA,TER)
        
        IF(IGSDEBUG(IA_SPESRV)) THEN
            CALL OPS('RPRT: SUB  ',SUB,SUB)
            CALL OPS('RPRT: TER  ',TER,TER)
            CALL OPS('RPRT: LAST ',LAST,LAST)
            CALL OPS('RPRT: AGTTAB(ALSTRA,TER)',AGTTAB(ALSTRA,TER),AGTTAB(ALSTRA,TER))
            CALL OPS('RPRT: AGTTAB(ALSWAG,TER)',AGTTAB(ALSWAG,TER),AGTTAB(ALSWAG,TER))
            CALL OPS('RPRT: AGTTAB(ALSCAN,TER)',AGTTAB(ALSCAN,TER),AGTTAB(ALSCAN,TER))
            CALL OPS('RPRT: AGTTAB(ALSVAL,TER)',AGTTAB(ALSVAL,TER),AGTTAB(ALSVAL,TER))
        ENDIF
        
        IF(SUB.EQ.2)  LAST=AGTTAB(ALSWAG,TER)
        IF(SUB.EQ.3)  LAST=AGTTAB(ALSCAN,TER)
        IF(SUB.EQ.4)  LAST=AGTTAB(ALSVAL,TER)
        IF(SUB.EQ.6)  LAST=AGTTAB(ALSIVA,TER)
        IF(SUB.EQ.7)  LAST=AGTTAB(ALSVAL,TER)
        IF(SUB.EQ.8)  LAST=AGTTAB(ALSUPA,TER)
        IF(SUB.EQ.9)  LAST=AGTTAB(ALSORD,TER)
        IF(SUB.EQ.10) LAST=AGTTAB(ALSVAL,TER) !V59
C
        IF(IGSDEBUG(IA_SPESRV)) THEN
            CALL OPS('295:RPRT: LAST ',LAST,LAST)
        ENDIF
        TRABUF(TSNEW)=LAST
        I4TEMP=CONTRL+TRABUF(TTRN)
        OUTTAB(1)=I1TEMP(1)
C----+------------------------------------------------------------------
C V64| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        I1_MSG_ID(1) = OUTTAB(12)
        I1_MSG_ID(2) = OUTTAB(11)
        I1_MSG_ID(3) = OUTTAB(10)
        I1_MSG_ID(4) = OUTTAB( 9)
        I1_MSG_ID(5) = OUTTAB( 8)
        I1_MSG_ID(6) = OUTTAB( 7)
        I1_MSG_ID(7) = OUTTAB( 6)
        I1_MSG_ID(8) = OUTTAB( 5)
        TRABUF(TSDT1) = I4_MSG_ID(1)
        TRABUF(TSDT2) = I4_MSG_ID(2)
C----+------------------------------------------------------------------
C V64| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------

C
C READ TERMINALS LAST TRANSACTION FROM LOG FILE
C ---------------------------------------------
        CALL RLOG(LAST,LOGREC,SPE,ST)
        IF(IGSDEBUG(IA_SPESRV)) THEN
            CALL OPS('305:RPRT: AFTER RLOG: ST ',ST,ST)
        ENDIF
        IF(ST.NE.0) THEN
          TRABUF(TERR)=INVL
          TRABUF(TSTAT)=REJT
          OUTTAB(2)=ERRTYP
          OUTTAB(5)=TRABUF(TERR)
          OUTTAB(6)=0
          IND=7
          GOTO 5000
        ENDIF
        CALL LOGTRA(WRKBUF,LOGREC)
10      CONTINUE
        TRABUF(TSOLD)=WRKBUF(TTYP)
        TRABUF(TGAM)=WRKBUF(TGAM)
        TRABUF(TGAMTYP)=WRKBUF(TGAMTYP)
        TRABUF(TGAMIND)=WRKBUF(TGAMIND)

C BUILD THE MESSAGE ACCORDING TO THE REPRINT TYPE.
C -----------------------------------------------
        IF(WRKBUF(TTYP).EQ.TWAG) GOTO 1000
        IF(WRKBUF(TTYP).EQ.TCAN) GOTO 2000
        IF(WRKBUF(TTYP).EQ.TVAL) GOTO 3000
        IF(WRKBUF(TTYP).EQ.TREF) GOTO 3000
C***    IF(WRKBUF(TTYP).EQ.TCLM) GOTO 3000        !NOT USED IN FINLAND
        IF(WRKBUF(TTYP).EQ.TCRS.AND.WRKBUF(TITYP).EQ.IVAL) GOTO 3500
        IF(WRKBUF(TTYP).EQ.TCRS.AND.WRKBUF(TITYP).EQ.IMNU) GOTO 6000
        IF(WRKBUF(TTYP).EQ.TRET) GOTO 4000
C
C EURO MIL PROJECT - SEE WHAT TYPE IS
C
        IF(WRKBUF(TTYP) .EQ. TEUR) THEN          
          IF (WRKBUF(TEUTYP) .EQ. TWAG) THEN
            LAST_E = AGTTAB(ALSWAG,TER)
            IF (LAST_E .EQ. LAST) GOTO 9001
          ENDIF
          IF (WRKBUF(TEUTYP) .EQ. TCAN) THEN
            LAST_E = AGTTAB(ALSCAN,TER)
            IF (LAST_E .EQ. LAST) GOTO 9002
          ENDIF
          IF (WRKBUF(TEUTYP) .EQ. TVAL) THEN
            LAST_E = AGTTAB(ALSVAL,TER)
            IF (LAST_E .EQ. LAST) GOTO 9003          
          ENDIF
          EUROMILSEND = .FALSE.
        ENDIF
C----+------------------------------------------------------------------
C V64| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF(WRKBUF(TTYP) .EQ. TIGS) THEN
            TRABUF(TSDT3) = 0 
            TRABUF(TSDT4) = 0 
            I4TEMP = 0
            I1TEMP(1) = ZEXT(32)
            I1TEMP(2) = ZEXT(32)
            I1TEMP(3) = ZEXT(32)
            I1TEMP(4) = ZEXT(32)
            TRABUF(TSDT5) = I4TEMP 
            TRABUF(TSDT6) = I4TEMP 
            TRABUF(TSDT7) = I4TEMP
            TRABUF(TSDT8) = 0 
            TRABUF(TSDT9) = 0 
            TRABUF(TSDT10) = 0 
            TRABUF(TSDT11) = 0 
            TRABUF(TSDT12) = 0 
            TRABUF(TSDT13) = 0 
            TRABUF(TSDT14) = 0 
            TRABUF(TSDT15) = 0 
            IF(IGSDEBUG(IA_SPESRV)) THEN
                WRITE(LINE,99000) I8_MSG_ID, (I1_MSG_ID(I), I = 8,1,-1)
99000           FORMAT('MSG ID = ',I,' [ ',(8(Z2.2,1X)),']')
                CALL OPSTXT(TRIM(LINE))
            
99001           FORMAT('TRABUF(TSDT',I0,A,') = ',I,' [ ',(4(Z2.2,1X)),']')
                DO J = 1,15
                    I4TEMP = TRABUF(TSDT1 -1 +J)
                    IF(J .LT. 10) THEN
                        WRITE(LINE,99001) J, ' ',  I4TEMP, (I1TEMP(I), I = 4,1,-1)
                    ELSE
                        WRITE(LINE,99001) J, '',  I4TEMP, (I1TEMP(I), I = 4,1,-1)
                    ENDIF 
                    CALL OPSTXT(TRIM(LINE))
                ENDDO
            ENDIF
C            TRABUF(TAGT) = AGTTAB(AGTNUM,TER)
C            TRABUF(TTER) = TER
C            TRABUF(TCDC) = DAYCDC
            IF(IGSDEBUG(IA_SPESRV)) THEN
                CALL OPSTXT('356:RPRT: LAST TRX TYPE IS TIGS')
                CALL OPS('357:RPRT: WRKBUF(TIGS_TTYP)',WRKBUF(TIGS_TTYP),WRKBUF(TIGS_TTYP))
            ENDIF
            IGSSEND = .TRUE.
            
            GTYP = ZEXT(WRKBUF(TGAMTYP))
            GINDX = ZEXT(WRKBUF(TGAMIND))

            IF(IGSDEBUG(IA_SPESRV)) THEN
                CALL OPS('429:RPRT: P(IGSPRNT)',ZEXT(P(IGSPRNT)), ZEXT(P(IGSPRNT)))
                CALL OPS('429:RPRT: GTYP',ZEXT(GTYP), ZEXT(GTYP))
                CALL OPS('429:RPRT: GINDX',ZEXT(GINDX), ZEXT(GINDX))
                CALL OPS('429:RPRT: IGS_GAMNUM(GTYP,GINDX)',
     *                          ZEXT(IGS_GAMNUM(GTYP,GINDX)), 
     *                          ZEXT(IGS_GAMNUM(GTYP,GINDX)))
                CALL OPS('429:RPRT: IGSGAMFLG(IGSPGRNT)',
     *                          ZEXT(IGSGAMFLG(P(IGSPGRNT),IGS_GAMNUM(GTYP,GINDX))), 
     *                          ZEXT(IGSGAMFLG(P(IGSPGRNT),IGS_GAMNUM(GTYP,GINDX))))
            ENDIF
            IF(P(IGSPRNT) .EQ. 1 
     *      .OR. P(IGSPPLA) .EQ. 1
     *      .OR. IGSGAMFLG(P(IGSPGRNT),IGS_GAMNUM(GTYP,GINDX)) .EQ. .TRUE.) THEN
                TRABUF(TERR)=SUPR
                TRABUF(TSTAT)=REJT
                IGSSEND = .FALSE.
                IF(IGSDEBUG(IA_SPESRV)) THEN
                    CALL OPSTXT('434:RPRT: IGSSEND .EQ. .FALSE. (SUPR)')
                ENDIF
                RETURN
            ENDIF
            
            ! Handling IGS wager reprints
            IF(WRKBUF(TIGS_TTYP) .EQ. IGSWAG) THEN
                LAST_IGS = AGTTAB(ALSWAG,TER)
                IF(LAST_IGS .EQ. LAST) GOTO 12100
            ENDIF
            ! Handling IGS cancellation reprints
            IF(WRKBUF(TIGS_TTYP) .EQ. IGSCAN) THEN
                LAST_IGS = AGTTAB(ALSCAN,TER)
                IF(LAST_IGS .EQ. LAST) GOTO 12200
            ENDIF
C            ! Handling IGS validation reprints
C            IF(WRKBUF(TIGS_TTYP) .EQ. IGSVAL) THEN
C                LAST_IGS = AGTTAB(ALSTRA,TER)
C                IF(LAST_IGS .EQ. LAST) GOTO 12300
C            ENDIF
            ! Handling IGS payment reprints
            IF(WRKBUF(TIGS_TTYP) .EQ. IGSPAY) THEN
                LAST_IGS = AGTTAB(ALSVAL,TER)
                IF(LAST_IGS .EQ. LAST) GOTO 12400
            ENDIF
            IF(IGSDEBUG(IA_SPESRV)) THEN
                CALL OPSTXT('457:RPRT: IGSSEND .EQ. .FALSE. (DEF)')
            ENDIF
            IGSSEND = .FALSE.
            RETURN 
        ENDIF
        IF(IGSDEBUG(IA_SPESRV)) THEN
            CALL OPSTXT('463:RPRT: DEFAULT CASE - HANDLE AS ERROR')
        ENDIF
C----+------------------------------------------------------------------
C V64| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------

        TRABUF(TERR)=INVL
        TRABUF(TSTAT)=REJT
        OUTTAB(2)=ERRTYP
        OUTTAB(5)=TRABUF(TERR)
        OUTTAB(6)=0
        IND=7
        GOTO 5000              !Calculate checksum and return.
C
C EURO MIL PROJECT - WAGGER REPRINT
C
9001    CONTINUE
        OUTTAB(2)=WAGTYP
        IND = 5
C 1RST PART - FROM HOST-->TERMINAL MESSAGE
C
C GAME TYPE AND INDEX
C
        OUTTAB(IND+0) = WRKBUF(TGAMTYP) !#5 GAME TYPE
        OUTTAB(IND+1) = WRKBUF(TGAMIND) !#6 GAME INDEX
        IND = IND + 2
C EXTERNAL EURO SERIAL AND CHECKDIGITS
        I4TEMP = WRKBUF(TEUSER)
        OUTTAB(IND+0)=I1TEMP(3)   !#7   External (wager) serial #
        OUTTAB(IND+1)=I1TEMP(2)   !#8
        OUTTAB(IND+2)=I1TEMP(1)   !#9
        IND=IND+3

        OUTTAB(IND)= WRKBUF(TEUCHK) !#10 CHECK DIGITS
        IND=IND+1
C
C OFFSET FIRST CDC DATE
C
        I4TEMP = WRKBUF(TEUWOFS1)
        OUTTAB(IND+0) = I1TEMP(2)        !#11
        OUTTAB(IND+1) = I1TEMP(1)        !#12
C
C OFFSET SECOND CDC DATE
C
        I4TEMP = WRKBUF(TEUWOFS2)
        OUTTAB(IND+2) = I1TEMP(2)        !#13
        OUTTAB(IND+3) = I1TEMP(1)        !#14
        IND=IND+4
C
C FIRST WEEK AND YEAR DRAW DATE
C
        I4TEMP = WRKBUF(TEUWBEGW)
        OUTTAB(IND+0) = I1TEMP(1)        !#15
        I4TEMP = WRKBUF(TEUWBEGY)
        OUTTAB(IND+1) = I1TEMP(1)        !#16
        IND = IND + 2
C
C SECOND WEEK AND YEAR DRAW DATE
C
        I4TEMP = WRKBUF(TEUWENDW)
        OUTTAB(IND+0) = I1TEMP(1)        !#17
        I4TEMP = WRKBUF(TEUWENDY)
        OUTTAB(IND+1) = I1TEMP(1)        !#18
        IND = IND + 2
C
C TIME
C
        I4TEMP = WRKBUF(TEUWTIMEH)
        OUTTAB(IND+0) = I1TEMP(1)        !#19
        I4TEMP = WRKBUF(TEUWTIMEM)
        OUTTAB(IND+1) = I1TEMP(1)        !#20
        I4TEMP = WRKBUF(TEUWTIMES)       
        OUTTAB(IND+2) = I1TEMP(1)        !#21
        IND = IND + 3
C----+---+-------------+------------------------------------------------
C V65|BEG| M16 PROJECT | WAGER: ADD NEW HOST-TERMINAL MESSAGE FIELDS
C----+---+-------------+------------------------------------------------
C
C OPTION FLAGS
C
C        OUTTAB(IND+0) = 0               !#22
C        IND = IND + 1
        OPTION = 0
        CALL OGETEUROPT(WRKBUF, OPTION)
        I4TEMP = OPTION
        OUTTAB(IND+0) = I1TEMP(1) !#22                                          !OPTION FLAGS
        IND = IND + 1
C
C OPTION DATA
C
C       SET UP SM DATA
        IF(IAND(OPTION,'08'X).NE.0) THEN
          I4TEMP = WRKBUF(TEUW_SMWTB)                                           !SM TOTAL BETS
          OUTTAB(IND+0) = I1TEMP(2) !#23
          OUTTAB(IND+1) = I1TEMP(1) !#24
C
          I4TEMP = WRKBUF(TEUW_SMWSN)                                           !SM EXTERNAL SERIAL NUMBER 
          OUTTAB(IND+2) = I1TEMP(3) !#25
          OUTTAB(IND+3) = I1TEMP(2) !#26
          OUTTAB(IND+4) = I1TEMP(1) !#27
C
          I4TEMP = WRKBUF(TEUW_SMWCD)                                           !SM CHECK DIGITS
          OUTTAB(IND+5) = I1TEMP(1) !#28
C
          I4TEMP = WRKBUF(TEUW_SMWDN)                                           !SM DRAW NUMBER
          OUTTAB(IND+6) = I1TEMP(1) !#29
C
          I4TEMP = WRKBUF(TEUW_SMWDY)                                           !SM DRAW YEAR
          OUTTAB(IND+7) = I1TEMP(1) !#30
C
          I4TEMP = WRKBUF(TEUW_SMWOF)                                           !OFFSET TO SM DRAW CDC DATE
          OUTTAB(IND+8) = I1TEMP(1) !#31
C
          I4TEMP = WRKBUF(TEUW_SMWB1)                                           !SM WAGER FIRST NUMBER
          OUTTAB(IND+ 9) = I1TEMP(1) !#32
          OUTTAB(IND+10) = I1TEMP(2) !#33
          OUTTAB(IND+11) = I1TEMP(3) !#34
          OUTTAB(IND+12) = I1TEMP(4) !#35
          I4TEMP = WRKBUF(TEUW_SMWB2)
          OUTTAB(IND+13) = I1TEMP(3) !#36
          OUTTAB(IND+14) = I1TEMP(2) !#37
          OUTTAB(IND+15) = I1TEMP(1) !#38
C
          I4TEMP = WRKBUF(TEUW_SMWE1)                                           !SM WAGER LAST NUMBER
          OUTTAB(IND+16) = I1TEMP(1) !#39
          OUTTAB(IND+17) = I1TEMP(2) !#40
          OUTTAB(IND+18) = I1TEMP(3) !#41
          OUTTAB(IND+19) = I1TEMP(4) !#42
          I4TEMP = WRKBUF(TEUW_SMWE2)
          OUTTAB(IND+20) = I1TEMP(3) !#43
          OUTTAB(IND+21) = I1TEMP(2) !#44
          OUTTAB(IND+22) = I1TEMP(1) !#45
C
          IND = IND + 23
        ENDIF
C       SET UP SoM DATA
        IF(IAND(OPTION,'04'X).NE.0) THEN
          I4TEMP = WRKBUF(TEUW_SHWTB)                                           !SoM TOTAL BETS
          OUTTAB(IND+0) = I1TEMP(2) !#23 or #46
          OUTTAB(IND+1) = I1TEMP(1) !#24 or #47
C
          I4TEMP = WRKBUF(TEUW_SHWDN)                                           !SoM DRAW NUMBER
          OUTTAB(IND+2) = I1TEMP(1) !#25 or #48
C
          I4TEMP = WRKBUF(TEUW_SHWDY)                                           !SoM DRAW YEAR
          OUTTAB(IND+3) = I1TEMP(1) !#26 or #49
C
          I4TEMP = WRKBUF(TEUW_SHWOF)                                           !OFFSET TO SoM DRAW CDC DATE
          OUTTAB(IND+4) = I1TEMP(1) !#27 or #50
C
          I4TEMP = WRKBUF(TEUW_SHWB1)                                           !SoM WAGER FIRST NUMBER
          OUTTAB(IND+5) = I1TEMP(1) !#28 or #51
          OUTTAB(IND+6) = I1TEMP(2) !#29 or #52
          OUTTAB(IND+7) = I1TEMP(3) !#30 or #53
          OUTTAB(IND+8) = I1TEMP(4) !#31 or #54
          I4TEMP = WRKBUF(TEUW_SHWB2)
          OUTTAB(IND+ 9) = I1TEMP(3) !#32 or #55
          OUTTAB(IND+10) = I1TEMP(2) !#33 or #56
          OUTTAB(IND+11) = I1TEMP(1) !#34 or #57
C
          I4TEMP = WRKBUF(TEUW_SHWE1)                                           !SoM WAGER LAST NUMBER
          OUTTAB(IND+12) = I1TEMP(1) !#35 or #58
          OUTTAB(IND+13) = I1TEMP(2) !#36 or #59
          OUTTAB(IND+14) = I1TEMP(3) !#37 or #60
          OUTTAB(IND+15) = I1TEMP(4) !#38 or #61
          I4TEMP = WRKBUF(TEUW_SHWE2)
          OUTTAB(IND+16) = I1TEMP(3) !#39 or #62
          OUTTAB(IND+17) = I1TEMP(2) !#40 or #63
          OUTTAB(IND+18) = I1TEMP(1) !#41 or #64
C
          IND = IND + 19
        ENDIF
C----+---+-------------+------------------------------------------------
C V65|END| M16 PROJECT | WAGER: ADD NEW HOST-TERMINAL MESSAGE FIELDS
C----+---+-------------+------------------------------------------------
C
C 2ND PART - WAGER TERMINAL TO HOST
C
C
C GAME INDEX SYSTEM TYPE
C
        I4TEMP=ISHFT(WRKBUF(TGAMIND),4)
        IF ((WRKBUF(TEUWNBET) .EQ. 1) .AND. ((WRKBUF(TEUWNMK) .NE. 5) .OR. (WRKBUF(TEUWNST) .NE. 2))) THEN
           I4TEMP=I4TEMP + 1
        ELSE 
           I4TEMP=I4TEMP !game index & syst type
        ENDIF
        
        OUTTAB(IND)=I4TEMP !#23 or #42 or #65
        IND=IND+1
C
C GET DURATION & NUMBER OF BOARDS
C
        I4TEMP=ISHFT(WRKBUF(TEUWDUR),4)+WRKBUF(TEUWNBET) !duration & number of boards
        OUTTAB(IND)=I4TEMP !#24 or #43 or #66
        IND=IND+1
C----+---+-------------+------------------------------------------------
C V65|BEG| M16 PROJECT | EM WAGER: SET OPTION FLAGS AND OPTION DATA AND
C        |             |           ADD NEW TERMINAL-HOST MESSAGE FIELDS
C----+---+-------------+------------------------------------------------
!
!        EUROOPTIONS = 0
!        EUROOPTIONS = EUROOPTIONS + EUROSN
!        OUTTAB(IND) = EUROOPTIONS       ! #24
!        OUTTAB(IND + 1 ) = 0
!C
!C GET QUICK PICK FLAG 
!C
!        IF (WRKBUF(TEUWQP) .NE. 0) THEN
!           EUROOPTIONS = EUROOPTIONS + EUROQP
!           OUTTAB(IND) = EUROOPTIONS    ! #24
!           I4TEMP=WRKBUF(TEUWQP)
!           OUTTAB(IND+2)=I1TEMP(2)      ! #25
!           OUTTAB(IND+3)=I1TEMP(1)      ! #26
!           IND=IND + 2
!        ENDIF
!        IND = IND + 2
!C
!C GET SYSTEM NUMBER 
!C 
!        OUTTAB(IND) = WRKBUF(TEUWNMK)           ! #25 OR #27
!        OUTTAB(IND + 1) = WRKBUF(TEUWNST)       ! #26 OR #28
!        IND = IND + 2
        OPTION = 0
        CALL IGETEUROPT(WRKBUF, OPTION)
        I4TEMP = OPTION
        OUTTAB(IND+0) = I1TEMP(2) !#25 or #44 or #67                            !OPTION FLAGS
        OUTTAB(IND+1) = I1TEMP(1) !#26 or #45 or #68
        IND = IND + 2
C
C OPTION DATA
C
C       SET UP QUICK PICK FLAGS BY BOARD
        IF(IAND(OPTION,'0200'X).NE.0) THEN
          I4TEMP = WRKBUF(TEUWQP)                                               !QUICK PICK FLAGS BY BOARD
          OUTTAB(IND+0) = I1TEMP(2) !#27 or #46 or #69
          OUTTAB(IND+1) = I1TEMP(1) !#28 or #47 or #70 
          IND = IND + 2
        ENDIF
C       SET UP SYSTEM NUMBER PRESENT
        IF(IAND(OPTION,'0100'X).NE.0) THEN
          I4TEMP = WRKBUF(TEUWNMK)                                              !NUMBER OF MARKS
          OUTTAB(IND+0) = I1TEMP(1) !#27 or #29 or #48 or #71
          I4TEMP = WRKBUF(TEUWNST)                                              !NUMBER OF STARS
          OUTTAB(IND+1) = I1TEMP(1) !#28 or #30 or #49 or #72
          IND = IND + 2
        ENDIF
C
C PLAYER NIF
C
          I4TEMP = WRKBUF(TEUW_PLNIF)                                           !PLAYER NIF (NEW FIELD)
          OUTTAB(IND+0) = I1TEMP(4) !#29 or #31 or #50 or #72
          OUTTAB(IND+1) = I1TEMP(3) !#30 or #32 or #51 or #73
          OUTTAB(IND+2) = I1TEMP(2) !#31 or #33 or #52 or #74
          OUTTAB(IND+3) = I1TEMP(1) !#32 or #34 or #53 or #75
          IND = IND + 4
C
C EM SALES CHANNEL
C
          I4TEMP = WRKBUF(TEUW_EUWCH)                                           !EM SALES CHANNEL (NEW FIELD)
          OUTTAB(IND+0) = I1TEMP(1) !#33 or #35 or #54 or #76
          IND = IND + 1
C----+---+-------------+------------------------------------------------
C V65|END| M16 PROJECT | EM WAGER: SET OPTION FLAGS AND OPTION DATA AND
C        |             |           ADD NEW TERMINAL-HOST MESSAGE FIELDS
C----+---+-------------+------------------------------------------------
C
C SET DRAW INDICATOR AND TRANSACTION ID
C
CV65        OUTTAB(IND) = 0 ! #27 OR #29
        OUTTAB(IND) = WRKBUF(TEUWDRWIND) !#34 or #36 or #55 or #77              !V65
        IND = IND +1 
C
C SET TRANSACTION ID !V65
C
        OUTTAB(IND+0) = 0 !#35 or #37 or #56 or #78                             !V65
        OUTTAB(IND+1) = 0 !#36 or #38 or #57 or #79                             !V65
        OUTTAB(IND+2) = 0 !#37 or #39 or #58 or #80                             !V65
        OUTTAB(IND+3) = 0 !#38 or #40 or #59 or #81                             !V65
        OUTTAB(IND+4) = 0 !#39 or #41 or #60 or #82                             !V65
        OUTTAB(IND+5) = 0 !#40 or #42 or #61 or #83                             !V65
        OUTTAB(IND+6) = 0 !#41 or #43 or #62 or #84                             !V65
        OUTTAB(IND+7) = 0 !#42 or #44 or #63 or #85                             !V65
        IND = IND + 8
C
C GET BOARD 
C
!        DO I=TEUWBOARD,120
        DO I=TEUWBOARD,TEUWBEND                                                 !V65
           IF(WRKBUF(I) .NE. 0) THEN
              CALL MOVBYT(WRKBUF(I),1,OUTTAB,IND,4) !#43.. or #45... or #64... or #86...
              IND=IND+4
           ENDIF
        ENDDO 
     
        GOTO 5000
C
C EURO MIL CANCEL REPRINT
C
9002    CONTINUE

        OUTTAB(2)=CANTYP

        IND=5

C 1RST PART - FROM HOST-->TERMINAL MESSAGE
C ----------------------------------------
C
C TIME
C
        I4TEMP = WRKBUF(TEUCTIMEH)
        OUTTAB(IND+0) = I1TEMP(1)        !#5
        I4TEMP = WRKBUF(TEUCTIMEM)
        OUTTAB(IND+1) = I1TEMP(1)        !#6
        I4TEMP = WRKBUF(TEUCTIMES)       
        OUTTAB(IND+2) = I1TEMP(1)        !#7
        IND = IND + 3  
C
C CANCEL EXTERNAL SERIAL NUMBER
C        
        I4TEMP = WRKBUF(TEUSER)
        OUTTAB(IND+0)=I1TEMP(3)   !#8   External (CANCEL) serial #
        OUTTAB(IND+1)=I1TEMP(2)   !#9
        OUTTAB(IND+2)=I1TEMP(1)   !#10
        IND=IND+3

        OUTTAB(IND)= WRKBUF(TEUCHK) !#11 CHECK DIGITS
        IND=IND+1
C        
C GAME TYPE AND INDEX
C
        OUTTAB(IND+0) = WRKBUF(TGAMTYP) !#12 GAME TYPE
        OUTTAB(IND+1) = WRKBUF(TGAMIND) !#13 GAME INDEX
        IND = IND + 2
C
C CANCEL STATUS
C
        OUTTAB(IND+0) = 0       !#14 CANCEL STATUS
        IND = IND + 1
C
C CANCEL AMOUNT 
C
        I4TEMP = WRKBUF(TEUCAM)
        OUTTAB(IND+0)=I1TEMP(4)   !#15   
        OUTTAB(IND+1)=I1TEMP(3)   !#16   
        OUTTAB(IND+2)=I1TEMP(2)   !#17
        OUTTAB(IND+3)=I1TEMP(1)   !#18
        IND = IND + 4
C
C MONDAY DRAW INDICATOR
C
        OUTTAB(IND+0)= 0   !#19   
        IND = IND + 1       
C----+---+-------------+------------------------------------------------
C V65|BEG| M16 PROJECT | EM CANCEL: ADD NEW OPTION FIELDS
C----+---+-------------+------------------------------------------------
C
C DECODE OPTION FLAGS
C
        OPTION = 0
        CALL OGETEUROPT(WRKBUF, OPTION)
        I4TEMP = OPTION
        OUTTAB(IND+0) = I1TEMP(1) !#20                                          !OPTION FLAGS
        IND = IND + 1
C
C DECODE OPTION DATA
C
C       SET UP SM CANCELLATION DATA
        IF(IAND(OPTION,'80'X).NE.0) THEN
          I4TEMP = WRKBUF(TEUC_SMWSN)                                           !SM WAGER EXTERNAL SERIAL NUMBER
          OUTTAB(IND+0) = I1TEMP(3) !#21
          OUTTAB(IND+1) = I1TEMP(2) !#22
          OUTTAB(IND+2) = I1TEMP(1) !#23
C       
          I4TEMP = WRKBUF(TEUC_SMWCD)                                           !SM WAGER CHECK DIGITS
          OUTTAB(IND+3) = I1TEMP(1) !#24
C       
          I4TEMP = WRKBUF(TEUC_SMCSN)                                           !SM CANCELLATION EXTERNAL SERIAL NUMBER
          OUTTAB(IND+4) = I1TEMP(3) !#25
          OUTTAB(IND+5) = I1TEMP(2) !#26
          OUTTAB(IND+6) = I1TEMP(1) !#27
C       
          I4TEMP = WRKBUF(TEUC_SMCCD)                                           !SM CANCELLATION CHECK DIGITS
          OUTTAB(IND+7) = I1TEMP(1) !#28
C       
          I4TEMP = WRKBUF(TEUC_SMWCA)                                           !SM CANCEL AMOUNT (WAGER UNITS)
          OUTTAB(IND+ 8) = I1TEMP(4) !#29
          OUTTAB(IND+ 9) = I1TEMP(3) !#30
          OUTTAB(IND+10) = I1TEMP(2) !#31
          OUTTAB(IND+11) = I1TEMP(1) !#32
          IND = IND + 12
        ENDIF
C----+---+-------------+------------------------------------------------
C V65|END| M16 PROJECT | EM CANCEL: ADD NEW OPTION FIELDS
C----+---+-------------+------------------------------------------------

C 2ND PART - FROM TERMINAL-->HOST MESSAGE
C ---------------------------------------
C
C       GET WAGER SERIAL #
C       -----------------------
        I4TEMP = WRKBUF(TEUCWSER)
        OUTTAB(IND+0)=I1TEMP(3) !#33
        OUTTAB(IND+1)=I1TEMP(2) !#34
        OUTTAB(IND+2)=I1TEMP(1) !#35
        IND=IND+3
C       GET CHECK DIGITS
C       ----------------
        OUTTAB(IND)=WRKBUF(TEUCWCKD) !#36
        IND = IND + 1
        GOTO 5000
C
C EURO MIL VALIDATION REPRINT
C
9003    CONTINUE

C-------->>V62 --------------------------
C        OUTTAB(2)=VALTYP
        IF(WRKBUF(TEUVSBT).EQ.VNBNK) THEN
          OUTTAB(2)=VBKTYP       ! EUROMILLIONS BANK VALIDATION
C----+------------------------------------------------------------------
C V63| Included new message subtype for new validations' reprint
C----+------------------------------------------------------------------
        ELSEIF(  WRKBUF(TEUVSBT).EQ.VNDON) THEN
          OUTTAB(2)=VNVTYP
        ELSE
          OUTTAB(2)=VALTYP
C----+------------------------------------------------------------------
C V63| Included new message subtype for new validations' reprint
C----+------------------------------------------------------------------
        ENDIF
C-------- V62<<--------------------------

        IND=5

C 1RST PART - FROM HOST-->TERMINAL MESSAGE
C ----------------------------------------
C
C TIME
C 
        I4TEMP = WRKBUF(TEUVTIMEH)
        OUTTAB(IND+0) = I1TEMP(1)        ! #5
        I4TEMP = WRKBUF(TEUVTIMEM)
        OUTTAB(IND+1) = I1TEMP(1)        ! #6
        I4TEMP = WRKBUF(TEUVTIMES)       
        OUTTAB(IND+2) = I1TEMP(1)        ! #7
        IND = IND + 3  
C
C VALIDATION EXTERNAL SERIAL NUMBER
C        
        I4TEMP = WRKBUF(TEUSER)
        OUTTAB(IND+0)=I1TEMP(3)   !#8   External (VALID) serial #
        OUTTAB(IND+1)=I1TEMP(2)   !#9
        OUTTAB(IND+2)=I1TEMP(1)   !#10
        IND=IND+3

        OUTTAB(IND)= WRKBUF(TEUCHK) !#11 CHECK DIGITS
        IND=IND+1
C        
C GAME TYPE AND INDEX
C
        OUTTAB(IND+0) = WRKBUF(TGAMTYP) !#12 GAME TYPE
        OUTTAB(IND+1) = WRKBUF(TGAMIND) !#13 GAME INDEX
        IND = IND + 2
C
C VALIDATION STATUS
C
        OUTTAB(IND+0) = WRKBUF(TEUVST)  !#14 VALID STATUS
        IND = IND + 1
C
C CASH AMOUNT 
C
        I4TEMP = WRKBUF(TEUVCAM)
        OUTTAB(IND+0)=I1TEMP(4)   !#15   
        OUTTAB(IND+1)=I1TEMP(3)   !#16   
        OUTTAB(IND+2)=I1TEMP(2)   !#17
        OUTTAB(IND+3)=I1TEMP(1)   !#18
        IND = IND + 4
C
C REFUND AMOUNT 
C
        I4TEMP = WRKBUF(TEUVRAM)
        OUTTAB(IND+0)=I1TEMP(4)   !#19
        OUTTAB(IND+1)=I1TEMP(3)   !#20
        OUTTAB(IND+2)=I1TEMP(2)   !#21
        OUTTAB(IND+3)=I1TEMP(1)   !#22
        IND = IND + 4       

C V56 Euromillions Validation Prize over 42M ...

C
C CASH AMOUNT HIGH HALF WORD
C
        I4TEMP = WRKBUF(TEUVCAMH)
        OUTTAB(IND+0)=0           !#23
        OUTTAB(IND+1)=0           !#24
        OUTTAB(IND+2)=I1TEMP(2)   !#25
        OUTTAB(IND+3)=I1TEMP(1)   !#26
        IND = IND + 4
C
C REFUND AMOUNT HIGH HALF WORD
C
        I4TEMP = WRKBUF(TEUVRAMH)
        OUTTAB(IND+0)=0           !#27
        OUTTAB(IND+1)=0           !#28
        OUTTAB(IND+2)=I1TEMP(2)   !#29
        OUTTAB(IND+3)=I1TEMP(1)   !#30
        IND = IND + 4

C ... V56 Euromillions Validation Prize over 42M

C----+---+-------------+------------------------------------------------
C V65|BEG| M16 PROJECT | EM VALIDATION: ADD NEW OPTION FIELDS
C----+---+-------------+------------------------------------------------
        IF(WRKBUF(TEUVSBT).EQ.VNREG .OR. WRKBUF(TEUVSBT).EQ.VNINQ .OR.
     *     WRKBUF(TEUVSBT).EQ.VNDON .OR. WRKBUF(TEUVSBT).EQ.VNBNK .OR.
     *     WRKBUF(TEUVSBT).EQ.VNIBO) THEN
C
C OPTION FLAGS
C
          OPTION = 0
          CALL OGETEUROPT(WRKBUF, OPTION)
          I4TEMP = OPTION
          OUTTAB(IND+0) = I1TEMP(1) !#31                                        !OPTION FLAGS
          IND = IND + 1
C
C OPTION DATA
C
C         SET UP PLAYER NIF
          IF(IAND(OPTION,'80'X).NE.0) THEN
            I4TEMP = WRKBUF(TEUV_PLNIF)                                         !PLAYER NIF
            OUTTAB(IND+0) = I1TEMP(4) !#32
            OUTTAB(IND+1) = I1TEMP(3) !#33
            OUTTAB(IND+2) = I1TEMP(2) !#34
            OUTTAB(IND+3) = I1TEMP(1) !#35
            IND = IND + 4
          ENDIF
        ENDIF
C----+---+-------------+------------------------------------------------
C V65|END| M16 PROJECT | EM VALIDATION: ADD NEW OPTION FIELDS
C----+---+-------------+------------------------------------------------

C 2ND PART - FROM TERMINAL-->HOST MESSAGE
C ---------------------------------------

C       GET WAGER JULIAN DATE
C       ---------------------
        I4TEMP=WRKBUF(TEUVWJUL)
        OUTTAB(IND+0)=I1TEMP(2) !#32 or #36
        OUTTAB(IND+1)=I1TEMP(1) !#33 or #37
        IND=IND+2
C    
C       GET VALIDATION SERIAL #
C       -----------------------
        I4TEMP = WRKBUF(TEUVWSER)
        OUTTAB(IND+0)=I1TEMP(3) !#34 or #38
        OUTTAB(IND+1)=I1TEMP(2) !#35 or #39
        OUTTAB(IND+2)=I1TEMP(1) !#36 or #40
        IND=IND+3
C
C       GET CHECK DIGITS
C       ----------------
        OUTTAB(IND)=WRKBUF(TEUVWCKD) !#37 or #41
        IND=IND+1
C
C-------->>V62 --------------------------
C
C BANK VALIDATION
C
        IF(WRKBUF(TEUVSBT).EQ.VNBNK) THEN
C
C PLAYER ID TYPE
C
          OUTTAB(IND) = WRKBUF(TEUVPLIDTYP) !#38 or #42
          IND=IND+1
C
C PLAYER CARD/TELEPHONE NUMBER
C
          I4TEMP = WRKBUF(TEUVPLCARD)
          OUTTAB(IND+0)=I1TEMP(4) !#39 or #43
          OUTTAB(IND+1)=I1TEMP(3) !#40 or #44
          OUTTAB(IND+2)=I1TEMP(2) !#41 or #45
          OUTTAB(IND+3)=I1TEMP(1) !#42 or #46
          IND=IND+4
C
C NIB
C
          I4TEMP = WRKBUF(TEUVNIBBB)
          OUTTAB(IND+0)=I1TEMP(2) !#43 or #47
          OUTTAB(IND+1)=I1TEMP(1) !#44 or #48

          IND=IND+2
          I4TEMP = WRKBUF(TEUVNIBBO)
          OUTTAB(IND+0)=I1TEMP(2) !#45 or #49
          OUTTAB(IND+1)=I1TEMP(1) !#46 or #50
          IND=IND+2
          NIBBA1 = WRKBUF(TEUVNIBBA1)
          NIBBA2 = WRKBUF(TEUVNIBBA2)
          NIB = NIBBA1*100 + NIBBA2
          OUTTAB(IND+0)=I1NIB(5) !#47 or #51
          OUTTAB(IND+1)=I1NIB(4) !#48 or #52
          OUTTAB(IND+2)=I1NIB(3) !#49 or #53
          OUTTAB(IND+3)=I1NIB(2) !#50 or #54
          OUTTAB(IND+4)=I1NIB(1) !#51 or #55
          IND=IND+5
          OUTTAB(IND+0)=WRKBUF(TEUVNIBCD)  !#52 or #56
          IND=IND+1
      ENDIF
C
C-------- V59<<--------------------------
C
        IF (WRKBUF(TEUVST) .NE. 11) GOTO 5000
C
C 3RD PART - FROM HOST-->TERMINAL MESSAGE
C ----------------------------------------
C
C GAME TYPE AND INDEX
C
        OUTTAB(IND+0) = WRKBUF(TGAMTYP) !#37
        OUTTAB(IND+1) = WRKBUF(TGAMIND) !#38
        IND = IND + 2
C
C EXTERNAL EURO SERIAL AND CHECKDIGITS
C
        I4TEMP = WRKBUF(TEUEVWSER)
        OUTTAB(IND+0)=I1TEMP(3)   !#39   External (wager) serial #
        OUTTAB(IND+1)=I1TEMP(2)   !#40
        OUTTAB(IND+2)=I1TEMP(1)   !#41
        IND=IND+3
C
        OUTTAB(IND)= WRKBUF(TEUEVWCKD) !#42 CHECK DIGITS
        IND=IND+1
C
C OFFSET FIRST CDC DATE
C
        I4TEMP = WRKBUF(TEUVEOFS1)
        OUTTAB(IND+0) = I1TEMP(2)        ! #43
        OUTTAB(IND+1) = I1TEMP(1)        ! #44
C
C OFFSET SECOND CDC DATE
C
        I4TEMP = WRKBUF(TEUVEOFS2)
        OUTTAB(IND+2) = I1TEMP(2)        ! #45
        OUTTAB(IND+3) = I1TEMP(1)        ! #46
        IND=IND+4
C
C FIRST WEEK AND YEAR DRAW DATE
C
        I4TEMP = WRKBUF(TEUVEBEGW)
        OUTTAB(IND+0) = I1TEMP(1)        ! #47
        I4TEMP = WRKBUF(TEUVEBEGY)
        OUTTAB(IND+1) = I1TEMP(1)        ! #48
        IND = IND + 2
C
C SECOND WEEK AND YEAR DRAW DATE
C
        I4TEMP = WRKBUF(TEUVEENDW)
        OUTTAB(IND+0) = I1TEMP(1)        ! #49
        I4TEMP = WRKBUF(TEUVEENDY)
        OUTTAB(IND+1) = I1TEMP(1)        ! #50
        IND = IND + 2
C
C TIME
C
        I4TEMP = WRKBUF(TEUVETIMEH)
        OUTTAB(IND+0) = I1TEMP(1)        ! #51
        I4TEMP = WRKBUF(TEUVETIMEM)
        OUTTAB(IND+1) = I1TEMP(1)        ! #52
        I4TEMP = WRKBUF(TEUVETIMES)       
        OUTTAB(IND+2) = I1TEMP(1)        ! #53
        IND = IND + 3
C
C OPTION FLAGS = 0
C
        OUTTAB(IND+0) = 0               ! #54
        IND = IND + 1
C
C 4 PART - WAGER TERMINAL TO HOST
C
C
C GAME INDEX SYSTEM TYPE
C
        I4TEMP=ISHFT(WRKBUF(TGAMIND),4)
        IF ((WRKBUF(TEUVENBET) .EQ. 1) .AND. ((WRKBUF(TEUVENMK) .NE. 5) .OR. (WRKBUF(TEUVENST) .NE. 2))) THEN
           I4TEMP=I4TEMP + 1
        ELSE 
           I4TEMP=I4TEMP !game index & syst type
        ENDIF
        
        OUTTAB(IND)=I4TEMP              !#55
        IND=IND+1
C
C GET DURATION & NUMBER OF BOARDS
C
        I4TEMP=ISHFT(WRKBUF(TEUVEDUR),4)+WRKBUF(TEUVENBET) !duration & number of boards
        OUTTAB(IND)=I4TEMP              ! #56
        IND=IND+1
        
        EUROOPTIONS = 0
        EUROOPTIONS = EUROOPTIONS + EUROSN
        OUTTAB(IND) = EUROOPTIONS       ! #57
        OUTTAB(IND + 1 ) = 0            ! #58
C
C GET QUICK PICK FLAG 
C
        IF (WRKBUF(TEUVEQP) .NE. 0) THEN
           EUROOPTIONS = EUROOPTIONS + EUROQP
           OUTTAB(IND) = EUROOPTIONS    ! #59
           I4TEMP=WRKBUF(TEUVEQP)
           OUTTAB(IND+2)=I1TEMP(2)      ! #60
           OUTTAB(IND+3)=I1TEMP(1)      ! #61
           IND=IND + 2
        ENDIF
        IND = IND + 2
C
C GET SYSTEM NUMBER 
C 
        OUTTAB(IND) = WRKBUF(TEUVENMK)          ! #59 OR 62
        OUTTAB(IND + 1) = WRKBUF(TEUVENST)      ! #60 OR 63
        IND = IND + 2
C
C SET MONDAY FLAG INDICATOR AND TRANSACTION ID
C
        OUTTAB(IND) = 0                 ! #61 OR 64
        IND = IND +1 
C
C GET BOARD 
C
        DO I=TEUVEBOARD,120
           IF(WRKBUF(I) .NE. 0) THEN
              CALL MOVBYT(WRKBUF(I),1,OUTTAB,IND,4) 
              IND=IND+4  ! #62 OR 65
           ENDIF
        ENDDO 
        GOTO 5000
        
        
C WAGER REPRINT
C -------------
1000    CONTINUE
        OUTTAB(2)=WAGTYP
        IF(SUB.EQ.5) OUTTAB(2)=GRPTYP
        IND = 5
C
C GAME TYPE AND INDEX
C
        OUTTAB(IND+0) = WRKBUF(TGAMTYP)
        OUTTAB(IND+1) = WRKBUF(TGAMIND)
        IND = IND + 2
C
C IF PASSIVE THEN REPRINT IS FOR SALE ONLY
C
        IF(WRKBUF(TGAMTYP).EQ.TPAS) THEN
           IF(WRKBUF(TWEPOP).NE.EPASSAL) THEN
              OUTTAB(2) = ERRTYP
              OUTTAB(5) = INVL
              OUTTAB(6) = 0
              IND=7
              GOTO 5000  
           ENDIF
        ENDIF
C
C BUILD WAGER BODY MESSAGE.
C
        CALL WAGER_BODY(WRKBUF,OUTTAB,IND)
        GOTO 5000
C
C CANCELLATION REPRINT
C --------------------
2000    CONTINUE

        OUTTAB(2)=CANTYP

        IND=5

C 1RST PART - FROM HOST-->TERMINAL MESSAGE
C ----------------------------------------

C       GET TIME
C       --------
        CALL PUTIME(WRKBUF(TTIM),OUTTAB,IND)

C       GET SERIAL #
C       ------------
        CALL OUTGEN(WRKBUF(TCDC),WRKBUF(TSER),I4TEMP,CHECK)
        OUTTAB(IND+0)=I1TEMP(3)
        OUTTAB(IND+1)=I1TEMP(2)
        OUTTAB(IND+2)=I1TEMP(1)
        IND=IND+3

C       GET CHECK DIGITS
C       ----------------
        OUTTAB(IND)=CHECK
        IND=IND+1

C       GET GAME TYPE & GAME INDEX
C       --------------------------
        OUTTAB(IND)=WRKBUF(TGAMTYP)
        OUTTAB(IND+1)=WRKBUF(TGAMIND)
        IND=IND+2

C       GET CANCEL STATUS (ALWAYS=0)
C       ----------------------------
        OUTTAB(IND)=0
        IND=IND+1

C       GET CANCEL AMOUNT
C       -----------------
        I4TEMP=WRKBUF(TWTOT)-WRKBUF(TWDAMT)
        OUTTAB(IND+0)=I1TEMP(4)
        OUTTAB(IND+1)=I1TEMP(3)
        OUTTAB(IND+2)=I1TEMP(2)
        OUTTAB(IND+3)=I1TEMP(1)
        IND=IND+4

C       GET MONDAY FLAG INDICATOR
C       -------------------------
        I4TEMP = 0
        IF(WRKBUF(TGAMTYP) .EQ. TLTO) I4TEMP = WRKBUF(TWLMFI)
        OUTTAB(IND) = I1TEMP(1)
        IND = IND + 1  

C 2ND PART - FROM TERMINAL-->HOST MESSAGE
C ---------------------------------------

C       GET WAGER SERIAL #
C       ------------------
        CALL OUTGEN(WRKBUF(TCDC),WRKBUF(TWCSER),I4TEMP,CHECK)
        OUTTAB(IND+0)=I1TEMP(3)
        OUTTAB(IND+1)=I1TEMP(2)
        OUTTAB(IND+2)=I1TEMP(1)
        IND=IND+3

C       GET CHECK DIGITS
C       ----------------
        CALL ISBYTE(CHECK,OUTTAB,IND-1)
        IND=IND+1

        GOTO 5000

C VALIDATION REPRINT  (CASH OR CLAIM REPRINT)
C -------------------------------------------

3000    CONTINUE

        IF(WRKBUF(TGAMTYP).EQ.TPAS) GOTO 4000
C-------->>V59 --------------------------
!       OUTTAB(2)=VALTYP
        IF(WRKBUF(TVTYPE).EQ.VNBNK) THEN
          OUTTAB(2)=VBKTYP       ! ONLINE GAMES BANK VALIDATION
C----+------------------------------------------------------------------
C V63| Included new message subtype for new validations' reprint
C----+------------------------------------------------------------------
        ELSEIF(  WRKBUF(TVTYPE).EQ.VNDON) THEN
          OUTTAB(2)=VNVTYP
        ELSE
          OUTTAB(2)=VALTYP
C----+------------------------------------------------------------------
C V63| Included new message subtype for new validations' reprint
C----+------------------------------------------------------------------
        ENDIF
C-------- V59<<--------------------------
        IND=5

C 1RST PART - FROM HOST-->TERMINAL MESSAGE
C ----------------------------------------

C       GET TIME
C       --------

        CALL PUTIME(WRKBUF(TTIM),OUTTAB,IND)

C       GET SERIAL #
C       ------------
        CALL OUTGEN(WRKBUF(TCDC),WRKBUF(TSER),I4TEMP,CHECK)
        OUTTAB(IND+0)=I1TEMP(3)
        OUTTAB(IND+1)=I1TEMP(2)
        OUTTAB(IND+2)=I1TEMP(1)
        IND=IND+3

C       GET CHECK DIGITS
C       ----------------
        OUTTAB(IND)=CHECK
        IND=IND+1

C       GET GAME TYPE & GAME INDEX
C       --------------------------
        OUTTAB(IND)=WRKBUF(TGAMTYP)
        OUTTAB(IND+1)=WRKBUF(TGAMIND)
        IND=IND+2

C       GET VALIDATION CODE
C       -------------------
        VCODE=10                            !NO EXCHANGE
        IF(WRKBUF(TVEXC).NE.0) VCODE = 11   !WITH EXCHANGE

        IF(WRKBUF(TVSTS).EQ.VSBNK.OR.WRKBUF(TVSTS).EQ.VSBNKM) VCODE=12 !BNK INFO
     
        OUTTAB(IND)=VCODE
        IND=IND+1

C       GET CASH AMOUNTS (VALIDATION UNITS)
C       -----------------------------------
C***    AMTCLM=0
C***    AMTWON=0
C***    AMTREF=WRKBUF(TVREF)
        OPSAMT=WRKBUF(TVOPPAY)+WRKBUF(TVKOPPAY)  !V16
C-------->>V59 ---------------------------------------------------------
        IF((WRKBUF(TVTYPE).EQ.VNDON .OR. WRKBUF(TVTYPE).EQ.VNBNK) .AND.
     *      WRKBUF(TVPAY)+WRKBUF(TVKPAY) .LE. P(VALPRZHI)) THEN
          OPSAMT=0
        ENDIF
C-------- V59<<---------------------------------------------------------

C***    IF(WRKBUF(TTYP).EQ.TVAL) AMTWON=WRKBUF(TVPAY)+WRKBUF(TVKPAY)
        AMTWON=WRKBUF(TVPAY)+WRKBUF(TVKPAY)

C***    There is no CLM in Finland.
C***    IF(WRKBUF(TTYP).EQ.TCLM) AMTCLM=WRKBUF(TVPAY)+WRKBUF(TVKPAY)
C***    I4TEMP=AMTCLM
C***    OUTTAB(IND+0)=I1TEMP(4)
C***    OUTTAB(IND+1)=I1TEMP(3)
C***    OUTTAB(IND+2)=I1TEMP(2)
C***    OUTTAB(IND+3)=I1TEMP(1)
C***    IND=IND+4

        IF (SEPARATED_REFUND) THEN
           I4TEMP=AMTWON 
           OUTTAB(IND+0)=I1TEMP(4)
           OUTTAB(IND+1)=I1TEMP(3)
           OUTTAB(IND+2)=I1TEMP(2)
           OUTTAB(IND+3)=I1TEMP(1)
           IND=IND+4

C***       I4TEMP=AMTREF
           I4TEMP=OPSAMT              !V16
           OUTTAB(IND+0)=I1TEMP(4)
           OUTTAB(IND+1)=I1TEMP(3)
           OUTTAB(IND+2)=I1TEMP(2)
           OUTTAB(IND+3)=I1TEMP(1)
           IND=IND+4
        ELSE
C***       I4TEMP=AMTWON + AMTREF
           I4TEMP=AMTWON
           OUTTAB(IND+0)=I1TEMP(4)
           OUTTAB(IND+1)=I1TEMP(3)
           OUTTAB(IND+2)=I1TEMP(2)
           OUTTAB(IND+3)=I1TEMP(1)
           IND=IND+4
        ENDIF

        IF(WRKBUF(TVSTS).EQ.VSBNK.OR.WRKBUF(TVSTS).EQ.VSBNKM) THEN
           I4TEMP = WRKBUF(TVBNKID)
           OUTTAB(IND+0) = I1TEMP(4)
           OUTTAB(IND+1) = I1TEMP(3)
           OUTTAB(IND+2) = I1TEMP(2)
           OUTTAB(IND+3) = I1TEMP(1)
           IND = IND + 4

           I4TEMP = WRKBUF(TVBNKNUM)
           OUTTAB(IND+0) = I1TEMP(4)
           OUTTAB(IND+1) = I1TEMP(3)
           OUTTAB(IND+2) = I1TEMP(2)
           OUTTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
        ENDIF
 
C 2ND PART - FROM TERMINAL-->HOST MESSAGE
C ---------------------------------------
C
C       GET WAGER JULIAN DATE
C       ---------------------
        DATE(VCDC)=WRKBUF(TVCDC)
        CALL CDATE(DATE)
        I4TEMP=DATE(VJUL)
        OUTTAB(IND+0)=I1TEMP(2)
        OUTTAB(IND+1)=I1TEMP(1)
        IND=IND+2
    
C       GET VALIDATION SERIAL #
C       -----------------------
        CALL OUTGEN(WRKBUF(TVCDC),WRKBUF(TVSER),I4TEMP,CHECK)
        OUTTAB(IND+0)=I1TEMP(3)
        OUTTAB(IND+1)=I1TEMP(2)
        OUTTAB(IND+2)=I1TEMP(1)
        IND=IND+3

C       GET CHECK DIGITS
C       ----------------
        OUTTAB(IND)=CHECK
        IND=IND+1
C
C-------->>V59 --------------------------
C
C BANK VALIDATION
C
      IF(WRKBUF(TVTYPE).EQ.VNBNK) THEN
C
C PLAYER ID TYPE
C
        OUTTAB(IND) = WRKBUF(TVPLIDTYP)
        IND=IND+1
C
C PLAYER CARD/TELEPHONE NUMBER
C
        I4TEMP = WRKBUF(TVPLCARD)
        OUTTAB(IND+0)=I1TEMP(4) 
        OUTTAB(IND+1)=I1TEMP(3)
        OUTTAB(IND+2)=I1TEMP(2)
        OUTTAB(IND+3)=I1TEMP(1)
        IND=IND+4
C
C NIB
C
        I4TEMP = WRKBUF(TVNIBBB)
        OUTTAB(IND+0)=I1TEMP(2)
        OUTTAB(IND+1)=I1TEMP(1)
        IND=IND+2

        I4TEMP = WRKBUF(TVNIBBO)
        OUTTAB(IND+0)=I1TEMP(2)
        OUTTAB(IND+1)=I1TEMP(1)
        IND=IND+2

        NIBBA1 = WRKBUF(TVNIBBA1)
        NIBBA2 = WRKBUF(TVNIBBA2)
        NIB = NIBBA1*100 + NIBBA2
        OUTTAB(IND+0)=I1NIB(5) 
        OUTTAB(IND+1)=I1NIB(4)
        OUTTAB(IND+2)=I1NIB(3)
        OUTTAB(IND+3)=I1NIB(2)
        OUTTAB(IND+4)=I1NIB(1)  
        IND=IND+5

        OUTTAB(IND+0)=WRKBUF(TVNIBCD)
        IND=IND+1
      ENDIF
C
C-------- V59<<--------------------------

C IF EXCHANGE CODE, THEN REPRINT EXCHANGE TICKET
C ----------------------------------------------

        IF(VCODE.NE.11) GOTO 5000

C 3RD PART - FROM HOST-->TERMINAL MESSAGE
C ----------------------------------------
        CALL RLOG(WRKBUF(TVEXC),LOGREC,SPE,ST)
        IF(ST.NE.0) THEN
          TRABUF(TERR)=INVL
          TRABUF(TSTAT)=REJT
          OUTTAB(2)=ERRTYP
          OUTTAB(5)=TRABUF(TERR)
          OUTTAB(6)=0
          IND=7
          GOTO 5000
        ENDIF
        CALL LOGTRA(WRKBUF,LOGREC)
C
C GAME TYPE AND INDEX
C
        OUTTAB(IND+0) = WRKBUF(TGAMTYP)
        OUTTAB(IND+1) = WRKBUF(TGAMIND)
        IND = IND + 2
C
C BUILD WAGER BODY MESSAGE.
C
        CALL WAGER_BODY(WRKBUF,OUTTAB,IND)
        GOTO 5000
C
C INSTANT VALIDATION REPRINTS
C
3500    CONTINUE
        OUTTAB(2)=INSTYP
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      IPS VALIDATION C2T - PART 2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C TIME
C
        IND=5
        I4TEMP=WRKBUF(TTIM)
        OUTTAB(IND+0) = I1TEMP(3)
        OUTTAB(IND+1) = I1TEMP(2)
        OUTTAB(IND+2) = I1TEMP(1)
        IND=IND+3
C
C JULIAN DATE
C
        I4TEMP=DAYJUL
        OUTTAB(IND+0) = I1TEMP(2)
        OUTTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
C SERIAL NUMBER AND CHECK DIGITS
C
        CALL OUTGEN(WRKBUF(TCDC),WRKBUF(TSER),I4TEMP,CHECK)
        OUTTAB(IND+0) = I1TEMP(3)
        OUTTAB(IND+1) = I1TEMP(2)
        OUTTAB(IND+2) = I1TEMP(1)
        OUTTAB(IND+3) = CHECK
        IND=IND+4
C
C INSTANT RESULT CODE ONE
C
        IF(WRKBUF(TIERR).EQ.INOER .OR.
     *     WRKBUF(TIERR).EQ.INQRY) THEN
           I4TEMP=0
        ELSE
           I4TEMP=1
        ENDIF
        OUTTAB(IND+0) = I1TEMP(1)
        IND=IND+1
C
C INSTANT RESULT CODE TWO
C
        I4TEMP = WRKBUF(TIBCH) 
        IF(WRKBUF(TIERR).NE.INOER) I4TEMP = WRKBUF(TIERR)
        OUTTAB(IND+0) = I1TEMP(1)
        IND=IND+1
C
C-------->>V60 ---------------------------------------------------------
        IF(WRKBUF(TIVMT) .EQ. IBVMT) THEN
        
C
C INSTANT OPTION DATA
C
        IVOFLG_C2T = 0   !RESET OPTION FLAGS
        IVOFLG_IND = IND !SAVE OPTION FLAGS INDEX
        IND = IND + 1    !OPTION FLAG IS 1 BYTE LONG
C
C SET BANK VALIDATION MODE FLAG
C
          IF(WRKBUF(TISTS1).EQ.INOER) THEN
            IVOFLG_C2T = IOR(IVBVM_C2T,IVOFLG_C2T)
C
C BANK VALIDATION MODE - 1 BYTE
C
            I4TEMP = WRKBUF(TIVALM)
            OUTTAB(IND+0) = I1TEMP(1)
            IND = IND + 1
          ENDIF
C
C SET NET CASH AMOUNT BIT FLAG
C
          IF(TSBIT(AGTTAB(AGTTYP,WRKBUF(TTER)),AGTPRV) .AND. WRKBUF(TISTS1).EQ.INOER) THEN
            IF(WRKBUF(TIPRZ1).GT.P(VALPRZHI) .AND.
     *         WRKBUF(TINETPRZ).GT.0) THEN
C
C SET NET CASH AMOUNT FLAG
C
              IVOFLG_C2T = IOR(IVNCA_C2T, IVOFLG_C2T)
C
C NET CASH AMOUNT - 4 BYTES
C
              I4TEMP = WRKBUF(TINETPRZ)
              OUTTAB(IND+0) = I1TEMP(4)
              OUTTAB(IND+1) = I1TEMP(3)
              OUTTAB(IND+2) = I1TEMP(2)
              OUTTAB(IND+3) = I1TEMP(1)
              IND = IND + 4
            ENDIF
          ENDIF
C
C INSTANT OPTION FLAGS - 1 BYTE
C
          I4TEMP = ZEXT(IVOFLG_C2T)
          OUTTAB(IVOFLG_IND+0) = I1TEMP(1)
C
C VALIDATION STATUS
C
          I4TEMP = WRKBUF(TISTS1)
          IF(WRKBUF(TISTS1).EQ.INOER) I4TEMP = 0
          OUTTAB(IND+0)=I1TEMP(1)
          IND=IND+1
C
C AMOUNT WON 
C
          I4TEMP=WRKBUF(TIPRZ1)
          OUTTAB(IND+0) = I1TEMP(4)
          OUTTAB(IND+1) = I1TEMP(3)
          OUTTAB(IND+2) = I1TEMP(2)
          OUTTAB(IND+3) = I1TEMP(1)
          IND=IND+4
C
C TICKET STATUS
C
          I4TEMP=WRKBUF(TIPCKSTS1)
          OUTTAB(IND+0) = I1TEMP(1)
          IND = IND + 1
        
        ELSEIF(WRKBUF(TIVMT) .EQ. IRVMT) THEN !OLD BANK VALIDATION MODE
C
C-------- V60<<---------------------------------------------------------
C
C INSTANT OPTION FLAG
C
          OPTION = 0
          I4TEMP = OPTION 
          OUTTAB(IND+0)=I1TEMP(1)
          IND=IND+1
C
C VALIDATION STATUS
C
          DO 3600 I=0,WRKBUF(TIBCH)-1
             I4TEMP = WRKBUF(TISTS1+I)
             IF(WRKBUF(TISTS1+I).EQ.INOER) I4TEMP = 0
             OUTTAB(IND+0)=I1TEMP(1)
             IND=IND+1
C
C AMOUNT WON 
C
             I4TEMP=WRKBUF(TIPRZ1+I)
             OUTTAB(IND+0) = I1TEMP(4)
             OUTTAB(IND+1) = I1TEMP(3)
             OUTTAB(IND+2) = I1TEMP(2)
             OUTTAB(IND+3) = I1TEMP(1)
             IND=IND+4

             I4TEMP=WRKBUF(TIPCKSTS1+I)
C          OUTTAB(IND+0) = I1TEMP(2)
             OUTTAB(IND+0) = I1TEMP(1)
             IND = IND + 1
3600      CONTINUE
        ENDIF
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      IPS VALIDATION T2C - PART 3
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C-------->>V60 ---------------------------------------------------------
C
C OPTION DATA
C
        IVOFLG_T2C = 0   !RESET OPTION FLAGS
        IVOFLG_IND = IND !SAVE OPTION FLAGS INDEX
        IND = IND + 2    !OPTION FLAG IS 2 BYTES LONG
        IF(WRKBUF(TIVMT) .EQ. IBVMT) THEN
C
C SET BANK VALIDATION MODE FLAG
C
          IVOFLG_T2C = IOR(IVBVM_T2C,IVOFLG_T2C)
C
C BANK VALIDATION MODE - 1 BYTE
C
          I4TEMP = WRKBUF(TIVALM)
          OUTTAB(IND+0) = I1TEMP(1)
          IND = IND + 1
          IF(WRKBUF(TIVALM) .EQ. IVBM_BNK) THEN !BANK TRANSFER ACCEPTED
C
C SET BANK DATA FLAG
C
            IVOFLG_T2C = IOR(IVBNK_T2C, IVOFLG_T2C)
C
C PLAYER ID TYPE - 1 BYTE
C
            I4TEMP=WRKBUF(TIPLIDTYP)
            OUTTAB(IND+0) = I1TEMP(1)
            IND = IND + 1
C
C PLAYER ID - 4 BYTES
C
            I4TEMP=WRKBUF(TIPLCARD)
            OUTTAB(IND+0) = I1TEMP(4)
            OUTTAB(IND+1) = I1TEMP(3)
            OUTTAB(IND+2) = I1TEMP(2)
            OUTTAB(IND+3) = I1TEMP(1)
            IND = IND + 4
C
C BRANCH - 2 BYTES
C
            I4TEMP=WRKBUF(TINIBBB)
            OUTTAB(IND+0) = I1TEMP(2)
            OUTTAB(IND+1) = I1TEMP(1)
            IND = IND + 2
C
C OFFICE - 2 BYTES
C
            I4TEMP=WRKBUF(TINIBBO)
            OUTTAB(IND+0) = I1TEMP(2)
            OUTTAB(IND+1) = I1TEMP(1)
            IND = IND + 2
C
C ACCOUNT NUMBER - 5 BYTES
C
            NIBBA1 = WRKBUF(TINIBBA1)
            NIBBA2 = WRKBUF(TINIBBA2)
            NIB = NIBBA1*100 + NIBBA2
            OUTTAB(IND+0)=I1NIB(5)
            OUTTAB(IND+1)=I1NIB(4)
            OUTTAB(IND+2)=I1NIB(3)
            OUTTAB(IND+3)=I1NIB(2)
            OUTTAB(IND+4)=I1NIB(1)
            IND = IND + 5
C
C CHECK DIGITS - 1 BYTE
C
            I4TEMP=WRKBUF(TINIBCD)
            OUTTAB(IND+0) = I1TEMP(1)
            IND = IND + 1
          ENDIF
        ENDIF
C
C INSTANT OPTION FLAGS - 2 BYTES
C
        I4TEMP = ZEXT(IVOFLG_T2C)
        OUTTAB(IVOFLG_IND+0) = I1TEMP(2)
        OUTTAB(IVOFLG_IND+1) = I1TEMP(1)
C
C-------- V60<<---------------------------------------------------------
C
C BATCH FLAG
C
        I4TEMP=WRKBUF(TIIND)
        OUTTAB(IND+0) = I1TEMP(1)
        IND=IND+1
C
C RETAILER NUMBER 
C
        I4TEMP = WRKBUF(TIVAGT)
        OUTTAB(IND + 0) = I1TEMP(4)
        OUTTAB(IND + 1) = I1TEMP(3)
        OUTTAB(IND + 2) = I1TEMP(2)
        OUTTAB(IND + 3) = I1TEMP(1)
        IND = IND + 4
C
C VALIDATION MODE
C
        I4TEMP = WRKBUF(TIVALM)
        OUTTAB(IND + 0) = I1TEMP(1)
        IND = IND + 1
C
C ENVELOPE INDENTIFICATION NUMBER
C
        I4TEMP = WRKBUF(TIVENV)
        OUTTAB(IND + 0) = I1TEMP(4)
        OUTTAB(IND + 1) = I1TEMP(3)
        OUTTAB(IND + 2) = I1TEMP(2)
        OUTTAB(IND + 3) = I1TEMP(1)
        IND = IND + 4
C
C NUMBER OF VALIDATIONS IN BATCH
C
        I4TEMP=WRKBUF(TIBCH)
        OUTTAB(IND+0) = I1TEMP(1)
        IND=IND+1
C
C INSTANT GAME NUMBER
C
        DO 3700 I=0,WRKBUF(TIBCH)-1
           I4TEMP=WRKBUF(TIGAM1+I)
           OUTTAB(IND+0) = I1TEMP(2)
           OUTTAB(IND+1) = I1TEMP(1)
           IND=IND+2
C
C INSTANT LOT/PACK NUMBER
C
           I4TEMP=WRKBUF(TIPCK1+I)
           OUTTAB(IND+0) = I1TEMP(4)
           OUTTAB(IND+1) = I1TEMP(3)
           OUTTAB(IND+2) = I1TEMP(2)
           OUTTAB(IND+3) = I1TEMP(1)
           IND=IND+4
C
C INSTANT VIRN NUMBER (9 DIGITS)
C
           I4TEMP=WRKBUF(TIVRN1+I)
           OUTTAB(IND+0) = I1TEMP(4)
           OUTTAB(IND+1) = I1TEMP(3)
           OUTTAB(IND+2) = I1TEMP(2)
           OUTTAB(IND+3) = I1TEMP(1)
           IND=IND+4
C
C INSTANT LATEX NUMBER
C
           I4TEMP=WRKBUF(TILTX1+I)
           OUTTAB(IND+0) = I1TEMP(2)
           OUTTAB(IND+1) = I1TEMP(1)
           IND=IND+2
C
C INSTANT AMOUNT (NOT USED)
C
C-------->>V61 ---------------------------------------------------------
           I4TEMP=0
!           OUTTAB(IND+0) = I1TEMP(2)
!           OUTTAB(IND+1) = I1TEMP(1)
!           IND=IND+2
           OUTTAB(IND+0) = I1TEMP(4)
           OUTTAB(IND+1) = I1TEMP(3)
           OUTTAB(IND+2) = I1TEMP(2)
           OUTTAB(IND+3) = I1TEMP(1)
           IND=IND+4
C-------- V61<<---------------------------------------------------------
C
C INSTANT TIME CASHED ( TIME IS SET BYT HOST, NOT BY TERMINAL )
C
C           I4TEMP=WRKBUF(TITIM1+I)/2
C           OUTTAB(IND+0) = I1TEMP(2)
C           OUTTAB(IND+1) = I1TEMP(1)
C           IND=IND+2
C
C INSTANT CDC CASHED ( CDC IS SET BY HOST, NOT BY TERMINAL )
C
C           I4TEMP=WRKBUF(TICDC1+I)
C           OUTTAB(IND+0) = I1TEMP(2)
C           OUTTAB(IND+1) = I1TEMP(1)
C           IND=IND+2
3700    CONTINUE
C
        OUTLEN=IND-1
        GOTO 5000
C
C INSTANT SUPPLY ORDER REPRINTS
C -----------------------------
C
6000    CONTINUE
        OUTTAB(2)=ORDTYP
C
C TIME
C
        IND=5
        I4TEMP=WRKBUF(TTIM)
        OUTTAB(IND+0) = I1TEMP(3)
        OUTTAB(IND+1) = I1TEMP(2)
        OUTTAB(IND+2) = I1TEMP(1)
        IND=IND+3
C
C JULIAN DATE
C
        I4TEMP=DAYJUL
        OUTTAB(IND+0) = I1TEMP(2)
        OUTTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
C SERIAL NUMBER AND CHECK DIGITS
C
        CALL OUTGEN(WRKBUF(TCDC),WRKBUF(TSER),I4TEMP,CHECK)
        OUTTAB(IND+0) = I1TEMP(3)
        OUTTAB(IND+1) = I1TEMP(2)
        OUTTAB(IND+2) = I1TEMP(1)
        OUTTAB(IND+3) = CHECK
        IND=IND+4
C
C INSTANT RESULT CODE ONE
C
        I4TEMP=0
        IF(WRKBUF(TIERR).NE.INOER) I4TEMP=1
        OUTTAB(IND+0) = I1TEMP(1)
        IND=IND+1
C
C INSTANT RESULT CODE TWO
C
        I4TEMP=0
        IF(WRKBUF(TIERR).NE.INOER) I4TEMP=WRKBUF(TIERR)
        OUTTAB(IND+0) = I1TEMP(1)
        IND=IND+1
C
C RESULT INFO
C
        I4TEMP=WRKBUF(TSINF)
        OUTTAB(IND+0) = I1TEMP(4)
        OUTTAB(IND+1) = I1TEMP(3)
        OUTTAB(IND+2) = I1TEMP(2)
        OUTTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
C ORDER NUMBER
C
        I4TEMP=WRKBUF(TSORD)
        OUTTAB(IND+0) = I1TEMP(4)
        OUTTAB(IND+1) = I1TEMP(3)
        OUTTAB(IND+2) = I1TEMP(2)
        OUTTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
C NUMBER OF ORDERS IN BATCH
C
        I4TEMP=WRKBUF(TIBCH)
        OUTTAB(IND+0) = I1TEMP(2)
        OUTTAB(IND+1) = I1TEMP(1)
        IND=IND+2
C
C OUT OF STOCK FLAGS
C
        DO 6050 I=0,WRKBUF(TIBCH)-1
C
          I4TEMP=0
          IF(TSBIT(TRABUF(TSSTK1),I)) I4TEMP=1
          OUTTAB(IND+0) = I1TEMP(1)
          IND=IND+1
C
6050    CONTINUE
C
C RETAILER NUMBER
C
        I4TEMP=WRKBUF(TAGT)
        OUTTAB(IND+0) = I1TEMP(4)
        OUTTAB(IND+1) = I1TEMP(3)
        OUTTAB(IND+2) = I1TEMP(2)
        OUTTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
C NUMBER OF ORDERS IN BATCH
C
        I4TEMP=WRKBUF(TIBCH)
        OUTTAB(IND+0) = I1TEMP(2)
        OUTTAB(IND+1) = I1TEMP(1)
        IND=IND+2

C
C INSTANT GAME (Last nibble: game type; First 3 nibbles: game number)
C
        DO 6100 I=0,WRKBUF(TIBCH)-1
C
          I4TEMP=WRKBUF(TSGAM+I)
          OUTTAB(IND+0) = I1TEMP(2)
          OUTTAB(IND+1) = I1TEMP(1)
          IND=IND+2
C
C SUPPLY ORDER QUANTITY
C
          I4TEMP=WRKBUF(TSQTY+I)
          OUTTAB(IND+0) = I1TEMP(2)
          OUTTAB(IND+1) = I1TEMP(1)
          IND=IND+2
C
6100    CONTINUE
C
        OUTLEN=IND-1
        GOTO 5000
C
C SEND INFO FOR PASSIVE LOTTERY (VALIDATIONS AND RETURNED TICKETS)
C 1ST PART - FROM HOST-->TERMINAL MESSAGE
C
4000    CONTINUE
C
        IF(WRKBUF(TTYP).EQ.TRET) THEN
            OUTTAB(2) = RPATYP          ! RETURN PASSIVE TYPE
        ELSE
            IF(WRKBUF(TVTYPE).EQ.VPNBNK) THEN
               OUTTAB(2) = BPATYP       ! PASSIVE BANK VALIDATION    
            ELSE  
               OUTTAB(2) = VPATYP       ! VALIDATION PASSIVE TYPE
            ENDIF
        ENDIF
        IND  = 5
C
C TIME
C
        I4TEMP = WRKBUF(TTIM)
        OUTTAB(IND+0) = I1TEMP(3)
        OUTTAB(IND+1) = I1TEMP(2)
        OUTTAB(IND+2) = I1TEMP(1)
        IND  = IND+3
C
C VALIDATION SERIAL NUMBER
C
        CALL OUTGEN(WRKBUF(TCDC),WRKBUF(TSER),I4TEMP,CHECK)
        OUTTAB(IND+0) = I1TEMP(3)
        OUTTAB(IND+1) = I1TEMP(2)
        OUTTAB(IND+2) = I1TEMP(1)
        IND  = IND+3
C
C VALIDATION CHECK DIGITS
C
        OUTTAB(IND) = CHECK
        IND  = IND+1
C
C SAVE IND TO TOTAL AMOUNT (VALIDATIONS OR RETURNED TICKETS)
C
        TOTIND = IND
        IND  = IND + 4
C
C GAME INDEX AND RETURN TYPE
C
        IF(WRKBUF(TTYP).EQ.TVAL) WRKBUF(TPRETYP) = 0

        GIND        = WRKBUF(TGAMIND)
        OUTTAB(IND) = IAND(WRKBUF(TPRETYP), '0F'X) +
     *                ISHFT(GIND,4)
        IND  = IND + 1
C
C OFFLINE AGENT #
C
        I4TEMP        = 0
        IF (WRKBUF(TPOFFTER).GT.0)  I4TEMP = AGTTAB(AGTNUM,WRKBUF(TPOFFTER))
        OUTTAB(IND+0) = I1TEMP(4)
        OUTTAB(IND+1) = I1TEMP(3)
        OUTTAB(IND+2) = I1TEMP(2)
        OUTTAB(IND+3) = I1TEMP(1)
        IND = IND + 4
C
C # OF TICKETS
C
        OUTTAB(IND) = WRKBUF(TPTCK)
        IND = IND + 1
C
C SEND INFO FOR EACH TICKET
C
        TOTVAL = 0
        DO  TCKS = 1, WRKBUF(TPTCK)
C
C STATUS (VALIDATION OR RETURNED)
C
            ST = WRKBUF(TPSTS1+OFFTRA*(TCKS-1))
            IF (ST.EQ.RETAFDR) ST = RETURND
            OUTTAB(IND) = ST
            IND = IND + 1
C
C CHECK EMISSION #
C
            WEEK   = 0
            YEAR   = 0
            FOUND  = .FALSE.
C
            I4TEMP = WRKBUF(TPEMIS1+OFFTRA*(TCKS-1))
            IF  (I4TEMP.GT.0) THEN
C
C CREATE AUXILIARY VARIABLES (OFFSET TO PASCOM)
C
                OFF_AUXEMIS = CURDRW   
                DO  WHILE(OFF_AUXEMIS.LE.PAGEMI .AND. .NOT.FOUND)
                    IF (PASEMIS(OFF_AUXEMIS,GIND).EQ.I4TEMP) THEN
                        FOUND = .TRUE.
                    ELSE
                        OFF_AUXEMIS = OFF_AUXEMIS + 1
                    ENDIF
                ENDDO
C
                IF  (FOUND) THEN
                    I4TEMP = PASDRAW(OFF_AUXEMIS,GIND)          !GET SCML DRAW #
                    CALL GETPASDRW(I4TEMP,WEEK,YEAR)
                ENDIF
            ENDIF

            OUTTAB(IND) = WEEK
            IND         = IND + 1
C
            OUTTAB(IND) = MOD(YEAR,100)
            IND         = IND + 1
C
C TICKET NUMBER
C
            I4TEMP = WRKBUF(TPNUM1+OFFTRA*(TCKS-1))
            OUTTAB(IND+0) = I1TEMP(4)
            OUTTAB(IND+1) = I1TEMP(3)
            OUTTAB(IND+2) = I1TEMP(2)
            OUTTAB(IND+3) = I1TEMP(1)
            IND = IND + 4
C
C SERIE
C
            OUTTAB(IND) = WRKBUF(TPSER1+OFFTRA*(TCKS-1))
            IND = IND + 1
C
C FRACTION (FIRST FRACTION IF GOOD RETURNS)
C
            OUTTAB(IND) = WRKBUF(TPTEN1+OFFTRA*(TCKS-1))
            IND = IND + 1
C
C AMOUNT (VALIDATIONS OR # OF RETURNED FRACTIONS)
C
            IF   (WRKBUF(TTYP).EQ.TRET) THEN
                 QTDFRAC = 0
                 IF ( FOUND ) THEN
                    IF ( WRKBUF(TPRETYP).EQ.ALLTCK ) THEN
                       QTDFRAC = PASNOFFRA(OFF_AUXEMIS,GIND)
                       IF  (WRKBUF(TGAMIND).EQ.PSBPOP)   QTDFRAC = QTDFRAC * 2

                    ELSEIF (WRKBUF(TPRETYP).EQ.BYFRAC ) THEN
                       QTDFRAC = 1

                    ELSEIF (WRKBUF(TPRETYP).EQ.HALFTCK) THEN

                        IF (WRKBUF(TGAMIND).EQ.PSBPOP) THEN
                            QTDFRAC = PASNOFFRA(OFF_AUXEMIS,GIND)
                        ELSE
                            QTDFRAC = PASNOFFRA(OFF_AUXEMIS,GIND) / 2
                        ENDIF

                    ELSEIF (WRKBUF(TPRETYP).EQ.QUARTCK) THEN
                        QTDFRAC = PASNOFFRA(OFF_AUXEMIS,GIND) / 4

                    ENDIF
                 ENDIF
                 I4TEMP = QTDFRAC                         ! USING ONLY ONE BYTE
            ELSE
                 I4TEMP = WRKBUF(TPPAY1+OFFTRA*(TCKS-1))  ! PRIZE AMOUNT !!!
            ENDIF

            OUTTAB(IND+0) = I1TEMP(4)
            OUTTAB(IND+1) = I1TEMP(3)
            OUTTAB(IND+2) = I1TEMP(2)
            OUTTAB(IND+3) = I1TEMP(1)
 
            IND  = IND + 4

            IF(  WRKBUF(TTYP).EQ.TVAL                       .AND.
     *           WRKBUF(TPSTS1+OFFTRA*(TCKS-1)) .EQ. VWINNER     )
     *           TOTVAL = TOTVAL + I4TEMP
        ENDDO
C
C TOTAL AMOUNT (FOR VALIDATIONS) AND 0 FOR RETURNED TICKETS
C
        I4TEMP           = TOTVAL
        OUTTAB(TOTIND+0) = I1TEMP(4)
        OUTTAB(TOTIND+1) = I1TEMP(3)
        OUTTAB(TOTIND+2) = I1TEMP(2)
        OUTTAB(TOTIND+3) = I1TEMP(1)

        IF(WRKBUF(TTYP).EQ.TRET) GOTO 5000
C
C-------->>V59 -----------------------------------------------------------------
C
C OPTION DATA
C
        PVOFLG = 0       !RESET OPTION FLAGS
        PVOFLG_IND = IND !SAVE OPTION FLAGS INDEX
        IND = IND + 1
        IF(WRKBUF(TPTCK).EQ.1 .AND. WRKBUF(TPPAY1).GT.P(VALPRZHI)) THEN
          PVOFLG = IOR(VPNCA, PVOFLG) !Z10000000 - NET CASH AMOUNT (VALIDATION UNITS) – 4 BYTES
          I4TEMP = WRKBUF(TVOPPAY)
          OUTTAB(IND+0) = I1TEMP(4)
          OUTTAB(IND+1) = I1TEMP(3)
          OUTTAB(IND+2) = I1TEMP(2)
          OUTTAB(IND+3) = I1TEMP(1)
          IND = IND + 4
        ENDIF
C
C OPTION FLAGS
C
        OUTTAB(PVOFLG_IND) = PVOFLG
C
C-------- V59<<-----------------------------------------------------------------
C
C WAGER JULIAN,SERIAL CHECK
C
        IF(WRKBUF(TVEPTYP).EQ.0) THEN
           OUTTAB(IND+0)=0             
           OUTTAB(IND+1)=0             
           OUTTAB(IND+2)=0             
           OUTTAB(IND+3)=0             
           OUTTAB(IND+4)=0             
           OUTTAB(IND+5)=0             
           IND=IND+6
        ELSE
           DATE(VCDC)=WRKBUF(TVCDC)
           CALL CDATE(DATE)
           I4TEMP=DATE(VJUL)
           OUTTAB(IND+0)=I1TEMP(2)
           OUTTAB(IND+1)=I1TEMP(1)
           IND=IND+2
C
           CALL OUTGEN(WRKBUF(TVCDC),WRKBUF(TVSER),I4TEMP,CHECK)
           OUTTAB(IND+0)=I1TEMP(3)
           OUTTAB(IND+1)=I1TEMP(2)
           OUTTAB(IND+2)=I1TEMP(1)
           IND=IND+3
C
           OUTTAB(IND)=CHECK
           IND=IND+1
        ENDIF           
C
C BANK VALIDATION
C PLAYER CARD
C
        IF(WRKBUF(TVTYPE).EQ.VPNBNK) THEN
C
C PLAYER ID TYPE !V59
C
              OUTTAB(IND) = WRKBUF(TVPLIDTYP) !V59
              IND=IND+1
C
C PLAYER CARD/TELEPHONE NUMBER
C
              I4TEMP = WRKBUF(TVPLCARD)
              OUTTAB(IND+0)=I1TEMP(4) 
              OUTTAB(IND+1)=I1TEMP(3)
              OUTTAB(IND+2)=I1TEMP(2)
              OUTTAB(IND+3)=I1TEMP(1)
              IND=IND+4
C
C NIB
C
              I4TEMP = WRKBUF(TVNIBBB)
              OUTTAB(IND+0)=I1TEMP(2)
              OUTTAB(IND+1)=I1TEMP(1)
              IND=IND+2

              I4TEMP = WRKBUF(TVNIBBO)
              OUTTAB(IND+0)=I1TEMP(2)
              OUTTAB(IND+1)=I1TEMP(1)
              IND=IND+2

              NIBBA1 = WRKBUF(TVNIBBA1)
              NIBBA2 = WRKBUF(TVNIBBA2)
              NIB = NIBBA1*100 + NIBBA2
              OUTTAB(IND+0)=I1NIB(5) 
              OUTTAB(IND+1)=I1NIB(4)
              OUTTAB(IND+2)=I1NIB(3)
              OUTTAB(IND+3)=I1NIB(2)
              OUTTAB(IND+4)=I1NIB(1)  
              IND=IND+5

              OUTTAB(IND+0)=WRKBUF(TVNIBCD)
              IND=IND+1
     
        ENDIF
C
        GOTO 5000
C----+------------------------------------------------------------------
C V64| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C----+------------------------------------------------------------------
C    |
C    |
C V64| Handling IGS wager reprints
C    |
C    |
C----+------------------------------------------------------------------
12100   CONTINUE
        IF(IGSDEBUG(IA_SPESRV)) THEN
            CALL OPSTXT('1975:RPRT: HANDLING IGS WAGER REPRINTS')
        ENDIF
C----+------------------------------------------------------------------
C V64| Getting terminal message id from input message
C----+------------------------------------------------------------------
        I1_MSG_ID(1) = OUTTAB( 5)
        I1_MSG_ID(2) = OUTTAB( 6)
        I1_MSG_ID(3) = OUTTAB( 7)
        I1_MSG_ID(4) = OUTTAB( 8)
        I1_MSG_ID(5) = OUTTAB( 9)
        I1_MSG_ID(6) = OUTTAB(10)
        I1_MSG_ID(7) = OUTTAB(11)
        I1_MSG_ID(8) = OUTTAB(12)
        IF(IGSDEBUG(IA_SPESRV)) THEN
            WRITE(LINE,99100) (I1_MSG_ID(I), I = 1,8)
99100       FORMAT('MSG ID = [ ',(8(Z2.2,1X)),']')
            CALL OPSTXT(TRIM(LINE))
        ENDIF
C----+------------------------------------------------------------------
C V64| Setting game type
C----+------------------------------------------------------------------
        OUTTAB( 5) = WRKBUF(TGAMTYP)
C----+------------------------------------------------------------------
C V64| Setting game index
C----+------------------------------------------------------------------
        OUTTAB( 6) = WRKBUF(TGAMIND)
C----+------------------------------------------------------------------
C V64| Setting Agent Id
C----+------------------------------------------------------------------
        I4TEMP = TRABUF(TAGT)
        OUTTAB( 7) = I1TEMP(4)
        OUTTAB( 8) = I1TEMP(3)
        OUTTAB( 9) = I1TEMP(2)
        OUTTAB(10) = I1TEMP(1)
C----+------------------------------------------------------------------
C V64| Setting Message Id
C----+------------------------------------------------------------------
        OUTTAB(11) = I1_MSG_ID(1)
        OUTTAB(12) = I1_MSG_ID(2)
        OUTTAB(13) = I1_MSG_ID(3)
        OUTTAB(14) = I1_MSG_ID(4)

        OUTTAB(15) = I1_MSG_ID(5)
        OUTTAB(16) = I1_MSG_ID(6)
        OUTTAB(17) = I1_MSG_ID(7)
        OUTTAB(18) = I1_MSG_ID(8)

        OUTLEN = 18
        CALL CALCULATE_MSG_CHECKSUM(TER,OUTTAB,OUTLEN)
        RETURN
C----+------------------------------------------------------------------
C    |
C    |
C V64| Handling IGS cancellation reprints
C    |
C    |
C----+------------------------------------------------------------------
12200   CONTINUE
        IF(IGSDEBUG(IA_SPESRV)) THEN
            CALL OPSTXT('2028:RPRT: HANDLING IGS CANCELLATION REPRINTS')
        ENDIF
C----+------------------------------------------------------------------
C V64| Getting terminal message id from input message
C----+------------------------------------------------------------------
        I1_MSG_ID(1) = OUTTAB( 5)
        I1_MSG_ID(2) = OUTTAB( 6)
        I1_MSG_ID(3) = OUTTAB( 7)
        I1_MSG_ID(4) = OUTTAB( 8)
        I1_MSG_ID(5) = OUTTAB( 9)
        I1_MSG_ID(6) = OUTTAB(10)
        I1_MSG_ID(7) = OUTTAB(11)
        I1_MSG_ID(8) = OUTTAB(12)
        IF(IGSDEBUG(IA_SPESRV)) THEN
            WRITE(LINE,99200) (I1_MSG_ID(I), I = 1,8)
99200       FORMAT('MSG ID = [ ',(8(Z2.2,1X)),']')
            CALL OPSTXT(TRIM(LINE))
        ENDIF
C----+------------------------------------------------------------------
C V64| Setting game type
C----+------------------------------------------------------------------
        OUTTAB( 5) = WRKBUF(TGAMTYP)
C----+------------------------------------------------------------------
C V64| Setting game index
C----+------------------------------------------------------------------
        OUTTAB( 6) = WRKBUF(TGAMIND)
C----+------------------------------------------------------------------
C V64| Setting Agent Id
C----+------------------------------------------------------------------
        I4TEMP = TRABUF(TAGT)
        OUTTAB( 7) = I1TEMP(4)
        OUTTAB( 8) = I1TEMP(3)
        OUTTAB( 9) = I1TEMP(2)
        OUTTAB(10) = I1TEMP(1)
C----+------------------------------------------------------------------
C V64| Setting Message Id
C----+------------------------------------------------------------------
        OUTTAB(11) = I1_MSG_ID(1)
        OUTTAB(12) = I1_MSG_ID(2)
        OUTTAB(13) = I1_MSG_ID(3)
        OUTTAB(14) = I1_MSG_ID(4)

        OUTTAB(15) = I1_MSG_ID(5)
        OUTTAB(16) = I1_MSG_ID(6)
        OUTTAB(17) = I1_MSG_ID(7)
        OUTTAB(18) = I1_MSG_ID(8)

        OUTLEN = 18
        CALL CALCULATE_MSG_CHECKSUM(TER,OUTTAB,OUTLEN)
        RETURN
C----+------------------------------------------------------------------
C    |
C    |
C V64| Handling IGS validation reprints
C    |
C    |
C----+------------------------------------------------------------------
12300   CONTINUE
        IF(IGSDEBUG(IA_SPESRV)) THEN
            CALL OPSTXT('2081:RPRT: HANDLING IGS VALIDATION REPRINTS')
        ENDIF
C----+------------------------------------------------------------------
C V64| Getting terminal message id from input message
C----+------------------------------------------------------------------
        I1_MSG_ID(1) = OUTTAB( 5)
        I1_MSG_ID(2) = OUTTAB( 6)
        I1_MSG_ID(3) = OUTTAB( 7)
        I1_MSG_ID(4) = OUTTAB( 8)
        I1_MSG_ID(5) = OUTTAB( 9)
        I1_MSG_ID(6) = OUTTAB(10)
        I1_MSG_ID(7) = OUTTAB(11)
        I1_MSG_ID(8) = OUTTAB(12)
        IF(IGSDEBUG(IA_SPESRV)) THEN
            WRITE(LINE,99300) (I1_MSG_ID(I), I = 1,8)
99300       FORMAT('MSG ID = [ ',(8(Z2.2,1X)),']')
            CALL OPSTXT(TRIM(LINE))
        ENDIF
C----+------------------------------------------------------------------
C V64| Setting game type
C----+------------------------------------------------------------------
        OUTTAB( 5) = WRKBUF(TGAMTYP)
C----+------------------------------------------------------------------
C V64| Setting game index
C----+------------------------------------------------------------------
        OUTTAB( 6) = WRKBUF(TGAMIND)
C----+------------------------------------------------------------------
C V64| Setting Agent Id
C----+------------------------------------------------------------------
        I4TEMP = TRABUF(TAGT)
        OUTTAB( 7) = I1TEMP(4)
        OUTTAB( 8) = I1TEMP(3)
        OUTTAB( 9) = I1TEMP(2)
        OUTTAB(10) = I1TEMP(1)
C----+------------------------------------------------------------------
C V64| Setting Message Id
C----+------------------------------------------------------------------
        OUTTAB(11) = I1_MSG_ID(1)
        OUTTAB(12) = I1_MSG_ID(2)
        OUTTAB(13) = I1_MSG_ID(3)
        OUTTAB(14) = I1_MSG_ID(4)

        OUTTAB(15) = I1_MSG_ID(5)
        OUTTAB(16) = I1_MSG_ID(6)
        OUTTAB(17) = I1_MSG_ID(7)
        OUTTAB(18) = I1_MSG_ID(8)

        OUTLEN = 18
        CALL CALCULATE_MSG_CHECKSUM(TER,OUTTAB,OUTLEN)
        RETURN
C----+------------------------------------------------------------------
C    |
C    |
C V64| Handling IGS payment reprints
C    |
C    |
C----+------------------------------------------------------------------
12400   CONTINUE
        IF(IGSDEBUG(IA_SPESRV)) THEN
            CALL OPSTXT('2134:RPRT: HANDLING IGS PAYMENT REPRINTS')
        ENDIF
C----+------------------------------------------------------------------
C V64| Getting terminal message id from input message
C----+------------------------------------------------------------------
        I1_MSG_ID(1) = OUTTAB( 5)
        I1_MSG_ID(2) = OUTTAB( 6)
        I1_MSG_ID(3) = OUTTAB( 7)
        I1_MSG_ID(4) = OUTTAB( 8)
        I1_MSG_ID(5) = OUTTAB( 9)
        I1_MSG_ID(6) = OUTTAB(10)
        I1_MSG_ID(7) = OUTTAB(11)
        I1_MSG_ID(8) = OUTTAB(12)
        IF(IGSDEBUG(IA_SPESRV)) THEN
            WRITE(LINE,99400) (I1_MSG_ID(I), I = 1,8)
99400       FORMAT('MSG ID = [ ',(8(Z2.2,1X)),']')
            CALL OPSTXT(TRIM(LINE))
        ENDIF
C----+------------------------------------------------------------------
C V64| Setting game type
C----+------------------------------------------------------------------
        OUTTAB( 5) = WRKBUF(TGAMTYP)
C----+------------------------------------------------------------------
C V64| Setting game index
C----+------------------------------------------------------------------
        OUTTAB( 6) = WRKBUF(TGAMIND)
C----+------------------------------------------------------------------
C V64| Setting Agent Id
C----+------------------------------------------------------------------
        I4TEMP = TRABUF(TAGT)
        OUTTAB( 7) = I1TEMP(4)
        OUTTAB( 8) = I1TEMP(3)
        OUTTAB( 9) = I1TEMP(2)
        OUTTAB(10) = I1TEMP(1)
C----+------------------------------------------------------------------
C V64| Setting Message Id
C----+------------------------------------------------------------------
        OUTTAB(11) = I1_MSG_ID(1)
        OUTTAB(12) = I1_MSG_ID(2)
        OUTTAB(13) = I1_MSG_ID(3)
        OUTTAB(14) = I1_MSG_ID(4)

        OUTTAB(15) = I1_MSG_ID(5)
        OUTTAB(16) = I1_MSG_ID(6)
        OUTTAB(17) = I1_MSG_ID(7)
        OUTTAB(18) = I1_MSG_ID(8)

        OUTLEN = 18
        CALL CALCULATE_MSG_CHECKSUM(TER,OUTTAB,OUTLEN)
        RETURN
C----+------------------------------------------------------------------
C V64| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------

C
C CALCULATE CHECKSUM
C
5000    CONTINUE
        OUTLEN=IND-1
        I4CCITT   = TRABUF(TCHK)
        OUTTAB(3) = I1CCITT(2)
        OUTTAB(4) = I1CCITT(1)
        CHKLEN=OUTLEN-1
        CALL GETCCITT(OUTTAB,1,CHKLEN,MYCHKSUM)
        I4CCITT   = MYCHKSUM
        OUTTAB(3) = I1CCITT(2)
        OUTTAB(4) = I1CCITT(1)

D       WRITE(5,909) OUTTAB(IND1),OUTTAB(IND2)
D 909    FORMAT(1X,' AGAIN : ',Z8,'  ',Z8)

c
c apagar
c       
C        TYPE *,'REPRINT'
C       DO I4TEMP=0,IND
C              TYPE 9998,I4TEMP+1,OUTTAB(I4TEMP) 
C           ENDDO
C           TYPE *,' ' 
C9998    FORMAT(' REPRINT: ',I3.1,' - ', Z3.2)


        RETURN      ! REPRINT.FCC
        END


C----+------------------------------------------------------------------
C V64| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------

        SUBROUTINE CALCULATE_MSG_CHECKSUM(TER,OUTBUF,OUTLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:IGSDEBUG.DEF'
C
        BYTE      OUTBUF(*)
        INTEGER*4 OUTLEN
        
        INTEGER*4 MYCHKSUM, CHKLEN
        INTEGER*4 TER, MSG_OFFSET
        
        CHARACTER*255 LINE
        
        
        MSG_OFFSET = 3

        BASECHKSUM = IAND(DAYCDC,'FFFF'X)
        I4CCITT   = IAND(BASECHKSUM+TER,'FFFF'X)
        OUTBUF(MSG_OFFSET + 0) = I1CCITT(2)
        OUTBUF(MSG_OFFSET + 1) = I1CCITT(1)

        I4CCITT = 0
        I1CCITT(2) = OUTBUF(MSG_OFFSET + 0)
        I1CCITT(1) = OUTBUF(MSG_OFFSET + 1)
        IF(IGSDEBUG(IA_SPESRV)) THEN
            WRITE(LINE, 901) OUTBUF(MSG_OFFSET + 0), OUTBUF(MSG_OFFSET + 1), I4CCITT
901         FORMAT('BASECHKSUM = [',Z2.2, ' ', Z2.2, '] : ', I8)
            TYPE *,IAM(), '', TRIM(LINE)
        ENDIF
        
        CHKLEN=OUTLEN-1
        CALL GETCCITT(OUTBUF,1,CHKLEN,MYCHKSUM)
        I4CCITT = MYCHKSUM
        OUTBUF(MSG_OFFSET + 0) = I1CCITT(2)
        OUTBUF(MSG_OFFSET + 1) = I1CCITT(1)

        I4CCITT = 0
        I1CCITT(2) = OUTBUF(MSG_OFFSET + 0)
        I1CCITT(1) = OUTBUF(MSG_OFFSET + 1)
        IF(IGSDEBUG(IA_SPESRV)) THEN
            WRITE(LINE, 902) OUTBUF(MSG_OFFSET + 0), OUTBUF(MSG_OFFSET + 1), I4CCITT
902         FORMAT('CALCCHKSUM = [',Z2.2, ' ', Z2.2, '] : ', I8)
            TYPE *,IAM(), '', TRIM(LINE)
        ENDIF
        
        RETURN
        END
C----+------------------------------------------------------------------
C V64| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        
