C SUBROUTINE PRINTRA
C
C PRINTRA.FOR
C
C V55 19-MAR-2021 SCML New Terminals Project, fields added CHANNEL, MESSAGEID, SERIALNUM
C V54 13-APR-2016 SCML M16 PROJECT
C V53 04-MAR-2015 SCML TMIR Report generation bug fix
C V52 31-MAR-2014 SCML Placard Project.
C V51 03-DEC-2013 SCML Added support for new accounting report
C V50 14-NOV-2013 SCML Added new validation messages
C V49 24-OCT-2013 SCML Added bank transfer data to instant validation: 
C                      Player Identification and NIB.
C                      Added net prize amount to instant validation.
C V48 07-OCT-2103 SCML PHONNBR and PLAYCRD description added for Passive
C                      New ticket status NBCH added to VALST_PAS
C                      Subroutine PASTRN changed
C V47 06-JUN-2011 FJG Process basic EM transactions
C V46 06-JAN-2011 FJG MILLENNIUM MXSRV
C V45 30-NOV-2010 FJG TWEMSER/TWEMCHK replaced by TWLNKSER/TWLNKCHK
C V44 12-APR-2010 RXK Count for ePassive wagers only sale transactions
C V43 12-MAR-2010 RXK Changes for ePassive
C V42 15-JUL-2009 FJG Add QP flag
C V31 25-MAR-2009 MMO CHANGED TWEMSER/TWEMCHK JOKER/EM.
C V30 11-MAR-2009 MMO JOKER/EM.
C V29 06-JUN-2005 FRP Modify for IPS Distribution.
C V28 28-OCT-2003 FRP Modify for Batch2 Totobola Changes.
C V27 29-NOV-2000 UXN TOTOGOLO ADDED.
C V26 29-FEB-2000 OXK Format 901 fixed to allow FREEWK
C V25 28-FEB-2000 RXK Promotion ("add 1 free week") added.
C V24 01-FEB-2000 UXN TNFRAC added.
C V23 09-DEC-1999 OXK Added system index SYST(5) = 'UNKNOWN'
C V22 13-OCT-1999 RXK World Tour added.
C V21 14-MAY-1999 UXN Super Triple added.
C V20 14-MAY-1997 UXN CRSVALUNIT replaced by 10.
C V19 22-AUG-1996 RXK Bug fixed (the case when Pitka system bet rejected 
C                     before bet info placed into trabuf)
C V18 17-MAY-1996 HXK Update from Wojtek, Siew Mun
C V17 12-FEB-1996 RXK Fix for displayed number of rows in the case of Pitka 
C                     system bet
C V16 06-FEB-1996 HXK Correction for Lotto and Vakio system numbers
C V15 18-DEC-1995 PXB Added super double and todays couple games
C V14 10-AUG-1995 RXK Couponid and "RANK" added for Ravi
C V13 05-DEC-1994 HXK Merging from 25-Nov -> 5 Dec
C V12 28-NOV-1994 PXB Bug fix on refund amount.
C V11 23-NOV-1994 HXK Added Bingo
C V10 18-OCT-1993 HXK fix for REFunds.
C V09 28-SEP-1993 HXK Added Viking lotto stuff.
C V08 06-SEP-1993 HXK Added TCV65 and TCPPP commands to TMIR
C V07 03-SEP-1993 HXK Added TFILE names from TNAMES.DEF
C V06 12-JUL-1993 SXH Released for Finland
C V05 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V04 07-JAN-1993 TD   CHANGED CALL TO THE FOLLOWING SUBROUTINES
C                      LTBET,SCBET,SPBET,TSBET, AND WIBET.  THIS WAS PERFORMED
C                      TO SOLVE DUPLICATE FILE PROBLEMS
C V03 14-APR-1992 GCAN ONLY DISPLAY 'AND' BETS FOR NUMBERS.
C V02 12-NOV-1991 MTK  INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C SUBROUTINE TO PRINT TRANSACTIONS IN TMIR FORMAT
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
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
C       SUBROUTINE PRINTRA(TRABUF,PUNIT,DETAIL,LLINCNT,SCRAM,TOTAL,CARY)
        SUBROUTINE IGS_PRINTRA(TRABUF,PUNIT,DETAIL,LLINCNT,SCRAM,TOTAL,CARY)
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
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
C

        ! arguments
        INTEGER*4  PUNIT                   !
        INTEGER*4  TOTAL(TCRS+NUMCRS+1,2)  !
C
        LOGICAL    DETAIL                  !
        LOGICAL    SCRAM                   !
        LOGICAL    CARY                    !
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        LOGICAL IS_IGS_PRINTRA
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------

        ! variables
        INTEGER * 8 IGS_MID      ! MESSAGE ID
C
        INTEGER*4  CN                      !
        INTEGER*4  CT                      !
        INTEGER*4  AMOUNT                  !
        INTEGER*4  VALSER                  !
        INTEGER*4  EXCSER                  !
        INTEGER*4  I                       !
        INTEGER*4  CANSER                  !
        INTEGER*4  CHECK                   !
        INTEGER*4  SERIAL                  !
        INTEGER*4  PAGE                    !
        INTEGER*4  BLANK                   !
        INTEGER*4  LINES                   !
        INTEGER*4  LINCNT                  !
        INTEGER*4  LLINCNT                  !
        INTEGER*4  BETS(20,14)             !
        INTEGER*4  DSPOLD(2),DSPNEW(2)        
        INTEGER*4  SUBSTIT
        INTEGER*4  RESERV/' RES'/
C
        INTEGER*4 TTSL_SYSCNT
        INTEGER*4 SYSCNT(3,2) /4,10,20,0,5,15/
C
        CHARACTER*80  CBETS(14)            !
        CHARACTER     SYST(0:5)*7          !
        CHARACTER     TOTFLG               !
        CHARACTER*11  KICKER               !
        CHARACTER*11  KICKER2              !
        CHARACTER*4   BETDUR               !
        CHARACTER*2   JOKSTS(2)            !
        REAL*8        COMMAND              !
C
        BYTE          ESRPC(64)            !MXSRV
        BYTE          ESTRA(200)           !MXSRV
        INTEGER*4     LEN                  !MXSRV        
C
        EQUIVALENCE (CBETS, BETS)
C
        INTEGER*4  NIB(6)
        CHARACTER*24 CNIB
        EQUIVALENCE (NIB,CNIB)

C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        DATA LINCNT/100/,LINES/0/,COMMAND/0/
C        DATA LINES/0/,COMMAND/0/
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        DATA BLANK/'    '/
        DATA SYST/'   NONE','   FULL','REDUCED','  U-SYS',
     *            ' CHANCE','UNKNOWN'/
        DATA JOKSTS/' N',' Y'/                                                    
        CHARACTER*2  JDTEXT(0:3)/'  ','R ','L ','B '/
        CHARACTER*2  FREEWK(0:1)/'  ','+1'/
        CHARACTER*14 RETTYP(0:4)
        CHARACTER*4  QPTXT(0:1) /'    ','*QP*'/	
        INTEGER*4    QPVAL
        LOGICAL      DUMM   ! AVOID COMPILATION WARNING

        DATA RETTYP/'--------------','ALL TICKET    ','BY FRACTION    ',
     *              'HALF TICKET   ','QUARTER TICKET'/

        INTEGER*4 TCKS

C
        INTEGER*4 HOUR,SEC,MNT
C
        INTEGER*4 DLEN,DID,OFF,J
        BYTE      DINF(500)
        CHARACTER*8 IAGT_NO !FUNCTION
C
C V52 - Start
        INTEGER*8  I8TMP
        INTEGER*4  I4TMP(2)
        EQUIVALENCE (I8TMP,I4TMP)
C---------------- V55 Begin -------------------------------------------        
        INTEGER*8  MESSID
        REAL*16     OVER8BYTES
        PARAMETER  (OVER8BYTES = 18446744073709551616.0)        
C        integer, parameter :: ep= selected_real_kind(21)
C        real(ep), parameter :: OVER8BYTES = 18446744073709551616.0_ep
        REAL*16     SERIALNUM_OLM
        CHARACTER*24  SERIALNUM_OLMSTR,  SERIAL_AUX
C---------------- V55 End----------------------------------------------        
        INTEGER*8  TMSGID ! TERMINAL MESSAGE ID
        INTEGER*8  HMSGID ! HOST MESSAGE ID
        INTEGER*8  TRXREF ! TRX EXTERNAL SERIAL
        
        INTEGER*4 CXERR_LEN
        PARAMETER (CXERR_LEN = 9) ! XERR HAS 9 CHARACTERS
        CHARACTER*12 CXERR ! XERR_I4LEN * 4
C V52 - End
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        IS_IGS_PRINTRA = .TRUE.
        LINCNT = LLINCNT
        GOTO 11111
        ENTRY PRINTRA(TRABUF,PUNIT,DETAIL,SCRAM,TOTAL,CARY)
        IS_IGS_PRINTRA = .FALSE.
11111   DUMM = CARY
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
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
        IF(TRABUF(TTYP).EQ.TEUR) GOTO 7000
        IF(TRABUF(TTYP).EQ.TWAG) GOTO 1000
        IF(TRABUF(TTYP).EQ.TCAN) GOTO 1000
        IF(TRABUF(TTYP).EQ.TINC) GOTO 1000
        IF((TRABUF(TTYP).EQ.TVAL.OR.TRABUF(TTYP).EQ.TREF).AND.
     *      TRABUF(TGAMTYP).NE.TPAS ) GOTO 2000
        IF(TRABUF(TTYP).EQ.TSPE) GOTO 3000
        IF(TRABUF(TTYP).EQ.TCMD) GOTO 4000
        IF(TRABUF(TTYP).EQ.TCRS) GOTO 5000
        IF((TRABUF(TTYP).EQ.TVAL .OR. TRABUF(TTYP).EQ.TRET) .AND.
     *      TRABUF(TGAMTYP).EQ.TPAS) GOTO 6000
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        IF(IS_IGS_PRINTRA) THEN
            LLINCNT = LINCNT
        ENDIF
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        RETURN
C
C PRINT WAGERS/CANCELS/DELETIONS
C
1000    CONTINUE
        LINCNT = LINCNT+1
        KICKER  = ' --NONE--  '
        KICKER2 = '           '
        CANSER = TRABUF(TWCSER)
        IF(TRABUF(TWQPF).NE.0) THEN
          QPVAL = 1
        ELSE
          QPVAL = 0
        ENDIF
        IF(SCRAM) CALL OUTGEN(TRABUF(TCDC),TRABUF(TWCSER),CANSER,CHECK)
        IF(TRABUF(TWCSER).EQ.0) CANSER=0
        IF(TRABUF(TWKGME).NE.0) THEN
            WRITE (KICKER,800) JDTEXT(IAND(ISHFT(TRABUF(TWKICK),-24),3)), 
     *                         IAND(TRABUF(TWKICK),'00FFFFFF'X),
     *                         JOKSTS(TRABUF(TWKFLG)+1)
            WRITE (KICKER2,800) JDTEXT(IAND(ISHFT(TRABUF(TWKICK2),-24),3)),
     *                         IAND(TRABUF(TWKICK2),'00FFFFFF'X),
     *                         JOKSTS(TRABUF(TWKFLG2)+1)
        ENDIF

        IF(TRABUF(TSTAT).NE.REJT .AND. TRABUF(TSTAT).NE.VOID .AND.
     *     TRABUF(TSTAT).NE.INCA .AND.
     *     TRABUF(TSTAT).NE.EXCH .AND. TRABUF(TWFFLG).NE.1) THEN
           IF(TRABUF(TGAMTYP).EQ.TPAS) THEN
              IF(TRABUF(TWEPOP).EQ.EPASSAL) THEN
                 TOTAL(TRABUF(TTYP),1) = TOTAL(TRABUF(TTYP),1)+1
                 TOTAL(TRABUF(TTYP),2) = TOTAL(TRABUF(TTYP),2) +
     *                                   TRABUF(TWTOT)
              ENDIF
           ELSE
              TOTAL(TRABUF(TTYP),1) = TOTAL(TRABUF(TTYP),1)+1
              TOTAL(TRABUF(TTYP),2) = TOTAL(TRABUF(TTYP),2) +
     *                                TRABUF(TWTOT)
           ENDIF
        ENDIF

        BETDUR='THRU'
        IF(TRABUF(TGAMTYP).EQ.TNBR.AND.TRABUF(TWNAND).EQ.1) BETDUR='AND '
        TOTFLG=' '
        IF(IAND(TRABUF(TTSTCS),'01'X).NE.0) TOTFLG='T'

        SUBSTIT = TTYPE(TRABUF(TTYP))
        IF(TRABUF(TGAMTYP).EQ.TPAS.AND.
     *    (TRABUF(TWEPOP).EQ.EPASRES.OR.TRABUF(TWEPOP).EQ.EPASREL))
     *     SUBSTIT = RESERV
C---------------- V55 Begin -------------------------------------------     
           I4TMP(1) = ZEXT(TRABUF(TWCOLMMIDL_TLTO)) 
           I4TMP(2) = ZEXT(TRABUF(TWCOLMMIDH_TLTO)) 
           MESSID = I8TMP  
    
           I4TMP(1) = ZEXT(TRABUF(TWCOLMSERL_TLTO)) 
           I4TMP(2) = ZEXT(TRABUF(TWCOLMSERM_TLTO))

           SERIALNUM_OLM = DFLOAT(ZEXT(TRABUF(TWCOLMSERH_TLTO)))*OVER8BYTES+DFLOAT(I8TMP)
           WRITE(SERIAL_AUX,990) SERIALNUM_OLM          
           SERIALNUM_OLMSTR = SERIAL_AUX(1:6)//'-'//SERIAL_AUX(7:8)//'-'//SERIAL_AUX(9:18)//'-'//SERIAL_AUX(19:21)           

           IF(TRABUF(TWCOLMCOMF_TLTO) .EQ. 1) THEN
                WRITE(PUNIT,980)  'Yes',MESSID,SERIALNUM_OLMSTR
           ELSE
                WRITE(PUNIT,985)  'No'
           ENDIF           
C---------------- V55 End -------------------------------------------           
           WRITE(PUNIT,901)  STAT(TRABUF(TSTAT)),
     *                       ERROR(TRABUF(TERR)),
     *                       SUBSTIT,
     *                       SERIAL,
     *                       DISTIM(TRABUF(TTIM)),
     *                       TRABUF(TTER),
     *                       TRABUF(TTRN),
     *                       TRABUF(TCDC),
     *                       TRABUF(TGAM),
     *                       GTNAMES(TRABUF(TGAMTYP)),
     *                       TRABUF(TGAMIND),
     *                       TRABUF(TSIZE),
     *                       TRABUF(TWBEG),
     *                       BETDUR,
     *                       TRABUF(TWEND),FREEWK(TRABUF(TWADDFW)),
     *                       KICKER,
     *                       CSMONY(TRABUF(TWTOT),10,BETUNIT),
     *                       TRABUF(TFRAC),QPTXT(QPVAL)         
C
        IF(CANSER .GT. 0) THEN
            IF(TRABUF(TSTAT).EQ.CASH.OR.TRABUF(TSTAT).EQ.EXCH.OR.
     *          TRABUF(TSTAT).EQ.CLAM.OR.TRABUF(TSTAT).EQ.XCHD.OR.
     *          TRABUF(TSTAT).EQ.XCLM) THEN
                WRITE(PUNIT,9018) TRABUF(TWBNKID),TRABUF(TWBNKNM),CANSER,
     *                            TFILE(TRABUF(TFIL)),KICKER2,
     *                            TRABUF(TWKBEG),TRABUF(TWKEND),
     *                            TRABUF(TCDC_SOLD)
            ELSE
                WRITE(PUNIT,9019) TRABUF(TWBNKID),TRABUF(TWBNKNM),CANSER,
     *                            TFILE(TRABUF(TFIL)),KICKER2,
     *                            TRABUF(TWKBEG),TRABUF(TWKEND),
     *                            TRABUF(TCDC_SOLD)
            ENDIF
            IF (TRABUF(TWLNKSER) .GT. 0) WRITE(PUNIT,90193) TRABUF(TWLNKSER)

        ELSEIF (TRABUF(TWLNKSER) .GT. 0) THEN
            WRITE(PUNIT,90192) TRABUF(TWBNKID),TRABUF(TWBNKNM),TRABUF(TWLNKSER),
     *                            TFILE(TRABUF(TFIL)),KICKER2,
     *                            TRABUF(TWKBEG),TRABUF(TWKEND),
     *                            TRABUF(TCDC_SOLD)
        ELSE
          IF(TRABUF(TGAMTYP).EQ.TPAS) THEN
            WRITE(PUNIT,90191) TRABUF(TWEPOP),TRABUF(TWBNKID),TRABUF(TWBNKNM),
     *                         TFILE(TRABUF(TFIL)),KICKER2,
     *                         TRABUF(TWKBEG),TRABUF(TWKEND),
     *                         TRABUF(TCDC_SOLD)
          ELSE
            WRITE(PUNIT,90194) TRABUF(TWBNKID),TRABUF(TWBNKNM),
     *                         TFILE(TRABUF(TFIL)),KICKER2,
     *                         TRABUF(TWKBEG),TRABUF(TWKEND),
     *                         TRABUF(TCDC_SOLD)            
          ENDIF
        ENDIF
C
        LINCNT=LINCNT+1    

        IF(DETAIL) THEN
            CALL FASTSET(BLANK,BETS,20*14)

            IF(TRABUF(TGAMTYP).EQ.TLTO)
     *        CALL PRINTRA_LTBET(TRABUF,CBETS,LINES)
            IF(TRABUF(TGAMTYP).EQ.TNBR)
     *        CALL NTBET(TRABUF,CBETS,LINES)
            IF(TRABUF(TGAMTYP).EQ.TSPT)
     *        CALL PRINTRA_SPBET(TRABUF,BETS,LINES)
            IF(TRABUF(TGAMTYP).EQ.TTGL)
     *        CALL PRINTRA_TGBET(TRABUF,CBETS,LINES)
            IF(TRABUF(TGAMTYP).EQ.TSCR)
     *        CALL PRINTRA_SCBET(TRABUF,CBETS,LINES)
            IF(TRABUF(TGAMTYP).EQ.TWIT)
     *        CALL PRINTRA_WIBET(TRABUF,CBETS,LINES)
            IF(TRABUF(TGAMTYP).EQ.TTSL)
     *        CALL PRINTRA_TSBET(TRABUF,CBETS,LINES)
            IF(TRABUF(TGAMTYP).EQ.TBNG)
     *        CALL PRINTRA_BGBET(TRABUF,CBETS,LINES)
            IF(TRABUF(TGAMTYP).EQ.TDBL)
     *        CALL PRINTRA_DBBET(TRABUF,CBETS,LINES)
            IF(TRABUF(TGAMTYP).EQ.TCPL)
     *        CALL PRINTRA_CPBET(TRABUF,CBETS,LINES)
            IF(TRABUF(TGAMTYP).EQ.TSSC)
     *        CALL PRINTRA_SSBET(TRABUF,CBETS,LINES)
            IF(TRABUF(TGAMTYP).EQ.TTRP)
     *        CALL PRINTRA_TRBET(TRABUF,CBETS,LINES)
            IF(TRABUF(TGAMTYP).EQ.TSTR)
     *        CALL PRINTRA_STBET(TRABUF,CBETS,LINES)
            IF(TRABUF(TGAMTYP).EQ.TPAS)
     *        CALL PRINTRA_PABET(TRABUF,CBETS,LINES)
C
            LINCNT = LINCNT + 1
            IF(LINCNT.GT.LINSPP) THEN
                CALL TITLE('TRANSACTION FILE REPORT','    TMIR',1,
     *                      PUNIT,PAGE,DAYCDC)
                WRITE(PUNIT,900)
                LINCNT=7
            ENDIF
C
C
C BET , FIRST LINE
C
            I = 1
            IF(TRABUF(TWSYST).GT.0) THEN
               IF(TRABUF(TGAMTYP).EQ.TLTO .OR.
     *            (TRABUF(TGAMTYP).EQ.TSPT .AND. TRABUF(TWSPFRG).LE.0) .OR.
     *            TRABUF(TGAMTYP).EQ.TTGL) THEN
                  WRITE(PUNIT,907) (BETS(K,I),K=1,20),
     *                              SYST(TRABUF(TWSYST)),
     *                              TRABUF(TWSIMP)
               ELSEIF(TRABUF(TGAMTYP).EQ.TSPT .AND. TRABUF(TWSPFRG).GT.0) THEN
                  WRITE(PUNIT,9022) BETS(1,LINES-1),(BETS(K,LINES-1),K=2,20)
                  WRITE(PUNIT,907) (BETS(K,I),K=1,20),
     *                              SYST(TRABUF(TWSYST)),
     *                              TRABUF(TWSIMP)
               ELSEIF(TRABUF(TGAMTYP).EQ.TTSL) THEN
                  IF(TRABUF(TWTSEL1).EQ.0) THEN
                         ! if rejected before bet data has been put in) 
                     WRITE(PUNIT,907) (BETS(K,I),K=1,20),
     *                                SYST(TRABUF(TWSYST)),
     *                                TRABUF(TWSYSN)
                  ELSE 
                     TTSL_SYSCNT=
     *                    SYSCNT(TRABUF(TWTSEL1)-3,TRABUF(TWSYSN)-2)
                     WRITE(PUNIT,907) (BETS(K,I),K=1,20),
     *                                SYST(TRABUF(TWSYST)),
     *                                TTSL_SYSCNT
                  ENDIF
               ELSEIF(TRABUF(TGAMTYP).EQ.TWIT) THEN
                  WRITE(PUNIT,9071) (BETS(K,I),K=1,20),
     *                               SYST(TRABUF(TWSYST)),
     *                               TRABUF(TWSYSN),
     *                               TRABUF(TWWCOUPID) 
               ELSEIF(TRABUF(TGAMTYP).EQ.TDBL) THEN
                  WRITE(PUNIT,9071) (BETS(K,I),K=1,20),
     *                               SYST(TRABUF(TWSYST)),
     *                               TRABUF(TWSYSN),
     *                               TRABUF(TWDBCOUPID) 
               ELSEIF(TRABUF(TGAMTYP).EQ.TCPL) THEN
                  WRITE(PUNIT,9071) (BETS(K,I),K=1,20),
     *                               SYST(TRABUF(TWSYST)),
     *                               TRABUF(TWSYSN),
     *                               TRABUF(TWCPCOUPID) 
               ELSEIF(TRABUF(TGAMTYP).EQ.TTRP) THEN
                  WRITE(PUNIT,9071) (BETS(K,I),K=1,20),
     *                              SYST(TRABUF(TWSYST)),
     *                              TRABUF(TWSYSN),
     *                              TRABUF(TWTTCOUPID) 
               ELSEIF(TRABUF(TGAMTYP).EQ.TSTR) THEN
                  WRITE(PUNIT,9071) (BETS(K,I),K=1,20),
     *                              SYST(TRABUF(TWSYST)),
     *                              TRABUF(TWSYSN),
     *                              TRABUF(TWSTCOUPID) 
               ELSEIF(TRABUF(TGAMTYP).EQ.TSSC) THEN
                  WRITE(PUNIT,9071) (BETS(K,I),K=1,20),
     *                              SYST(TRABUF(TWSYST)),
     *                              TRABUF(TWSYSN),
     *                              TRABUF(TWSSCOUPID) 
               ELSE
                  WRITE(PUNIT,907)  (BETS(K,I),K=1,20),
     *                              SYST(TRABUF(TWSYST)),
     *                              TRABUF(TWSYSN)
               ENDIF

            ELSE
               IF(TRABUF(TGAMTYP).EQ.TWIT) THEN
                  WRITE(PUNIT,9072) (BETS(K,I),K=1,20),
     *                    TRABUF(TWWCOUPID) 
               ELSEIF(TRABUF(TGAMTYP).EQ.TDBL) THEN
                  WRITE(PUNIT,9072) (BETS(K,I),K=1,20),
     *                              TRABUF(TWDBCOUPID) 
               ELSEIF(TRABUF(TGAMTYP).EQ.TCPL) THEN
                  WRITE(PUNIT,9072) (BETS(K,I),K=1,20),
     *                              TRABUF(TWCPCOUPID) 
               ELSEIF(TRABUF(TGAMTYP).EQ.TTRP) THEN
                  WRITE(PUNIT,9072) (BETS(K,I),K=1,20),
     *                              TRABUF(TWTTCOUPID) 
               ELSEIF(TRABUF(TGAMTYP).EQ.TSTR) THEN
                  WRITE(PUNIT,9072) (BETS(K,I),K=1,20),
     *                              TRABUF(TWSTCOUPID) 
               ELSEIF(TRABUF(TGAMTYP).EQ.TSSC) THEN
                  WRITE(PUNIT,9072) (BETS(K,I),K=1,20),
     *                              TRABUF(TWSSCOUPID) 
               ELSEIF(TRABUF(TGAMTYP).EQ.TSPT .AND. TRABUF(TWSPFRG).GT.0) THEN
                  WRITE(PUNIT,9022) BETS(1,LINES-1),(BETS(K,LINES-1),K=2,20)
                  WRITE(PUNIT,902) (BETS(K,I),K=1,20)    
               ELSE
                  WRITE(PUNIT,902) (BETS(K,I),K=1,20)    
               ENDIF
            ENDIF

C
C BET , NEXT LINES
C
            DO I = 2, LINES

		IF(I.EQ.LINES-1 .AND.
     *             TRABUF(TGAMTYP).EQ.TSPT .AND. TRABUF(TWSPFRG).GT.0) THEN
		  GOTO 1010
		ELSE
                  WRITE(PUNIT,902) (BETS(K,I),K=1,20)
	        ENDIF

                LINCNT = LINCNT + 1
                IF(LINCNT.GT.LINSPP) THEN
                    CALL TITLE('TRANSACTION FILE REPORT','    TMIR',1,
     *                          PUNIT,PAGE,DAYCDC)
                    WRITE(PUNIT,900)
                    LINCNT=7
                ENDIF

1010	        CONTINUE
            END DO

        ENDIF

C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        IF(IS_IGS_PRINTRA) THEN
            LLINCNT = LINCNT
        ENDIF
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        RETURN

C
C PRINT VALIDATIONS
C
2000    CONTINUE
        EXCSER = TRABUF(TVEXC)
        VALSER = TRABUF(TVSER)
        IF(SCRAM) CALL OUTGEN(TRABUF(TVCDC),TRABUF(TVSER),VALSER,CHECK)
        IF(SCRAM) CALL OUTGEN(TRABUF(TCDC),TRABUF(TVEXC),EXCSER,CHECK)
        IF(TRABUF(TVEXC).EQ.0) EXCSER=0
        IF(TRABUF(TVSER).EQ.0) VALSER=0

        AMOUNT = TRABUF(TVPAY) + TRABUF(TVKPAY)

C---------------- V55 Begin -------------------------------------------     
        I4TMP(1) = 0
        I4TMP(2) = 0 
        MESSID = I8TMP  
        
        I4TMP(1) = ZEXT(TRABUF(TVOLMSERL_IL)) 
        I4TMP(2) = ZEXT(TRABUF(TVOLMSERM_IL))

        SERIALNUM_OLM = DFLOAT(ZEXT(TRABUF(TVOLMSERH_IL)))*OVER8BYTES+DFLOAT(I8TMP)
        WRITE(SERIAL_AUX,990) SERIALNUM_OLM          
        SERIALNUM_OLMSTR = SERIAL_AUX(1:6)//'-'//SERIAL_AUX(7:8)//'-'//SERIAL_AUX(9:18)//'-'//SERIAL_AUX(19:21)           

        IF(TRABUF(TVOLMCOMF_TLTO) .EQ. 1) THEN
           WRITE(PUNIT,980)  'Yes',MESSID,SERIALNUM_OLMSTR
        ELSE
           WRITE(PUNIT,985)  'No'
        ENDIF        
C---------------- V55 End -------------------------------------------         

        WRITE(PUNIT,903) STAT(TRABUF(TSTAT)),
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
     *                   TRABUF(TSIZE),CMONY(AMOUNT,11,VALUNIT),
     *                   CMONY(TRABUF(TVREF),11,BETUNIT),
     *                   EXCSER
C
        IF(VALSER.GT.0) THEN
            WRITE(PUNIT,9020) TRABUF(TVCDC),VALSER,
     *                        CMONY(TRABUF(TVOPPAY),11,BETUNIT)
            LINCNT=LINCNT+1
        ENDIF         
C----+------------------------------------------------------------------
C V50| Adding data for new validation messages
C----+------------------------------------------------------------------
        IF(TRABUF(TTYP).EQ.TVAL.AND.
     *     TRABUF(TVTYPE).EQ.VNBNK) THEN
           CALL FASTSET(BLANK,NIB,6)
           WRITE(CNIB,801) TRABUF(TVNIBBB),TRABUF(TVNIBBO),TRABUF(TVNIBBA1),
     *                     TRABUF(TVNIBBA2),TRABUF(TVNIBCD)
           IF(TRABUF(TVPLIDTYP).EQ.PHONNBR) THEN
             WRITE(PUNIT,1003) TRABUF(TVPLCARD),(NIB(I),I=1,6)
           ELSEIF(TRABUF(TVPLIDTYP).EQ.PLAYCRD) THEN
             WRITE(PUNIT,1004) TRABUF(TVPLCARD),(NIB(I),I=1,6)
           ENDIF
        ENDIF
C----+------------------------------------------------------------------
C V50| Adding data for new validation messages
C----+------------------------------------------------------------------
        LINCNT = LINCNT + 1
        IF(TRABUF(TSTAT).NE.REJT)THEN
            TOTAL(TRABUF(TTYP),1) = TOTAL(TRABUF(TTYP),1)+1
            IF (TRABUF(TTYP) .EQ. TREF) THEN
              TOTAL(TRABUF(TTYP),2) = TOTAL(TRABUF(TTYP),2) + TRABUF(TVREF) 
            ELSE
              TOTAL(TRABUF(TTYP),2) = TOTAL(TRABUF(TTYP),2)+AMOUNT
            END IF
        ENDIF

C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        IF(IS_IGS_PRINTRA) THEN
            LLINCNT = LINCNT
        ENDIF
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        RETURN
C
C PRINT SPECIAL FUNCTIONS
C
3000    CONTINUE
        LINCNT = LINCNT + 1
        WRITE(PUNIT,904) STAT(TRABUF(TSTAT)),
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
     *                   SPFUN(TRABUF(TSFUN)),
     *                   TRABUF(TSOLD),
     *                   TRABUF(TSNEW),
     *                   IAGT_NO(TRABUF(TAGT))

	IF(TRABUF(TSFUN).EQ.TAGTINF) THEN
            WRITE(PUNIT,920) TRABUF(TSNEW)
            LINCNT = LINCNT + 1
             OFF = 1
             DO I=1,MIN0(12,TRABUF(TSNEW))
                DLEN = 0
                DID  = 0
                CALL MOVBYT(TRABUF(TSDT1),OFF,DID,1,1)    ! 1 bytes of ID
                OFF = OFF + 1
                CALL MOVBYT(TRABUF(TSDT1),OFF,DLEN,1,1)   ! 1 bytes of lenght
                OFF = OFF + 1
                CALL MOVBYT(TRABUF(TSDT1),OFF,DINF,1,DLEN)
                OFF = OFF + DLEN
                WRITE(PUNIT,921) I, FLDNAM(DID), DLEN, (DINF(J),J=1,DLEN)
                LINCNT = LINCNT + 1
             ENDDO
        ELSEIF(TRABUF(TSFUN).EQ.TMXL) THEN                              !MXSRV
          CALL MOVBYT(TRABUF(TMXL_RPCTAG),1,ESRPC,1,64)                 !MXSRV
          CALL MOVBYT(TRABUF(TMXL_DATA),1,ESTRA,1,TRABUF(TMXL_DATLEN))  !MXSRV
          WRITE(PUNIT,9221) TRABUF(TMXL_ERCODE)                         !MXSRV
          WRITE(PUNIT,9222) (ESRPC(K),K=1,64)                           !MXSRV
          LEN = TRABUF(TMXL_DATLEN)                                     !MXSRV
          WRITE(PUNIT,9223) (ESTRA(K),K=1,MIN(LEN,50))                  !MXSRV
          LINCNT = LINCNT + 4                                           !MXSRV
          IF(LEN.GT.50) THEN                                            !MXSRV
            WRITE(PUNIT,9223) (ESTRA(K),K=51,MIN(LEN,100))              !MXSRV
            LINCNT = LINCNT + 1                                         !MXSRV
          ENDIF                                                         !MXSRV
          IF(LEN.GT.100) THEN                                           !MXSRV
            WRITE(PUNIT,9223) (ESTRA(K),K=101,MIN(LEN,150))             !MXSRV
            LINCNT = LINCNT + 1                                         !MXSRV
          ENDIF                                                         !MXSRV

C----+------------------------------------------------------------------
C V51| Adding support for new accounting report
C----+------------------------------------------------------------------
        ELSEIF(TRABUF(TSFUN) .EQ. TNAP) THEN
            IF (DETAIL) THEN
              WRITE(PUNIT,9041) TRABUF(TSDT1)
     *                        , TRABUF(TSDT2)
     *                        , TRABUF(TSDT6)                                   !V54
     *                        , TRABUF(TSDT5)
     *                        , TRABUF(TSDT4) + 2000
     *                        , IAGT_NO(TRABUF(TSDT3))
              LINCNT = LINCNT + 2
            ENDIF
C----+------------------------------------------------------------------
C V51| Adding support for new accounting report
C----+------------------------------------------------------------------
C
C V52 - Start
C
        ELSEIF(TRABUF(TSFUN) .EQ. TREPR) THEN
          IF (DETAIL) THEN
            I4TMP(2) = TRABUF(TSDT2) ! HIGH
            I4TMP(1) = TRABUF(TSDT1) ! LOW
            TMSGID = I8TMP
            WRITE(PUNIT,930) TMSGID ! MESSAGE ID
            LINCNT = LINCNT + 1          
            
            IF(TRABUF(TSOLD) .EQ. TIGS .AND. TRABUF(TGAMTYP) .EQ. TODS) THEN
              
              WRITE(PUNIT,940) TRABUF(TSDT3) ! MESSAGE QUEUE SEQUENCE NUMBER FOR THIS TRANSACTION
              
              ! IGS ERROR CODE DESCRIPTION
              WRITE(CXERR,'(<XERR_I4LEN>A4)') (TRABUF(TSDT5+I),I=0,XERR_I4LEN-1)
              
              IF (TRABUF(TSDT4) .GE. SEC_LIDX .AND. TRABUF(TSDT4) .LE. SEC_RIDX) THEN
                WRITE(PUNIT,950) SYSTEMERRORCODE(TRABUF(TSDT4)), CXERR
                LINCNT = LINCNT + 1                                             !V54
              ELSE
                WRITE(PUNIT,9501) TRABUF(TSDT4), ' (UNKNOWN) ', CXERR
                LINCNT = LINCNT + 1                                             !V54
              ENDIF
              
              IF (TRABUF(TSDT8) .GE. RTYP_LIDX .AND. TRABUF(TSDT8) .LE. RTYP_RIDX) THEN
                WRITE(PUNIT,960) REPRINTTYPE(TRABUF(TSDT8)) ! TRX REPRINT TYPE
                LINCNT = LINCNT + 1                                             !V54
              ELSE
                WRITE(PUNIT,9601) TRABUF(TSDT8), ' (UNKNOWN)'
                LINCNT = LINCNT + 1                                             !V54
              ENDIF
              
              I4TMP(2) = TRABUF(TSDT14)
              I4TMP(1) = TRABUF(TSDT13)
              TRXREF = I8TMP
              
              ! TRX REFERENCE
              IF (SCRAM .EQ. .TRUE.) THEN
              WRITE(PUNIT,9701) TRABUF(TSDT9),
     *                         TRABUF(TSDT10),
     *                         TRABUF(TSDT11),
     *                         TRABUF(TSDT12),
     *                         TRXREF,
     *                         TRABUF(TSDT15)
                LINCNT = LINCNT + 1                                             !V54
              ELSE
                WRITE(PUNIT,970) TRABUF(TSDT9),
     *                           TRABUF(TSDT10),
     *                           TRABUF(TSDT11),
     *                           TRABUF(TSDT12),
     *                           TRXREF,
     *                           '***'
                LINCNT = LINCNT + 1                                             !V54
              ENDIF
            
C TESTES          ! TRX REFERENCE  !!! REMOVER ESTA INSTRUCAO TAL COMO A SUA RESPECTIVA FORMATACAO!!!
C              WRITE(PUNIT,9701) TRABUF(TSDT9),
C     *                         TRABUF(TSDT10),
C     *                         TRABUF(TSDT11),
C     *                         TRABUF(TSDT12),
C     *                         TRXREF,
C     *                         TRABUF(TSDT15)
            
            ENDIF
            WRITE(PUNIT,*)
            LINCNT = LINCNT + 1
          ENDIF 
        ELSEIF(TRABUF(TSFUN) .EQ. TSREP) THEN
          IF (DETAIL) THEN
            I4TMP(2) = TRABUF(TSDT6) ! HIGH
            I4TMP(1) = TRABUF(TSDT5) ! LOW
            HMSGID = I8TMP
C            WRITE(PUNIT,971) HMSGID ! MESSAGE ID
CV54            WRITE(PUNIT,971) TRABUF(TSDT1), TRABUF(TSDT2), HMSGID ! MESSAGE ID
            IF(TRABUF(TSDT1).EQ.1) THEN
              WRITE(PUNIT,971) TRABUF(TSDT1), 
     *                         TRABUF(TSDT2), 
     *                         TRABUF(TSDT3),                                   !V54
     *                         HMSGID, ! MESSAGE ID
     *                         TRABUF(TSDT7)                                    !V54
              LINCNT = LINCNT + 2
            ELSEIF(TRABUF(TSDT1).EQ.2 .OR. 
     *             TRABUF(TSDT1).EQ.4 .OR.
     *             TRABUF(TSDT1).EQ.8) THEN
              WRITE(PUNIT,9710) TRABUF(TSDT1), 
     *                          TRABUF(TSDT2), 
     *                          TRABUF(TSDT3),                                  !V54
     *                          HMSGID, ! MESSAGE ID
     *                          TRABUF(TSDT7)                                   !V54
              LINCNT = LINCNT + 2
            ENDIF
          ENDIF
C
C V52 - End
C
        ELSEIF(TRABUF(TSFUN) .EQ. TGREP) THEN                                   !V54
          IF (DETAIL) THEN
            IF(TRABUF(TSDT1).EQ.TEUM .OR.TRABUF(TSDT1).EQ.TRAF) THEN
              WRITE(PUNIT,972) TRABUF(TSDT1), TRABUF(TSDT2), TRABUF(TSDT3)
              LINCNT = LINCNT + 2
            ENDIF
          ENDIF
        ENDIF
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        IF(IS_IGS_PRINTRA) THEN
            LLINCNT = LINCNT
        ENDIF
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        RETURN
C
C PRINT COMMANDS
C
4000    CONTINUE

        LINCNT=LINCNT+1
        CT=TRABUF(TCMTYP)
        CN=TRABUF(TCMNUM)
        IF(CT.EQ.TCPAR) COMMAND = NAMPAR(CN)
        IF(CT.EQ.TCGEN) COMMAND = NAMGEN(CN)
        IF(CT.EQ.TCSPE) COMMAND = NAMSPE(CN)
        IF(CT.EQ.TCNET) COMMAND = NAMNET(CN)
        IF(CT.EQ.TCCOM) COMMAND = NAMCOM(CN)
        IF(CT.EQ.TCAGT) COMMAND = NAMAGT(CN)
        IF(CT.EQ.TCLTO) COMMAND = NAMLTO(CN)
        IF(CT.EQ.TCTGL) COMMAND = NAMTGL(CN)
        IF(CT.EQ.TCSPT) COMMAND = NAMSPT(CN)
        IF(CT.EQ.TCKIK) COMMAND = NAMKIK(CN)
        IF(CT.EQ.TCSCR) COMMAND = NAMSCR(CN)
        IF(CT.EQ.TCWIT) COMMAND = NAMWIT(CN)
        IF(CT.EQ.TCTSL) COMMAND = NAMTSL(CN)
        IF(CT.EQ.TCBNG) COMMAND = NAMBNG(CN)
        IF(CT.EQ.TCDBL) COMMAND = NAMDBL(CN)
        IF(CT.EQ.TCCPL) COMMAND = NAMCPL(CN)
        IF(CT.EQ.TCSTR) COMMAND = NAMSTR(CN)
        IF(CT.EQ.TCPAS) COMMAND = NAMPAS(CN)
C
C
        IF(CT.EQ.TCPAR.AND.(CN.EQ.SUPGWA.OR.CN.EQ.SUPGCA.
     *     OR.CN.EQ.SUPGVA.OR.CN.EQ.SUPRPT)) THEN
           DSPOLD(1)=JISHFT(TRABUF(TCMOLD),-1)
           IF(IAND(TRABUF(TCMDT1),1).EQ.1) DSPOLD(1)=IOR(DSPOLD(1),'80000000'X)
           DSPOLD(2)=JISHFT(TRABUF(TCMDT1),-1)
           DSPNEW(1)=JISHFT(TRABUF(TCMNEW),-1)
           IF(IAND(TRABUF(TCMDT2),1).EQ.1) DSPNEW(1)=IOR(DSPNEW(1),'80000000'X)
           DSPNEW(2)=JISHFT(TRABUF(TCMDT2),-1)
           
           WRITE(PUNIT,9051) STAT(TRABUF(TSTAT)),
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
     *                   COMMAND,
     *                   TRABUF(TCMTER),
     *                   DSPOLD(2),DSPOLD(1),DSPNEW(2),DSPNEW(1)
C
C V52 - Start
        ELSEIF(CT.EQ.TCPAR.AND.(CN.EQ.IGSPGWAG.OR.CN.EQ.IGSPGCAN.
     *     OR.CN.EQ.IGSPGVAL.OR.CN.EQ.IGSPGREP.OR.CN.EQ.IGSPGFIN.
     *     OR.CN.EQ.IGSPGRNT)) THEN
           WRITE(PUNIT,9052) STAT(TRABUF(TSTAT)),
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
     *                   COMMAND,
     *                   TRABUF(TCMTER),
     *                   TRABUF(TCMOLD),
     *                   TRABUF(TCMNEW)
C V52 - End
C               
        ELSE
           WRITE(PUNIT,905) STAT(TRABUF(TSTAT)),
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
     *                   COMMAND,
     *                   TRABUF(TCMTER),
     *                   TRABUF(TCMOLD),
     *                   TRABUF(TCMNEW)
        ENDIF

        IF(CT .EQ. TCSPT .AND. TRABUF(TSTAT) .EQ. GOOD) THEN
          IGS_MID = TRABUF(TCMDT4)
          IGS_MID = IAND(ISHFT(IGS_MID, 32), 'FFFFFFFF00000000'X) + IAND(TRABUF(TCMDT5), 'FFFFFFFF'X) 
          IF(CN .EQ. 5) WRITE(PUNIT, 9053) TRABUF(TCMDT1),
     *                                     TRABUF(TCMDT3), 
     *                                     TRABUF(TCMSRC), 
     *                                     IGS_MID
          IF(CN .EQ. 6) WRITE(PUNIT, 9054) TRABUF(TCMDT1), TRABUF(TCMDT3)
        ENDIF
C
        IF(CT .EQ. TCSPT .AND. TRABUF(TSTAT) .NE. GOOD .AND. CN .EQ. 5 .AND. TRABUF(TSUBERR) .EQ. 1) THEN  ! SPORT EVENT ALREADY CANCELLED
          IGS_MID = TRABUF(TCMDT4)
          IGS_MID = IAND(ISHFT(IGS_MID, 32), 'FFFFFFFF00000000'X) + IAND(TRABUF(TCMDT5), 'FFFFFFFF'X) 
          IF(CN .EQ. 5) WRITE(PUNIT, 9055) TRABUF(TCMDT1),
     *                                     TRABUF(TCMDT3), 
     *                                     TRABUF(TCMSRC), 
     *                                     IGS_MID
        ENDIF

C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        IF(IS_IGS_PRINTRA) THEN
            LLINCNT = LINCNT
        ENDIF
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        RETURN
C
C===========
C
C PRINT CROSS SYSTEM TRANSACTIONS
C
5000    CONTINUE
        LINCNT = LINCNT+1
        IF (TRABUF(TITYP).EQ.IEST) THEN
            WRITE(PUNIT,9327) STAT(TRABUF(TSTAT)),ERROR(TRABUF(TERR)),
     *                             TTYPE(TRABUF(TTYP)),SERIAL,
     *                             DISTIM(TRABUF(TTIM)),TRABUF(TTER),
     *                             TRABUF(TTRN),TRABUF(TCDC),
     *                             ITYPE(TRABUF(TITYP)),
     .                             TRABUF(TIERR),        
     *                             TRABUF(TISFT)
        ELSE
            WRITE(PUNIT,932) STAT(TRABUF(TSTAT)),ERROR(TRABUF(TERR)),
     *                             TTYPE(TRABUF(TTYP)),SERIAL,
     *                             DISTIM(TRABUF(TTIM)),TRABUF(TTER),
     *                             TRABUF(TTRN),TRABUF(TCDC),
     *                             ITYPE(TRABUF(TITYP)),
     .                             TRABUF(TIERR),        
     *                             TRABUF(TIBCH)
        ENDIF
C
        IF (DETAIL) THEN                      ! V13: If detailed report, then: 
          IF (TRABUF(TITYP) .EQ. IVAL) THEN
C---------------- V55 Begin -------------------------------------------
            I4TMP(1) = 0
            I4TMP(2) = 0 
            MESSID = I8TMP  
                    
            I4TMP(1) = ZEXT(TRABUF(TVOLMSERL_IL)) 
            I4TMP(2) = ZEXT(TRABUF(TVOLMSERM_IL))
                    
            SERIALNUM_OLM = DFLOAT(ZEXT(TRABUF(TVOLMSERH_IL)))*OVER8BYTES+DFLOAT(I8TMP)
            WRITE(SERIAL_AUX,990) SERIALNUM_OLM          
            SERIALNUM_OLMSTR = SERIAL_AUX(1:6)//'-'//SERIAL_AUX(7:8)//'-'//SERIAL_AUX(9:18)//'-'//SERIAL_AUX(19:21)           
                    
            IF(TRABUF(TVOLMCOMF_IL) .EQ. 1) THEN
                WRITE(PUNIT,980)  'Yes',MESSID,SERIALNUM_OLMSTR
            ELSE
                WRITE(PUNIT,985)  'No'
            ENDIF            
C---------------- V55 End -------------------------------------------                
            LINCNT = LINCNT + 11
            IF(TRABUF(TIVAGT).GT.0) THEN
              K = TRABUF(TIVAGT)
            ELSE
              K = TRABUF(TAGT)
            ENDIF
            WRITE(PUNIT,93211) K
            WRITE(PUNIT,9321)  (TRABUF(TIGAM1+K),K=0,TRABUF(TIBCH)-1)
            WRITE(PUNIT,9322)  (TRABUF(TIPCK1+K),K=0,TRABUF(TIBCH)-1)
            WRITE(PUNIT,9323)  (TRABUF(TIVRN1+K),
     *                         K=0,TRABUF(TIBCH)-1)
            WRITE(PUNIT,93231) (TRABUF(TIXRF   ),K=0,TRABUF(TIBCH)-1) ! ** V14
            WRITE(PUNIT,9324)  (TRABUF(TILTX1+K),K=0,TRABUF(TIBCH)-1)
            WRITE(PUNIT,93241) (TRABUF(TITIM1+K),K=0,TRABUF(TIBCH)-1)
            WRITE(PUNIT,93242) (TRABUF(TICDC1+K),K=0,TRABUF(TIBCH)-1)
            WRITE(PUNIT,93243) (TRABUF(TISTS1+K),K=0,TRABUF(TIBCH)-1)
            WRITE(PUNIT,93244) (TRABUF(TIPCKSTS1+K),K=0,TRABUF(TIBCH)-1)
            WRITE(PUNIT,9326)  (CMONY(TRABUF(TIPRZ1+K),12,1),
     *                           K=0,TRABUF(TIBCH)-1)
!-------->>V49 ---------------------------------------------------------
            WRITE(PUNIT,93261) (CMONY(TRABUF(TINETPRZ),12,1),
     *                           K=0,TRABUF(TIBCH)-1)
            LINCNT = LINCNT + 1
            IF(TRABUF(TIVMT) .EQ. IBVMT .AND. 
     *        TRABUF(TIVALM) .EQ. IVBM_BNK) THEN
              CALL FASTSET(BLANK,NIB,6)
              WRITE(CNIB,801) TRABUF(TINIBBB),TRABUF(TINIBBO),TRABUF(TINIBBA1),
     *                        TRABUF(TINIBBA2),TRABUF(TINIBCD)
              IF(TRABUF(TIPLIDTYP).EQ.PHONNBR) THEN
                WRITE(PUNIT,9328)  TRABUF(TIPLCARD)
                WRITE(PUNIT,93281) (NIB(I),I=1,6)
                LINCNT = LINCNT + 2
              ELSEIF(TRABUF(TIPLIDTYP).EQ.PLAYCRD) THEN
                WRITE(PUNIT,9329)  TRABUF(TIPLCARD)
                WRITE(PUNIT,93291) (NIB(I),I=1,6)
                LINCNT = LINCNT + 2
              ENDIF
            ENDIF
!-------- V49<<---------------------------------------------------------
          ELSE IF (TRABUF(TITYP).EQ.IISS) THEN
CV54            LINCNT = LINCNT + 5
            LINCNT = LINCNT + 6                                                 !V54
            WRITE(PUNIT,9425)  TRABUF(TIREP)
            WRITE(PUNIT,9426)  TRABUF(TINUM)
            WRITE(PUNIT,9421)  (TRABUF(TIGAM+K),K=0,TRABUF(TINUM)-1)
            WRITE(PUNIT,9423)  (TRABUF(TIPCK+K),K=0,TRABUF(TINUM)-1)
            WRITE(PUNIT,9424)  (TRABUF(TIRES+K),K=0,TRABUF(TINUM)-1)
            WRITE(PUNIT,9427)  TRABUF(TIXRF)                                    !V54
          ELSE IF (TRABUF(TITYP).EQ.ILOT) THEN
CV54            LINCNT = LINCNT + 7
            LINCNT = LINCNT + 8                                                 !V54
            WRITE(PUNIT,9435)  (TRABUF(TLREP))
            WRITE(PUNIT,9436)  (TRABUF(TLCLS))
            WRITE(PUNIT,9431)  (TRABUF(TLGAM))
            WRITE(PUNIT,9433)  (TRABUF(TLPCK))
            WRITE(PUNIT,9434)  (TRABUF(TLEND) - TRABUF(TLSTR)) 
            WRITE(PUNIT,9437)  CSMONY(TRABUF(TLAMT),10,1)
            WRITE(PUNIT,9438)  CSMONY(TRABUF(TLCOM),10,1)
            WRITE(PUNIT,9440)  TRABUF(TIXRF)                                    !V54
          ELSE IF (TRABUF(TITYP).EQ.ICAR) THEN
C---------------- V55 Begin -------------------------------------------
            I4TMP(1) = ZEXT(TRABUF(TGOLMMIDL_IL)) 
            I4TMP(2) = ZEXT(TRABUF(TGOLMMIDH_IL)) 
            MESSID = I8TMP  
                    
            I4TMP(1) = ZEXT(TRABUF(TGOLMSERL_IL)) 
            I4TMP(2) = ZEXT(TRABUF(TGOLMSERM_IL))
                    
            SERIALNUM_OLM = DFLOAT(ZEXT(TRABUF(TGOLMSERH_IL)))*OVER8BYTES+DFLOAT(I8TMP)
            WRITE(SERIAL_AUX,990) SERIALNUM_OLM          
            SERIALNUM_OLMSTR = SERIAL_AUX(1:6)//'-'//SERIAL_AUX(7:8)//'-'//SERIAL_AUX(9:18)//'-'//SERIAL_AUX(19:21)           
                    
            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
                WRITE(PUNIT,980)  'Yes',MESSID,SERIALNUM_OLMSTR
            ELSE
                WRITE(PUNIT,985)  'No'
            ENDIF            
C---------------- V55 End -------------------------------------------                 
CV54            LINCNT = LINCNT + 6
            LINCNT = LINCNT + 7                                                 !V54
            WRITE(PUNIT,9435)  (TRABUF(TCREP))
            WRITE(PUNIT,9436)  (TRABUF(TCCLS))
            WRITE(PUNIT,9431)  (TRABUF(TCGAM))
            WRITE(PUNIT,94331) (TRABUF(TCCAR))
            WRITE(PUNIT,94341) (TRABUF(TCEND) - TRABUF(TCSTA)) 
            WRITE(PUNIT,9439)  (TRABUF(TCCNT))
            WRITE(PUNIT,9440)  TRABUF(TIXRF)                                    !V54
          ELSE IF (TRABUF(TITYP).EQ.IQTA.OR.
     *             TRABUF(TITYP).EQ.IINV.OR.
     *             TRABUF(TITYP).EQ.ISET) THEN
C---------------- V55 Begin -------------------------------------------
            I4TMP(1) = ZEXT(TRABUF(TGOLMMIDL_IL)) 
            I4TMP(2) = ZEXT(TRABUF(TGOLMMIDH_IL)) 
            MESSID = I8TMP  
                    
            I4TMP(1) = ZEXT(TRABUF(TGOLMSERL_IL)) 
            I4TMP(2) = ZEXT(TRABUF(TGOLMSERM_IL))
                    
            SERIALNUM_OLM = DFLOAT(ZEXT(TRABUF(TGOLMSERH_IL)))*OVER8BYTES+DFLOAT(I8TMP)
            WRITE(SERIAL_AUX,990) SERIALNUM_OLM          
            SERIALNUM_OLMSTR = SERIAL_AUX(1:6)//'-'//SERIAL_AUX(7:8)//'-'//SERIAL_AUX(9:18)//'-'//SERIAL_AUX(19:21)           
            
            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
                WRITE(PUNIT,980)  'Yes',MESSID,SERIALNUM_OLMSTR
            ELSE
                WRITE(PUNIT,985)  'No'
            ENDIF            
C---------------- V55 End -------------------------------------------         
CV54            LINCNT = LINCNT + 4
            LINCNT = LINCNT + 5                                                 !V54
            WRITE(PUNIT,9441) TRABUF(TRGAM)
            WRITE(PUNIT,9442) TRABUF(TRCLS)
            WRITE(PUNIT,9443) TRABUF(TRNXT1)
            WRITE(PUNIT,9444) TRABUF(TRNXT2)
            WRITE(PUNIT,9445) TRABUF(TIXRF)                                     !V54
          ELSE IF(TRABUF(TITYP).EQ.IFIN) THEN
CV54            LINCNT = LINCNT + 5
            LINCNT = LINCNT + 6                                                 !V54
            WRITE(PUNIT,9452) TRABUF(TRTYP)
            WRITE(PUNIT,9451) TRABUF(TRSUB)
            WRITE(PUNIT,9453) TRABUF(TRCHN)
            WRITE(PUNIT,9454) TRABUF(TRCON1)
            WRITE(PUNIT,9455) TRABUF(TRCON2)
            WRITE(PUNIT,9456) TRABUF(TIXRF)                                     !V54
          ELSE IF(TRABUF(TITYP).EQ.IORD) THEN
C---------------- V55 Begin -------------------------------------------
            I4TMP(1) = ZEXT(TRABUF(TGOLMMIDL_IL)) 
            I4TMP(2) = ZEXT(TRABUF(TGOLMMIDH_IL)) 
            MESSID = I8TMP  
                    
            I4TMP(1) = ZEXT(TRABUF(TGOLMSERL_IL)) 
            I4TMP(2) = ZEXT(TRABUF(TGOLMSERM_IL))
                    
            SERIALNUM_OLM = DFLOAT(ZEXT(TRABUF(TGOLMSERH_IL)))*OVER8BYTES+DFLOAT(I8TMP)
            WRITE(SERIAL_AUX,990) SERIALNUM_OLM          
            SERIALNUM_OLMSTR = SERIAL_AUX(1:6)//'-'//SERIAL_AUX(7:8)//'-'//SERIAL_AUX(9:18)//'-'//SERIAL_AUX(19:21)           

            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
                WRITE(PUNIT,980)  'Yes',MESSID,SERIALNUM_OLMSTR
            ELSE
                WRITE(PUNIT,985)  'No'
            ENDIF            
C---------------- V55 End -------------------------------------------                
CV54            LINCNT = LINCNT + 3
            LINCNT = LINCNT + 4                                                 !V54
            WRITE(PUNIT,9461) TRABUF(TGPGAM)
            WRITE(PUNIT,9462) TRABUF(TGPNXT)
            WRITE(PUNIT,9463) CSMONY(TRABUF(TGPRCL),10,1)
            WRITE(PUNIT,9464) TRABUF(TIXRF)                                     !V54             
          ELSE IF(TRABUF(TITYP).EQ.IMNU) THEN
C---------------- V55 Begin -------------------------------------------
            I4TMP(1) = ZEXT(TRABUF(TGOLMMIDL_IL)) 
            I4TMP(2) = ZEXT(TRABUF(TGOLMMIDH_IL)) 
            MESSID = I8TMP  
                    
            I4TMP(1) = ZEXT(TRABUF(TGOLMSERL_IL)) 
            I4TMP(2) = ZEXT(TRABUF(TGOLMSERM_IL))
                    
            SERIALNUM_OLM = DFLOAT(ZEXT(TRABUF(TGOLMSERH_IL)))*OVER8BYTES+DFLOAT(I8TMP)
            WRITE(SERIAL_AUX,990) SERIALNUM_OLM          
            SERIALNUM_OLMSTR = SERIAL_AUX(1:6)//'-'//SERIAL_AUX(7:8)//'-'//SERIAL_AUX(9:18)//'-'//SERIAL_AUX(19:21)           

            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
                WRITE(PUNIT,980)  'Yes',MESSID,SERIALNUM_OLMSTR
            ELSE
                WRITE(PUNIT,985)  'No'
            ENDIF            
C---------------- V55 End -------------------------------------------                
CV54            LINCNT = LINCNT + 11
            LINCNT = LINCNT + 12                                                !V54
            WRITE(PUNIT,9471) (IAND(TRABUF(TSGAM+K),'00000FFF'X),K=0,7)
            WRITE(PUNIT,9471) (IAND(TRABUF(TSGAM+K),'00000FFF'X),K=8,15)
            WRITE(PUNIT,9471) (IAND(TRABUF(TSGAM+K),'00000FFF'X),K=16,23)
            WRITE(PUNIT,9471) (IAND(TRABUF(TSGAM+K),'00000FFF'X),K=24,31)
            WRITE(PUNIT,9471) (IAND(TRABUF(TSGAM+K),'00000FFF'X),K=32,39)
            WRITE(PUNIT,9472) (TRABUF(TSQTY+K),K=0,7)
            WRITE(PUNIT,9472) (TRABUF(TSQTY+K),K=8,15)
            WRITE(PUNIT,9472) (TRABUF(TSQTY+K),K=16,23)
            WRITE(PUNIT,9472) (TRABUF(TSQTY+K),K=24,31)
            WRITE(PUNIT,9472) (TRABUF(TSQTY+K),K=32,39)
            WRITE(PUNIT,9473) TRABUF(TSORD)
            WRITE(PUNIT,9474) TRABUF(TIXRF)                                     !V54          
          ELSE IF(TRABUF(TITYP).EQ.ICNF) THEN
C---------------- V55 Begin -------------------------------------------
            I4TMP(1) = ZEXT(TRABUF(TGOLMMIDL_IL)) 
            I4TMP(2) = ZEXT(TRABUF(TGOLMMIDH_IL)) 
            MESSID = I8TMP  
                    
            I4TMP(1) = ZEXT(TRABUF(TGOLMSERL_IL)) 
            I4TMP(2) = ZEXT(TRABUF(TGOLMSERM_IL))
                    
            SERIALNUM_OLM = DFLOAT(ZEXT(TRABUF(TGOLMSERH_IL)))*OVER8BYTES+DFLOAT(I8TMP)
            WRITE(SERIAL_AUX,990) SERIALNUM_OLM          
            SERIALNUM_OLMSTR = SERIAL_AUX(1:6)//'-'//SERIAL_AUX(7:8)//'-'//SERIAL_AUX(9:18)//'-'//SERIAL_AUX(19:21)           
                    
            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
                WRITE(PUNIT,980)  'Yes',MESSID,SERIALNUM_OLMSTR
            ELSE
                WRITE(PUNIT,985)  'No'
            ENDIF            
C---------------- V55 End -------------------------------------------                 
CV54            LINCNT = LINCNT + 3
            LINCNT = LINCNT + 4                                                 !V54
            WRITE(PUNIT,9481) TRABUF(TIINV1)
            WRITE(PUNIT,9482) TRABUF(TIINV2)
            WRITE(PUNIT,9483) TRABUF(TIINV3)
            WRITE(PUNIT,9485) TRABUF(TIXRF)                                     !V54
          ELSE IF(TRABUF(TITYP).EQ.IOACT) THEN
C---------------- V55 Begin -------------------------------------------
            I4TMP(1) = ZEXT(TRABUF(TGOLMMIDL_IL)) 
            I4TMP(2) = ZEXT(TRABUF(TGOLMMIDH_IL)) 
            MESSID = I8TMP  
                    
            I4TMP(1) = ZEXT(TRABUF(TGOLMSERL_IL)) 
            I4TMP(2) = ZEXT(TRABUF(TGOLMSERM_IL))
                    
            SERIALNUM_OLM = DFLOAT(ZEXT(TRABUF(TGOLMSERH_IL)))*OVER8BYTES+DFLOAT(I8TMP)
            WRITE(SERIAL_AUX,990) SERIALNUM_OLM          
            SERIALNUM_OLMSTR = SERIAL_AUX(1:6)//'-'//SERIAL_AUX(7:8)//'-'//SERIAL_AUX(9:18)//'-'//SERIAL_AUX(19:21)           
                    
            IF(TRABUF(TGOLMCOMF_IL) .EQ. 1) THEN
                WRITE(PUNIT,980)  'Yes',MESSID,SERIALNUM_OLMSTR
            ELSE
                WRITE(PUNIT,985)  'No'
            ENDIF            
C---------------- V55 End -------------------------------------------                
CV54            LINCNT = LINCNT + 3
            LINCNT = LINCNT + 4                                                 !V54
            WRITE(PUNIT,9484) TRABUF(TOINV1)
            WRITE(PUNIT,9482) TRABUF(TOINV2)
            WRITE(PUNIT,9483) TRABUF(TOINV3)
            WRITE(PUNIT,9485) TRABUF(TIXRF)                                     !V54
          ELSE IF (TRABUF(TITYP).EQ. IEST) THEN
            LINCNT = LINCNT + 1
            SEC = TRABUF(TIRSTTIM)
            HOUR = SEC/3600
            MNT = (SEC-HOUR*3600)/60
            SEC = SEC - (HOUR*3600+MNT*60)
            WRITE(PUNIT,9400) TRABUF(TISFT),TRABUF(TIRSTFLG),
     *                        HOUR,MNT,SEC, TRABUF(TIMINCB),
     *                        (TRABUF(TIPHONE1_1+I),I=0,2),
     *                        (TRABUF(TIPHONE2_1+I),I=0,2)
          ELSE IF(TRABUF(TITYP).EQ.IGTB) THEN
CV54            LINCNT = LINCNT + 6
            LINCNT = LINCNT + 7
            WRITE(PUNIT,9491) TRABUF(TIGMC)
            WRITE(PUNIT,9492) TRABUF(TIGMN+0)
            WRITE(PUNIT,9492) TRABUF(TIGMN+1)
            WRITE(PUNIT,9492) TRABUF(TIGMN+2)
            WRITE(PUNIT,9492) TRABUF(TIGMN+3)
            WRITE(PUNIT,9492) TRABUF(TIGMN+4)
            WRITE(PUNIT,9498) TRABUF(TIXRF)                                     !V54
          ELSE IF (TRABUF(TITYP).EQ.ISON) THEN
            LINCNT = LINCNT + 1
            WRITE(PUNIT,9402) TRABUF(TIGVT1),TRABUF(TIGVT2)
          ELSE IF(TRABUF(TITYP).EQ.IFSESON) THEN
            LINCNT = LINCNT + 2
            WRITE(PUNIT,9493) TRABUF(TIFSETYP)
            WRITE(PUNIT,9494) TRABUF(TIFSERSLT)
            IF ((TRABUF(TIFSETYP).EQ.0).AND.
     +          (TRABUF(TIFSERSLT).EQ.INOER)) THEN !SUCCESSFUL FSE SIGN-ON WITH
               LINCNT = LINCNT + 3
               WRITE(PUNIT,9495) TRABUF(TIFSEREP),
     *                            (TRABUF(TIFSENAMS+K),K=0,5)
               WRITE(PUNIT,9496) TRABUF(TIFSECLS)
               WRITE(PUNIT,9497) TRABUF(TIFSEOFF)
            ELSE
               LINCNT = LINCNT + 2
               WRITE(PUNIT,9495) TRABUF(TIFSEREP)
               WRITE(PUNIT,9496) TRABUF(TIFSECLS)
            ENDIF
            LINCNT = LINCNT + 1                                                 !V54
            WRITE(PUNIT,9498) TRABUF(TIXRF)                                     !V54
          ENDIF
        ENDIF  
C
        IF(TRABUF(TITYP).EQ.IVAL.AND.TRABUF(TSTAT).NE.REJT) THEN                
          DO 5010 I = 0,TRABUF(TIBCH)-1
            IF(TRABUF(TISTS1+I).EQ.INOER) THEN
              TOTAL(TRABUF(TTYP),1)   = TOTAL(TRABUF(TTYP),1)+1
              TOTAL(TRABUF(TTYP),2)   = TOTAL(TRABUF(TTYP),2)+
     *                                  TRABUF(TIPRZ1+I)
            ENDIF
5010      CONTINUE
        ENDIF
C
        TOTAL(TCRS+TRABUF(TITYP)+1,1) = TOTAL(TCRS+TRABUF(TITYP)+1,1) + 1
        TOTAL(TCRS+NUMCRS+1,1) = TOTAL(TCRS+NUMCRS+1,1) + 1
C
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        IF(IS_IGS_PRINTRA) THEN
            LLINCNT = LINCNT
        ENDIF
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        RETURN

C**********************************************************************
C
C PRINT PASSIVE VALIDATION AND RETURN TRANSACTIONS
C
6000    CONTINUE
        
C        COUNT  = 0
         AMOUNT = 0

       DO TCKS = 1, TRABUF(TPTCK)
            IF(TRABUF(TPSTS1+OFFTRA*(TCKS-1)).EQ.RETURND .OR. 
     *         TRABUF(TPSTS1+OFFTRA*(TCKS-1)).EQ.RETAFDR .OR.
     *         TRABUF(TPSTS1+OFFTRA*(TCKS-1)).EQ.VWINNER     ) THEN
 
               AMOUNT = AMOUNT + TRABUF(TPPAY1+OFFTRA*(TCKS-1))  

            ENDIF
        ENDDO

C        IF(TRABUF(TSTAT).EQ.GOOD .AND. TRABUF(TERR).EQ.NOER )THEN
C          TOTAL(TRABUF(TTYP),1) = TOTAL(TRABUF(TTYP),1) + COUNT
C          TOTAL(TRABUF(TTYP),2) = TOTAL(TRABUF(TTYP),2) + AMOUNT
C        ENDIF
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
C        IF(.NOT.DETAIL)RETURN
        IF(.NOT.DETAIL) THEN 
            IF(IS_IGS_PRINTRA) THEN
                LLINCNT = LINCNT
            ENDIF
            RETURN
        ENDIF
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------

C
C  CHECK IF NEW PAGE IS NEEDED
C
        IF(LINCNT.GT.(LINSPP-7)) THEN
              CALL TITLE('TRANSACTION FILE REPORT','    TMIR',
     *                    1,PUNIT,PAGE,DAYCDC,0,0)
              WRITE(PUNIT,900)
              LINCNT=7
        ENDIF
C
C PRINT MAIN LINE
C
        WRITE(PUNIT,1001) STAT(TRABUF(TSTAT)),ERROR(TRABUF(TERR)),
     *                   TTYPE(TRABUF(TTYP)),SERIAL,
     *                   DISTIM(TRABUF(TTIM)),TRABUF(TTER),
     *                   TRABUF(TTRN),TRABUF(TCDC),TRABUF(TGAM),
     *                   GTNAMES(TRABUF(TGAMTYP)),TRABUF(TGAMIND),
     *                   TRABUF(TSIZE),CMONY(AMOUNT,11,BETUNIT),
     *                   TRABUF(TPTCK),RETTYP(TRABUF(TPRETYP))

	IF (TRABUF(TPOFFTER).GT.0) THEN
	    WRITE(PUNIT,1002) TRABUF(TPOFFTER)
	ENDIF
        IF(TRABUF(TTYP).EQ.TVAL.AND.
     *     TRABUF(TVTYPE).EQ.VPNBNK) THEN
           CALL FASTSET(BLANK,NIB,6)
           WRITE(CNIB,801) TRABUF(TVNIBBB),TRABUF(TVNIBBO),TRABUF(TVNIBBA1),
     *                     TRABUF(TVNIBBA2),TRABUF(TVNIBCD)
           IF(TRABUF(TVPLIDTYP).EQ.PHONNBR) THEN               !V48
             WRITE(PUNIT,1003) TRABUF(TVPLCARD),(NIB(I),I=1,6) !V48
           ELSEIF(TRABUF(TVPLIDTYP).EQ.PLAYCRD) THEN           !V48
             WRITE(PUNIT,1004) TRABUF(TVPLCARD),(NIB(I),I=1,6) !V48
           ENDIF
        ENDIF
C
C PRINT DETAILED LINES FOR ALL
C TICKETS IN ONE TRANSACTION
C
        CALL PASTRN(TRABUF,PUNIT,LINES) 

        LINCNT = LINCNT + 1 + LINES

C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        IF(IS_IGS_PRINTRA) THEN
            LLINCNT = LINCNT
        ENDIF
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        RETURN
C===============================================================================
C       BASIC EM TRANSACTIONS PRINTOUT
C===============================================================================
7000    CONTINUE
C
        IF(LINCNT.GT.LINSPP) THEN
          CALL TITLE('TRANSACTION FILE REPORT','    TMIR',1,PUNIT,PAGE,DAYCDC,0,0)
          WRITE(PUNIT,900)
          LINCNT=7
        ENDIF        
C
        WRITE(PUNIT,97000) STAT(TRABUF(TSTAT)),
     *                     ERROR(TRABUF(TERR)),
     *                     TTYPE(TRABUF(TTYP)),
     *                     SERIAL,
     *                     DISTIM(TRABUF(TTIM)),
     *                     TRABUF(TTER),
     *                     TRABUF(TTRN),
     *                     TRABUF(TCDC),
     *                     TRABUF(TGAM),
     *                     GTNAMES(TRABUF(TGAMTYP)),
     *                     TRABUF(TGAMIND),
     *                     TRABUF(TSIZE),
     *                     TTYPE(TRABUF(TEUTYP))
C
        LINCNT = LINCNT + 1
C
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        IF(IS_IGS_PRINTRA) THEN
            LLINCNT = LINCNT
        ENDIF
C----+------------------------------------------------------------------
C V53| TMIR Report generation bug fix
C----+------------------------------------------------------------------
        RETURN        
C
1001    FORMAT(1X,3(2X,A4),I10,1X,A8,I6,Z4,I6,I5,1X,A8,
     *         I5,I5,' AMT>',A11,1X,'TCKS> ',I2,1X,'RETTYP> ',A14)
1002	FORMAT(3X,'TPOFFTER> ',I5.5)
!1003    FORMAT(3X,'PLAYER CARD/PHONE NUMBER> ',I10,'  NIB> ',6A4) 
1003    FORMAT(3X,'PHONE NUMBER> ',I10,'  NIB> ',6A4) !V48
1004    FORMAT(3X,' PLAYER CARD> ',I10,'  NIB> ',6A4) !V48

932     FORMAT(1X,3(2X,A4),I10,1X,A8,I6,Z4,I6,1X,A4,4X,I3,3X,I9)
9327    FORMAT(1X,3(2X,A4),I10,1X,A8,I6,Z4,I6,1X,A4,4X,I3,12X,
     *         '   GVT Rev:',A4)
9321    FORMAT(10X,'Instant game #    ',7(I12))
93211   FORMAT(10X,'Retailer, who gets the credit ',I12.7)
9322    FORMAT(10X,'Instant pack #    ',7(I12))
9323    FORMAT(10X,'Instant virn #    ',7(I12))   
93231   FORMAT(10X,'Instant crs ref # ',7(I12)) 
9324    FORMAT(10X,'Instant latex     ',7(I12))
93241   FORMAT(10X,'Instant Time      ',7(I12))
93242   FORMAT(10X,'Instant CDC       ',7(I12))
93243   FORMAT(10X,'Instant Status    ',7(I12))
93244   FORMAT(10X,'Instant Pack Sts  ',7(I12))
9326    FORMAT(10X,'Actual Amount     ',7(A12))
93261   FORMAT(10X,'Net Prize Amount  ',7(A12))
9328    FORMAT(10X,'Phone Number        ',I10) !V49
93281   FORMAT(10X,'Nib    ',6A4) !V49
9329    FORMAT(10X,'Player Card         ',I10) !V49
93291   FORMAT(10X,'Nib    ',6A4) !V49

9400    FORMAT(10X,'Revision:',A4,'  Reset Flag:',Z2,'  Reset Time:',
     *         2(I2.2,':'),I2.2,'  Min. To Call Back:',I5,
     *         '  Phone 1:',3A4,'  Phone 2:',3A4)
9401    FORMAT(10X,'GVT ID  :',Z8.8,Z8.8,'  Agt No :',I7.7,'  Agt pass:',
     *         I4.4,'  Class :',I2,'  Action 1:',I2,' Real stn ',I4,
     *         ' Status ',I1)
9402    FORMAT(10X,'GVT ID  :',Z8.8,Z8.8)
C
9421    FORMAT(10X,'Instant game #   ',10(I8))
9423    FORMAT(10X,'Instant book #   ',10(I8))   
9424    FORMAT(10X,'Instant result # ',10(I8)) 
9425    FORMAT(10X,'Concessionnaire #',1X,I7.7)
9426    FORMAT(10X,'Number of books  ',1X,I10)
9427    FORMAT(10X,'Instant crs ref #',1X,I8)                                   !V54
C
9431    FORMAT(10X,'Instant game #   ',(I12))
9433    FORMAT(10X,'Instant book #   ',(I12))   
94331   FORMAT(10X,'Instant carton # ',(I12))   
9434    FORMAT(10X,'Instant # tickets',(I12)) 
94341   FORMAT(10X,'Instant # packs  ',(I12)) 
9435    FORMAT(10X,'Concessionnaire# ',(I12))
9436    FORMAT(10X,'Instant class #  ',10X,I2.2)
9437    FORMAT(10X,'Instant amount   ',2X,A10)
9438    FORMAT(10X,'Instant commiss  ',2X,A10)
9439    FORMAT(10X,'Instant pack cnt ',(I12))
9440    FORMAT(10X,'Instant crs ref #',I12)                                     !V54
C
9441    FORMAT(10X,'Instant game #   ',I10)
9442    FORMAT(10X,'Instant class #  ',I10)
9443    FORMAT(10X,'Instant nxt game ',I10)
9444    FORMAT(10X,'Instant nxt book ',I10)
9445    FORMAT(10X,'Instant crs ref #',I10)                                     !V54
C
9451    FORMAT(10X,'Instant rpt clss ',I10)
9452    FORMAT(10X,'Instant rpt type ',I10)
9453    FORMAT(10X,'Instant chain #  ',I10)
9454    FORMAT(10X,'Instant nxt game ',I10)
9455    FORMAT(10X,'Instant nxt book ',I10)
9456    FORMAT(10X,'Instant crs ref #',I10)                                     !V54
C
9461    FORMAT(10X,'Instant game #   ',I10)
9462    FORMAT(10X,'Instant nxt game ',I10)
9463    FORMAT(10X,'Instant cred lim ',A10)
9464    FORMAT(10X,'Instant crs ref #',I10)                                     !V54
C
9471    FORMAT(10X,'Instant game     ',8(I9))
9472    FORMAT(10X,'Instant quantity ',8(I9))
9473    FORMAT(10X,'Instant order #  ',I10)
9474    FORMAT(10X,'Instant crs ref #',I10)                                     !V54
C
9481    FORMAT(10X,'Instant invoice# ',I10)
9482    FORMAT(10X,'Instant invoice# ',I10)
9483    FORMAT(10X,'Instant invoice# ',I10)
9484    FORMAT(10X,'Instant pack cnt ',I10)
9485    FORMAT(10X,'Instant crs ref #',I10)                                     !V54
C
9491    FORMAT(10X,'Instant class    ',I10)
9492    FORMAT(10X,'Instant game #   ',I10)
C
9493    FORMAT(10X,'Signon/off       ',8X,I2)
9494    FORMAT(10X,'Result           ',I10)
9495    FORMAT(10X,'FSE              ',I10,1X,6A4)
9496    FORMAT(10X,'Class            ',I10)
9497    FORMAT(10X,'Office           ',I10)
9498    FORMAT(10X,'Instant crs ref #',I10)                                     !V54
C
C===========
C
CCC     FORMAT statements

800     FORMAT(A2,I7.7,A2)
801     FORMAT(I4.4,1X,I4.4,I9.9,I2.2,1X,I2.2)
CV54900     FORMAT(/,' STATUS ERROR  TYPE',4X,'SERIAL',5X,
CV54     *         'TIME  TERM SEQ  DATE GAME GAMETYP  GIND ',
CV54     *         'SIZE  BEG        END     JOKER ',11X,
CV54     *          'FRACTION','  X BET',     
CV54     *         /,1X,131('='),/)
900     FORMAT(/,' STATUS ERROR  TYPE',4X,'SERIAL',5X,                          !V54
     *         'TIME  TERM SEQ  DATE GAME GAMETYP  GIND ',
     *         'SIZE  BEG        END     JOKER CMIL  M1LH',1X,
     *          'FRACTION','  X BET',
     *         /,1X,131('='),/)
901     FORMAT(1X,3(2X,A4),
     *         I10,1X,
C     *         I0,1X,
C     *         I4,1X,
     *         A8,I6,Z4,I6,I5,1X,A8,
     *         I5,I5,I5,1X,A4,1X,I5,A2,2X,A11,A10,1X,I4,1X,A4)   
C---------------- V55 Begin -------------------------------------------       
980     FORMAT(6X,'OLM Channel > ',A3,3X,'Message ID > ',I0,3X,'OLM Serial # > ',A)
985     FORMAT(6X,'OLM Channel > ',A3,3X)
990     FORMAT(F22.0)
C---------------- V55 End ---------------------------------------------
9018    FORMAT(3X,'BANK NUMBER ----> ',I8.8,' - ',I8.8,2X,
     *            'CASH NUM --> ',I10,2X,
     *            'TFILE:',A8,3X,A11,' Kb:',I4,' Ke:',I4,
     *            ' sold>',I5)
9019    FORMAT(3X,'BANK NUMBER ----> ',I8.8,' - ',I8.8,2X,
     *            'CANCEL NUM > ',I10,2X,
     *            'TFILE:'A8,3X,A11,' Kb:',I4,' Ke:',I4,
     *            ' sold>',I5)
90191   FORMAT(3X,'EP ',I1,' BANKNUMBER > ',I8.8,' - ',I8.8,2X,
     *            '                 ',10X,2X,
     *            'TFILE:',A8,3X,A11,' Kb:',I4,' Ke:',I4,
     *            ' sold>',I5)
90192   FORMAT(3X,'BANK NUMBER ----> ',I8.8,' - ',I8.8,2X,
     *            'LINKED WAGER --> ',I10,2X,
     *            'TFILE:',A8,3X,A11,' Kb:',I4,' Ke:',I4,
     *            ' sold>',I5)
90193   FORMAT(3X,'LINKED WAGER --> ',I10)
90194   FORMAT(3X,'BANK NUMBER > ',I8.8,' - ',I8.8,2X,
     *            '                 ',10X,2X,
     *            'TFILE:',A8,3X,A11,' Kb:',I4,' Ke:',I4,
     *            ' sold>',I5)
9020    FORMAT(3X,'VALIDATION SERIAL NUMBER > ',I4,1X,I10,' OPAMT:',A11)
9021    FORMAT(3X,'VALIDATION SER. NR. (CDC) > ',I3,1X,I10)
902     FORMAT(3X,20A4)
9022    FORMAT(3X,A4,'SUPER 14: ',19A4)
903     FORMAT(1X,3(2X,A4),I10,1X,A8,I6,Z4,I6,I5,1X,A8,
     *         I5,I5,' AMT>',A11,' REF>',A11,' EXCH>',I9)
904     FORMAT(1X,3(2X,A4),I10,1X,A8,I6,Z4,I6,I5,1X,A8,
     *         I5,I5,1X,A4,' OLD>',Z8,' NEW>',Z8,' AGT> ',A8)
C
C V52 - Start 
C9041    FORMAT(5X,'CLASS > ',I3 ,1X,' SUBCLASS > ',I3, 1X, ' CYCLE > ',I3.3,'/', I4.4
C     *          , 1X, ' AGT TO REP > ',A8, /)
CV549041    FORMAT(10X,'Class> ',I3 ,1X,' Subclass> ',I3, 1X, ' Cycle> ',I3.3,'/', I4.4
CV54     *          , 1X, ' Agent to report on> ',A8, /)
9041    FORMAT(10X,'Class> ',I0,                                                !V54
     *         1X,' Subclass> ',I0,
     *         1X,' MsQ Reference #> ',I0,
     *         1X,' Cycle> ',I3.3,'/', I4.4,
     *         1X,' Agent to report on> ',A8,/)
C V52 - End
C 
905     FORMAT(1X,3(2X,A4),I10,1X,A8,I6,Z4,I6,I5,1X,A8,
     *         I5,I5,1X,A8,' TER>',I4,' OLD>',Z8,' NEW>',Z8)
9051    FORMAT(1X,3(2X,A4),I10,1X,A8,I6,Z4,I6,I5,1X,A8,
     *         I5,I5,1X,A8,' TER>',I4,' OLD>',2Z8.8,/,T97' NEW>',2Z8.8)
C
C V52 - Start
9052    FORMAT(1X,3(2X,A4),I10,1X,A8,I6,Z4,I6,I5,1X,A8,
     *         I5,I5,1X,A8,' TER>',I4,' OLD>',Z8.8,' NEW>',Z8.8)
C
C V52 - End
C
9053    FORMAT(9X, 'DRAW', X, I6.6, X, 'EVENT NUMBER', X, I2.2, X, 'HAS BEEN CANCELLED. SOURCE:', X, A4, ', MESID:', X, I)
9054    FORMAT(9X, 'DRAW', X, I6.6, X, 'SET', X, I2.2, X, 'MAX CAN EVENTS TO CANCEL A DRAW')
9055    FORMAT(9X, 'DRAW', X, I6.6, X, 'EVENT NUMBER', X, I2.2, X, 'WAS ALREADY CANCELLED. SOURCE:', X, A4, ', MESID:', X, I)
C
907     FORMAT(3X,20A4,4X,'SYS TYPE > ',A7,3X,'ROWS > ',I5)
9071    FORMAT(3X,20A4,4X,'SYS TYPE > ',A7,3X,'ROWS > ',I5,' COUPID>',I2)
9072    FORMAT(3X,20A4,37X,' COUPID>',I2)
912     FORMAT(3X,20A4,3X,'ALT1> ',I2.2,3X,'ALT2> ',I2.2,   
     *         3X,'ALT3> ',I2.2)    
9121    FORMAT(3X,20A4,3X,'ALT1> ',I2.2,3X,'ALT2> ',I2.2,   
     *         3X,'ALT3> ',I2.2,'  RANK')    
9122    FORMAT(3X,20A4,3X,'ALT1> ',I2.2,3X,'ALT2> ',I2.2,   
     *         3X,'ALT3> ',I2.2,'  COUPID:',I3)    
920	FORMAT(1X,'Online Agent Update. ', I2,' items:')
921	FORMAT(1X,I2,' - ',A8,1X,'(',I2,' bytes) ','"',<DLEN>A1,'"')
9221    FORMAT(1X,'Error code ',I3.3)        !MXSRV
9222    FORMAT(1X,'Rpc Tag    ',64A1)        !MXSRV
9223    FORMAT(1X,'Data       ',60Z2.2)      !MXSRV
97000   FORMAT(1X,3(2X,A4),I10,1X,A8,I6,Z4,I6,I5,1X,A8,I5,I5,1X,'EMTYP: ',A4)

C V52 - Start
930     FORMAT(10X,'Terminal Message ID             ',I0)
940     FORMAT(10X,'MsgQ Reference #                ',I0)
950     FORMAT(10X,'Error Code                      ',A,A<CXERR_LEN>)
9501    FORMAT(10X,'Error Code                      ',I0,A,A<CXERR_LEN>)
960     FORMAT(10X,'Trx Reprint Type                ',A)
9601    FORMAT(10X,'Trx Reprint Type                ',I0,A)
970     FORMAT(10X,'Trx External Ref Serial #       ',I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',A3)
9701    FORMAT(10X,'Trx External Ref Serial #       ',I2.2,I2.2,I2.2,'-',I2.2,'-',I10.10,'-',I3.3)
C9701    FORMAT(10X,'Trx External Ref Serial #       ',I0,'/',I0,'/',I0,'-',I0,'-',I0,'-',I0) !!! REMOVER ESTA FORMATACAO TAL COMO A SUA RESPECTIVA CHAMADA !!!
CV54971     FORMAT(10X,'Class> ',I0,1X, ' Subclass> ',I0,1X, ' Host Message ID> ',I0)
971     FORMAT(10X,'Class> ',I0,
     *         1X,' Subclass> ',I0,
     *         1X,' MsgQ Ref #> ',I0,
     *         1X,' Host Message ID> ',I0,
     *         1X,' XSystems ND> ',B2.2,/)
9710    FORMAT(10X,'Class> ',I0,
     *         1X,' Subclass> ',I0,
     *         1X,' MsgQ Ref #> ',I0,
     *         1X,' Host Message ID> ',I0,
     *         1X,' XGames ND> ',B3.3,/)
C V52 - End
972     FORMAT(10X,'Class> ',I0,1X, ' Subclass> ',I0,1X, ' MsgQ Ref #> ',I0,/)  !V54
C
        END


C
C BUILD IMAGE FOR PASSIVE VALIDATION TRANSACTIONS
C ==============================================================
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PASTRN(TRABUF,PUNIT,LINES)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
C
C PARAMETERS
C
        INTEGER*4 PUNIT, LINES
C
C LOCAL VARIABLES
C
        INTEGER*4 TCKS, NUMTCKS
	CHARACTER*4 VALST_PAS(0:20),RETST_PAS(0:20),STR

        DATA VALST_PAS/'NWIN','BSVL','REDP','NWSE','NBCH','    ','    ', !V48 - added NBCH
     *                 'VWIN','APAD','UTKT','RTKT','WEMI','BTCK','BSER',
     *                 'BTEN','PTCK','    ','    ','    ','    ','    '/

        DATA RETST_PAS/'RET ','HRTC','CLEM','NOEM','SLTC','RADR','    ',
     *                 'VWIN','APAD','UTKT','RTKT','WEMI','BTCK','BSER',
     *                 'BTEN','PTCK','    ','    ','    ','    ','    '/

        NUMTCKS = TRABUF(TPTCK)
        LINES   = TRABUF(TPTCK)

!-------->>V48 -----------------------------------------------------------------
!        DO TCKS = 1, NUMTCKS
!	    IF (TRABUF(TTYP).EQ.TRET) THEN
!		STR = RETST_PAS(TRABUF(TPSTS1  + OFFTRA*(TCKS-1)))
!	    ELSE
!		STR = VALST_PAS(TRABUF(TPSTS1  + OFFTRA*(TCKS-1)))
!	    ENDIF
!
!            WRITE(PUNIT,900) TRABUF(TPNUM1  + OFFTRA*(TCKS-1)),
!     *                       TRABUF(TPEMIS1 + OFFTRA*(TCKS-1)),
!     *                       TRABUF(TPSER1  + OFFTRA*(TCKS-1)),
!     *                       TRABUF(TPTEN1  + OFFTRA*(TCKS-1)),
!     *                       STR,
!     *                       CMONY(TRABUF(TPPAY1  + OFFTRA*(TCKS-1)),10,VALUNIT)
!        ENDDO
!
!        RETURN
!900     FORMAT(9X,'TCK>',I6,' EMIS>',I6,' SER>',I2,' FRA>',I2,
!     *         ' STS> ',A4,' AMT>',A10)

        IF (TRABUF(TTYP).EQ.TVAL .AND. NUMTCKS .EQ. 1) THEN
            STR = VALST_PAS(TRABUF(TPSTS1))
            WRITE(PUNIT,900) TRABUF(TPNUM1),
     *                       TRABUF(TPEMIS1),
     *                       TRABUF(TPSER1),
     *                       TRABUF(TPTEN1),
     *                       STR,
     *                       CMONY(TRABUF(TPPAY1),10,VALUNIT),
     *                       CMONY(TRABUF(TVOPPAY),10,VALUNIT)
        ELSE
          DO TCKS = 1, NUMTCKS
            IF (TRABUF(TTYP).EQ.TRET) THEN
              STR = RETST_PAS(TRABUF(TPSTS1  + OFFTRA*(TCKS-1)))
            ELSE
              STR = VALST_PAS(TRABUF(TPSTS1  + OFFTRA*(TCKS-1)))
            ENDIF
          
            WRITE(PUNIT,901) TRABUF(TPNUM1  + OFFTRA*(TCKS-1)),
     *                       TRABUF(TPEMIS1 + OFFTRA*(TCKS-1)),
     *                       TRABUF(TPSER1  + OFFTRA*(TCKS-1)),
     *                       TRABUF(TPTEN1  + OFFTRA*(TCKS-1)),
     *                       STR,
     *                       CMONY(TRABUF(TPPAY1  + OFFTRA*(TCKS-1)),10,VALUNIT)
          ENDDO
        ENDIF

        RETURN
900     FORMAT(9X,'TCK>',I6,' EMIS>',I6,' SER>',I2,' FRA>',I2,
     *         ' STS> ',A4,' AMT>',A10,' NET AMT>',A10)       !V48
901     FORMAT(9X,'TCK>',I6,' EMIS>',I6,' SER>',I2,' FRA>',I2,
     *         ' STS> ',A4,' AMT>',A10)
!-------- V48<<-----------------------------------------------------------------

	END


