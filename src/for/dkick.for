C
C SUBROUTINE DKICK
C  
C V25 30-NOV-2010 FJG TWEMSER/TWEMCHK replaced by TWLNKSER/TWLNKCHK
C V24 13-JUL-2009 FJG ADD QP FLAG TO STANDALONE JOKER
C V23 25-MAR-2009 MMO CHANGED TWEMSER, TWEMCHK EXTERNAL EM/JOKER.
C V22 10-MAR-2009 MMO JOCKER/EM.
C V21 16-MAR-2000 RXK Number of simple bets set.
C V20 02-MAR-2000 RXK Agent flag not checked for promotion.
C V19 28-FEB-2000 RXK Promotion ("add 1 free week") added.
C V18 02-FEB-2000 UXN TNFRAC added.
C V17 16-MAR-1999 RXK Gtyp+gind 5+3 bits change.
C V16 04-FEB-1999 RXK Syntax error 120 changed
C V15 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting, 
C                     Instant Pass Thru Phase 1
C V15 28-NOV-1993 HXK Joker must be requested
C V14 07-NOV-1993 HXK Moved checksum to after last modification of meslen.
C V13 07-NOV-1993 GXA Moved check of checksum earlier in code.
C V12 17-AUG-1993 HXK Fix for mktmes rev bugs
C V11 27-JUL-1993 CXK Corrected bank numbers (SMH)
C V10 19-JUL-1993 GXA Corrected retreval of Kicker#'s added by GETQUE.
C V09 16-JUL-1993 GXA Added, decode of options bytes, changed check for 
C                     revisions to use secondary error filed for bitmap 
C                     (TSUBERR), check if we have ticket text befor we 
C                     check ticket text revisions.
C V08 23-JUN-1993 SXH Check kicker game before kicker control/text checks
C V07 14-JUN-1993 SXH Added bank cards, tidied up control/text revisions
C V06 10-MAY-1993 SXH Released for Finland VAX
C V05 21-JAN-1993 DAB Initial Release Based on Netherlands Bible, 12/92, 
C                     and Comm 1/93 update.  DEC Baseline
C V04 13-APR-1992 GCAN SET TWBEG AND TWEND HERE INSTEAD OF IN KIKCHK.
C V03 22-JAN-1992 GCAN SET TOTAL AMOUNT IN WAGER.
C V02 01-NOV-1991 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C ** Source - dkick.for **
C SUBROUTINE TO DECODE KICKER WAGERS
C
C ROUTINE CAN GENERATE FOLLOWING 'SYNTERRCOD'
C         0     NO ERROR
C        10     INVALID CHECKSUM
C       100     GAME INDEX OUT OF RANGE
C       105     INVALID OPTION, QUICKPICK FLAGS PRESENT
C       110     INVALID OPTION, BET MULTIPLIERS PRESENT
C       115     JOKER NUMBER MISSING AND NOT REQUESTED
C       120     INVALID NUMBER OF BOARDS FOR SYSTEM
C       125     GAME INDEX OUT OF RANGE
C       130     DRAW NUMBER OUT OF RANGE
C       135     DURATION OUT OF RANGE
C       140     CONFLICTING OPTION KICKER NOT SOLD, KICKER WAGER
C       145     WAGER AMOUNT NOT SET
C       150     MULTIDRAW COUNT INVALID
C       160     INCORRECT BANK ACCOUNT ID 
C       162     INCORRECT BANK ACCOUNT NUMBER
C       165     INCORRECT SYSTEM TYPE
C       170     INCORRECT SYSTEM SIZE
C       180     INCORRECT KICKER NUMBER
C       190     INCORRECT SERIAL NUMBER OF EM WAGER
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
        SUBROUTINE DKICK(TERMES,TRABUF,MESLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:LSYSCOM.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
        ! arguments
        BYTE       TERMES(*)        ! terminal message
 
        INTEGER*2  MESLEN           ! message length

        ! variables
        INTEGER*4  TERNUM           ! terminal number
        INTEGER*4  TEMP             ! temp variable
        INTEGER*4  TEMP1            !   "      "
        INTEGER*4  TEMP2            !   "      "
        INTEGER*4  TEMP3            !   "      "
        INTEGER*4  TEMP4            !   "      "
        INTEGER*4  OPTIONS          ! options byte
        INTEGER*4  IND              ! offset counter in terminal message
        INTEGER*4  CHKLEN           ! check sum length
        INTEGER*4  MYCHKSUM         ! calculated checksum
        INTEGER*4  ST               ! return status
        INTEGER*4  TREV             ! game text revision
        INTEGER*4  CREV             ! game control revision
        INTEGER*4  TIKTREV          ! ticket text revision
        INTEGER*4  JOKCREV          ! joker control revision
        INTEGER*4  JOKTREV          ! joker text revision
        INTEGER*4  JOKTIKTREV       ! joker ticket text revision
        INTEGER*4  KGIND            ! joker game index
        INTEGER*4  BNKLO
        INTEGER*4  BNKHI
C                   
        INTEGER*2  I2TEMP(2)        ! temp variable
        INTEGER*2  I2CREV(2)        ! I2 game control revision
        INTEGER*2  I2TREV(2)        ! I2 game text revision
        INTEGER*2  I2TIKTREV(2)     ! I2 ticket text revision
        INTEGER*2  I2JOKCREV(2)     ! I2 joker control revision
        INTEGER*2  I2JOKTREV(2)     ! I2 joker text revision
        INTEGER*2  I2JOKTIKTREV(2)  ! I2 joker ticket text revision

        LOGICAL    KICKER 
        LOGICAL    ADVANCE          ! joker advanced draw (not used in Finland)
        LOGICAL    JOK1_PART        ! joker 1 participation 
        LOGICAL    JOK2_PART        ! joker 2 participation 
        LOGICAL    JOK1_REQ         ! joker 1 number requested
        LOGICAL    JOK2_REQ         ! joker 2 number requested
        LOGICAL    JOK_TERM         ! terminal sending joker number

        EQUIVALENCE (TEMP,       I2TEMP)
        EQUIVALENCE (CREV,       I2CREV)              
        EQUIVALENCE (TREV,       I2TREV)              
        EQUIVALENCE (TIKTREV,    I2TIKTREV)           
        EQUIVALENCE (JOKCREV,    I2JOKCREV)           
        EQUIVALENCE (JOKTREV,    I2JOKTREV)           
        EQUIVALENCE (JOKTIKTREV, I2JOKTIKTREV)         

        ! start of code
C
C GET GENERIC GAME INFORMATION
C
        JOK1_PART       = .FALSE.
        JOK2_PART       = .FALSE.
        JOK1_REQ        = .FALSE.
        JOK2_REQ        = .FALSE.
        JOK_TERM        = .FALSE.
        KICKER          = .FALSE.
        ADVANCE         = .FALSE.
        TERNUM          = 0
        SYNTERRCOD      = 0
        TRABUF(TSTAT)   = REJT
        TRABUF(TGAMTYP) = TKIK
C
C GET TRANSACTION NUMBER
C
        TEMP         = ZEXT(TERMES(1))
        TRABUF(TTRN) = IAND(TEMP,15)
C
C GET CHECKSUM
C
        TEMP1 = ZEXT(TERMES(3))
        TEMP2 = ZEXT(TERMES(4))
        TRABUF(TCHK) = ISHFT(TEMP1,8) + TEMP2
C
C GET STATISTICS
C
        TRABUF(TTSTCS) = ZEXT(TERMES(5))
C
C GET GAME INDEX AND SYSTEM TYPE
C
        TEMP = ZEXT(TERMES(7))
        TRABUF(TWSYST)  = IAND(TEMP,15)
        TRABUF(TGAMIND) = ISHFT(TEMP,-4)

        ! check game number
        IF(TRABUF(TGAMIND).GE.1.AND.TRABUF(TGAMIND).LE.MAXIND)
     *    TRABUF(TGAM) = GTNTAB(TKIK,TRABUF(TGAMIND))
        IF(TRABUF(TGAM).LT.1.OR.TRABUF(TGAM).GT.MAXGAM) THEN
            TRABUF(TERR) = SYNT
            SYNTERRCOD   = 100
            RETURN
        ENDIF
        TRABUF(TWKGME)  = TRABUF(TGAM)
        TRABUF(TWKFLG) = 1
C
C GET DURATION AND NUMBER OF BOARDS
C
        TEMP    = ZEXT(TERMES(8))
        TRABUF(TWNBET)=IAND(TEMP,15)
        IF(TRABUF(TWSYST).NE.NOSYS .AND. TRABUF(TWNBET).NE.1) THEN
            TRABUF(TERR) = SYNT
            SYNTERRCOD   = 120
            RETURN
        ENDIF
        TRABUF(TWDUR)=ISHFT(TEMP,-4)
C
C Fractioning
C
        TRABUF(TFRAC)  = MAXFRC(TRABUF(TGAM))
        TRABUF(TNFRAC) = 1
C
C GET OPTIONS FIELD
C
        TEMP1 = ZEXT(TERMES(9))
        TEMP2 = ZEXT(TERMES(10))
        OPTIONS = ISHFT(TEMP1,8) + TEMP2
        IND=11
C
C CHECK FOR CONTROL REVISION 
C
        IF(IAND(OPTIONS,'8000'X).NE.0) THEN
            CREV=0
            CALL MOVBYT(TERMES,IND,CREV,3,2)
            IND=IND+2
            TEMP = KIKREV(TRABUF(TGAMIND))
            IF (I2CREV(2) .NE. I2TEMP(1)) THEN
                TRABUF(TERR) = GREV
                TRABUF(TSUBERR) = GREV_GAMCLT
            END IF
        ENDIF
C
C CHECK FOR TEXT REVISION (IGNORE FOR JOKER)
C
        IF(IAND(OPTIONS,'4000'X).NE.0) THEN
           TREV=0
           CALL MOVBYT(TERMES,IND,TREV,3,2)
           IND=IND+2
           TEMP = KIKREV(TRABUF(TGAMIND))
           IF(I2TREV(2) .NE. I2TEMP(2)) THEN
              TRABUF(TERR) = GREV
              TRABUF(TSUBERR) = TRABUF(TSUBERR) + GREV_GAMTXT
           END IF
        ENDIF
C
C CHECK FOR TICKET TEXT REVISION 
C
        IF(IAND(OPTIONS,'2000'X).NE.0) THEN
            TIKTREV=0
            CALL MOVBYT(TERMES,IND,TIKTREV,3,2)
            IND=IND+2
            TEMP = TKTMRV(TRABUF(TGAM))
            IF (I2TIKTREV(2) .NE. I2TEMP(1).AND.
     *          TKTMLN(TRABUF(TGAM)).GT.0)         THEN
                TRABUF(TERR) = GREV
                TRABUF(TSUBERR) = TRABUF(TSUBERR) + GREV_GAMTKT
            END IF
        ENDIF
C
C CHECK FOR JOKER 1 PARTICIPATION 
C
        IF(IAND(OPTIONS,'0080'X).NE.0) THEN
            JOK1_PART = .TRUE.
            TRABUF(TWKFLG) = 1
        ENDIF
C
C CHECK FOR JOKER 2 PARTICIPATION 
C
        IF(IAND(OPTIONS,'0040'X).NE.0) THEN
            JOK2_PART = .TRUE.
            TRABUF(TWKFLG2) = 1
        ENDIF
C
C CHECK FOR JOKER 1 REQUEST, 
C
        IF(IAND(OPTIONS,'0020'X).NE.0) THEN
            JOK1_REQ =.TRUE.
        END IF
C
C CHECK FOR JOKER 2 REQUEST
C
        IF(IAND(OPTIONS,'0010'X) .NE. 0) THEN
            JOK2_REQ =.TRUE.
        END IF

C
C CHECK FOR TERMINAL SENDING JOKER NUMBER
C
        IF(IAND(OPTIONS,'0002'X) .NE. 0) THEN
            JOK_TERM = .TRUE.
        ENDIF
C
C
C check for joker 1 problem - participation but no request or sending 
C
        IF (JOK1_PART .AND. (.NOT. JOK1_REQ .AND. .NOT. JOK_TERM)) THEN
            TRABUF(TERR) = SYNT
            SYNTERRCOD=115
            RETURN
        END IF
C
C if requested get the joker number put there
C by GETQUE/DISPAT and decrease message length by 3 bytes
C for each joker number
C
        IF (JOK1_REQ) THEN
          IF (JOK2_REQ) THEN
              ! do both joker 1 and 2
              TEMP = 0
              CALL MOVBYT(TERMES,ZEXT(MESLEN-(6-1)), TEMP, 1, 3) 
              TRABUF(TWKICK) = TEMP
              TEMP = 0
              CALL MOVBYT(TERMES,ZEXT(MESLEN-(3-1)), TEMP, 1, 3) 
              TRABUF(TWKICK2) = TEMP
              MESLEN = MESLEN - 6
          ELSE
              ! just joker 1
              TEMP = 0
              CALL MOVBYT(TERMES,ZEXT(MESLEN-(3-1)), TEMP, 1, 3) 
              TRABUF(TWKICK) = TEMP
              MESLEN = MESLEN - 3
          END IF
          TRABUF(TWQPF) = 1
        ELSE IF (JOK2_REQ) THEN
          ! just joker 2
          TEMP = 0
          CALL MOVBYT(TERMES,ZEXT(MESLEN-(3-1)), TEMP, 1, 3) 
          TRABUF(TWKICK2) = TEMP
          MESLEN = MESLEN - 3
          TRABUF(TWQPF) = 1          
        ENDIF
        !check max value of joker numbers
        IF ((TRABUF(TWKICK)  .GT. KIKMAX(TRABUF(TGAMIND))) .OR.
     *      (TRABUF(TWKICK2) .GT. KIKMAX(TRABUF(TGAMIND)))) THEN
             TRABUF(TERR)=INVL
             SYNTERRCOD = 180
             RETURN
        END IF
C
C Check Kicker game
C
        IF(TRABUF(TGAMTYP) .NE. TKIK) THEN
           CALL KIKCHK(TRABUF,ADVANCE,ST)
           IF(ST.NE.0) RETURN
           KGIND = GNTTAB(GAMIDX,TRABUF(TWKGME))
        ENDIF
C
C CHECK FOR JOKER CONTROL REVISION  
C
        IF(IAND(OPTIONS,'1000'X).NE.0) THEN
            JOKCREV=0
            CALL MOVBYT(TERMES,IND,JOKCREV,3,2)
            IND=IND+2
            TEMP = KIKREV(TRABUF(TGAMIND))
            IF (I2JOKCREV(2) .NE. I2TEMP(1)) THEN
                TRABUF(TERR) = GREV 
                TRABUF(TSUBERR) =IOR(TRABUF(TSUBERR),GREV_KIKCTL)
            END IF
        ENDIF
C
C CHECK FOR JOKER TEXT REVISION (NOT USED)
C
        IF(IAND(OPTIONS,'0800'X).NE.0) THEN
            JOKTREV=0
            CALL MOVBYT(TERMES,IND,JOKTREV,3,2)
            IND=IND+2
            TEMP = KIKREV(TRABUF(TGAMIND))
            IF (I2JOKTREV(2) .NE. I2TEMP(2)) THEN
                TRABUF(TERR) = GREV 
                TRABUF(TSUBERR) = TRABUF(TSUBERR) + GREV_KIKTXT
            END IF
        ENDIF
C
C CHECK FOR JOKER TICKET TEXT REVISION 
C
        IF(IAND(OPTIONS,'0400'X).NE.0) THEN
            JOKTIKTREV=0
            CALL MOVBYT(TERMES,IND,JOKTIKTREV,3,2)
            IND=IND+2
            TEMP = TKTMRV(TRABUF(TWKGME))
            IF (I2JOKTIKTREV(2) .NE. I2TEMP(1).AND.
     *          TKTMLN(TRABUF(TWKGME)).GT.0) THEN
                TRABUF(TERR) = GREV
                TRABUF(TSUBERR) = TRABUF(TSUBERR) + GREV_KIKTKT
            END IF
        ENDIF

        IF(TRABUF(TERR).EQ.GREV) RETURN

C
C CHECK FOR QP IF TRUE THEN GET QUICK PICK FLAGS
C
        IF(IAND(OPTIONS,'0200'X).NE.0) THEN
!           TEMP1 = ZEXT(TERMES(IND+0))
!           TEMP2 = ZEXT(TERMES(IND+1))
!           TRABUF(TWQPF) = ISHFT(TEMP1,8) + TEMP2
            TRABUF(TWQPF) = 1
            IND=IND+2
        ENDIF
C
C CHECK FOR SYSTEM BET FLAG IF TRUE GET SYSTEM NUMBER (2 bytes)
C
        IF(IAND(OPTIONS,'0100'X).NE.0) THEN
            IF(TRABUF(TWSYST).NE.FULSYS) THEN
                TRABUF(TERR)=INVL
                SYNTERRCOD=165
                RETURN
            ENDIF

            TEMP1 = ZEXT(TERMES(IND+0))
            TEMP2 = ZEXT(TERMES(IND+1))
            TRABUF(TWSYSN) = ISHFT(TEMP1,8) + TEMP2
            IND=IND+2
            IF(TRABUF(TWSYSN).GT.P(MAXKSYS)) THEN
                TRABUF(TERR)=SYNT
                SYNTERRCOD=170
                RETURN
            ENDIF
        ENDIF
        ! check max value of joker numbers
        IF((TRABUF(TWKICK)  .GT. KIKMAX(TRABUF(TGAMIND))) .OR.
     *     (TRABUF(TWKICK2) .GT. KIKMAX(TRABUF(TGAMIND)))) THEN
                    TRABUF(TERR)=INVL
        END IF
C
C CHECK MESSAGE CHECKSUM
C
        IF(P(SUPSUM).EQ.0) THEN
          IF(.NOT.BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTSUM)) THEN
            I4CCITT   = IAND(BASECHKSUM+TRABUF(TTER),'FFFF'X)
            TERMES(3) = I1CCITT(2)
            TERMES(4) = I1CCITT(1)
            CHKLEN=MESLEN-1
            CALL GETCCITT(TERMES,1,CHKLEN,MYCHKSUM)
            IF(MYCHKSUM.NE.TRABUF(TCHK)) THEN
                TRABUF(TERR)=CBAD
                SYNTERRCOD=10
                RETURN
            ENDIF
          ENDIF
        ENDIF
C
C                                                                               
C GET BANK NUMBER AND ACCOUNT NUMBER                                            
C                                                                               
        IF(IAND(OPTIONS,'0008'X).NE.0) THEN                                  
            TEMP1 = ZEXT(TERMES(IND))
            TEMP2 = ZEXT(TERMES(IND+1))
            TEMP3 = ZEXT(TERMES(IND+2))
            TEMP4 = ZEXT(TERMES(IND+3))
            TRABUF(TWBNKID) =  ISHFT(TEMP1,24) + ISHFT(TEMP2,16) +
     *                         ISHFT(TEMP3,8) + TEMP4

            IND=IND+4                                                        
            TEMP1 = ZEXT(TERMES(IND))
            TEMP2 = ZEXT(TERMES(IND+1))
            TEMP3 = ZEXT(TERMES(IND+2))
            TEMP4 = ZEXT(TERMES(IND+3))
            TRABUF(TWBNKNM) =  ISHFT(TEMP1,24) + ISHFT(TEMP2,16) +
     *                         ISHFT(TEMP3,8) + TEMP4

            IND=IND+4               
C
            BNKLO = 0
            BNKHI = 999999
            IF(BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTTBA)) THEN
              BNKLO = 10000000
              BNKHI = 16000000
            ENDIF
            IF(TRABUF(TWBNKID).LT.BNKLO.OR.TRABUF(TWBNKID).GT.BNKHI) THEN
              TRABUF(TERR) = INVL
              SYNTERRCOD = 160
              RETURN
            ENDIF
            IF(TRABUF(TWBNKNM).LT.0.OR.TRABUF(TWBNKNM).GT.99999999)THEN
              TRABUF(TERR) = INVL
              SYNTERRCOD = 162
              RETURN
            ENDIF
        ENDIF
C
C GET SERIAL NUMBER OF ASSOCIATED LINKED WAGER
C
        IF(IAND(OPTIONS,'0004'X) .NE. 0) THEN
            TEMP1 = ZEXT(TERMES(IND))
            TEMP2 = ZEXT(TERMES(IND+1))
            TEMP3 = ZEXT(TERMES(IND+2))
            TEMP4 = ZEXT(TERMES(IND+3))
            IND=IND+4
            TRABUF(TWLNKSER) = ISHFT(TEMP1,16) + ISHFT(TEMP2,8) + TEMP3
            TRABUF(TWLNKCHK) = TEMP4
        ENDIF
C
C GET KICKER NUMBER
C
        IF(IAND(OPTIONS,'0002'X) .NE. 0) THEN
            TEMP1 = ZEXT(TERMES(IND))
            TEMP2 = ZEXT(TERMES(IND+1))
            TEMP3 = ZEXT(TERMES(IND+2))
            TEMP4 = ZEXT(TERMES(IND+3))
            IND=IND+4
            TRABUF(TWKICK) = ISHFT(TEMP2,16) + ISHFT(TEMP3,8) + TEMP4
        ENDIF
C
C CHECK GAME NUMBER
C
        IF(TRABUF(TGAM).LT.1.OR.TRABUF(TGAM).GT.MAXGAM) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=125
            RETURN
        ENDIF
C
C CHECK DRAW NUMBER
C
        IF(KIKDRW(TRABUF(TGAMIND)).LT.1) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=130
            RETURN
        ENDIF
C
C CHECK DURATION
C
        IF(TRABUF(TWDUR).LT.1) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=135
            RETURN
        ENDIF
C
C CHECK KICKER SOLD FLAG
C
        IF(TRABUF(TGAMTYP).NE.TKIK) THEN
          IF(TRABUF(TWKFLG).EQ.0 .AND. TRABUF(TWKFLG2) .EQ. 0) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=140
            RETURN
          ENDIF
        ENDIF
C
C CALCULATE AMOUNT
C
100     CONTINUE
        TRABUF(TWAMT) = 0
        IF (TRABUF(TWKFLG) .NE. 0) THEN
            TRABUF(TWAMT) = IDNINT ( DFLOAT(KIKPRC(TRABUF(TGAMIND))) / P(PRFACTOR) )
        END IF
        IF (TRABUF(TWKFLG2) .NE. 0) THEN
            TRABUF(TWAMT) = TRABUF(TWAMT) + IDNINT ( DFLOAT(KIKPRC(TRABUF(TGAMIND))) / P(PRFACTOR) )
        END IF

        IF(TRABUF(TWAMT).EQ.0) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=145
            RETURN
        ENDIF
        TRABUF(TWSIMP) = 1
C
C GET START AND END DRAWS
C
        TRABUF(TFIL)=EARLY
        TRABUF(TWBEG) = KIKDRW(TRABUF(TGAMIND))
        IF(KIKSTS(TRABUF(TGAMIND)).GE.GAMBFD) THEN
            TRABUF(TWBEG)=TRABUF(TWBEG)+1
            TRABUF(TERR)=SDRW           !NO AFTER DRAW WAGERING
            RETURN
        ENDIF
        TRABUF(TWEND) = TRABUF(TWBEG) + TRABUF(TWDUR) - 1
C
        TRABUF(TWKBEG) = TRABUF(TWBEG)
        TRABUF(TWKEND) = TRABUF(TWEND)
        TRABUF(TWKDUR) = TRABUF(TWDUR)
        TRABUF(TWKAMT) = TRABUF(TWAMT) 

C
C CHECK IF MULTI DRAW ENABLED
C
        IF(TRABUF(TWDUR).NE.1 .AND. KIKMLT(TRABUF(TGAMIND)).EQ.0) THEN
            TRABUF(TERR)=INVL
            RETURN
        ENDIF
C
C CHECK MULTI DRAW COUNT
C
        IF(TRABUF(TWDUR).LT.1                        .OR.
     *     (TRABUF(TWDUR).GT.KIKMLT(TRABUF(TGAMIND)) .AND.
     *      KIKMLT(TRABUF(TGAMIND)).NE.0)            .OR.
     *     KIKMDS(TRABUF(TWDUR),TRABUF(TGAMIND)) .EQ. 0) THEN

            TRABUF(TERR)=INVL
            SYNTERRCOD=150
            RETURN
        ENDIF
C
C CHECK IF FREE WEEK PROMOTION SET
C
        IF(PROMO(PRMTYP,TRABUF(TGAM)).EQ.PRMFRW) THEN
           IF(TRABUF(TWDUR).GE.PROMO(PRMIFX,TRABUF(TGAM)))
     *        TRABUF(TWADDFW)=1
        ENDIF
C
C CHECK FOR ERROR SINCE WE COULD LET REVISION ERROR TROUGH IN ORDER TO
C ACCUMULATE THEM AND CATCH SYNTAX ONES.
C
        IF(TRABUF(TERR).NE.NOER) RETURN
C
        TRABUF(TSTAT) = GOOD
        TRABUF(TWMFRAC)=TRABUF(TFRAC)   
        TRABUF(TWTKC) = TKTCHG(TRABUF(TGAM))
        TRABUF(TWTOT) = TRABUF(TWAMT)*TRABUF(TWDUR) + TRABUF(TWTKC)
C
        RETURN
C
        END
