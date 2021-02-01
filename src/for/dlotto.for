C
C SUBROUTINE DLOTTO
C
C V23 12-DEC-2010 HXK LOTTO 2 CHANGES (ADDED LUCKY NUMBER)
C V22 11-DEC-2000 EPH Deal with different number of decimals for base price
C V21 05-DEC-2000 ANG Accept Lotto Wager without Joker number
C V20 02-MAR-2000 RXK Agent flag not checked for promotion.
C V19 28-FEB-2000 RXK Promotion ("add 1 free week") added.
C V18 02-FEB-2000 UXN TNFRAC added.
C V17 16-MAR-1999 RXK Gtyp+gind 5+3 bits change.
C V16 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting, 
C                     Instant Pass Thru Phase 1
C V15 28-NOV-1993 HXK Joker must be requested
C V14 07-NOV-1993 GXA Correction to earlier correction, Move checksum check to 
C                     after the Kicker #'s have bean decoded ans message length
C                     is final.
C V13 06-NOV-1993 HXK Move checksum earlier in routine.
C V12 06-NOV-1993 HXK Added data integrity check for kicker index.
C V11 17-AUG-1993 HXK Fix for mktmes rev bugs
C V10 27-JUL-1993 SXH Corrected bank numbers
C V09 19-JUL-1993 GXA Changed check for revisions to use secondary error filed
C                     for bitmap (TSUBERR) and added check to see if we have
C                     ticket text before we check ticket text rev
C V08 19-JUL-1993 SXH Fixed bug with Joker number and MESLEN
C V07 28-JUN-1993 GXA Changed Control/Text error checking to accumulate the error
C                     bits.
C V06 23-JUN-1993 SXH Check kicker game before kicker control/text checks
C V05 14-JUN-1993 SXH Added bank cards, tidied up text/control revisions
C V04 10-MAY-1993 SXH Released for Finland VAX
C V03 21-JAN-1993 DAB Initial Release Based on Netherlands Bible, 12/92, 
C                     and Comm 1/93 update. DEC Baseline
C V02 01-NOV-1991 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C DLOTTO.FOR
C ** Source - dlotto.for **
C
C SUBROUTINE TO DECODE LOTTO WAGERS
C
C ROUTINE CAN GENERATE FOLLOWING 'SYNTERRCOD'
C         0     NO ERROR
C         5     GAME INDEX OUT OF RANGE
C        10     INVALID CHECKSUM
C        20     CONFLICTING OPTION, SYSTEM NUMBER.EQ.NOSYS
C        30     GAME NUMBER OUT OF RANGE
C        40     DRAW NUMBER OUT OF RANGE
C        70     BOARDS OUT OF RANGE
C        75     VIKING LOTTO BOARDS OUT OF RANGE
C        80     BET DATA LENGTH OUT OF RANGE
C       100     DURATION OUT OF RANGE, (LESS THAN 1)
C       110     CONFLICTING OPTION, SYSTEM BET RELATED
C       115     JOKER 1 PARTICIPATING BUT NOT REQUESTED
C       120     JOKER 1 PARTICIPATING BUT NOT REQUESTED
C       130     WAGER AMOUNT SET TO ZERO
C       150     MULTI DRAW OUT OF RANGE
C       155     BET MULTIPLIER INVALID
C       160     INCORRECT BANK ACCOUNT ID
C       162     INCORRECT BANK ACCOUNT NUMBER
C       175     INCORRECT JOKER NUMBER PARTICIPATING
C       176     INCORRECT MONDAY FLAG INDICATOR
C       177     INVALID LUCKY NUMBER
C       180     INCORRECT CONTROL GAME REVISION
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
        SUBROUTINE DLOTTO(TERMES,TRABUF,MESLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:LSYSCOM.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        
C
        ! arguments
        BYTE      TERMES(*)         ! terminal message

        INTEGER*2 MESLEN            ! message lenth
 
        ! variables
        INTEGER*4 TEMP              ! temp variable
        INTEGER*4 TEMP1             !  "       "
        INTEGER*4 TEMP2             !  "       "
        INTEGER*4 TEMP3             !  "       "
        INTEGER*4 TEMP4             !  "       "
        INTEGER*4 I                 !  "       "
        INTEGER*4 CLEN              ! message length for checksum
        INTEGER*4 OPTIONS           ! options byte
        INTEGER*4 IND               ! offset counter in terminal message
        INTEGER*4 CHKLEN            ! check sum length
        INTEGER*4 MYCHKSUM          ! calculated checksum
        INTEGER*4 SYS               ! system type
        INTEGER*4 LEN               ! calculated message length
        INTEGER*4 ST                ! return status
        INTEGER*4 J                 ! loop counter
        INTEGER*4 CREV              ! game control revision
        INTEGER*4 TREV              ! game text revision
        INTEGER*4 TIKTREV           ! ticket text revision
        INTEGER*4 JOKCREV           ! joker control revision
        INTEGER*4 JOKTREV           ! joker text revision
        INTEGER*4 JOKTIKTREV        ! joker ticket text revision
        INTEGER*4 KGIND             ! joker game index
        INTEGER*4 BNKLO
        INTEGER*4 BNKHI
C
        INTEGER*2 I2TEMP(2)         ! temp var
        INTEGER*2 I2CREV(2)         ! I2 game control revision
        INTEGER*2 I2TREV(2)         ! I2 game text revision
        INTEGER*2 I2TIKTREV(2)      ! I2 ticket text revision
        INTEGER*2 I2JOKCREV(2)      ! I2 joker control revision
        INTEGER*2 I2JOKTREV(2)      ! I2 joker text revision
        INTEGER*2 I2JOKTIKTREV(2)   ! I2 joker ticket text revision
	REAL*8    TWAMT_REAL8       ! Auxiliary for rounding purposes

        LOGICAL   JOK1_PART         ! joker 1 participation 
        LOGICAL   JOK2_PART         ! joker 2 participation 
        LOGICAL   JOK1_REQ          ! joker 1 number requested
        LOGICAL   JOK2_REQ          ! joker 2 number requested
        LOGICAL   JOK_TERM          ! terminal sending joker number
        LOGICAL   ADVANCE           ! joker advance draw (not used in Finland)

        EQUIVALENCE (TEMP,       I2TEMP)
        EQUIVALENCE (CREV,       I2CREV)              
        EQUIVALENCE (TREV,       I2TREV)              
        EQUIVALENCE (TIKTREV,    I2TIKTREV)           
        EQUIVALENCE (JOKCREV,    I2JOKCREV)           
        EQUIVALENCE (JOKTREV,    I2JOKTREV)           
        EQUIVALENCE (JOKTIKTREV, I2JOKTIKTREV)         



        ! start of code
C
C GET GENERIC GAME INFORMATION and initialise locals
C
        JOK1_PART       = .FALSE.
        JOK2_PART       = .FALSE.
        JOK1_REQ        = .FALSE.
        JOK2_REQ        = .FALSE.
        JOK_TERM        = .FALSE.
        ADVANCE         = .FALSE.
        SYNTERRCOD      = 0
        TRABUF(TSTAT)   = REJT
        TRABUF(TGAMTYP) = TLTO
        CALL OPSTXT('DLOTTO CALLED...')
C
C GET TRANSACTION NUMBER
C
        TEMP = ZEXT(TERMES(1))
        TRABUF(TTRN)=IAND(TEMP,15)
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
        TRABUF(TWSYST)=IAND(TEMP,15)
        TRABUF(TGAMIND)=ISHFT(TEMP,-4)

        IF(TRABUF(TGAMIND).GE.1.AND.TRABUF(TGAMIND).LE.MAXIND)
     *      TRABUF(TGAM)=GTNTAB(TLTO,TRABUF(TGAMIND))

        IF(TRABUF(TGAM).LT.1.OR.TRABUF(TGAM).GT.MAXGAM) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=5
            RETURN
        ENDIF
C
C Fractioning
C
        TRABUF(TFRAC)  = MAXFRC(TRABUF(TGAM))
        TRABUF(TNFRAC) = 1
C
C GET DURATION AND NUMBER OF BOARDS
C
        TEMP    = ZEXT(TERMES(8))
        TRABUF(TWNBET)=IAND(TEMP,15)
        TRABUF(TWDUR)=ISHFT(TEMP,-4)
C
C SET START AND END DRAWS 
C
        TRABUF(TFIL)=EARLY
        TRABUF(TWBEG)=LTODRW(TRABUF(TGAMIND))
        IF(LTOSTS(TRABUF(TGAMIND)).GE.GAMBFD) THEN
            TRABUF(TWBEG)=TRABUF(TWBEG)+1
            TRABUF(TERR)=SDRW           !NO AFTER DRAW WAGERING
            RETURN
        ENDIF
        TRABUF(TWEND)=TRABUF(TWBEG)+TRABUF(TWDUR)-1
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
            TEMP = LTOREV(TRABUF(TGAMIND))
            IF (I2CREV(2) .NE. I2TEMP(1)) THEN
                TRABUF(TERR) = GREV
                TRABUF(TSUBERR) = TRABUF(TSUBERR) + GREV_GAMCLT
            ENDIF
        ENDIF
C
C CHECK FOR TEXT REVISION (IGNORE FOR LOTTO)
C
        IF(IAND(OPTIONS,'4000'X).NE.0) THEN
            TREV=0
            CALL MOVBYT(TERMES,IND,TREV,3,2)
            IND=IND+2
            TEMP = LTOREV(TRABUF(TGAMIND))
            IF (I2TREV(2) .NE. I2TEMP(2)) THEN
                TRABUF(TERR) = GREV 
                TRABUF(TSUBERR) = TRABUF(TSUBERR) + GREV_GAMTXT
            ENDIF
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
     *          TKTMRV(TRABUF(TGAM)).GT.0) THEN
                TRABUF(TERR) = GREV 
                TRABUF(TSUBERR) = TRABUF(TSUBERR) + GREV_GAMTKT
            ENDIF
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
        IF(IAND(OPTIONS,'0010'X).NE.0) THEN
            JOK2_REQ =.TRUE.
        END IF
C
C CHECK FOR TERMINAL SENDING JOKER NUMBER
C
        IF(IAND(OPTIONS,'0002'X).NE.0) THEN
            JOK_TERM = .TRUE.
        ENDIF
C
C CHECK FOR FIRST JOKER ALWAYS BEING SET FOR LOTTO (IN FINLAND)(NOT IN PORTUGAL)
C
C**     IF(.NOT.JOK1_REQ) THEN
C**           TRABUF(TERR) = GREV
C**           TRABUF(TSUBERR) = IOR(TRABUF(TSUBERR),GREV_KIKCTL)
C**     ENDIF
C
C check for joker 1 problem - participation but no request or sending 
C
        IF (JOK1_PART .AND. (.NOT. JOK1_REQ .AND. .NOT. JOK_TERM)) THEN
            TRABUF(TERR) = SYNT
            SYNTERRCOD=115
            RETURN
        END IF
C
C check for joker 2 problem - participation but no request
C
        IF (JOK2_PART .AND. .NOT. JOK2_REQ) THEN
            TRABUF(TERR) = SYNT
            SYNTERRCOD=120
            RETURN
        ENDIF
C
C CHECK FOR QP IF TRUE THEN GET QUICK PICK FLAGS
C
        IF(IAND(OPTIONS,'0200'X).NE.0) THEN
            TEMP1 = ZEXT(TERMES(IND+0))
            TEMP2 = ZEXT(TERMES(IND+1))
            TRABUF(TWQPF) = ISHFT(TEMP1,8) + TEMP2
            IND=IND+2
        ENDIF
C
C CHECK FOR SYSTEM BET FLAG IF TRUE GET SYSTEM NUMBER (2 bytes)
C
        IF(IAND(OPTIONS,'0100'X).NE.0) THEN
            IF(TRABUF(TWSYST).EQ.NOSYS) THEN
                TRABUF(TERR)=SYNT
                SYNTERRCOD=20
                RETURN
            ENDIF

            TEMP1 = ZEXT(TERMES(IND+0))
            TEMP2 = ZEXT(TERMES(IND+1))
            TRABUF(TWSYSN) = ISHFT(TEMP1,8) + TEMP2
            IND=IND+2
        ENDIF
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
C if requested get the joker number put there
C by GETQUE/DISPAT and decrease message length by 3 bytes
C for each joker number
        IF(JOK1_REQ .OR. JOK2_REQ) THEN                   
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
          ELSE IF (JOK2_REQ) THEN
            ! just joker 2
            TEMP = 0
            CALL MOVBYT(TERMES,ZEXT(MESLEN-(3-1)), TEMP, 1, 3) 
            TRABUF(TWKICK2) = TEMP
            MESLEN = MESLEN - 3
          ENDIF
        ENDIF
C
C GET KICKER NUMBER
C
        IF(IAND(OPTIONS,'0002'X).NE.0) THEN
            TEMP1 = ZEXT(TERMES(IND))
            TEMP2 = ZEXT(TERMES(IND+1))
            TEMP3 = ZEXT(TERMES(IND+2))
            TEMP4 = ZEXT(TERMES(IND+3))
            IND=IND+4
            TRABUF(TWKICK) = ISHFT(TEMP1,24) + ISHFT(TEMP2,16) +
     *                       ISHFT(TEMP3,8) + TEMP4

        ENDIF
C
C CHECK KICKER GAME even if not participating
C
        IF(JOK1_REQ .OR. JOK2_REQ .OR. JOK_TERM) THEN
            CALL KIKCHK(TRABUF,ADVANCE,ST)
            IF(ST.NE.0) THEN
              TRABUF(TERR) = SYNT
              SYNTERRCOD = 175
              RETURN
            ENDIF
            KGIND = GNTTAB(GAMIDX,TRABUF(TWKGME))
            IF(KGIND.LT.1.OR.KGIND.GT.NUMKIK) THEN
               TRABUF(TERR)=SYNT
               SYNTERRCOD=123
               RETURN
            ENDIF
            IF(TRABUF(TFRAC).NE.MAXFRC(TRABUF(TWKGME))) TRABUF(TFRAC)=0     
        ENDIF
C
C CHECK MESSAGE CHECKSUM
C
        CALL OPS('P(SUPSUM)-->',P(SUPSUM),P(SUPSUM)) 
        IF(P(SUPSUM).EQ.0) THEN
          CALL OPSTXT('--CHECKSUM NOT SUPRESSED--')
          CALL OPS('AGTSUM:',AGTSUM,AGTSUM)
          CALL OPS('AGTTYP,TRABUF(TTER):',AGTTYP,TRABUF(TTER),AGTTYP,TRABUF(TTER))
          CALL OPS('BTEST',BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTSUM),BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTSUM))
          IF(.NOT.BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTSUM)) THEN
            I4CCITT   = IAND(BASECHKSUM+TRABUF(TTER),'FFFF'X)
            TERMES(3) = I1CCITT(2)
            TERMES(4) = I1CCITT(1)
            CHKLEN=MESLEN-1
            CALL OPS('CHECKSUM length:',CHKLEN,CHKLEN)
            CALL GETCCITT(TERMES,1,CHKLEN,MYCHKSUM)
            CALL OPS('CHECKSUM MILL:',MYCHKSUM,MYCHKSUM)
            CALL OPS('CHECKSUM OLM:',TRABUF(TCHK),TRABUF(TCHK))
            IF(MYCHKSUM.NE.TRABUF(TCHK)) THEN
                TRABUF(TERR)=CBAD
                SYNTERRCOD=10
                RETURN
            ENDIF
          ENDIF
        ENDIF
C
C CHECK FOR JOKER CONTROL REVISION 
C
        IF(JOK1_REQ .OR. JOK2_REQ .OR. JOK_TERM) THEN
          IF(IAND(OPTIONS,'1000'X).NE.0) THEN
            JOKCREV=0
            CALL MOVBYT(TERMES,IND,JOKCREV,3,2)
            IND=IND+2
            IF(KGIND.EQ.0) THEN
               TRABUF(TERR)=SYNT
               SYNTERRCOD = 121
               RETURN
            ELSE
               TEMP = KIKREV(KGIND)
            ENDIF
            IF (I2JOKCREV(2) .NE. I2TEMP(1)) THEN
                TRABUF(TERR) = GREV
                TRABUF(TSUBERR) = IOR(TRABUF(TSUBERR),GREV_KIKCTL)
            ENDIF
          ENDIF
C
C CHECK FOR JOKER TEXT REVISION (NOT USED)
C
          IF(IAND(OPTIONS,'0800'X).NE.0) THEN
            JOKTREV=0
            CALL MOVBYT(TERMES,IND,JOKTREV,3,2)
            IND=IND+2
            IF(KGIND.EQ.0) THEN
               TRABUF(TERR)=SYNT
               SYNTERRCOD = 122
               RETURN
            ELSE
               TEMP = KIKREV(KGIND)
            ENDIF
            IF (I2JOKTREV(2) .NE. I2TEMP(2)) THEN
                TRABUF(TERR) = GREV
                TRABUF(TSUBERR) = IOR(TRABUF(TSUBERR),GREV_KIKTXT)
            ENDIF
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
     *          TKTMRV(TRABUF(TWKGME)).GT.0) THEN
                TRABUF(TERR) = GREV
                TRABUF(TSUBERR) = IOR(TRABUF(TSUBERR),GREV_KIKTKT)
            ENDIF
          ENDIF
        ENDIF
C
        IF(TRABUF(TERR).EQ.GREV) THEN 
          SYNTERRCOD = 180
          RETURN
        ENDIF
C
C CHECK GAME NUMBER
C
        IF(TRABUF(TGAM).LT.1.OR.TRABUF(TGAM).GT.MAXGAM) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=30
            RETURN
        ENDIF
C
C CHECK DRAW NUMBER
C
        IF(LTODRW(TRABUF(TGAMIND)).LT.1) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=40
            RETURN
        ENDIF
C
C CHECK DURATION
C
        IF(TRABUF(TWDUR).LT.1) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=100
            RETURN
        ENDIF
C
C CHECK FOR SYSTEM BETTING
C
        IF(TRABUF(TWSYST).NE.NOSYS) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=110
            SYS=TRABUF(TWSYSN)
            IF(TRABUF(TWMFLG).NE.0) RETURN
            IF(SYS.LT.1.OR.SYS.GT.LSYSMAX) RETURN
            IF(TRABUF(TWSYST).EQ.FULSYS.AND.
     *        LSYS_ATR(SYS).NE.FULSYS) RETURN
            IF(TRABUF(TWSYST).EQ.REDSYS.AND.
     *        LSYS_ATR(SYS).NE.REDSYS) RETURN
            IF(TRABUF(TWSYST).EQ.CHCSYS.AND.
     *        LSYS_ATR(SYS).NE.REDSYS) RETURN
            IF(TRABUF(TWSYST).LT.FULSYS.OR.
     *        TRABUF(TWSYST).GT.CHCSYS) RETURN
            TRABUF(TERR)=NOER
            SYNTERRCOD=0
            CALL FASTSET(LSYS_NUMMRK(SYS),TRABUF(TWNMRK),TRABUF(TWNBET))
            TRABUF(TWSIMP)=LSYS_BOARD(SYS)
        ELSE
            IF(TRABUF(TWMFLG).EQ.0) THEN
                CALL FASTSET(LTONUM(TRABUF(TGAMIND)),
     *                       TRABUF(TWNMRK),TRABUF(TWNBET))
                TRABUF(TWSIMP)=TRABUF(TWNBET)
            ENDIF
        ENDIF
C
C GET LOTTO MONDAY FLAG INDICATOR ( PLAYING FOR LOTTO MONDAY )
C
        TRABUF(TWLMFI) = ZEXT(TERMES(IND))
        IND = IND + 1
        IF(TRABUF(TWLMFI) .NE. 0 .AND. TRABUF(TWLMFI) .NE. 1) THEN
          SYNTERRCOD = 176
          TRABUF(TERR) = SYNT
          RETURN
        ENDIF        
C
C GET LUCKY IF TOTOLOTO
C
	TRABUF(TWLUCK) = ZEXT(TERMES(IND))
	IND = IND + 1
	IF(LTOLFL(TRABUF(TGAMIND)).NE.0) THEN
	  IF( TRABUF(TWLUCK).LT.1 .OR. 
     *        TRABUF(TWLUCK).GT.LTOLFL(TRABUF(TGAMIND)) ) THEN
            SYNTERRCOD = 177
            TRABUF(TERR) = SYNT
            RETURN
	  ENDIF
	ENDIF
C
C GET LOTTO BET DATA ALSO CHECK LENGTH
C
        LEN=MESLEN-IND+1
        CLEN=LEN
        IF(JOK1_REQ) CLEN=CLEN+12
        IF(JOK2_REQ) CLEN=CLEN+12

        IF(TRABUF(TWMFLG).NE.0) CLEN=CLEN+12
        IF(LEN.LE.0 .OR.
     *    (CLEN.GT.72.AND.TRABUF(TSIZE).LE.2) .OR.
     *     CLEN.GT.132) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=80
            RETURN
        ENDIF
        CALL LIB$MOVC3(LEN, TERMES(IND), TRABUF(TWBORD))
C
C
        TWAMT_REAL8 = TRABUF(TWSIMP)*LTOPRC(TRABUF(TGAMIND)) / DFLOAT(P(PRFACTOR))
        TRABUF(TWAMT) = IDNINT(TWAMT_REAL8)
        IF(TRABUF(TWAMT).EQ.0) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=130
            RETURN
        ENDIF
C
        ! check boards
        IF(TRABUF(TWNBET).LT.1.OR.TRABUF(TWNBET).GT.MAXBRD)THEN    
            TRABUF(TERR)=SYNT                                      
            SYNTERRCOD=70                                          
            RETURN                                                 
        ENDIF                                                      
C
C CHECK MULTI DRAW 
C
        IF(TRABUF(TWDUR).LT.1                        .OR.
     *     (TRABUF(TWDUR).GT.LTOMLT(TRABUF(TGAMIND)) .AND.
     *      LTOMLT(TRABUF(TGAMIND)).NE.0)            .OR.
     *     LTOMDS(TRABUF(TWDUR),TRABUF(TGAMIND)) .EQ. 0) THEN
            TRABUF(TERR) = INVL
            SYNTERRCOD=150
            RETURN
        ENDIF


C
C CHECK BET MULTIPLIERS/TYPES
C
        IF(TRABUF(TWMFLG).NE.0) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=155
            DO 30 I=0,TRABUF(TWNBET)-1
                IF(TRABUF(TWMULT+I).LT.1) RETURN
                DO 20 J=1,LTGBET
                    IF(TRABUF(TWNMRK+I).EQ.LTOBET(J,TRABUF(TGAMIND))) GOTO 30
20              CONTINUE
                RETURN
30          CONTINUE
            TRABUF(TERR)=NOER
            SYNTERRCOD=0
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
        TRABUF(TSTAT)=GOOD
        TRABUF(TWMFRAC)=TRABUF(TFRAC)   
        TRABUF(TWTKC)=TKTCHG(TRABUF(TGAM))
C        TRABUF(TWTOT)=TRABUF(TWAMT)*TRABUF(TWDUR)+
C     *                TRABUF(TWKAMT)*TRABUF(TWKDUR)+
C     *                TRABUF(TWTKC)
        TRABUF(TWTOT)=TRABUF(TWAMT)*TRABUF(TWDUR)+
     *                TRABUF(TWKAMT)*TRABUF(TWKDUR)+
     *                TRABUF(TWTKC)



        RETURN

        END
