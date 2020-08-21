C
C SUBROUTINE DBINGO
C  
C V10 02-FEB-2000 UXN TNFRAC added.
C V09 16-MAR-1999 RXK Gtyp+gind 5+3 bits change. 
C V08 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting, 
C                     Instant Pass Thru Phase 1
C V07 07-NOV-1995 HXK Tightened up Dcode routines, removed kicker
C V06 24-NOV-1994 HXK Fixed for log displacement of Bingo A,B bet 
C V05 01-NOV-1994 HXK Moved code for Bingo seed tob prior to checksum checking
C V04 01-NOV-1994 HXK Made minor changes, TWBSED now set in GETQUE
C V03 24-OCT-1994 HXK Allow bet without Kicker
C V02 15-OCT-1994 HXK Adding /developing Bingo (15.Oct.94)
C V01 02-OCT-1994 HXK Initial revision.  
C
C SUBROUTINE TO DECODE 1X2 BINGO WAGERS
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
CC MESSAGE FORMAT
C
C  BYTES  LENGTH             DESCRIPTION
C
C   1       1    !  CONTROL(0010)  ! SEQUENCE NUMBER   !
C   2       1    !  TYPE   (0000)  ! SUBTYPE = TBNG    !
C  3-4      2    !        MESSAGE CHECKSUM             !
C   5       1    !           STATISTICS                !
C   6       1    !  GAME INDEX     ! SYSTEM TYPE       !
C   7       1    !  DURATION       ! # BOARDS          !
C  8-9      2    !           OPTION FLAGS              !
C 10-?      m    !           OPTION DATA               !
C
C
C  THE NUMBERS FOR FULL HOUSE ARE GENERATED AT CENTRAL.
C  THEY  ARE STORED IN BITMAPS WITH OFFSET 15 PER COLUMN.
C  
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE DBINGO(TERMES,TRABUF,MESLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'

        ! arguments
        BYTE       TERMES(*)         !

        INTEGER*2  MESLEN            !

        ! variables
        INTEGER*4  MESS(EDLEN)       !
        INTEGER*4  ST                !
        INTEGER*4  SBTIME            !
        INTEGER*4  SW                !
        INTEGER*4  SBRDLN            !
        INTEGER*4  LEN               !
        INTEGER*4  SNUM              !
        INTEGER*4  SOFF              !
        INTEGER*4  MYCHKSUM          !
        INTEGER*4  CHKLEN            !
        INTEGER*4  SWEKNUM           !
        INTEGER*4  IND               !
        INTEGER*4  OPTIONS           !
        INTEGER*4  TEMP              !
        INTEGER*4  TEMP1             !
        INTEGER*4  TEMP2             !
        INTEGER*4  TEMP3             !
        INTEGER*4  TEMP4             !
        INTEGER*4  SSYS              !
        INTEGER*4  CLEN
        INTEGER*4  CREV              ! game control revision
        INTEGER*4  TREV              ! game text revision
        INTEGER*4  TIKTREV           ! ticket text revision
        INTEGER*4  JOKCREV           ! joker control revision
        INTEGER*4  JOKTREV           ! joker text revision
        INTEGER*4  JOKTIKTREV        ! joker ticket text revision
        INTEGER*4  KGIND             ! joker game index
        INTEGER*4  BNKLO
        INTEGER*4  BNKHI
C

        INTEGER*2  I2TEMP(2)         !
        INTEGER*2  I2CREV(2)         ! I2 game control revision
        INTEGER*2  I2TREV(2)         ! I2 game text revision
        INTEGER*2  I2TIKTREV(2)      ! I2 ticket text revision
        INTEGER*2  I2JOKCREV(2)      ! I2 joker control revision
        INTEGER*2  I2JOKTREV(2)      ! I2 joker text revision
        INTEGER*2  I2JOKTIKTREV(2)   ! I2 joker ticket text revision

        LOGICAL    JOK1_PART         ! joker 1 participation 
        LOGICAL    JOK2_PART         ! joker 2 participation 
        LOGICAL    JOK1_REQ          ! joker 1 number requested
        LOGICAL    JOK2_REQ          ! joker 2 number requested
        LOGICAL    ADVANCE           ! joker advance draw (not used in Finland)

        EQUIVALENCE (TEMP,       I2TEMP)
        EQUIVALENCE (CREV,       I2CREV)              
        EQUIVALENCE (TREV,       I2TREV)              
        EQUIVALENCE (TIKTREV,    I2TIKTREV)           
        EQUIVALENCE (JOKCREV,    I2JOKCREV)           
        EQUIVALENCE (JOKTREV,    I2JOKTREV)           
        EQUIVALENCE (JOKTIKTREV, I2JOKTIKTREV)         
C
C
C GET GENERIC GAME INFORMATION
C
        ADVANCE         = .FALSE.
        SYNTERRCOD      = 0
        TRABUF(TSTAT)   = REJT
        TRABUF(TGAMTYP) = TBNG
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

        IF(TRABUF(TGAMIND) .GE. 1 .AND. TRABUF(TGAMIND) .LE. NUMBGO)
     *    TRABUF(TGAM) = GTNTAB(TBNG,TRABUF(TGAMIND))

        IF(TRABUF(TGAM) .LT. 1 .OR. TRABUF(TGAM) .GT. MAXGAM) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=5
            RETURN
        ENDIF
C
C GET DURATION AND NUMBER OF BOARDS
C
        TEMP    = ZEXT(TERMES(8))
        TRABUF(TWNBET)=IAND(TEMP,15)
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
            TEMP = BNGREV(TRABUF(TGAMIND))
            IF (I2CREV(2) .NE. I2TEMP(1)) THEN
                TRABUF(TERR) = GREV
                TRABUF(TSUBERR) = TRABUF(TSUBERR) + GREV_GAMCLT
            ENDIF
        ENDIF

C
C CHECK FOR TEXT REVISION
C
        IF(IAND(OPTIONS,'4000'X).NE.0) THEN
            TREV=0
            CALL MOVBYT(TERMES,IND,TREV,3,2)
            IND=IND+2
            TEMP = BNGREV(TRABUF(TGAMIND))
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
          TRABUF(TERR)=SYNT
          SYNTERRCOD=221
          RETURN
        ENDIF
C
C CHECK FOR JOKER 2 PARTICIPATION 
C
        IF(IAND(OPTIONS,'0040'X).NE.0) THEN
            JOK2_PART = .TRUE.
            TRABUF(TWKFLG2) = 1
          TRABUF(TERR)=SYNT
          SYNTERRCOD=222
          RETURN
        ENDIF
C
C CHECK FOR JOKER 1 REQUEST, 
C
        IF(IAND(OPTIONS,'0020'X).NE.0) THEN
            JOK1_REQ =.TRUE.
          TRABUF(TERR)=SYNT
          SYNTERRCOD=223
          RETURN
        END IF
C
C CHECK FOR JOKER 2 REQUEST
C
        IF(IAND(OPTIONS,'0010'X).NE.0) THEN
            JOK2_REQ =.TRUE.
          TRABUF(TERR)=SYNT
          SYNTERRCOD=224
          RETURN
        END IF

        IF(TRABUF(TERR).EQ.GREV) RETURN

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
C Get the Bingo seed put there
C by GETQUE/DISPAT and decrease message length by 4 bytes
C for each Bingo
C
        TEMP = 0
        CALL MOVBYT(TERMES,ZEXT(MESLEN-(4-1)), TEMP, 1, 4)
        TRABUF(TWBSED) = TEMP
        MESLEN = MESLEN - 4
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
C CHECK GAME INDEX
C
        IF(BNGDRW(TRABUF(TGAMIND)).LT.1) THEN
          TRABUF(TERR)=SYNT
          SYNTERRCOD=40
          RETURN
        ENDIF
C
C CHECK NUMBER OF BETS
C
        IF(TRABUF(TWNBET).NE.1) THEN
          TRABUF(TERR)=SYNT
          SYNTERRCOD=70
          RETURN
        ENDIF
C
C CHECK DURATION
C 
        IF(TRABUF(TWDUR).NE.1) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=100
            RETURN
        ENDIF

C
C CHECK FOR SYSTEM BETTING
C
        IF(TRABUF(TWSYST).NE.NOSYS) THEN
            TRABUF(TERR) = SYNT
            SYNTERRCOD=115
            RETURN
        ENDIF

        IF(TRABUF(TERR).NE.NOER) RETURN

        IF(TRABUF(TTYP).NE.TINC) CALL GETB_BTMTRA(TRABUF)
C 
        TRABUF(TWAMT)=TRABUF(TWNBET)*BNGPRC(TRABUF(TGAMIND))
C
C CHECK GAME STATUS
C
        IF(BNGSTS(TRABUF(TGAMIND)).LT.GAMOPN) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=141
            RETURN
        ENDIF
C
C SET START AND END DRAWS
C
        TRABUF(TFIL)=EARLY
        TRABUF(TWBEG)=BNGDRW(TRABUF(TGAMIND))
        IF(BNGSTS(TRABUF(TGAMIND)).GE.GAMBFD) THEN 
            TRABUF(TFIL)=LATE
            TRABUF(TERR)=SDRW
            RETURN
        ENDIF
        TRABUF(TWEND)=TRABUF(TWBEG)+TRABUF(TWDUR)-1

C
C CHECK MULTI DRAW 
C
        IF(TRABUF(TWDUR) .LT.1                       .OR.
     *     (TRABUF(TWDUR).GT.BNGMLT(TRABUF(TGAMIND)) .AND.
     *      BNGMLT(TRABUF(TGAMIND)).NE.0)            .OR.
     *     BNGMDS(TRABUF(TWDUR),TRABUF(TGAMIND)) .EQ. 0) THEN
            TRABUF(TERR)=INVL
            SYNTERRCOD=150
            RETURN
        ENDIF

C
C CHECK FOR ERROR SINCE WE COULD LET REVISION ERROR THROUGH IN ORDER TO
C ACCUMULATE THEM AND CATCH SYNTAX ONES.
C
        IF(TRABUF(TERR).NE.NOER) RETURN
C
        TRABUF(TSTAT)=GOOD
        TRABUF(TWMFRAC)=TRABUF(TFRAC)   
        TRABUF(TWTKC)=TKTCHG(TRABUF(TGAM))
        TRABUF(TWTOT)=TRABUF(TWAMT)*TRABUF(TWDUR)+
     *                TRABUF(TWKAMT)*TRABUF(TWKDUR)+
     *                TRABUF(TWTKC)
C
        RETURN
C
        END
