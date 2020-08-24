C
C SUBROUTINE DSPORT
C V30 11-APR-2011 RXK Value of TWNBET checked
C V29 27-OCT-2003 FRP Modify for Batch2 Totobola Changes.
C V28 12-DEC-2000 EPH Base price upt o 4 decimals
C V27 17-FEB-2000 UXN JOK1_REQ,JOK2_REQ initialized.
C v26 15-feb-2000 OXK 'Needed?' check to 'Joker requested' check (Vakio changes)
C V25 02-FEB-2000 UXN TNFRAC added.
C V24 16-MAR-1999 RXK Gtyp+gind 5+3 bits change.
C V23 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting,
C                     Instant Pass Thru Phase 1
C V22 28-NOV-1993 HXK Joker must be requested
C V21 07-NOV-1993 GXA Correction to earlier correction, Move checksum check to
C                     after the Kicker #'s have bean decoded ans message length
C                     is final.
C V20 07-NOV-1993 HXK Added missing return statement.
C V19 06-NOV-1993 HXK Moved checksum earlier, increased integrity checks.
C V18 17-AUG-1993 HXK Store sysn as 1; terminal sees simp as sysn
C V17 17-AUG-1993 HXK Fix for mktmes rev bugs
C V16 05-AUG-1993 HXK Changed system checking, now full systems have row equival.
C                     value passed to central by terminal. checkrow calculates
C                     actual number of rows (twsimp) and this is compared to the
C                     terminal's (twsysn)
C V15 02-AUG-1993 HXK Removed debug stuff, changed full system checking
C V14 02-AUG-1993 HXK Added debug comments
C V13 02-AUG-1993 HXK Commented out full sys being made equal to max sys
C V12 27-JUL-1993 SXH Corrected  numbers
C V11 19-JUL-1993 SXH Fixed comment problem
C V10 19-JUL-1993 SXH Fixed bug with joker number and MESLEN
C V09 19-JUL-1993 GXA Changed check for revisions to use secondary error filed
C                     for bitmap (TSUBERR) and added check to see if we have
C                     ticket text before we check ticket text rev
C V08 28-JUN-1993 GXA Changed Control/Text error checking to accumulate the
C                     error bits.
C V07 23-JUN-1993 SXH Check kicker game before kicker control/text checks
C V06 14-JUN-1993 SXH Released for Finland
C V05 21-JAN-1993 DAB Initial Release Based on Netherlands Bible, 12/92,
C                     and Comm 1/93 update. DEC Baseline
C V04 10-FEB-1992 GCAN CHECK FOR ADVANCED FLAG (MEANING ADVANCED KICKER ONLY)
C                      NO ADVANCED OF MAIN GAME IS POSSIBLE AT THIS POINT.
C V03 21-JAN-1992 GCAN ADDED TICKET CHARGE.
C V02 01-NOV-1991 MTK  INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C ** Source - dsport.for **
C
C DSPORT.FOR
C
C
C SUBROUTINE TO DECODE 1X2 SPORT WAGERS
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
        SUBROUTINE DSPORT(TERMES,TRABUF,MESLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:LSYSCOM.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'

        ! arguments
        BYTE       TERMES(*)         !

        INTEGER*2  MESLEN            !

        ! variables
        INTEGER*4  ST            !
        INTEGER*4  LEN               !
        INTEGER*4  SNUM              !
        INTEGER*4  MYCHKSUM          !
        INTEGER*4  CHKLEN            !
        INTEGER*4  IND               !
        INTEGER*4  OPTIONS           !
        INTEGER*4  TEMP              !
        INTEGER*4  TEMP1             !
        INTEGER*4  TEMP2             !
        INTEGER*4  TEMP3             !
        INTEGER*4  TEMP4             !
        INTEGER*4  CLEN              !
        INTEGER*4  CREV              ! game control revision
        INTEGER*4  TREV              ! game text revision
        INTEGER*4  TIKTREV           ! ticket text revision
        INTEGER*4  JOKCREV           ! joker control revision
        INTEGER*4  JOKTREV           ! joker text revision
        INTEGER*4  JOKTIKTREV        ! joker ticket text revision
        INTEGER*4  KGIND             ! joker game index
        INTEGER*4  BNKLO
        INTEGER*4  BNKHI
        INTEGER*4  WQPREV            ! weighted quick pick table revision #
C
        INTEGER*2  I2TEMP(2)         !
        INTEGER*2  I2CREV(2)         ! I2 game control revision
        INTEGER*2  I2TREV(2)         ! I2 game text revision
        INTEGER*2  I2TIKTREV(2)      ! I2 ticket text revision
        INTEGER*2  I2JOKCREV(2)      ! I2 joker control revision
        INTEGER*2  I2JOKTREV(2)      ! I2 joker text revision
        INTEGER*2  I2JOKTIKTREV(2)   ! I2 joker ticket text revision
	REAL*8     TWAMT_REAL8       ! Auxiliary for keeping precision

        LOGICAL    JOK1_PART         ! joker 1 participation 
        LOGICAL    JOK2_PART         ! joker 2 participation 
        LOGICAL    JOK1_REQ          ! joker 1 number requested
        LOGICAL    JOK2_REQ          ! joker 2 number requested
        LOGICAL    JOK_TERM          ! terminal sending joker number
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
        TRABUF(TGAMTYP) = TSPT
C
	JOK1_REQ  = .FALSE.
	JOK2_REQ  = .FALSE.
	JOK1_PART = .FALSE.
	JOK2_PART = .FALSE.	
        JOK_TERM  = .FALSE.
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

        IF(TRABUF(TGAMIND) .GE. 1 .AND. TRABUF(TGAMIND) .LE. MAXIND)
     *    TRABUF(TGAM) = GTNTAB(TSPT,TRABUF(TGAMIND))

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
C CHECK FOR NUMBER OF BETS ( MINIMUN TWO BETS )
C
        IF(TRABUF(TWSYST) .EQ. NOSYS .AND. MOD(TRABUF(TWNBET), 2) .NE. 0 .OR.
     *    TRABUF(TWNBET).GT.10) THEN
          TRABUF(TERR) =SYNT
          SYNTERRCOD = 185
          RETURN
        ENDIF
C
C Fractioning
C      
        TRABUF(TFRAC)  = MAXFRC(TRABUF(TGAM))
        TRABUF(TNFRAC) = 1
C
C CHECK IF THE SPORTS FULL DRAW HAS BEEN CANCELLED
C
        IF(SPTDCD(TRABUF(TGAMIND)) .NE. 0) THEN   ! THE FULL DRAW HAS BEEN CANCELLED
           TRABUF(TSTAT) = REJT
           TRABUF(TERR)  = SFDC
           RETURN
        ENDIF
C
C SET START AND END DRAWS
C
        TRABUF(TFIL)=EARLY
        TRABUF(TWBEG)=SPTDRW(TRABUF(TGAMIND))
        IF(SPTSTS(TRABUF(TGAMIND)).GE.GAMBFD) THEN 
            TRABUF(TFIL)=LATE
            TRABUF(TERR)=SDRW
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
            TEMP = SPTREV(TRABUF(TGAMIND))
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
            TEMP = SPTREV(TRABUF(TGAMIND))
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
     *          TKTMRV(TRABUF(TGAM)).GT.0)                THEN
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
C CHECK FOR JOKER HAVING TO BE PLAYED 
C
        IF((KGNTAB(TRABUF(TGAM)).NE.0).AND.
     *     (.NOT.JOK1_REQ .AND. .NOT.JOK_TERM) .AND.
     *     JOK1_PART) THEN
            TRABUF(TERR) = GREV
            TRABUF(TSUBERR) = IOR(TRABUF(TSUBERR),GREV_KIKCTL)
        ENDIF
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
        END IF
C
C THE JOKER GAME HAS BEEN COMPLETED REMOVED BUT THE "KGNTAB" SETUP CAN'T BE REMOVED FROM THE MAIN GAME WITH
C THE BLDSYS BECAUSE THIS SETUP IS USED DURING THE INTERFACE GENERATION AND REPORT IN THE STOPSYS FOR
C THE PENDING JOKER VALIDATIONS, SO SET THE TERMINAL JOKER REQUEST CONTROL TO REJECT ALL JOKER WAGER BY SOFTWARE
C
        IF(JOK1_REQ  .EQ. .TRUE. .OR.  ! JOKER 1 REQUESTED
     *     JOK2_REQ  .EQ. .TRUE. .OR.  ! JOKER 2 REQUESTED
     *     JOK1_PART .EQ. .TRUE. .OR.  ! JOKER 1 PARTICIPATING
     *     JOK2_PART .EQ. .TRUE. .OR.  ! JOKER 2 PARTICIPATING
     *     JOK_TERM  .EQ. .TRUE.)      ! THE TERMINAL SEND THE JOKER NUMBER TO PLAY
     *
     *  THEN
           TRABUF(TERR) = SYNT
           SYNTERRCOD = 105
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
C
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
               SYNTERRCOD = 121
               RETURN
            ELSE
               TEMP = KIKREV(KGIND)
            ENDIF
            IF (I2JOKTREV(2) .NE. I2TEMP(2)) THEN
                TRABUF(TERR) = GREV
                TRABUF(TSUBERR) = TRABUF(TSUBERR) + GREV_KIKTXT
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
     *          TKTMRV(TRABUF(TWKGME)).GT.0)              THEN
                TRABUF(TERR) = GREV
                TRABUF(TSUBERR) = TRABUF(TSUBERR) + GREV_KIKTKT
            ENDIF
          ENDIF
        ENDIF

        IF(TRABUF(TERR).EQ.GREV) THEN
          SYNTERRCOD = 180
          RETURN
        ENDIF
C
C GET WEIGHTED QP TABLE REVISION NUMBER
C
        IF(IAND(OPTIONS,'0001'X).NE.0) THEN
            TEMP1 = ZEXT(TERMES(IND))
            TEMP2 = ZEXT(TERMES(IND+1))
            TEMP3 = ZEXT(TERMES(IND+2))
            TEMP4 = ZEXT(TERMES(IND+3))
            IND=IND+4
            WQPREV = ISHFT(TEMP1,24) + ISHFT(TEMP2,16) +
     *                       ISHFT(TEMP3,8) + TEMP4
            IF (WQPREV.NE.SPSVER(TRABUF(TGAMIND)).AND.	  ! not for tebe.
     *          .NOT.BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTTBA)) THEN
                TRABUF(TERR) = GREV
                TRABUF(TSUBERR) = TRABUF(TSUBERR) + GREV_GAMWQP
                RETURN
            ENDIF
            TRABUF(TWWEQP) = 1
        ENDIF
C
C CHECK GAME NUMBER
C 
        IF(TRABUF(TGAM).LT.1 .OR. TRABUF(TGAM).GT.MAXGAM) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=30
            RETURN
        ENDIF

C
C CHECK DURATION
C 
        IF(TRABUF(TWDUR).LT.1 .OR. TRABUF(TWDUR).GT.
     *                             SPTMLT(TRABUF(TGAMIND))) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=70
            RETURN
        ENDIF

C
C CHECK FOR SYSTEM BETTING
C
        IF(TRABUF(TWSYST).NE.NOSYS) THEN
            SNUM = TRABUF(TWSYSN)
            IF(TRABUF(TWSYST).EQ.FULSYS) THEN
              SNUM=1          !FORCE FULL SYSTEM TO FREE SYS
            ENDIF
            TRABUF(TERR) = SYNT
            IF(TRABUF(TWSYST).LT.FULSYS  .OR.
     *         TRABUF(TWSYST).GT.CHCSYS) THEN
                SYNTERRCOD=116
                RETURN
            ENDIF

            IF(TRABUF(TWSYST).NE.FULSYS .AND.
     *         (SNUM.LT.1 .OR. SNUM.GT.SPGSYS)) THEN
                SYNTERRCOD=115
                RETURN
            ENDIF

            IF(TRABUF(TWSYST).EQ.REDSYS .AND. SPSATR(SNUM).NE.REDSYS) THEN
                SYNTERRCOD=111
                RETURN
            ENDIF

            IF(TRABUF(TWSYST).EQ.USYS .AND. SPSATR(SNUM).NE.USYS) THEN
                SYNTERRCOD=112
                RETURN
            ENDIF

            IF(TRABUF(TWSYST).EQ.CHCSYS .AND. SPSATR(SNUM).NE.CHCSYS) THEN
                SYNTERRCOD=113
                RETURN
            ENDIF

            IF(TRABUF(TWSYST).EQ.FULSYS .AND. SPSATR(SNUM).NE.FULSYS) THEN
                SYNTERRCOD=114
                RETURN
            ENDIF

            TRABUF(TERR)=NOER
            SYNTERRCOD=0
        ENDIF

C
C GET SPORT BET DATA
C
        LEN=MESLEN-IND+1
        CLEN=LEN
        IF(JOK1_REQ) CLEN=CLEN+12
        IF(JOK2_REQ) CLEN=CLEN+12

        IF(LEN.LE.0 .OR.
     *    (CLEN.GT.72 .AND. TRABUF(TSIZE).LE.2) .OR.
     *     CLEN.GT.132) THEN
          TRABUF(TERR)=SYNT
            SYNTERRCOD=80
            RETURN
        ENDIF

        CALL LIB$MOVC3(LEN, TERMES(IND), TRABUF(TWBORD))
        TRABUF(TWSRW)=SPTMAX(TRABUF(TGAMIND))
        TRABUF(TWSPFRG)=SPTFRG(TRABUF(TGAMIND))
C
C CHECK SPORTS BET
C
        CALL CHKROW(TRABUF)
        IF(TRABUF(TERR).NE.NOER) RETURN
        IF(TRABUF(TWSYST).EQ.FULSYS.AND.
     *     TRABUF(TWSIMP).NE.TRABUF(TWSYSN)) THEN
          TRABUF(TERR)=SYNT
          SYNTERRCOD=117
        ENDIF

C
C SET FULL SYS NUMBER TO 1 FOR POOLS AND LOGGING; CALCULATE TWAMT
C
        IF(TRABUF(TWSYST).EQ.FULSYS) TRABUF(TWSYSN)=1  !ALL FULL SYSTEMS -> 1
	TWAMT_REAL8 = TRABUF(TWSIMP) * ( SPTPRC(TRABUF(TGAMIND)) / DFLOAT(P(PRFACTOR)) )
        TRABUF(TWAMT)=IDNINT(TWAMT_REAL8)
C        TRABUF(TWAMT)=TRABUF(TWSIMP)*SPTPRC(TRABUF(TGAMIND))

C
C CHECK GAME AND DRAW STATUS
C
        IF(SPTSTS(TRABUF(TGAMIND)).LT.GAMOPN) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=141
            RETURN
        ENDIF
C
        IF(SPTDRW(TRABUF(TGAMIND)).EQ.0) THEN
            TRABUF(TERR)=SYNT
            SYNTERRCOD=142
            RETURN
        ENDIF
C
C CHECK MULTI DRAW 
C
        IF(TRABUF(TWDUR) .LT.1                       .OR.
     *     (TRABUF(TWDUR).GT.SPTMLT(TRABUF(TGAMIND)) .AND.
     *      SPTMLT(TRABUF(TGAMIND)).NE.0)            .OR.
     *     SPTMDS(TRABUF(TWDUR),TRABUF(TGAMIND)) .EQ. 0) THEN

            TRABUF(TERR)=INVL
            SYNTERRCOD=150
            RETURN
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
        TRABUF(TWTOT)= TRABUF(TWAMT)*TRABUF(TWDUR)+
     *                 TRABUF(TWKAMT)*TRABUF(TWKDUR)+
     *                 TRABUF(TWTKC) 
C        TRABUF(TWTOT)=TRABUF(TWAMT)*TRABUF(TWDUR)+
C     *                TRABUF(TWKAMT)*TRABUF(TWKDUR)+
C     *                TRABUF(TWTKC)

        IF(TRABUF(TWSIMP).GT.P(MAXSPT)) TRABUF(TERR)=INVL

        RETURN

        END
