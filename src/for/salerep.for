C SUBROUTINE SALEREP
C
C SALEREP.FOR
C
C V34 28-MAR-2017 HXK  Omit Joker details if Joker deactivated > 90 days
C V33 09-MAR-2016 SCML M16 PROJECT: added new game SM into financial reports
C V32 17-AUG-2015 SCML Correcting financial report message initialization error
C V31 07-APR-2014 SCML Adding support for IGS Placard
C V30 02-JAN-2011 HXK  COMBINE LOTTO3 AND LOTTO4 INTO ONE GAME (TOTOLOTO)
C V29 17-MAY-1999 UXN  Super Triple added.
C V28 18-MAR-1999 RXK  Game type/game index change. Hack for V5 removed.
C V27 22-JAN-1998 UXN  Super Score and Todays Triple added.
C V26 18-FEB-1997 HXK  Cleaned up hack for AGTXFR
C V25 07-FEB-1997 HXK  Hack for AGTXFR (temporarily using AGTHCH)
C V24 23-NOV-1995 PXB  Couple and Double games added
C V23 05-MAY-1995 HXK  V5 entered into database again!!!!
C V22 22-FEB-1995 HXK  HACK FOR V5
C V21 02-SEP-1994 HXK  Merge of May,June RFSS batch 
C V20 21-APR-1994 HXK  NO LONGER CHANGE MANAGER TO .FALSE.
C V19 01-FEB-1994 HXK  CHANGED TIME AGAIN.
C V18 31-JAN-1994 HXK  CHANGED TIME.
C V17 22-JAN-1994 HXK  SUMMED TSCR,TWIT GAMES FOR COMMISSIONS.
C V16 09-JAN-1994 HXK  FIRST_WIT WAS .FALSE., SHOULD HAVE BEEN TRUE.
C V15 04-JAN-1994 HXK  ACCUMULATE SCORE, WINNERS TIP.
C V14 23-SEP-1993 HXK  Use terminal number instaed of agent number for 
C                      privileged reports.
C V13 01-SEP-1993 SXH  Weird character in comment
C V12 01-SEP-1993 SXH  No change.
C V11 01-SEP-1993 SXH  Update CLASS dependent code to reflect Finnish usage
C                      Comment out CALL to SALINV
C V10 17-AUG-1993 SXH  Increased SALES array to 64, comment out INVOICE check 
C                      code for testing
C V09 16-AUG-1993 SXH  Debugged
C V08 13-AUG-1993 SXH  Use PUTIME (for hours,mins,secs)
C V07 03-AUG-1993 HXK  PUT IN OPTION BYTE (SET TO ZERO) FOR ALL SALES REPORTS.
C V06 28-JUN-1993 HXK  changed err message length from 5 to 6
C V05 21-JUN-1993 HXK  CHANGED FOR FINLAND VAX CONVERSION
C V04 10-JUN-1993 HXK  Changed AGTINF.DEF, AGTCOM.DEF includes.
C V03 21-JAN-1993 DAB  Initial Release
C                      Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                      DEC Baseline
C V02 15-NOV-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C CALLING SEQUENCE:
C     CALL SALEREP(TRABUF,MESTAB,OUTLEN)
C INPUT
C     TRABUF - INTERNAL TRANSACTION FORMAT
C     MESTAB - TERMINAL INPUT MESSAGE
C OUTPUT
C     MESTAB - TERMINAL OUTPUT MESSAGE
C     OUTLEN - OUTPUT MESSAGE LENGTH
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
        SUBROUTINE SALEREP(TRABUF,MESTAB,OUTLEN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SPECOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
C----+-----------------------------------------------------------------
C V31| Adding support for IGS
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:IGSDEBUG.DEF'
C----+-----------------------------------------------------------------
C V31| Adding support for IGS
C----+------------------------------------------------------------------
C
        ! arguments
        BYTE       MESTAB(*)              !

        INTEGER*2  OUTLEN                 !

        ! variables
        INTEGER*4  I4TEMP                 !
        INTEGER*4  MYCHKSUM               !
        INTEGER*4  CHKLEN                 !
        INTEGER*4  IND                    !
        INTEGER*4  CLASS                  !
        INTEGER*4  SUBCLASS               !
        INTEGER*4  TER                    !
        INTEGER*4  RTER                   !
        INTEGER*4  ST                     !
        INTEGER*4  CDC                    !
        INTEGER*4  CLRKNUM                !
        INTEGER*4  SALOFF                 !
        INTEGER*4  I                      !
        INTEGER*4  ERRTYP                 !
        INTEGER*4  REPPNUM                !
        INTEGER*4  RAGT                   !
        INTEGER*4  SALES(64)              !
        INTEGER*4  MESS(EDLEN)            !
        INTEGER*4  REPTYP                 !
        INTEGER*4  SIND                   !
        INTEGER*4  GNUM                   !
        INTEGER*4  GTYP                   !
        INTEGER*4  GIND                   !
        INTEGER*4  DEFINED_GAMES          !

        INTEGER*2  I2TEMP(2)              !

        BYTE       I1TEMP(4)              !

        LOGICAL    PRIV                   !
        LOGICAL    MANAGER                !
        LOGICAL    HEAD                   !
        LOGICAL    POST                   !
C	
        EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)
C
        DATA ERRTYP /Z90/
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        INTEGER*8 I8TSTAMP, UX_TS
        INTEGER*1 I1TSTAMP(8)
        INTEGER*4 I4TSTAMP(2)
        EQUIVALENCE(I8TSTAMP,I1TSTAMP)
        EQUIVALENCE(I8TSTAMP,I4TSTAMP)
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------

        ! functions
        LOGICAL    NO_JOK_DET
        EXTERNAL   NO_JOK_DET
C
C INITIATE VARIABLES
C
        DEFINED_GAMES = 0
C
C GET SALES REPORT OPTIONS
C
        RTER=0
        CLASS    = ZEXT( MESTAB(5) )
        SUBCLASS = ZEXT( MESTAB(6) )
        TRABUF(TSDT1)=CLASS
        TRABUF(TSDT2)=SUBCLASS
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        CALL GET_UNIX_TIME_MS(UX_TS)
        
        I8TSTAMP = UX_TS
        
        TRABUF(TSDT6) = I4TSTAMP(2) ! HIGH
        TRABUF(TSDT5) = I4TSTAMP(1) ! LOW       

        IF(IGSDEBUG(IA_SPESRV)) THEN
            CALL OPS('SALEREP: TRABUF(TSDT1)(C)',TRABUF(TSDT1),TRABUF(TSDT1))
            CALL OPS('SALEREP: TRABUF(TSDT2)(S)',TRABUF(TSDT2),TRABUF(TSDT2))
            CALL OPS('SALEREP: TRABUF(TSDT6)(H)',TRABUF(TSDT6),TRABUF(TSDT6))
            CALL OPS('SALEREP: TRABUF(TSDT5)(L)',TRABUF(TSDT5),TRABUF(TSDT5))
        ENDIF
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------

C
C GET REPORT TYPE
C
        REPTYP = ZEXT( MESTAB(7) )
C
C GET REPORT PASS NUMBER
C
        I4TEMP=0
        I1TEMP(2)=MESTAB(8)
        I1TEMP(1)=MESTAB(9)
        REPPNUM=I4TEMP
C
        TER=TRABUF(TTER)
C
C CHECK REPORT PASS NUMBER FOR CLASS 4 - on-line validation
C                                    8 - commission
C                                    9 - invoice
C*** NOT IMPLEMENTED FOR FINLAND ***
C
C       IF(CLASS.EQ.4 .OR. CLASS.EQ.8 .OR. CLASS .EQ.9) THEN
C           IF(AGTTAB(APSNUM+1,TER).NE.REPPNUM) THEN
C                type *,'report pass number problem, reppnum ',reppnum
C               TRABUF(TERR)=INVL
C               GOTO 8000
C           ENDIF
C       ENDIF

C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
          IF(IGSDEBUG(IA_SPESRV)) THEN
              CALL OPS('SALEREP:CLASS   ',CLASS,CLASS)
              CALL OPS('SALEREP:SUBCLASS',SUBCLASS,SUBCLASS)
              CALL OPS('SALEREP:REPTYP  ',REPTYP,REPTYP)
              CALL OPS('SALEREP:REPPNUM ',REPPNUM,REPPNUM)
          ENDIF
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------

C
C CHECK IF ANY SALES REPORTS ARE SUPRESSED
C
        IF(TSBIT(P(SUPRPT),SALREP)) THEN
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
          IF(IGSDEBUG(IA_SPESRV)) THEN
              CALL OPSTXT('SALEREP: ALL SALES REPORT SUPPRESSED')
          ENDIF
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            TRABUF(TERR)=SUPR
            GOTO 8000
        ENDIF

        IF(TSBIT(P(SUPRPT),DSLREP)) THEN
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
          IF(IGSDEBUG(IA_SPESRV)) THEN
              CALL OPSTXT('SALEREP: DAILY SALES REPORT SUPPRESSED')
          ENDIF
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            IF(CLASS.GE.1.AND.CLASS.LE.8) THEN
                TRABUF(TERR)=SUPR
                GOTO 8000
            ENDIF
        ENDIF

        IF(TSBIT(P(SUPRPT),INVREP)) THEN
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
          IF(IGSDEBUG(IA_SPESRV)) THEN
              CALL OPSTXT('SALEREP: INVOICE REPORT SUPPRESSED')
          ENDIF
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
           IF(CLASS.EQ.9) THEN
              TRABUF(TERR)=SUPR
              GOTO 8000
           ENDIF
        ENDIF
C
C CHECK FOR PRIVILEGED TERMINAL
C IF PRIVILEGED TERMINAL GET REPORT TERMINAL NUMBER
C
        TER=TRABUF(TTER)
        PRIV=.FALSE.
        IF(TSBIT(AGTTAB(AGTTYP,TER),AGTPRV)) PRIV=.TRUE.
        IF(PRIV) THEN
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            IF(IGSDEBUG(IA_SPESRV)) THEN
                CALL OPS('SALEREP: TERMINAL IS PRIVILEGED', TER,TER)
            ENDIF
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            I1TEMP(4) = ZEXT( MESTAB(10) )
            I1TEMP(3) = ZEXT( MESTAB(11) )
            I1TEMP(2) = ZEXT( MESTAB(12) )
            I1TEMP(1) = ZEXT( MESTAB(13) )
C
C  commented out baseline agent number stuff
C
            RAGT = I4TEMP
            IF(RAGT .NE. 0) THEN 
              DO I = 1, NUMAGT
                IF(RAGT .EQ. AGTTAB(AGTNUM,I)) THEN 
                  RTER = I
                  GOTO 19000 
                ENDIF   
              END DO 
            ENDIF
C
C TERMINAL NOT FOUND
C
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            IF(IGSDEBUG(IA_SPESRV)) THEN
                CALL OPS('SALEREP: COULD NOT FIND AGENT', RAGT,RAGT)
            ENDIF
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            TRABUF(TERR) = INVL
            GOTO 8000
C
C WE FOUND TERMINAL NUMBER
C
19000        CONTINUE
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            IF(IGSDEBUG(IA_SPESRV)) THEN
                CALL OPS('SALEREP: FOUND TERMINAL', RTER,RTER)
            ENDIF
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            TRABUF(TSDT4)=RTER
            IF(RTER.LT.1.OR.RTER.GT.NUMAGT) THEN
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
                IF(IGSDEBUG(IA_SPESRV)) THEN
                    CALL OPS('SALEREP: TERMINAL OUT OF RANGE', RTER,RTER)
                ENDIF
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
                TRABUF(TERR)=INVL
                GOTO 8000
            ENDIF
        ELSE
            RTER=TER
        ENDIF
C
C CHECK FOR HEAD OF CHAIN TERMINAL
C
        HEAD=.FALSE.  !HEAD is always false as Finland does not have chains 
        TER=TRABUF(TTER)
        IF(.NOT.PRIV) THEN
            IF(HEAD.AND.(REPTYP.EQ.1.OR.REPTYP.EQ.3).AND.CLASS.EQ.9) THEN
                I1TEMP(4) = ZEXT( MESTAB(10) )
                I1TEMP(3) = ZEXT( MESTAB(11) )
                I1TEMP(2) = ZEXT( MESTAB(12) )
                I1TEMP(1) = ZEXT( MESTAB(13) )
                RAGT      = I4TEMP
                IF(RAGT.NE.0) THEN
                    DO I=1,NUMAGT
                        IF(RAGT.EQ.AGTTAB(AGTNUM,I)) RTER=I
                    END DO
                ENDIF

                IF(RTER.EQ.0) RTER=TER
                TRABUF(TSDT4)=RTER
                IF(RTER.LT.1.OR.RTER.GT.NUMAGT) THEN
                    TRABUF(TERR)=INVL
                    GOTO 8000
                ENDIF

                IF(AGTHTB(ACHCOD,TER).NE.AGTHTB(ACHCOD,RTER)) THEN
                    TRABUF(TERR)=INVL
                    GOTO 8000
                ENDIF
            ELSE
                RTER=TER
            ENDIF
        ENDIF
C
C CHECK FOR ASSOCIATED POST
C
        POST=.FALSE.
        TER=TRABUF(TTER)
        IF(.NOT.PRIV.AND..NOT.HEAD) THEN
            IF(TSBIT(AGTTAB(AGTTYP,TER),AGTPST)) POST=.TRUE.
            IF(POST.AND.REPTYP.EQ.1) THEN
                RTER=AGTHTB(ASSTER,TER)
                TRABUF(TSDT4)=RTER
                IF(RTER.LT.1.OR.RTER.GT.NUMAGT) THEN
                    TRABUF(TERR)=INVL
                    GOTO 8000
                ENDIF
            ELSE
                RTER=TER
            ENDIF
        ENDIF
C
C REJECT BAD REPORT TYPE
C
        IF(.NOT.HEAD.AND.REPTYP.EQ.2) TRABUF(TERR)=INVL
        IF(.NOT.HEAD.AND.REPTYP.EQ.3) TRABUF(TERR)=INVL
        IF(.NOT.PRIV.AND..NOT.HEAD.AND.REPTYP.EQ.1) TRABUF(TERR)=INVL
        IF(.NOT.PRIV.AND..NOT.HEAD.AND.REPTYP.EQ.2) TRABUF(TERR)=INVL
        IF(TRABUF(TERR).NE.NOER) GOTO 8000
C
C CHECK TO SEE IF MANAGER IS SIGNED ON.
C
        MANAGER=.FALSE.
        IF(AGTTAB(AGTNCL,RTER).EQ.1) MANAGER=.TRUE.
        IF(MANAGER.AND.
     *    (SUBCLASS.GE.1.AND.SUBCLASS.LE.7)) MANAGER=.FALSE.
C
C ZERO OUT SALES ARRAY
C
        CALL FASTSET(0,SALES,64)
C
C READ CLERK FILE
C
        IF(MANAGER.AND.(SUBCLASS.EQ.0.OR.
     *     SUBCLASS.EQ.8)) THEN
            IF(P(SUPFIL).EQ.1) THEN
                TRABUF(TERR)=SUPR
                GOTO 8000
            ENDIF
            CALL READW(CLRKFDB,RTER,CLRKREC,ST)
            IF(ST.NE.0) THEN
                MESS(1)=SPE
                MESS(2)=TEGEN
                MESS(3)=4
                CALL FASTMOV(SFNAMES(1,CLK),MESS(4),5)
                MESS(9)=RTER
                CALL QUEMES(MESS)
                GOTO 8000
            ENDIF
        ENDIF
C
C READ ASF IF WEEK TO DATE OR INVOICE
C
        IF(CLASS.EQ.9 .OR.
     *    (SUBCLASS.GE.1.AND.SUBCLASS.LE.8)) THEN
            IF(P(SUPFIL).EQ.1) THEN
                TRABUF(TERR)=SUPR
                CALL OPSTXT('P(SUPFIL).EQ.1')
                GOTO 8000
            ENDIF

            CALL READW(ASFFDB,RTER,ASFREC,ST)
            IF(ST.NE.0) THEN
                MESS(1)=SPE
                MESS(2)=TEGEN
                MESS(3)=4
                CALL FASTMOV(SFNAMES(1,ASF),MESS(4),5)
                MESS(9)=RTER
                CALL QUEMES(MESS)
                GOTO 8000
            ENDIF
        ENDIF
C
C PUT TIME IN OUTPUT MESSAGE(hours, mins, secs)
C
        IND=5
        CALL PUTIME(TRABUF(TTIM), MESTAB, IND)
C
C PUT CLASS IN OUTPUT MESSAGE
C
        MESTAB(IND) = CLASS
        IND=IND+1
C
C PUT SUBCLASS IN OUTPUT MESSAGE
C
        MESTAB(IND) = SUBCLASS
        IND=IND+1
C
C PUT CHAIN REPORT TYPE IN OUTPUT MESSAGE
C
        IF(AGTHTB(ACHLNK,RTER).EQ.-1) THEN
            MESTAB(IND) = 4
        ELSE
            MESTAB(IND) = REPTYP
        ENDIF
        IND=IND+1
C
C PUT NEXT AGENT NUMBER IN CHAIN INTO OUTPUT MESSAGE
C
        IF(REPTYP.EQ.3.) THEN
            I4TEMP = AGTHTB(ACHLNK,RTER)
            IF(I4TEMP.GT.0.AND.I4TEMP.LE.NUMAGT) THEN
                I4TEMP=AGTTAB(AGTNUM,I4TEMP)
            ENDIF
            MESTAB(IND+0) = I1TEMP(4)
            MESTAB(IND+1) = I1TEMP(3)
            MESTAB(IND+2) = I1TEMP(2)
            MESTAB(IND+3) = I1TEMP(1)
            IND = IND + 4
        ENDIF
C
C SAVE SPACE FOR CDC DATE IN OUTPUT MESSAGE
C
        IND=IND+2
C
C PUT AGENT NUMBER IN OUTPUT MESSAGE
C
        I4TEMP = AGTTAB(AGTNUM,RTER)
        MESTAB(IND+0) = I1TEMP(4)
        MESTAB(IND+1) = I1TEMP(3)
        MESTAB(IND+2) = I1TEMP(2)
        MESTAB(IND+3) = I1TEMP(1)
        IND=IND+4
C
C PUT CLERK NUMBER IN OUTPUT MESSAGE  FOR  TOTAL SUMMARY REPORT
C
        IF(CLASS.EQ.1) THEN
          CLRKNUM=AGTHTB(AGTPASOFF,RTER)
          MESTAB(IND) = CLRKNUM
          IND=IND+1
        ENDIF
C
        SALOFF = 0

        ! get active game types and indices for reports which need them
        IF (CLASS .EQ. 2  .OR.      ! on-line sales
     *      CLASS .EQ. 4  .OR.      ! on-line validations
     *      CLASS .EQ. 8  .OR.      ! commissions
     *      CLASS .EQ. 9) THEN      ! invoice

            IF (CLASS .EQ. 9) THEN
                MESTAB(IND) = '08'X ! set for invoice report 
                IND = IND+1 
            ELSE
                !OPTION BYTE --> MULTI DRAW SALES NOT BROKEN DOWN
                MESTAB(IND) = '00'X  
                IND = IND+1 
            END IF

            ! store IND for # games defined
            SIND=IND
            IND = IND+1
C
C EURO MIL PROJECT - INCLUDE EURO MIL GAME TYPE AND INDEX
C
            MESTAB(IND + 0) = TEUM !(EURO MIL GAME TYPE)
            MESTAB(IND + 1) = 15
            IND = IND + 2

C----+---+-------------+------------------------------------------------
C V33|BEG| M16 PROJECT | ADD NEW GAME SM
C----+---+-------------+------------------------------------------------
            MESTAB(IND + 0) = TRAF                                              !RAFFLE GAME TYPE
            MESTAB(IND + 1) = 15                                                !GAME INDEX INITIALIZED WITH 0xF, WHICH MEANS REPORT NOT AVAILABLE YET
            IND = IND + 2
C----+---+-------------+------------------------------------------------
C V33|END| M16 PROJECT | ADD NEW GAME SM
C----+---+-------------+------------------------------------------------

C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            MESTAB(IND + 0) = TODS !(Oddset game type)
            MESTAB(IND + 1) = 15
            IND = IND + 2
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------

            ! get active game types and indices
            DO 100 GNUM=1,MAXGAM
              GTYP = GNTTAB(GAMTYP, GNUM)
              GIND = GNTTAB(GAMIDX, GNUM)
      	      IF(GTYP .LT. 1 .OR. GTYP .GT. MAXTYP) GOTO 100
      	      IF(GIND .LT. 1 .OR. GIND .GT. MAXIND) GOTO 100
	      IF(GNUM.EQ.7) GOTO 100  !Lotto4 cnt/amt will be added to Lotto3
              IF(GTYP.EQ.TKIK.AND.NO_JOK_DET()) GOTO 100
              MESTAB(IND + 0) = GTYP
              MESTAB(IND + 1) = GIND
              DEFINED_GAMES = DEFINED_GAMES + 1
              IND = IND + 2
100         CONTINUE
C
C EURO MIL PROJECT - EURO NEED TO SUM ONE MORE GAME
C
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
C            MESTAB(SIND) = ZEXT(DEFINED_GAMES) + 1 !# GAMES SENT (+1 IS EUML)
!            MESTAB(SIND) = ZEXT(DEFINED_GAMES) + 2 !# GAMES SENT (+1 IS EUML +2 IS ODDSET)
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------

C----+---+-------------+------------------------------------------------
C V33|BEG| M16 PROJECT | ADD NEW GAME SM TO TOTAL OF GAMES SENT
C----+---+-------------+------------------------------------------------
            MESTAB(SIND) = ZEXT(DEFINED_GAMES) + 3 !# GAMES SENT (+1 IS EUML +2 IS SM +3 IS ODDSET)
C----+---+-------------+------------------------------------------------
C V33|BEG| M16 PROJECT | ADD NEW GAME SM TO TOTAL OF GAMES SENT
C----+---+-------------+------------------------------------------------

        END IF
C
C TOTAL SUMMARY REPORT
C
        IF(CLASS.EQ.1) THEN
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            IF(IGSDEBUG(IA_SPESRV)) THEN
                CALL OPSTXT('SALEREP: CLASS .EQ. 1: CALLING SALSUM')
            ENDIF
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            MESTAB(IND)=0    !OPTION BYTE SET TO ZERO
            IND=IND+1
            CALL SALSUM(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
            GOTO 1000
        ENDIF
C
C PROCESS ONLINE SALES REPORT
C
        IF(CLASS.EQ.2) THEN
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            IF(IGSDEBUG(IA_SPESRV)) THEN
                CALL OPSTXT('SALEREP: CLASS .EQ. 2: CALLING SALSAL')
            ENDIF
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            CALL SALSAL(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
            GOTO 1000
        ENDIF
C
C PROCESS ONLINE VALIDATIONS REPORT
C
        IF(CLASS.EQ.4) THEN
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            IF(IGSDEBUG(IA_SPESRV)) THEN
                CALL OPSTXT('SALEREP: CLASS .EQ. 4: CALLING SALVAL')
            ENDIF
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            CALL SALVAL(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
            GOTO 1000
        ENDIF
C
C PROCESS INSTANT VALIDATIONS REPORT
C
        IF(CLASS.EQ.5) THEN
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            IF(IGSDEBUG(IA_SPESRV)) THEN
                CALL OPSTXT('SALEREP: CLASS .EQ. 5: CALLING SALIVL')
            ENDIF
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            CALL SALIVL(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
            GOTO 1000
        ENDIF
C
C GET COMMISSION REPORTS
C
        IF(CLASS.EQ.8) THEN
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            IF(IGSDEBUG(IA_SPESRV)) THEN
                CALL OPSTXT('SALEREP: CLASS .EQ. 8: CALLING SALCOM')
            ENDIF
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
            CALL SALCOM(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER,TRABUF)
            GOTO 1000
        ENDIF
C
C PROCESS INVOICE REPORT  *** this is now called directly from SPESRV
C                             as the invoice report is segmented, and is
C                             subtype 7
C
C       IF(CLASS.EQ.9) THEN
C           !IF(ASFINV(ASFEND,1).LE.0) THEN
C           !    TRABUF(TERR)=INVL
C           !    GOTO 8000
C           !ENDIF
C
CC IF NOT MANAGER THEN DO NOT ALLOW INVOICE REPORT
C
CC          IF(AGTHTB(AGTPASOFF,TER).NE.1) THEN
CC              TRABUF(TERR)=INVL
CC              GOTO 8000
CC          ENDIF
C
C           CALL SALINV(SALES,SUBCLASS,HEAD,MANAGER,
C     *                  SALOFF,CDC,RTER,REPTYP)
C           GOTO 1000
C       ENDIF
C
C BUILD THE WINNERS SOLD BY AGENT REPORT       ! NOT USED IN FINLAND
C
C       IF(CLASS.EQ.10)THEN
C         CALL SALWIN(SALES,SUBCLASS,MANAGER,SALOFF,CDC,RTER)
C       ENDIF
C
C
C BUILD OUTPUT MESSAGE FOR ALL REPORTS
C
1000    CONTINUE
        IF(TRABUF(TERR).NE.NOER) GOTO 8000
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        ! Setting error flags for both Euromillions and IGS Systems
        IF (CLASS .EQ. 1) THEN
            MESTAB(8) = IOR(MOD(MESTAB(8),16),'30'X)
        ENDIF 
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        
C
C PUT CDC INTO OUTPUT MESSAGE
C
        I4TEMP = CDC
        MESTAB(11) = I1TEMP(2)
        MESTAB(12) = I1TEMP(1)
C
C EURO MIL PROJECT - INCLUDE SPACE FOR EURO MIL PROJECT
C
        IF (CLASS .EQ. 2  .OR.      ! on-line sales
     *      CLASS .EQ. 4  .OR.      ! on-line validations
     *      CLASS .EQ. 8  .OR.      ! commissions
     *      CLASS .EQ. 9) THEN      ! invoice

           MESTAB(IND+0) = 0
           MESTAB(IND+1) = 0
C----+------------------------------------------------------------------
C V32| Correcting financial report message initialization error
C----+------------------------------------------------------------------
           MESTAB(IND+2) = 0
           MESTAB(IND+3) = 0
C----+------------------------------------------------------------------
C V32| Correcting financial report message initialization error
C----+------------------------------------------------------------------
           MESTAB(IND+4) = 0
           MESTAB(IND+5) = 0
           MESTAB(IND+6) = 0
           MESTAB(IND+7) = 0
           IND = IND + 8
           
C----+---+-------------+------------------------------------------------
C V33|BEG| M16 PROJECT | INCLUDE SPACE FOR SM GAME
C----+---+-------------+------------------------------------------------
           MESTAB(IND+0) = 0
           MESTAB(IND+1) = 0
           MESTAB(IND+2) = 0
           MESTAB(IND+3) = 0
           MESTAB(IND+4) = 0
           MESTAB(IND+5) = 0
           MESTAB(IND+6) = 0
           MESTAB(IND+7) = 0
           IND = IND + 8
C----+---+-------------+------------------------------------------------
C V33|END| M16 PROJECT | INCLUDE SPACE FOR SM GAME
C----+---+-------------+------------------------------------------------
           
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
           MESTAB(IND+0) = 0
           MESTAB(IND+1) = 0
           MESTAB(IND+2) = 0
           MESTAB(IND+3) = 0
           MESTAB(IND+4) = 0
           MESTAB(IND+5) = 0
           MESTAB(IND+6) = 0
           MESTAB(IND+7) = 0
           IND = IND + 8
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        ENDIF
        

C
C PUT SALES INFO INTO OUTPUT MESSAGE
C
        DO I = 1, SALOFF
            I4TEMP = SALES(I)
            MESTAB(IND+0) = I1TEMP(4)
            MESTAB(IND+1) = I1TEMP(3)
            MESTAB(IND+2) = I1TEMP(2)
            MESTAB(IND+3) = I1TEMP(1)
            IND=IND+4
        END DO
        OUTLEN = IND-1 
C        CALL OPS('GOTO 9000',outlen,outlen)
        GOTO 9000
C
C ERROR IN REPORT REQUEST FROM TERMINAL
C
8000    CONTINUE
        TRABUF(TSTAT)=REJT
        MESTAB(2) = ERRTYP
        MESTAB(5) = TRABUF(TERR)
        MESTAB(6) = 0
        OUTLEN=6
C
C CALCULATE CHECKSUM FOR MESSAGE BACK TO TERMINAL
C
9000    CONTINUE
        I4CCITT = TRABUF(TCHK)
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)
        CHKLEN=OUTLEN-1
        CALL GETCCITT(MESTAB,1,CHKLEN,MYCHKSUM)
        I4CCITT = MYCHKSUM
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)  
C
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF(IGSDEBUG(IA_SPESRV)) THEN
            CALL OPSTXT('SALEREP: RETURN')
            CALL DUMP_MESSAGE(31,706,MESTAB,OUTLEN)
        ENDIF
C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------

        RETURN

        END





C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        SUBROUTINE GET_JULIAN_DAY_FOR_YEAR(YYYY,MM,DD,JDAY)
        IMPLICIT NONE
        
        INTEGER*4 YYYY,MM,DD,JDAY
        
        INTEGER*4 REG_YEAR(12)
        INTEGER*4 BIS_YEAR(12)
        INTEGER*4 I
        
        
        I = 1
        REG_YEAR(I) = 31
        BIS_YEAR(I) = 31

        I = 2                          
        REG_YEAR(I) = REG_YEAR(I - 1) + 28
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 29
        
        I = 3                          
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31
        
        I = 4                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 30
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 30
        
        I = 5                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31
        
        I = 6                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 30
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 30
        
        I = 7                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31
        
        I = 8                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31
        
        I = 9                           
        REG_YEAR(I) = REG_YEAR(I - 1) + 30
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 30
        
        I = 10                          
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31
        
        I = 11                          
        REG_YEAR(I) = REG_YEAR(I - 1) + 30
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 30
        
        I = 12                          
        REG_YEAR(I) = REG_YEAR(I - 1) + 31
        BIS_YEAR(I) = BIS_YEAR(I - 1) + 31

        ! Handle bissext years
        IF(  (MOD(YYYY,4)   .EQ. 0
     *  .AND. MOD(YYYY,400) .EQ. 0)
     *  .OR. (MOD(YYYY,4)   .EQ. 0
     *  .AND. MOD(YYYY,100) .NE. 0) ) THEN
            JDAY = DD
            IF(MM .GT. 1) THEN
                JDAY = JDAY + BIS_YEAR(MM - 1)
            ENDIF
        ! Handle regular years
        ELSE
            JDAY = DD
            IF(MM .GT. 1) THEN
                JDAY = JDAY + REG_YEAR(MM - 1)
            ENDIF
        ENDIF
        
        RETURN
        END




        SUBROUTINE GET_UNIX_TIME_MS(UX_TIME_MS)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C ARGUMENTS
        INTEGER*8 UX_TIME_MS

C INTERNAL VARIABLES
        INTEGER*4 TIM(8), I, YYYY,MM,DD, JDAY
        CHARACTER*12 CLOCK(3)
C
C       values (1) is the 4-digit year
C       values (2) is the month of the year
C       values (3) is the day of the year
C       values (4) is the time difference with respect to
C                   Coordinated Universal Time (UTC) in minutes
C       values (5) is the hour of the day (range 0 to 23)
C       values (6) is the minutes of the hour (range 0 to 59).
C       values (7) is the seconds of the minute (range 0 to 59).
C       values (8) is the milliseconds of the second (range 0 to 999).


        CALL DATE_AND_TIME(CLOCK(1),CLOCK(2),CLOCK(3),TIM)

        UX_TIME_MS = TIM(8)
     *             + TIM(7) * 1000
     *             + TIM(6) * 1000 * 60
     *             + TIM(5) * 1000 * 60 * 60
     
        DO I = 1970, TIM(1) - 1
            CALL GET_JULIAN_DAY_FOR_YEAR(I,12,31,JDAY)
            UX_TIME_MS = UX_TIME_MS + JDAY * 1000 * 60 * 60 * 24
        ENDDO
        CALL GET_JULIAN_DAY_FOR_YEAR(TIM(1),TIM(2),TIM(3),JDAY)
        JDAY = JDAY - 1 ! Must remove one day, because of that day's milliseconds
        UX_TIME_MS = UX_TIME_MS + (JDAY * 1000 * 60 * 60 * 24)

        RETURN
        END



C----+------------------------------------------------------------------
C V31| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
