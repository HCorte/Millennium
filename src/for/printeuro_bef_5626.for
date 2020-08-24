C SUBROUTINE PRINTEURO_BEF_5626
C
C V07 04-JUL-2016 SCML M16 PROJECT
C-----------------------------------------------------------------------
C V06 01-AUG-2014 SCML Added LINCNT and DETAIL arguments to the subroutine
C V05 10-OCT-2013 SCML New Validation Messages
C V04 12-APR-2011 FJG ACCENTURE MERGE FOR EM2
C V03 26-JAN-2011 FJG More Out of Bounds problems
C V02 11-NOV-2010 FJG EUROMILLIONS VALIDATION PRIZE OVER 42M
C     18-NOV-2010 FJG Batch2: Fix NOBOUNDS checking errors
C
C SUBROUTINE TO PRINT EURO MIL TRANSACTIONS IN TMIR FORMAT BEFORE M16 
C PROJECT (SINCE V07)
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE PRINTEURO_BEF_5626(TRABUF,PUNIT,DETAIL,LINCNT,EM_JUSTONE,SCRAM,
     *                       SUMEUROVALID,SUMEUROWAGER,SUMEUROCANCEL)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA_BEF_5626.DEF'
        INCLUDE 'INCLIB:TNAMES.DEF'
        INCLUDE 'INCLIB:NAMCMD.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:HASF.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C

        ! arguments
        INTEGER*4  PUNIT,SUMEUROVALID,SUMEUROWAGER,SUMEUROCANCEL                 !
C
        LOGICAL    DETAIL                  ! V06
        LOGICAL    EM_JUSTONE              !
        LOGICAL    SCRAM                   !
        LOGICAL    DUMMY                   ! Avoid warning

        ! variables
        CHARACTER*6 PQFLAG 
        BYTE OUTTAB(500)
        INTEGER*4 I,IND,JUL
        INTEGER*2 DBUF(12)
        CHARACTER*21 VALSUBTYP(16)
        CHARACTER*31 VALSTATUS(0:18)
        CHARACTER*31 VALSTATUSE(0:30)
        CHARACTER*12 TRANSTYPE
        CHARACTER*30 CANSTATUS(0:5)
        INTEGER*4  CHECK                   !
        INTEGER*4  SERIAL                  !
        INTEGER*4  PAGE                    !
        INTEGER*4  LINCNT,ST,TRABUFERROR                  !
        INTEGER*4 I4TEMP
        INTEGER*2 I2TEMP(2)
        BYTE      I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
        INTEGER*4  LINES
!       REAL*8        COMMAND     ! Avoid Warning
C        DATA LINCNT/100/,LINES/0/ ! Avoid Warning COMMAND/0/
        DATA LINES/0/ ! V06
        
        INTEGER*4  EMVALAMT(2)
        INTEGER*8  EMVALAMTI8
        REAL*8     EMVALAMTR8
        EQUIVALENCE (EMVALAMT,EMVALAMTI8)

C----+------------------------------------------------------------------
C V05| New Validation Messages
C----+------------------------------------------------------------------
        INTEGER*4  NIB(6)
        CHARACTER*24 CNIB
        EQUIVALENCE (NIB,CNIB)
        INTEGER*4  BLANK
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
          DATA VALSUBTYP /
     *                'Regular'                 ! x10
     *              , 'Mid-Tier Cash'           ! x11
     *              , 'Claim'                   ! x12
     *              , 'Validation Detail'       ! x13
     *              , ' '                       ! x14
     *              , ' '                       ! x15
     *              , 'NV Inq.Regular'          ! x16
     *              , 'NV Inq.Mid-Tier'         ! x17
     *              , 'NV Inq.Cash Accepted'    ! x18
     *              , 'NV Bank Transf.Accepted' ! x19
     *              , 'NV Inq.Mid-Tier BT Only' ! x1A
     *              , ' '                       ! x1B
     *              , ' '                       ! x1C
     *              , ' '                       ! x1D
     *              , ' '                       ! x1E
     *              , 'Validation Error'        ! x1F
     *                   /
          DATA BLANK /'    '/
C----+------------------------------------------------------------------
C V05| New Validation Messages
C----+------------------------------------------------------------------
        VALSTATUSE(0) = 'No Results Yet Or Not A Winner'
        VALSTATUSE(1) = 'Results Not Confirmed'
        VALSTATUSE(2) = 'No Such Ticket'
        VALSTATUSE(3) = 'Cant Pay Yet'
        VALSTATUSE(4) = 'Already Cashed'
        VALSTATUSE(9) = 'Cash At Lottery'
        VALSTATUSE(18) = 'No Details Available'
        VALSTATUSE(30) = 'Winner Holding Limit'
        VALSTATUSE(5) = '---------------'
        VALSTATUSE(6) = 'Prize Expired'
C        
        VALSTATUS(11) = 'Cashed With Exchange'
        VALSTATUS(10) = 'No Exchange Ticket'
        VALSTATUS(5) = '---------------'
        CANSTATUS(0) = 'Good Cancel'     
        CANSTATUS(1) = 'Time Limit Exceeded'
        CANSTATUS(2) = 'Invalid Cancel'
        CANSTATUS(3) = 'Already Cancel'
        CANSTATUS(4) = 'Wrong Terminal'
        CANSTATUS(5) = '---------------'
C
        DUMMY = EM_JUSTONE ! Avoid Warning
C
        IF(LINCNT .GT. LINSPP) THEN
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
C PRINT WAGERS
C
1000    CONTINUE
        TRANSTYPE = 'APOSTA'
        LINCNT = LINCNT+1
        IF ((TRABUF(TSTAT) .EQ. GOOD).AND.(TRABUF(TERR) .EQ. NOER)) SUMEUROWAGER = SUMEUROWAGER + 1 
        PQFLAG = ' AUTO'
        WRITE(PUNIT,1901) STAT(TRABUF(TSTAT)),
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
     *                   TRANSTYPE
        IF (DETAIL) THEN
          IF (TRABUF(TEUWQP) .EQ. 0) PQFLAG = 'MANUAL'
          DBUF(VCDC)=TRABUF(TCDC)
          CALL CDATE(DBUF)
          JUL=DBUF(VJUL) + 500
        
          WRITE(PUNIT,902) JUL,
     *                     TRABUF(TEUSER),
C     *                     TRABUF(TEUCHK),
     *                     '***',
     *                     TRABUF(TEUWBEGW),
     *                     TRABUF(TEUWBEGY),
     *                     TRABUF(TEUWENDW),
     *                     TRABUF(TEUWENDY),
     *                     TRABUF(TEUWDUR),
     *                     TRABUF(TEUWDRWIND)
          WRITE(PUNIT,903) TRABUF(TEUWNBET),
     *                     PQFLAG,
     *                     TRABUF(TEUWNMK),
     *                     TRABUF(TEUWNST),
     *                     TRABUF(TEUWTIMEH),
     *                     TRABUF(TEUWTIMEM),
     *                     TRABUF(TEUWTIMES)
          LINCNT = LINCNT+2
          IND = 1
          DO I=TEUWBOARD,120
            IF(TRABUF(I) .NE. 0) THEN
               CALL MOVBYT(TRABUF(I),1,OUTTAB,IND,4) 
               IND=IND+4
            ENDIF
          ENDDO
          CALL TRANSFBOARD(OUTTAB,TRABUF(TEUWNBET),TRABUF(TEUWNMK),TRABUF(TEUWNST),50,11,ST,PUNIT)
          LINES = TRABUF(TEUWNBET)
          WRITE(PUNIT,*)
          LINCNT = LINCNT+LINES+1
        ENDIF
        RETURN
C
C PRINT CANCEL
C
2000    CONTINUE
        TRANSTYPE = 'CANCELAMENTO'
        LINCNT = LINCNT+1
        TRABUFERROR = TRABUF(TERR)
        IF ((TRABUF(TSTAT) .EQ. GOOD) .AND. (TRABUFERROR .EQ. NOER)) SUMEUROCANCEL = SUMEUROCANCEL + 1 
        
        WRITE(PUNIT,1901) STAT(TRABUF(TSTAT)),
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
     *                   TRANSTYPE
        IF (DETAIL) THEN
          DBUF(VCDC)=TRABUF(TCDC)
          CALL CDATE(DBUF)
          JUL=DBUF(VJUL) + 500

          IF (TRABUF(TEUCST) .NE. 0) TRABUF(TEUCAM) = 0
        
          WRITE(PUNIT,2902) JUL,
     *                      TRABUF(TEUSER),
     *                      TRABUF(TEUCHK),
     *                      CANSTATUS(TRABUF(TEUCST))
        
          WRITE(PUNIT,2903) TRABUF(TEUCWJUL),
     *                      TRABUF(TEUCWSER),
     *                      TRABUF(TEUCWCKD),
     *                      CMONY(TRABUF(TEUCAM),11,VALUNIT)
  
          WRITE(PUNIT,*)
          LINCNT = LINCNT+3
        ENDIF
        RETURN 
C
C PRINT VALIDATIONS
C
3000    CONTINUE
        TRANSTYPE = 'VALIDACAO'
        LINCNT = LINCNT+1
        PQFLAG = ' AUTO'          
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
        IF (  (TRABUF(TSTAT) .EQ. GOOD)
     *  .AND. (     TRABUF(TEUVSBT) .EQ. VREG
     *         .OR. TRABUF(TEUVSBT) .EQ. VNDON
     *         .OR. TRABUF(TEUVSBT) .EQ. VNBNK
     *        )
     *  .AND. (    (TRABUF(TEUVCAM) .NE. 0)
     *         .OR.(TRABUF(TEUVCAMH) .NE. 0)
     *        )
     *  ) THEN
           SUMEUROVALID = SUMEUROVALID + 1 
        ENDIF

C----+------------------------------------------------------------------
C V05| Adding New Validation Messages and clarifying variables
C----+------------------------------------------------------------------

        WRITE(PUNIT,1901) STAT(TRABUF(TSTAT)),
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
     *                   TRANSTYPE
        IF (DETAIL) THEN
          DBUF(VCDC)=TRABUF(TCDC)
          CALL CDATE(DBUF)
          JUL=DBUF(VJUL) + 500
          EMVALAMT(2) = TRABUF(TEUVCAMH)
          EMVALAMT(1) = TRABUF(TEUVCAM)
          EMVALAMTR8  = DFLOAT(EMVALAMTI8)/100.0D0

          IF (TRABUF(TEUVSBT) .EQ. 15) WRITE(PUNIT,3901)
     *                     VALSUBTYP(TRABUF(TEUVSBT)+1),
     *                     VALSTATUSE(TRABUF(TEUVST)),
     *                     '              ' ! If Validation Error then do not print the Cash Amount
C    *                     CMONYI8(EMVALAMT,14,VALUNIT)
C    *                     CMONY(TRABUF(TEUVCAM),11,VALUNIT)
        
          IF (TRABUF(TEUVSBT) .NE. 15) WRITE(PUNIT,3902)
     *                     VALSUBTYP(TRABUF(TEUVSBT)+1),
     *                     VALSTATUS(TRABUF(TEUVST)),
     *                     EMVALAMTR8
C    *                     CMONY(TRABUF(TEUVCAM),11,VALUNIT)
          WRITE(PUNIT,3903) JUL, 
     *                     TRABUF(TEUSER),
     *                     TRABUF(TEUCHK),
     *                     TRABUF(TEUVWJUL),
     *                     TRABUF(TEUVWSER),
     *                     TRABUF(TEUVWCKD),
     *                     TRABUF(TEUVTIMEH),
     *                     TRABUF(TEUVTIMEM),
     *                     TRABUF(TEUVTIMES)
          LINCNT = LINCNT+1
C----+------------------------------------------------------------------          
C V05| New Validation Messages
C----+------------------------------------------------------------------
          IF(TRABUF(TEUVSBT) .EQ. VNBNK) THEN
             CALL FASTSET(BLANK,NIB,6)
             WRITE(CNIB,3914) TRABUF(TEUVNIBBB)
     *                      , TRABUF(TEUVNIBBO)
     *                      , TRABUF(TEUVNIBBA1)
     *                      , TRABUF(TEUVNIBBA2)
     *                      , TRABUF(TEUVNIBCD)
             IF(TRABUF(TEUVPLIDTYP).EQ.PHONNBR) THEN
               WRITE(PUNIT,3915) TRABUF(TEUVPLCARD),(NIB(I),I=1,6)
               LINCNT = LINCNT+1 
             ELSEIF(TRABUF(TEUVPLIDTYP).EQ.PLAYCRD) THEN
               WRITE(PUNIT,3916) TRABUF(TEUVPLCARD),(NIB(I),I=1,6)
               LINCNT = LINCNT+1 
             ENDIF
          ENDIF
C----+------------------------------------------------------------------          
C V05| New Validation Messages
C----+------------------------------------------------------------------          
        
          IF (TRABUF(TEUVST) .EQ. 11) THEN
             IF (TRABUF(TEUVEQP) .EQ. 0) PQFLAG = 'MANUAL'           
             I4TEMP = TRABUF(TEUEVWCKD)
                      
             WRITE(PUNIT,3904) JUL,
     *                        TRABUF(TEUEVWSER),
     *                        ZEXT(I1TEMP(1)),
C     *                        TRABUF(TEUEVWCKD),
     *                        TRABUF(TEUVEBEGW),
     *                        TRABUF(TEUVEBEGY),
     *                        TRABUF(TEUVEENDW),
     *                        TRABUF(TEUVEENDY),
     *                        TRABUF(TEUVEDUR) 
          
             WRITE(PUNIT,903) TRABUF(TEUVENBET),
     *                        PQFLAG,
     *                        TRABUF(TEUVENMK),
     *                        TRABUF(TEUVENST),
     *                        TRABUF(TEUVETIMEH),
     *                        TRABUF(TEUVETIMEM),
     *                        TRABUF(TEUVETIMES)
             LINCNT = LINCNT+2
             IND = 1
             DO I=TEUVEBOARD,120
               IF(TRABUF(I) .NE. 0) THEN
                 CALL MOVBYT(TRABUF(I),1,OUTTAB,IND,4) 
	               IND=IND+4
               ENDIF
             ENDDO
!
! EUROMILLIONS EVOLUTION PROJECT
!           CALL TRANSFBOARD(OUTTAB,TRABUF(TEUVENBET),TRABUF(TEUVENMK),TRABUF(TEUVENST),50,9,ST,PUNIT) 
             CALL TRANSFBOARD(OUTTAB,TRABUF(TEUVENBET),TRABUF(TEUVENMK),TRABUF(TEUVENST),50,11,ST,PUNIT) 
!
             LINES = TRABUF(TEUVENBET)
             LINCNT = LINCNT+LINES
          ENDIF

          WRITE(PUNIT,*)
          LINCNT = LINCNT+1
        ENDIF
        RETURN
C
C===========
C
CCC     FORMAT statements

800     FORMAT(A2,I7.7,A2)
900     FORMAT(/,' STATUS ERROR  TYPE',4X,'SERIAL',5X,
     *         'TIME  TERM SEQ  DATE GAME GAMETYP  GIND ',
     *         'SIZE  BEG        END     JOKER ',11X,
     *          'FRACTION','  X BET',
     *         /,1X,131('='),/)
1901     FORMAT(1X,3(2X,A4),I10,1X,A8,I6,Z4,I6,I5,1X,A8,
     *         I5,I5,2X,A12)
901     FORMAT(1X,3(2X,A4),I10,1X,A8,I6,Z4,I6,I5,1X,A8,
     *         I5,I5,I5,1X,A4,1X,I5,A2,2X,A11,A10,1X,I4,1X,I3)     
C902     FORMAT('   EXTSER: < ',I3.3,'-',I8.8,'-',I3.3,' >    Draw Beg: <',I3.2,'/',I2.2,
C
C EM EVOLUTION PROJECT
C
!902     FORMAT('   EXTSER: < ',I3.3,'-',I8.8,'-',A3,' >    Draw Beg: <',I3.2,'/',I2.2,
!     *         ' >   Draw End: <',I3.2,'/',I2.2,' > Duration: <',I2.2,'>')
!903     FORMAT('   # Of Boards: <',I2,'>     Qp: <',A6,'>     # Of Marks: <',I2,
!     *         '>     # Of Stars: <',I1,'>     Euromil Time: <',I2.2,':',I2.2,':',I2.2,'>')
902     FORMAT('   EXTSER: < ',I3.3,'-',I8.8,'-',A3,' >    Draw Beg: <',I4.3,'/',I2.2,
     *         ' >   Draw End: <',I4.3,'/',I2.2,' > Duration: <',I2.2,'>',' Draw Indicator: <',I1,'>')
903     FORMAT('   # Of Boards: <',I2,'>     Qp: <',A6,'>     # Of Marks: <',I2,
     *         '>     # Of Stars: <',I2,'>     Euromil Time: <',I2.2,':',I2.2,':',I2.2,'>')
C
C
C----+------------------------------------------------------------------          
C V05| New Validation Messages
C----+------------------------------------------------------------------
C3901     FORMAT('   Val Subtyp: < ',A16,
C     *          ' > Val Status: < ',A31,' > Cash Amount: < ',A14,' >')     
3901     FORMAT('   Val Subtyp: < ',A23,
     *          ' > Val Status: < ',A31,' > Cash Amount: < ',A14,' >')     
C----+------------------------------------------------------------------          
C V05| New Validation Messages
C----+------------------------------------------------------------------
C3902     FORMAT('   EXTSER: < ',I3.3,'-',I8.8,'-',I3.3,' > Val Subtyp: <',A21,
3902     FORMAT('   Val Subtyp: < ',A16,
     *          ' > Val Status: < ',A31,' > Cash Amount: < ',F14.2,' >')
3903     FORMAT('   EXTSER: < ',I3.3,'-',I8.8,'-',I3.3,
     *          ' > Wager External Serial: < ',I3.3,'-',I8.8,'-',I3.3,
     *          ' >       Euromil Time: <',I2.2,':',I2.2,':',I2.2,'>')
C----+------------------------------------------------------------------          
C V05| New Validation Messages
C----+------------------------------------------------------------------
3914     FORMAT(I4.4,1X,I4.4,I9.9,I2.2,1X,I2.2)        ! NIB Print
3915     FORMAT(3X,'PHONE NUMBER: < ',I10,' > NIB: < ',6A4, ' >' ) ! Phone Nr
3916     FORMAT(3X,' PLAYER CARD: < ',I10,' > NIB: < ',6A4, ' >') ! Player Card
C----+------------------------------------------------------------------          
C V05| New Validation Messages
C----+------------------------------------------------------------------
     
C
C EM EVOLUTION PROJECT
C         
!3904     FORMAT('   EXTSER (Exchange Ticket): < ',I3.3,'-',I8.8,'-',I3.3,
!     *          ' >    Draw Beg: <',I3.2,'/',I2.2,
!     *          ' >   Draw End: <',I3.2,'/',I2.2,' > Duration: <',I2.2,'>')
3904     FORMAT('   EXTSER (Exchange Ticket): < ',I3.3,'-',I8.8,'-',I3.3,
     *          ' >    Draw Beg: <',I4.3,'/',I2.2,
     *          ' >   Draw End: <',I4.3,'/',I2.2,' > Duration: <',I2.2,'>')
C
2902     FORMAT('   EXTSER: < ',I3.3,'-',I8.8,'-',I3.3,
     *          ' > Can Status: < ',A51,' >')
2903     FORMAT('    Wager External Serial: <',I3.3,'-',I8.8,'-',I3.3,'>',
     *          '    Cancel Amount: < ',A11,' >')
     
        END
C**********************************************
C SUBROUTINE TRANSFBOARD
C**********************************************
C        
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE TRANSFBOARD(TERMES,NBOARD,SYSTEMMARK,SYSTEMSTAR,MAXNUM,MAXSTAR,ST,PUNIT)
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
C
        INTEGER*4   BRD,PNT,VAL,CNT,XBYT,NIB,SVAL,SCNT,J,I
        LOGICAL     LEFT
C
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
2100	  CONTINUE
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
       
C
C          WRITE(MARKS,100) VAL
          CNT = CNT+1
          MARKS(CNT) = VAL
	  IF(CNT .LT. SYSTEMMARK) GOTO 2100
C
C STAR NUMBERS 
C	  
          SVAL = 0
	  SCNT = 0
2200	  CONTINUE
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
	  WRITE(PUNIT,100) BRD,(MARKS(J+1),J=0,SYSTEMMARK-1),(STARS(I+1),I=0,SYSTEMSTAR-1)
c	  WRITE(PUNIT,101) '             ', 	
C
2900    CONTINUE
	ST = 0
        
C
9000    CONTINUE
        IF (ST .NE. 0) WRITE(PUNIT,*) 'ERROR ',ST 
        RETURN
100     FORMAT('   Board ',I2,'    Numbers: <',<SYSTEMMARK>I3.2,' > Stars: <',<SYSTEMSTAR>I3.2,' >')
101     FORMAT(A12,A10,<SYSTEMSTAR>I3)
        END
