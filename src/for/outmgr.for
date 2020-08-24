C PROGRAM OUTMGR
C
C OUTMGR.FOR
C
C V10 07-OCT-2016 SCML Set up TRABUF(TEUMESSQ) when TRABUF(TSTAT) is SDOR
C V09 08-MAR-2016 SCML M16 PROJECT
C V08 26-MAR-2014 SCML IGS Placard Project - Bug fix
C V07 12-MAY-2014 SCML Retry Bug Fix. 
C V06 07-JAN-2014 SCML Fix overflow problem in validation last transaction update
C V05 04-DEC-2013 SCML Marking transaction data for new accounting report
C                      Update logic of BCRS and SDOR for TGREP and TNAP special functions
C V04 20-NOV-2013 SCML UPDATE AGTTAB(ALSVAL,TER) WHEN TEUVSBT IS VNDON AND VNBNK.
C V03 12-APR-2011 FJG ACCENTURE MERGE FOR EM2
C V02 30-MAR-2011 ACN EUROMILLIONS VALIDATION PRIZE OVER 42M
C
C The process OUTMGR is responsible for creating the response message to the corresponding 
C TERMINAL ALTURA and to send to the logger to record the message in TMF.
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM OUTMGR
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:APUCOM.DEF'
C----+------------------------------------------------------------------
C V08| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        INCLUDE 'INCLIB:IGSDEBUG.DEF'
C----+------------------------------------------------------------------
C V08| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        
        INTEGER*4  MESS(EDLEN),WRKBUF(TRALEN)       !
        INTEGER*4  TASK              !
        INTEGER*4  BUF               !
        INTEGER*4  STATUS,RETRYIND
        INTEGER*4  MES_LEN,I,IND,TER,RETRY,OPTIONS,LEN
C        INTEGER*4 SIZEOFRBUF
        INTEGER*4  I4TEMP,TEMP1,TEMP2
        INTEGER*2  I2TEMP(2)
        BYTE       I1TEMP(4)
        EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
        BYTE ERRORNUM 
        INTEGER*4 MYCHKSUM
        INTEGER*4 MTYPE,SUBTYP

        INTEGER*4 MES_IND
        INTEGER*4 ODATIND                                                       !OPTION DATA INDEX !V09

C----+------------------------------------------------------------------
C V06| Fix overflow problem in validation last transaction update
C----+------------------------------------------------------------------
        INTEGER*8 I8AUX_AMOUNT
        INTEGER*4 I4AUX_AMOUNT(2)
        EQUIVALENCE(I8AUX_AMOUNT,I4AUX_AMOUNT)
C----+------------------------------------------------------------------
C V06| Fix overflow problem in validation last transaction update
C----+------------------------------------------------------------------

        CALL OPSTXT(' Copyright 2004 SCML/Accenture. All rights reserved. ')
        CALL SNIF_AND_WRKSET

        TASK    = EUO
        MESS(1) = TASK

        CALL OPSTXT(' ******************* OUTMGR ****************')
C
C WAIT FOR SOMETHING TO DO
C IF END OF DAY THEN CALL GSTOP(GEXIT_SUCCESS)
C
10      CONTINUE
        IF(DAYSTS .EQ. DSCLOS) CALL GSTOP(GEXIT_SUCCESS)
        IF(DAYSTS .EQ. DSSUSP) THEN
          CALL HOLD(0,STATUS)
          IF(DAYSTS .EQ. DSOPEN) GOTO 10
          GOTO 10
        ENDIF
C
C VERIFY STATUS OF INMGR AND COMMGR
C
c        CALL CHECKPROCESS()
        CALL HOLD(0,STATUS)
C
C GET BUFFER NUMBER FROM TOP OF QUEUE.
C IF NO WAGERS QUEUED, GO BACK TO WAIT STATE.
C
20      CONTINUE
        CALL TOPQUE(TASK,BUF)
        IF(BUF .EQ. 0) GOTO 10
C        CALL OPSTXT('EURO MIL MESSAGE out')
C
C VERIFY STATUS OF INMGR AND COMMGR
C
        CALL CHECKPROCESS()
        
C        CALL OPSTXT(' ******************* OUTMGR ****************')
C
C IF NOT MASTER SYSTEM THEN SEND DATA TO WRKTAB
C
C        IF (P(SYSTYP) .NE. LIVSYS) THEN
C           CALL OPS('VALOR DE BPRO(BOUTTAB,BUF)',BPRO(BOUTTAB,BUF))
C           SIZEOFRBUF = HPRO(OUTLEN,BUF)
C           CALL OPS('RECEBI DADOS NO BACKUP',SIZEOFRBUF,0)
C           DO I=0, SIZEOFRBUF
C              BPRO(WRKTAB*4-3+I,BUF) = BPRO(BOUTTAB+I,BUF)
C           ENDDO
C       ENDIF
C
C GET TRABUF OUT OF PROCOM BUFFER
C
        CALL FASTSET(0,TRABUF,TRALEN)
C        IF (P(SYSTYP) .NE. LIVSYS) THEN
C           CALL LOGTRA(TRABUF,PRO(INPTAB,BUF))
C           CALL OPS('PRO(INPTAB,BUF)',PRO(INPTAB,BUF),PRO(INPTAB,BUF))
C           CALL OPS('OUTMGR TYPE: ',TRABUF(TTYP))
C           CALL OPS('ouMGR AGENT: ',TRABUF(TAGT))
C           CALL TRALOG(TRABUF,APUBUF(2,BUF))
C        ENDIF        
        CALL LOGTRA(TRABUF,APUBUF(2,BUF))
        
C       CALL OPS('VALOR DE TAGT: ',TRABUF(TAGT),TRABUF(TAGT))
C       CALL OPS('Trans Type',TRABUF(TTYP),TRABUF(TTYP))
C
C SET UP TRABUF
C
c        CALL OPS('TRABUF(TTYP)',TRABUF(TTYP),TRABUF(TTYP))
        TER =HPRO(TERNUM,BUF)
        TRABUF(TSER)=PRO(SERIAL,BUF)
        TRABUF(TTIM)=PRO(TSTAMP,BUF)
        TRABUF(TSIZE)=HPRO(NUMLRC,BUF)
        TRABUF(TCDC) = DAYCDC     
C        CALL OPS('TRABUF(TEUTYP)',TRABUF(TEUTYP))
C
C PROCESS RETRYS
C
        IF (TRABUF(TERR) .EQ. RETY .AND. TRABUF(TSTAT) .EQ. REJT) THEN
          CALL LOGTRA(WRKBUF,PRO(WRKTAB,BUF))
          RETRYIND = 0
          IF (WRKBUF(TEUTYP) .EQ. TWAG) THEN
             IF(WRKBUF(TSTAT) .NE. GOOD) THEN
               ERRORNUM = 5 
               CALL OUTERRORMES(WRKBUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),ERRORNUM,RETRY)
               GOTO 2000
             ENDIF

             BPRO(BOUTTAB+RETRYIND+0,BUF) = '20'X+WRKBUF(TTRN)
             RETRYIND = RETRYIND + 1
             
             BPRO(BOUTTAB+RETRYIND+0,BUF) = 0
             RETRYIND = RETRYIND + 3
C
C GAME TYPE AND INDEX
C           
             BPRO(BOUTTAB+RETRYIND+0,BUF) = WRKBUF(TGAMTYP) !#5 GAME TYPE
             RETRYIND=RETRYIND+1

             BPRO(BOUTTAB+RETRYIND+0,BUF) = WRKBUF(TGAMIND) !#6 GAME INDEX
             RETRYIND=RETRYIND+1
C
C EXTERNAL EURO SERIAL AND CHECKDIGITS
C
             I4TEMP = WRKBUF(TEUSER)
             TRABUF(TEUSER) = WRKBUF(TEUSER)
             BPRO(BOUTTAB+RETRYIND+0,BUF)=ZEXT(I1TEMP(3))   !#7   External (wager) serial #
             BPRO(BOUTTAB+RETRYIND+1,BUF)=ZEXT(I1TEMP(2))   !#8
             BPRO(BOUTTAB+RETRYIND+2,BUF)=ZEXT(I1TEMP(1))   !#9
             RETRYIND=RETRYIND+3

	     TRABUF(TEUCHK) = WRKBUF(TEUCHK)
             BPRO(BOUTTAB+RETRYIND+0,BUF)=WRKBUF(TEUCHK) !#10 CHECK DIGITS
             RETRYIND=RETRYIND+1
             
C
C OFFSET FIRST CDC DATE
C
             I4TEMP = WRKBUF(TEUWOFS1)
	     TRABUF(TEUWOFS1) = WRKBUF(TEUWOFS1)
             BPRO(BOUTTAB+RETRYIND+0,BUF) = ZEXT(I1TEMP(2))        ! #11
             BPRO(BOUTTAB+RETRYIND+1,BUF) = ZEXT(I1TEMP(1))        ! #12
C
C OFFSET SECOND CDC DATE
C
             I4TEMP = WRKBUF(TEUWOFS2)
	     TRABUF(TEUWOFS2) = WRKBUF(TEUWOFS2)
             BPRO(BOUTTAB+RETRYIND+2,BUF) = ZEXT(I1TEMP(2))        ! #13
             BPRO(BOUTTAB+RETRYIND+3,BUF) = ZEXT(I1TEMP(1))        ! #14
             RETRYIND=RETRYIND+4
C
C FIRST WEEK AND YEAR DRAW DATE
C
             I4TEMP = WRKBUF(TEUWBEGW)
	     TRABUF(TEUWBEGW) = WRKBUF(TEUWBEGW)
             BPRO(BOUTTAB+RETRYIND+0,BUF) = ZEXT(I1TEMP(1))        ! #15
             I4TEMP = WRKBUF(TEUWBEGY)
	     TRABUF(TEUWBEGY) = WRKBUF(TEUWBEGY)
             BPRO(BOUTTAB+RETRYIND+1,BUF) = ZEXT(I1TEMP(1))        ! #16
             RETRYIND = RETRYIND + 2
C
C SECOND WEEK AND YEAR DRAW DATE
C
             I4TEMP = WRKBUF(TEUWENDW)
	     TRABUF(TEUWENDW) = WRKBUF(TEUWENDW)
             BPRO(BOUTTAB+RETRYIND+0,BUF) = ZEXT(I1TEMP(1))       ! #17
             I4TEMP = WRKBUF(TEUWENDY)
	     TRABUF(TEUWENDY) = WRKBUF(TEUWENDY)
             BPRO(BOUTTAB+RETRYIND+1,BUF) = ZEXT(I1TEMP(1))        ! #18
             RETRYIND = RETRYIND + 2
C
C TIME
C
             I4TEMP = WRKBUF(TEUWTIMEH)
	     TRABUF(TEUWTIMEH) = WRKBUF(TEUWTIMEH)
             BPRO(BOUTTAB+RETRYIND+0,BUF) = ZEXT(I1TEMP(1))        ! #19
             I4TEMP = WRKBUF(TEUWTIMEM)
	     TRABUF(TEUWTIMEM) = WRKBUF(TEUWTIMEM)
             BPRO(BOUTTAB+RETRYIND+1,BUF) = ZEXT(I1TEMP(1))        ! #20
             I4TEMP = WRKBUF(TEUWTIMES)       
	     TRABUF(TEUWTIMES) = WRKBUF(TEUWTIMES)
             BPRO(BOUTTAB+RETRYIND+2,BUF) = ZEXT(I1TEMP(1))  	 ! #21
             RETRYIND = RETRYIND + 3
C----+---+-------------+------------------------------------------------
C V09|BEG| M16 PROJECT | WAGER RETRY: SET UP NEW TRABUF FIELDS
C----+---+-------------+------------------------------------------------
C
C OPTION FLAGS
C
C             BPRO(BOUTTAB+RETRYIND+0,BUF) = 0  		! #22
             OPTIONS = 0
             CALL OGETEUROPT(WRKBUF, OPTIONS)
             BPRO(BOUTTAB+RETRYIND+0,BUF) = OPTIONS ! #22
             RETRYIND = RETRYIND + 1
C
C OPTION DATA
C
C            SET UP SM DATA
             IF(IAND(OPTIONS,'08'X).NE.0) THEN
               TRABUF(TEUW_SMWFL) = WRKBUF(TEUW_SMWFL)                          !SM DATA PRESENT FLAG
C
               I4TEMP = WRKBUF(TEUW_SMWTB)
               TRABUF(TEUW_SMWTB) = WRKBUF(TEUW_SMWTB)                          !SM TOTAL BETS
               BPRO(BOUTTAB+RETRYIND+0,BUF) = I1TEMP(2)
               BPRO(BOUTTAB+RETRYIND+1,BUF) = I1TEMP(1)
C
               I4TEMP = WRKBUF(TEUW_SMWSN)
               TRABUF(TEUW_SMWSN) = WRKBUF(TEUW_SMWSN)                          !SM EXTERNAL SERIAL NUMBER
               BPRO(BOUTTAB+RETRYIND+2,BUF) = I1TEMP(3)
               BPRO(BOUTTAB+RETRYIND+3,BUF) = I1TEMP(2)
               BPRO(BOUTTAB+RETRYIND+4,BUF) = I1TEMP(1)
C
               I4TEMP = WRKBUF(TEUW_SMWCD)
               TRABUF(TEUW_SMWCD) = WRKBUF(TEUW_SMWCD)                          !SM CHECK DIGITS
               BPRO(BOUTTAB+RETRYIND+5,BUF) = I1TEMP(1)
C
               I4TEMP = WRKBUF(TEUW_SMWDN)
               TRABUF(TEUW_SMWDN) = WRKBUF(TEUW_SMWDN)                          !SM DRAW NUMBER
               BPRO(BOUTTAB+RETRYIND+6,BUF) = I1TEMP(1)
C
               I4TEMP = WRKBUF(TEUW_SMWDY)
               TRABUF(TEUW_SMWDY) = WRKBUF(TEUW_SMWDY)                          !SM DRAW YEAR
               BPRO(BOUTTAB+RETRYIND+7,BUF) = I1TEMP(1)
C
               I4TEMP = WRKBUF(TEUW_SMWOF)
               TRABUF(TEUW_SMWOF) = WRKBUF(TEUW_SMWOF)                          !OFFSET TO SM DRAW CDC DATE
               BPRO(BOUTTAB+RETRYIND+8,BUF) = I1TEMP(1)
C
               I4TEMP = WRKBUF(TEUW_SMWB1)
               TRABUF(TEUW_SMWB1) = WRKBUF(TEUW_SMWB1)                          !SM WAGER FIRST NUMBER
               BPRO(BOUTTAB+RETRYIND+ 9,BUF) = I1TEMP(1)
               BPRO(BOUTTAB+RETRYIND+10,BUF) = I1TEMP(2)
               BPRO(BOUTTAB+RETRYIND+11,BUF) = I1TEMP(3)
               BPRO(BOUTTAB+RETRYIND+12,BUF) = I1TEMP(4)
               I4TEMP = WRKBUF(TEUW_SMWB2)
               TRABUF(TEUW_SMWB2) = WRKBUF(TEUW_SMWB2)
               BPRO(BOUTTAB+RETRYIND+13,BUF) = I1TEMP(3)
               BPRO(BOUTTAB+RETRYIND+14,BUF) = I1TEMP(2)
               BPRO(BOUTTAB+RETRYIND+15,BUF) = I1TEMP(1)
C
               I4TEMP = WRKBUF(TEUW_SMWE1)
               TRABUF(TEUW_SMWE1) = WRKBUF(TEUW_SMWE1)                          !SM WAGER LAST NUMBER
               BPRO(BOUTTAB+RETRYIND+16,BUF) = I1TEMP(1)
               BPRO(BOUTTAB+RETRYIND+17,BUF) = I1TEMP(2)
               BPRO(BOUTTAB+RETRYIND+18,BUF) = I1TEMP(3)
               BPRO(BOUTTAB+RETRYIND+19,BUF) = I1TEMP(4)
               I4TEMP = WRKBUF(TEUW_SMWE2)
               TRABUF(TEUW_SMWE2) = WRKBUF(TEUW_SMWE2)
               BPRO(BOUTTAB+RETRYIND+20,BUF) = I1TEMP(3)
               BPRO(BOUTTAB+RETRYIND+21,BUF) = I1TEMP(2)
               BPRO(BOUTTAB+RETRYIND+22,BUF) = I1TEMP(1)
C
               RETRYIND = RETRYIND + 23
             ENDIF
C            SET UP SoM DATA
             IF(IAND(OPTIONS,'04'X).NE.0) THEN
               TRABUF(TEUW_SHWFL) = WRKBUF(TEUW_SHWFL)                          !SoM DATA PRESENT FLAG
C
               I4TEMP = WRKBUF(TEUW_SHWTB)
               TRABUF(TEUW_SHWTB) = WRKBUF(TEUW_SHWTB)                          !SoM TOTAL BETS
               BPRO(BOUTTAB+RETRYIND+0,BUF) = I1TEMP(2)
               BPRO(BOUTTAB+RETRYIND+1,BUF) = I1TEMP(1)
C
               I4TEMP = WRKBUF(TEUW_SHWDN)
               TRABUF(TEUW_SHWDN) = WRKBUF(TEUW_SHWDN)                          !SoM DRAW NUMBER
               BPRO(BOUTTAB+RETRYIND+2,BUF) = I1TEMP(1)
C
               I4TEMP = WRKBUF(TEUW_SHWDY)
               TRABUF(TEUW_SHWDY) = WRKBUF(TEUW_SHWDY)                          !SoM DRAW YEAR
               BPRO(BOUTTAB+RETRYIND+3,BUF) = I1TEMP(1)
C
               I4TEMP = WRKBUF(TEUW_SHWOF)
               TRABUF(TEUW_SHWOF) = WRKBUF(TEUW_SHWOF)                          !OFFSET TO SoM DRAW CDC DATE
               BPRO(BOUTTAB+RETRYIND+4,BUF) = I1TEMP(1)
C
               I4TEMP = WRKBUF(TEUW_SHWB1)
               TRABUF(TEUW_SHWB1) = WRKBUF(TEUW_SHWB1)                          !SoM WAGER FIRST NUMBER
               BPRO(BOUTTAB+RETRYIND+5,BUF) = I1TEMP(1)
               BPRO(BOUTTAB+RETRYIND+6,BUF) = I1TEMP(2)
               BPRO(BOUTTAB+RETRYIND+7,BUF) = I1TEMP(3)
               BPRO(BOUTTAB+RETRYIND+8,BUF) = I1TEMP(4)
               I4TEMP = WRKBUF(TEUW_SHWB2)
               TRABUF(TEUW_SHWB2) = WRKBUF(TEUW_SHWB2)
               BPRO(BOUTTAB+RETRYIND+ 9,BUF) = I1TEMP(3)
               BPRO(BOUTTAB+RETRYIND+10,BUF) = I1TEMP(2)
               BPRO(BOUTTAB+RETRYIND+11,BUF) = I1TEMP(1)
C
               I4TEMP = WRKBUF(TEUW_SHWE1)
               TRABUF(TEUW_SHWE1) = WRKBUF(TEUW_SHWE1)                          !SoM WAGER LAST NUMBER
               BPRO(BOUTTAB+RETRYIND+12,BUF) = I1TEMP(1)
               BPRO(BOUTTAB+RETRYIND+13,BUF) = I1TEMP(2)
               BPRO(BOUTTAB+RETRYIND+14,BUF) = I1TEMP(3)
               BPRO(BOUTTAB+RETRYIND+15,BUF) = I1TEMP(4)
               I4TEMP = WRKBUF(TEUW_SHWE2)
               TRABUF(TEUW_SHWE2) = WRKBUF(TEUW_SHWE2)
               BPRO(BOUTTAB+RETRYIND+16,BUF) = I1TEMP(3)
               BPRO(BOUTTAB+RETRYIND+17,BUF) = I1TEMP(2)
               BPRO(BOUTTAB+RETRYIND+18,BUF) = I1TEMP(1)
C
               RETRYIND = RETRYIND + 19
             ENDIF
C----+---+-------------+------------------------------------------------
C V09|END| M16 PROJECT | WAGER RETRY - SET UP NEW TRABUF FIELDS
C----+---+-------------+------------------------------------------------
C
             HPRO(OUTLEN,BUF) = RETRYIND
             I4CCITT   = TRABUF(TCHK)                         
             BPRO(BOUTTAB+2,BUF) = I1CCITT(2)                 
             BPRO(BOUTTAB+3,BUF) = I1CCITT(1)                 
             CALL GETCCITT(BPRO(BOUTTAB,BUF),1,RETRYIND-1,MYCHKSUM)
             I4CCITT   = MYCHKSUM                             
             BPRO(BOUTTAB+2,BUF) = I1CCITT(2)                 
             BPRO(BOUTTAB+3,BUF) = I1CCITT(1)                 
             TRABUF(TSTAT) = GOOD
             GOTO 2000
          ELSEIF (WRKBUF(TEUTYP) .EQ. TCAN) THEN
             IF(WRKBUF(TSTAT) .NE. GOOD) THEN
               ERRORNUM = 5 
               CALL OUTERRORMES(WRKBUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),ERRORNUM,RETRY)
               IF (WRKBUF(TEUTYP) .EQ. TCAN) TRABUF(TEUCST) = 5
               GOTO 2000
             ENDIF

             BPRO(BOUTTAB+RETRYIND+0,BUF) = '20'X+WRKBUF(TTRN)
             RETRYIND = RETRYIND + 1
             
             IF (WRKBUF(TEUCST) .EQ. 0) BPRO(BOUTTAB+RETRYIND+0,BUF) = '20'X
             IF (WRKBUF(TEUCST) .NE. 0) BPRO(BOUTTAB+RETRYIND+0,BUF) = '2F'X
             RETRYIND = RETRYIND + 3
C
C TIME
C
            I4TEMP = WRKBUF(TEUCTIMEH)
            TRABUF(TEUCTIMEH) = WRKBUF(TEUCTIMEH)
            BPRO(BOUTTAB+RETRYIND+0,BUF) = ZEXT(I1TEMP(1))       ! #5
            I4TEMP = WRKBUF(TEUCTIMEM)
            TRABUF(TEUCTIMEM) = WRKBUF(TEUCTIMEM)
            BPRO(BOUTTAB+RETRYIND+1,BUF) = ZEXT(I1TEMP(1))       ! #6
            I4TEMP = WRKBUF(TEUCTIMES)       
	    TRABUF(TEUCTIMES) = WRKBUF(TEUCTIMES)
            BPRO(BOUTTAB+RETRYIND+2,BUF) = ZEXT(I1TEMP(1))  	 ! #7
            RETRYIND = RETRYIND + 3  
C
C CANCEL EXTERNAL SERIAL NUMBER
C        
            I4TEMP = WRKBUF(TEUSER)
            TRABUF(TEUSER) = WRKBUF(TEUSER)
            BPRO(BOUTTAB+RETRYIND+0,BUF)=ZEXT(I1TEMP(3))   !#8   External (CANCEL) serial #
            BPRO(BOUTTAB+RETRYIND+1,BUF)=ZEXT(I1TEMP(2))   !#9
            BPRO(BOUTTAB+RETRYIND+2,BUF)=ZEXT(I1TEMP(1))   !#10
            RETRYIND=RETRYIND+3

            BPRO(BOUTTAB+RETRYIND+0,BUF)= WRKBUF(TEUCHK) !#11 CHECK DIGITS
            TRABUF(TEUCHK) = WRKBUF(TEUCHK)            
            RETRYIND=RETRYIND+1
C        
C GAME TYPE AND INDEX
C
	    BPRO(BOUTTAB+RETRYIND+0,BUF) = WRKBUF(TGAMTYP) !#12 GAME TYPE
	    BPRO(BOUTTAB+RETRYIND+1,BUF) = WRKBUF(TGAMIND) !#13 GAME INDEX
	    RETRYIND = RETRYIND + 2
C
C CANCEL STATUS
C
            TRABUF(TEUCST) = WRKBUF(TEUCST)            
            BPRO(BOUTTAB+RETRYIND+0,BUF) = WRKBUF(TEUCST) 	! # 14 CANCEL STATUS
            RETRYIND = RETRYIND + 1
            IF (WRKBUF(TEUCST) .EQ. 0) THEN

C
C CANCEL AMOUNT 
C
              I4TEMP = WRKBUF(TEUCAM)
              TRABUF(TEUCAM) = WRKBUF(TEUCAM)            
              BPRO(BOUTTAB+RETRYIND+0,BUF)=ZEXT(I1TEMP(4))   !#15   
              BPRO(BOUTTAB+RETRYIND+1,BUF)=ZEXT(I1TEMP(3))   !#16   
              BPRO(BOUTTAB+RETRYIND+2,BUF)=ZEXT(I1TEMP(2))   !#17
              BPRO(BOUTTAB+RETRYIND+3,BUF)=ZEXT(I1TEMP(1))   !#18
              RETRYIND = RETRYIND + 4
C
C MONDAY DRAW INDICATOR
C
              BPRO(BOUTTAB+RETRYIND+0,BUF)= 0   !#19   
              RETRYIND = RETRYIND + 1                      
C----+---+-------------+------------------------------------------------
C V09|BEG| M16 PROJECT | CANCELLATION RETRY: SET UP NEW TRABUF FIELDS
C----+---+-------------+------------------------------------------------
C
C OPTION FLAGS
C
              OPTIONS = 0
              CALL OGETEUROPT(WRKBUF, OPTIONS)
              BPRO(BOUTTAB+RETRYIND+0,BUF) = OPTIONS ! #20
              RETRYIND = RETRYIND + 1
C
C OPTION DATA
C
C             SET UP SM CANCELLATION DATA
              IF(IAND(OPTIONS,'80'X).NE.0) THEN
                TRABUF(TEUC_SMCFL) = WRKBUF(TEUC_SMCFL)                         !SM CANCELLATION DATA PRESENT FLAG
C
                I4TEMP = WRKBUF(TEUC_SMWSN)
                TRABUF(TEUC_SMWSN) = WRKBUF(TEUC_SMWSN)                         !SM WAGER EXTERNAL SERIAL NUMBER
                BPRO(BOUTTAB+RETRYIND+0,BUF) = I1TEMP(3)
                BPRO(BOUTTAB+RETRYIND+1,BUF) = I1TEMP(2)
                BPRO(BOUTTAB+RETRYIND+2,BUF) = I1TEMP(1)
C
                I4TEMP = WRKBUF(TEUC_SMWCD)
                TRABUF(TEUC_SMWCD) = WRKBUF(TEUC_SMWCD)                         !SM WAGER CHECK DIGITS
                BPRO(BOUTTAB+RETRYIND+3,BUF) = I1TEMP(1)
C
                I4TEMP = WRKBUF(TEUC_SMCSN)
                TRABUF(TEUC_SMCSN) = WRKBUF(TEUC_SMCSN)                         !SM CANCELLATION EXTERNAL SERIAL NUMBER
                BPRO(BOUTTAB+RETRYIND+4,BUF) = I1TEMP(3)
                BPRO(BOUTTAB+RETRYIND+5,BUF) = I1TEMP(2)
                BPRO(BOUTTAB+RETRYIND+6,BUF) = I1TEMP(1)
C
                I4TEMP = WRKBUF(TEUC_SMCCD)
                TRABUF(TEUC_SMCCD) = WRKBUF(TEUC_SMCCD)                         !SM CANCELLATION CHECK DIGITS
                BPRO(BOUTTAB+RETRYIND+7,BUF) = I1TEMP(1)
C
                I4TEMP = WRKBUF(TEUC_SMWCA)
                TRABUF(TEUC_SMWCA) = WRKBUF(TEUC_SMWCA)                         !SM CANCEL AMOUNT (WAGER UNITS)
                BPRO(BOUTTAB+RETRYIND+ 8,BUF) = I1TEMP(4)
                BPRO(BOUTTAB+RETRYIND+ 9,BUF) = I1TEMP(3)
                BPRO(BOUTTAB+RETRYIND+10,BUF) = I1TEMP(2)
                BPRO(BOUTTAB+RETRYIND+11,BUF) = I1TEMP(1)
                RETRYIND = RETRYIND + 12
              ENDIF
C             SET UP SoM CACELLATION FLAG
              IF(IAND(OPTIONS,'40'X).NE.0) THEN
                TRABUF(TEUC_SHCFL) = WRKBUF(TEUC_SHCFL)                         !SoM CANCELLATION FLAG
              ENDIF
C----+---+-------------+------------------------------------------------
C V09|END| M16 PROJECT | CANCELLATION RETRY: SET UP NEW TRABUF FIELDS
C----+---+-------------+------------------------------------------------
            ENDIF

             HPRO(OUTLEN,BUF) = RETRYIND
             I4CCITT   = TRABUF(TCHK)                         
             BPRO(BOUTTAB+2,BUF) = I1CCITT(2)                 
             BPRO(BOUTTAB+3,BUF) = I1CCITT(1)                 
             CALL GETCCITT(BPRO(BOUTTAB,BUF),1,RETRYIND-1,MYCHKSUM)
             I4CCITT   = MYCHKSUM                             
             BPRO(BOUTTAB+2,BUF) = I1CCITT(2)                 
             BPRO(BOUTTAB+3,BUF) = I1CCITT(1)                 
             TRABUF(TSTAT) = GOOD
             GOTO 2000
          ELSEIF (WRKBUF(TEUTYP) .EQ. TVAL) THEN
             IF(WRKBUF(TSTAT) .NE. GOOD) THEN
               ERRORNUM = 5 
               CALL OUTERRORMES(WRKBUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),ERRORNUM,RETRY)
               IF (WRKBUF(TEUTYP) .EQ. TVAL) TRABUF(TEUVST) = 5
               GOTO 2000
             ENDIF

             BPRO(BOUTTAB+RETRYIND+0,BUF) = '20'X+WRKBUF(TTRN)
             RETRYIND = RETRYIND + 1
             
             BPRO(BOUTTAB+RETRYIND+0,BUF) = '10'X + WRKBUF(TEUVSBT)
             TRABUF(TEUVSBT) = WRKBUF(TEUVSBT)            
             RETRYIND = RETRYIND + 3
C
C TIME
C 
             I4TEMP = WRKBUF(TEUVTIMEH)
             TRABUF(TEUVTIMEH) = WRKBUF(TEUVTIMEH)            
             BPRO(BOUTTAB+RETRYIND+0,BUF) = ZEXT(I1TEMP(1))     ! #5
             I4TEMP = WRKBUF(TEUVTIMEM)
             TRABUF(TEUVTIMEM) = WRKBUF(TEUVTIMEM)            
             BPRO(BOUTTAB+RETRYIND+1,BUF) = ZEXT(I1TEMP(1))     ! #6
             I4TEMP = WRKBUF(TEUVTIMES)       
             TRABUF(TEUVTIMES) = WRKBUF(TEUVTIMES)            
             BPRO(BOUTTAB+RETRYIND+2,BUF) = ZEXT(I1TEMP(1))     ! #7
             RETRYIND = RETRYIND + 3  
C
C VALIDATION EXTERNAL SERIAL NUMBER
C        
             I4TEMP = WRKBUF(TEUSER)
             TRABUF(TEUSER) = WRKBUF(TEUSER)            
             BPRO(BOUTTAB+RETRYIND+0,BUF)=I1TEMP(3)   !#8   External (VALID) serial #
             BPRO(BOUTTAB+RETRYIND+1,BUF)=I1TEMP(2)   !#9
             BPRO(BOUTTAB+RETRYIND+2,BUF)=I1TEMP(1)   !#10
             RETRYIND=RETRYIND+3
             
             BPRO(BOUTTAB+RETRYIND+0,BUF)= WRKBUF(TEUCHK) !#11 CHECK DIGITS
             TRABUF(TEUCHK) = WRKBUF(TEUCHK)            
             RETRYIND=RETRYIND+1
C        
C GAME TYPE AND INDEX
C
	     BPRO(BOUTTAB+RETRYIND+0,BUF) = WRKBUF(TGAMTYP) !#12 GAME TYPE
	     BPRO(BOUTTAB+RETRYIND+1,BUF) = WRKBUF(TGAMIND) !#13 GAME INDEX
	     RETRYIND = RETRYIND + 2
C
C VALIDATION STATUS
C
             BPRO(BOUTTAB+RETRYIND+0,BUF) = WRKBUF(TEUVST) 	! # 14 VALID STATUS
             TRABUF(TEUVST) = WRKBUF(TEUVST)            
             RETRYIND = RETRYIND + 1
             
             IF(WRKBUF(TEUVSBT) .NE. 15) THEN
C
C CASH AMOUNT 
C
               I4TEMP = WRKBUF(TEUVCAM)
               TRABUF(TEUVCAM) = WRKBUF(TEUVCAM)            
               BPRO(BOUTTAB+RETRYIND+0,BUF)=I1TEMP(4)   !#15   
               BPRO(BOUTTAB+RETRYIND+1,BUF)=I1TEMP(3)   !#16   
               BPRO(BOUTTAB+RETRYIND+2,BUF)=I1TEMP(2)   !#17
               BPRO(BOUTTAB+RETRYIND+3,BUF)=I1TEMP(1)   !#18
               RETRYIND = RETRYIND + 4
C
C REFUND AMOUNT 
C
               I4TEMP = WRKBUF(TEUVRAM)
               TRABUF(TEUVRAM) = WRKBUF(TEUVRAM)            
               BPRO(BOUTTAB+RETRYIND+0,BUF)=I1TEMP(4)   !#19   
               BPRO(BOUTTAB+RETRYIND+1,BUF)=I1TEMP(3)   !#20   
               BPRO(BOUTTAB+RETRYIND+2,BUF)=I1TEMP(2)   !#21
               BPRO(BOUTTAB+RETRYIND+3,BUF)=I1TEMP(1)   !#22
               RETRYIND = RETRYIND + 4
               
! V02 EUROMILLIONS VALIDATION PRIZE OVER 42M ...

C
C CASH AMOUNT HIGH HALF WORD
C 
               I4TEMP = WRKBUF(TEUVCAMH)
               TRABUF(TEUVCAMH) = WRKBUF(TEUVCAMH)            
               BPRO(BOUTTAB+RETRYIND+0,BUF)=I1TEMP(4)   !#23   
               BPRO(BOUTTAB+RETRYIND+1,BUF)=I1TEMP(3)   !#24   
               BPRO(BOUTTAB+RETRYIND+2,BUF)=I1TEMP(2)   !#25
               BPRO(BOUTTAB+RETRYIND+3,BUF)=I1TEMP(1)   !#26
               RETRYIND = RETRYIND + 4
C
C REFUND AMOUNT HIGH HALF WORD
C 
               I4TEMP = WRKBUF(TEUVRAMH)
               TRABUF(TEUVRAMH) = WRKBUF(TEUVRAMH)            
               BPRO(BOUTTAB+RETRYIND+0,BUF)=I1TEMP(4)   !#27   
               BPRO(BOUTTAB+RETRYIND+1,BUF)=I1TEMP(3)   !#28   
               BPRO(BOUTTAB+RETRYIND+2,BUF)=I1TEMP(2)   !#29
               BPRO(BOUTTAB+RETRYIND+3,BUF)=I1TEMP(1)   !#30
               RETRYIND = RETRYIND + 4

! ... V02 EUROMILLIONS VALIDATION PRIZE OVER 42M
C----+---+-------------+------------------------------------------------
C V09|BEG| M16 PROJECT | VALIDATION RETRY: SET UP NEW TRABUF FIELDS
C----+---+-------------+------------------------------------------------
               IF(WRKBUF(TEUVSBT).EQ.VNREG .OR. WRKBUF(TEUVSBT).EQ.VNINQ .OR.
     *            WRKBUF(TEUVSBT).EQ.VNDON .OR. WRKBUF(TEUVSBT).EQ.VNBNK .OR.
     *            WRKBUF(TEUVSBT).EQ.VNIBO) THEN
C
C OPTION FLAGS
C
                 OPTIONS = 0
                 CALL OGETEUROPT(WRKBUF, OPTIONS)
                 BPRO(BOUTTAB+RETRYIND+0,BUF) = OPTIONS ! #31
                 RETRYIND = RETRYIND + 1
C
C OPTION DATA
C
C                SET UP PLAYER NIF
                 IF(IAND(OPTIONS,'80'X).NE.0) THEN
                   TRABUF(TEUV_NIFFL) = WRKBUF(TEUV_NIFFL)                      !PLAYER NIF PRESENT FLAG
C                
                   I4TEMP = WRKBUF(TEUV_PLNIF)
                   TRABUF(TEUV_PLNIF) = WRKBUF(TEUV_PLNIF)                      !PLAYER NIF
                   BPRO(BOUTTAB+RETRYIND+0,BUF) = I1TEMP(4)
                   BPRO(BOUTTAB+RETRYIND+1,BUF) = I1TEMP(3)
                   BPRO(BOUTTAB+RETRYIND+2,BUF) = I1TEMP(2)
                   BPRO(BOUTTAB+RETRYIND+3,BUF) = I1TEMP(1)
                   RETRYIND = RETRYIND + 4
                 ENDIF
C                SET UP PLAYER NIF CONFIRMATION REQUIRED FLAG
                 IF(IAND(OPTIONS,'40'X).NE.0) THEN
                   TRABUF(TEUV_NIFCF) = WRKBUF(TEUV_NIFCF)                      !PLAYER NIF CONFIRMATION REQUIRED FLAG
                 ENDIF
C                SET UP SoM VALIDATION FLAG
                 IF(IAND(OPTIONS,'20'X).NE.0) THEN
                   TRABUF(TEUV_SHVFL) = WRKBUF(TEUV_SHVFL)                      !SoM VALIDATION FLAG
                 ENDIF
               ENDIF
C----+---+-------------+------------------------------------------------
C V09|END| M16 PROJECT | VALIDATION RETRY: SET UP NEW TRABUF FIELDS
C----+---+-------------+------------------------------------------------
C
             ENDIF

             HPRO(OUTLEN,BUF) = RETRYIND
             I4CCITT   = TRABUF(TCHK)                         
             BPRO(BOUTTAB+2,BUF) = I1CCITT(2)                 
             BPRO(BOUTTAB+3,BUF) = I1CCITT(1)                 
             CALL GETCCITT(BPRO(BOUTTAB,BUF),1,RETRYIND-1,MYCHKSUM)
             I4CCITT   = MYCHKSUM                             
             BPRO(BOUTTAB+2,BUF) = I1CCITT(2)                 
             BPRO(BOUTTAB+3,BUF) = I1CCITT(1)                 
             TRABUF(TSTAT) = GOOD
             GOTO 2000
          ELSE 
            ERRORNUM = 1 
            CALL OUTERRORMES(WRKBUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),ERRORNUM,RETRY)
            GOTO 2000        
          ENDIF
        ENDIF

C
C IF WAGGER SUPPRESSED THEN SEND TO TERM ERROR MESSAGE
C
C        IF ((((P(EUSPWAG) .NE. 0) .OR. (P(SUPWAG) .NE. 0)) .AND. (TRABUF(TEUTYP) .EQ. TWAG)) .OR.
C     *  (((P(EUSPCAN) .NE. 0) .OR. (P(SUPCAN) .NE. 0)) .AND. (TRABUF(TEUTYP) .EQ. TCAN)) .OR.
C     *  (((P(EUSPVAL) .NE. 0) .OR. (P(SUPVAL) .NE. 0)) .AND. (TRABUF(TEUTYP) .EQ. TVAL))) THEN 
        IF (TRABUF(TERR) .EQ. SUPR) THEN
CV09          ERRORNUM = 3 ! FUNCTION SUPPRESS
          ERRORNUM = SUPR                                                       !FUNCTION SUPPRESS
          
          CALL OUTERRORMES(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),ERRORNUM,RETRY) 
          AGTTAB(ALSTRA,TER) = TRABUF(TSER)      
          IF (TRABUF(TEUTYP) .EQ. TCAN) TRABUF(TEUCST) = 5
          IF (TRABUF(TEUTYP) .EQ. TVAL) TRABUF(TEUVST) = 5

          GOTO 2000       
        ENDIF
C----+---+-------------+------------------------------------------------
C V09|BEG| M16 PROJECT | SET MESSAGE ERROR CODE ACCORDINGLY
C----+---+-------------+------------------------------------------------
        IF(TRABUF(TERR) .EQ. SYNT) THEN
          ERRORNUM = SYNT                                                       !SYNTAX ERROR
          CALL OUTERRORMES(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),
     *                     ERRORNUM,RETRY) 
          AGTTAB(ALSTRA,TER) = TRABUF(TSER)      
          IF (TRABUF(TEUTYP) .EQ. TCAN) TRABUF(TEUCST) = 5
          IF (TRABUF(TEUTYP) .EQ. TVAL) TRABUF(TEUVST) = 5
          GOTO 2000
        ENDIF
C
        IF(TRABUF(TERR) .EQ. NOTON) THEN
          ERRORNUM = NOTON                                                      !NOT SIGNED ON
          CALL OUTERRORMES(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),
     *                     ERRORNUM,RETRY) 
          AGTTAB(ALSTRA,TER) = TRABUF(TSER)
          IF (TRABUF(TEUTYP) .EQ. TCAN) TRABUF(TEUCST) = 5
          IF (TRABUF(TEUTYP) .EQ. TVAL) TRABUF(TEUVST) = 5
          GOTO 2000
        ENDIF
C
        IF(TRABUF(TERR) .EQ. GREV) THEN
          ERRORNUM = GREV                                                       !GAME REVISION ERROR
          CALL OUTERRORMES(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),
     *                     ERRORNUM,RETRY) 
          AGTTAB(ALSTRA,TER) = TRABUF(TSER)
          GOTO 2000
        ENDIF
C----+---+-------------+------------------------------------------------
C V09|END| M16 PROJECT | SET MESSAGE ERROR CODE ACCORDINGLY
C----+---+-------------+------------------------------------------------

C
C TIME OUT MESSAGE CREATE OUTPUT MESSAGE TO ALTURA (ERROR MESSAGE)
C
C----+------------------------------------------------------------------
C V08| Bugfix
C----+------------------------------------------------------------------
C        IF ((PRO(REMSTS,BUF) .EQ. RMTMOT) .OR. (TRABUF(TERR) .EQ. BCRS)) THEN
        IF ((HPRO(REMSTS,BUF) .EQ. RMTMOT) .OR. (TRABUF(TERR) .EQ. BCRS)) THEN
CC----+------------------------------------------------------------------
C V08| Bugfix
C----+------------------------------------------------------------------
C
C IF TRANSACTION IS SPECIAL FUNCTION AND NOT A GAME RESULTS AND NOT 
C A ACCOUNTABILITY REPORT THAN RESPONSE TO ALTURA WHAT WE HAVE
C          
          IF (TRABUF(TTYP) .EQ. TSPE) THEN
             MTYPE = ZEXT(BPRO(WRKTAB*4-3+16,BUF))
             SUBTYP = IAND(MTYPE,15)
             MTYPE = ISHFT(MTYPE,-4)
C-------->>V05 ---------------------------------------------------------
C             IF (SUBTYP .NE. 1) GOTO 4321
C             TRABUF(TSDT1) = TEUM
C             TRABUF(TSDT2) = 1
C             TRABUF(TSDT3) = 0              
             IF (SUBTYP .NE. 1 .AND. SUBTYP .NE. 10) GOTO 4321
             IF (SUBTYP .EQ. 1 ) THEN
C----+---+-------------+------------------------------------------------
C V09|BEG| M16 PROJECT | GAME RESULTS: SET UP TRABUF FIELDS (BCRS)
C----+---+-------------+------------------------------------------------
!               TRABUF(TSDT1) = TEUM
!               TRABUF(TSDT2) = 1
!               TRABUF(TSDT3) = 0
               TRABUF(TSDT1) = ZEXT(BPRO(BWRKTAB+19,BUF))                       !GAME TYPE
               TRABUF(TSDT2) = ZEXT(BPRO(BWRKTAB+20,BUF))                       !GAME INDEX
               I1TEMP(4)     = ZEXT(BPRO(BWRKTAB+5,BUF))
               I1TEMP(3)     = ZEXT(BPRO(BWRKTAB+6,BUF))
               I1TEMP(2)     = ZEXT(BPRO(BWRKTAB+7,BUF))
               I1TEMP(1)     = ZEXT(BPRO(BWRKTAB+8,BUF))
               TRABUF(TSDT3) = I4TEMP                                           !MESSAGE QUEUE SEQUENCE NUMBER
C----+---+-------------+------------------------------------------------
C V09|END| M16 PROJECT | GAME RESULTS: SET UP TRABUF FIELDS (BCRS)
C----+---+-------------+------------------------------------------------
             ELSE
               ! CLASS
               I4TEMP = ZEXT (BPRO(WRKTAB*4-3+19,BUF))
               TRABUF(TSDT1) = I4TEMP
               ! SUBCLASS
               I4TEMP = ZEXT (BPRO(WRKTAB*4-3+20,BUF))
               TRABUF(TSDT2) = I4TEMP
               ! ACCOUNTABILITY YEAR
               I4TEMP = ZEXT(BPRO(WRKTAB*4-3+21,BUF))
               TRABUF(TSDT4) = I4TEMP
               ! ACCOUNTABILITY ROUND
               I4TEMP    = 0
               I1TEMP(2) = ZEXT(BPRO(WRKTAB*4-3+22,BUF))
               I1TEMP(1) = ZEXT(BPRO(WRKTAB*4-3+23,BUF))
               TRABUF(TSDT5) = I4TEMP
               ! AGENT NUMBER
               I4TEMP = 0
               I1TEMP(4) = ZEXT (BPRO(WRKTAB*4-3+26,BUF))
               I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+27,BUF))
               I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+28,BUF))
               I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+29,BUF))
               TRABUF(TSDT3) = I4TEMP
               !MESSAGE QUEUE SEQUENCE NUMBER
               I4TEMP    = 0
               I1TEMP(4) = ZEXT (BPRO(WRKTAB*4-3+5,BUF))
               I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+6,BUF))
               I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+7,BUF))
               I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+8,BUF))
               TRABUF(TSDT6) = I4TEMP                                           !MESSAGE QUEUE SEQUENCE NUMBER
             ENDIF
C-------- V05<<---------------------------------------------------------
          ENDIF
             
             
          !!! Create message error
          I1TEMP(4) = ZEXT (BPRO(WRKTAB*4-3+5,BUF))
          I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+6,BUF))
          I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+7,BUF))
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+8,BUF))
          TRABUF(TEUMESSQ) = I4TEMP	
          
          RETRY = ISHFT(ZEXT (BPRO(WRKTAB*4-3+15,BUF)),-4)
          
          IND = 0 
          
C          IF (TRABUF(TEUTYP) .NE. TWAG) IND = 1
C           
C          I1TEMP(4) = 0
C          I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+17+IND,BUF))
C          I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+18+IND,BUF))
C          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+19+IND,BUF))
C          TRABUF(TEUSER) = I4TEMP
C
C          I1TEMP(4) = 0
C          I1TEMP(3) = 0
C          I1TEMP(2) = 0
C          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+20+IND,BUF))
C          TRABUF(TEUCHK) = I4TEMP     
          
          TRABUF(TSTAT) = REJT
          TRABUF(TERR) = BCRS
          
          MESS(2) = TEEUM
          MESS(3) = 3
          MESS(4) = BUF
C----+---+-------------+------------------------------------------------
C V09|BEG| M16 PROJECT | ADD NEW VALUES TO TIMEOUT ERRLOG MESSAGE
C----+---+-------------+------------------------------------------------
          I4TEMP = 0
          I1TEMP(2) = ZEXT (BPRO(BWRKTAB + 13,BUF))
          I1TEMP(1) = ZEXT (BPRO(BWRKTAB + 14,BUF))
          MESS(5) = I4TEMP                                                      !TERMINAL NUMBER
C
          I4TEMP = 0
          I1TEMP(1) = ZEXT (BPRO(BWRKTAB + 16,BUF))
          MESS(6) = I4TEMP                                                      !MESSAGE TYPE/SUBTYPE
C
          MESS(7) = TRABUF(TSER)                                                !TRANSACTION INTERNAL SERIAL NUMBER
C----+---+-------------+------------------------------------------------
C V09|END| M16 PROJECT | ADD NEW VALUES TO TIMEOUT ERRLOG MESSAGE
C----+---+-------------+------------------------------------------------
C
          CALL QUEMES(MESS)
          
          ERRORNUM = 5
          
          CALL OUTERRORMES(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),ERRORNUM,RETRY)
          
C          CALL OPS('valor de da msg 1 ',ZEXT (PRO(OUTTAB,BUF)),ZEXT (PRO(OUTTAB,BUF)))
          
          CALL OPSTXT('TIME OUT MESSAGE') !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
          IF (TRABUF(TEUTYP) .EQ. TCAN) TRABUF(TEUCST) = 5
          IF (TRABUF(TEUTYP) .EQ. TVAL) TRABUF(TEUVST) = 5
          AGTTAB(ALSTRA,TER) = TRABUF(TSER)
          GOTO 2000
        ENDIF 

C
C ERROR WHILE TRY TO PUT MESSAGE INTO MESSAGEQ 
C CREATE OUTPUT MESSAGE TO ALTURA (ERROR MESSAGE)
C       
C----+------------------------------------------------------------------
C V08| Bugfix
C----+------------------------------------------------------------------
C        IF ((PRO(REMSTS,BUF) .EQ. RMDOWN) .OR. (TRABUF(TERR) .EQ. SDOR)) THEN
        IF ((HPRO(REMSTS,BUF) .EQ. RMDOWN) .OR. (TRABUF(TERR) .EQ. SDOR)) THEN
C----+------------------------------------------------------------------
C V08| Bugfix
C----+------------------------------------------------------------------
C
C IF TRANSACTION IS SPECIAL FUNCTION AND NOT A GAME RESULTS AND NOT 
C A ACCOUNTABILITY REPORT THAN RESPONSE TO ALTURA WHAT WE HAVE
C          
          IF (TRABUF(TTYP) .EQ. TSPE) THEN
C-------->>V05 ---------------------------------------------------------
            MTYPE = ZEXT(BPRO(WRKTAB*4-3+16,BUF))
            SUBTYP = IAND(MTYPE,15)
            MTYPE = ISHFT(MTYPE,-4)
C            IF (SUBTYP .NE. 1) GOTO 4321
C            TRABUF(TSDT1) = TEUM
C            TRABUF(TSDT2) = 1
C            TRABUF(TSDT3) = 0
            IF (SUBTYP .NE. 1 .AND. SUBTYP .NE. 10) GOTO 4321
            IF (SUBTYP .EQ. 1) THEN                                             !GAME RESULTS REPORT
C----+---+-------------+------------------------------------------------
C V09|BEG| M16 PROJECT | GAME RESULTS: SET UP TRABUF FIELDS (SDOR)
C----+---+-------------+------------------------------------------------
!              TRABUF(TSDT1) = TEUM
!              TRABUF(TSDT2) = 1
!              TRABUF(TSDT3) = 0
              TRABUF(TSDT1) = ZEXT(BPRO(BWRKTAB+19,BUF))                        !GAME TYPE
              TRABUF(TSDT2) = ZEXT(BPRO(BWRKTAB+20,BUF))                        !GAME INDEX
              I1TEMP(4)     = ZEXT(BPRO(BWRKTAB+5,BUF))
              I1TEMP(3)     = ZEXT(BPRO(BWRKTAB+6,BUF))
              I1TEMP(2)     = ZEXT(BPRO(BWRKTAB+7,BUF))
              I1TEMP(1)     = ZEXT(BPRO(BWRKTAB+8,BUF))
              TRABUF(TSDT3) = I4TEMP                                            !MESSAGE QUEUE SEQUENCE NUMBER
C----+---+-------------+------------------------------------------------
C V09|END| M16 PROJECT | GAME RESULTS: SET UP TRABUF FIELDS (SDOR)
C----+---+-------------+------------------------------------------------
            ELSE                                                                !ACCOUNTABILITY REPORT
              ! CLASS
              I4TEMP = ZEXT (BPRO(WRKTAB*4-3+19,BUF))
              TRABUF(TSDT1) = I4TEMP
              ! SUBCLASS
              I4TEMP = ZEXT (BPRO(WRKTAB*4-3+20,BUF))
              TRABUF(TSDT2) = I4TEMP
              ! ACCOUNTABILITY YEAR
              I4TEMP = ZEXT(BPRO(WRKTAB*4-3+21,BUF))
              TRABUF(TSDT4) = I4TEMP
              ! ACCOUNTABILITY ROUND
              I4TEMP    = 0
              I1TEMP(2) = ZEXT(BPRO(WRKTAB*4-3+22,BUF))
              I1TEMP(1) = ZEXT(BPRO(WRKTAB*4-3+23,BUF))
              TRABUF(TSDT5) = I4TEMP
              ! AGENT NUMBER TO REPORT ON
              I4TEMP = 0
              I1TEMP(4) = ZEXT (BPRO(WRKTAB*4-3+26,BUF))
              I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+27,BUF))
              I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+28,BUF))
              I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+29,BUF))
              TRABUF(TSDT3) = I4TEMP
              !MESSAGE QUEUE SEQUENCE NUMBER
              I4TEMP    = 0
              I1TEMP(4) = ZEXT (BPRO(WRKTAB*4-3+5,BUF))
              I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+6,BUF))
              I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+7,BUF))
              I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+8,BUF))
              TRABUF(TSDT6) = I4TEMP                                            !MESSAGE QUEUE SEQUENCE NUMBER !V09
            ENDIF
C-------- V05<<---------------------------------------------------------
          ENDIF
C
C----+---+-------------+------------------------------------------------
C V10|BEG|             | SET UP MESSAGEQ # IF STATUS IS SDOR
C----+---+-------------+------------------------------------------------
CV10            I1TEMP(4) = ZEXT (BPRO(WRKTAB*4-3+5,BUF))
CV10            I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+6,BUF))
CV10            I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+7,BUF))
CV10            I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+8,BUF))
CV10            TRABUF(TEUMESSQ) = I4TEMP
C
          IF(TRABUF(TSTAT) .EQ. SDOR) THEN
            I1TEMP(4) = ZEXT (BPRO(WRKTAB*4-3+5,BUF))
            I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+6,BUF))
            I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+7,BUF))
            I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+8,BUF))
            TRABUF(TEUMESSQ) = I4TEMP
          ENDIF
C----+---+-------------+------------------------------------------------
C V10|END|             | SET UP MESSAGEQ # IF STATUS IS SDOR
C----+---+-------------+------------------------------------------------
C
          RETRY = ISHFT(ZEXT (BPRO(WRKTAB*4-3+15,BUF)),-4)
          
          TRABUF(TSTAT) = REJT
          TRABUF(TERR) = SDOR
         
          ERRORNUM = 5
          CALL OPSTXT('ERROR IN MESSAGEQ')
          CALL OUTERRORMES(TRABUF,PRO(OUTTAB,BUF),HPRO(OUTLEN,BUF),ERRORNUM,RETRY)
          AGTTAB(ALSTRA,TER) = TRABUF(TSER)
          IF (TRABUF(TEUTYP) .EQ. TCAN) TRABUF(TEUCST) = 5
          IF (TRABUF(TEUTYP) .EQ. TVAL) TRABUF(TEUVST) = 5
          GOTO 2000                 
        ENDIF
        
4321    CONTINUE
C
C DECODE EURO MILHOES - IF TWAG = WAGGER
C
        IF (TRABUF(TEUTYP) .EQ. TWAG) THEN
          MES_LEN = ZEXT (HPRO(OUTLEN,BUF))
C          CALL OPS('TAMANHO DA MENSAGEM',MES_LEN,ZEXT (HPRO(OUTLEN,BUF)))
          MTYPE = ZEXT(BPRO(WRKTAB*4-3+16,BUF))
          MTYPE = ISHFT(MTYPE,-4)

C
C UPDATE SERIAL NUMBER IN AGENT TABLE
C          
          IF (MTYPE .EQ. 0) THEN
          	AGTTAB(ALSTRA,TER) = TRABUF(TSER)  
C                AGTTAB(ALSWAG,TER) = TRABUF(TSER)
          ENDIF
C
C  DECODE MESSAGE SEQ # AND SET UP TRABUF(TEUMESSQ)
C                    
          I1TEMP(4) = ZEXT (BPRO(WRKTAB*4-3+5,BUF))
          I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+6,BUF))
          I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+7,BUF))
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+8,BUF))
          TRABUF(TEUMESSQ) = I4TEMP
C          CALL OPS('VALOR DO MESSAGEQ SEQ #',TRABUF(TEUMESSQ),I4TEMP)
CV10          I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+17,BUF))
CV10          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+18,BUF))
C          CALL OPS('CHECKSUM DO RTS',I4TEMP,I4TEMP)
          IF (MTYPE .EQ. 9) THEN
             TRABUF(TSTAT) = REJT
             TRABUF(TERR) = INVL
             GOTO 100
          ENDIF
C
C  DECODE SERIAL NUMBER AND SET UP TRABUF(TEUSER)
C                    
          I1TEMP(4) = 0
          I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+21,BUF))
          I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+22,BUF))
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+23,BUF))
          TRABUF(TEUSER) = I4TEMP
C          CALL OPS('VALOR DO SERIAL NUMBER',TRABUF(TEUSER),I4TEMP)

C
C  DECODE CHECK DIGITS AND SET UP TRABUF(TEUCHK)
C                    
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+24,BUF))
          TRABUF(TEUCHK) = I4TEMP         
C          CALL OPS('VALOR DO CHECK DIGITS',TRABUF(TEUCHK),I4TEMP)
C
C DECODE OFFSETS (FIRST AND SECOND)
C
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+25,BUF))
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+26,BUF))
          TRABUF(TEUWOFS1) = I4TEMP         

          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+27,BUF))
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+28,BUF))
          TRABUF(TEUWOFS2) = I4TEMP         
          
C
C  DECODE FIRST WEEK DRAW DATE 
C
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+29,BUF))
          TRABUF(TEUWBEGW) = I4TEMP
C
C  DECODE FIRST YEAR DRAW DATE 
C
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+30,BUF))
          TRABUF(TEUWBEGY) = I4TEMP
C
C  DECODE SECOND WEEK DRAW DATE
C
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+31,BUF))
          TRABUF(TEUWENDW) = I4TEMP
C
C  DECODE SECOND YEAR DRAW DATE
C          
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+32,BUF))
          TRABUF(TEUWENDY) = I4TEMP
C
C  DECODE TIME: HOUR
C          
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+33,BUF))
          TRABUF(TEUWTIMEH) = I4TEMP
C
C  DECODE TIME: MIN
C                    
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+34,BUF))
          TRABUF(TEUWTIMEM) = I4TEMP
C
C  DECODE TIME: SEC
C          
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+35,BUF))
          TRABUF(TEUWTIMES) = I4TEMP
C----+---+-------------+------------------------------------------------
C V09|BEG| M16 PROJECT | WAGER: SET UP NEW TRABUF FIELDS
C----+---+-------------+------------------------------------------------
C
C  DECODE OPTION FLAGS
C
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = BPRO(BWRKTAB+36,BUF)
          OPTIONS   = I4TEMP
          ODATIND   = 36                                                        !SAVE OPTION DATA INDEX
C
C   DECODE OPTION DATA
C
C         CHECK SM DATA PRESENT FLAG
          IF(IAND(OPTIONS,'08'X).NE.0) THEN
C           SET UP TRABUF(TEUW_SMWFL)
            TRABUF(TEUW_SMWFL) = 1                                              !SM WAGER DATA IS PRESENT
C           DECODE SM TOTAL BETS AND SET UP TRABUF(TEUW_SMWTB)
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = BPRO(BWRKTAB+ODATIND+1,BUF)
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+2,BUF)
            TRABUF(TEUW_SMWTB) = I4TEMP                                         !SM TOTAL BETS
C           DECODE SM EXTERNAL SERIAL NUMBER AND SET UP TRABUF(TEUW_SMWSN)
            I1TEMP(4) = 0
            I1TEMP(3) = BPRO(BWRKTAB+ODATIND+3,BUF)
            I1TEMP(2) = BPRO(BWRKTAB+ODATIND+4,BUF)
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+5,BUF)
            TRABUF(TEUW_SMWSN) = I4TEMP                                         !SM EXTERNAL SERIAL NUMBER
C           DECODE SM CHECK DIGITS AND SET UP TRABUF(TEUW_SMWCD)
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = 0
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+6,BUF)
            TRABUF(TEUW_SMWCD) = I4TEMP                                         !SM CHECK DIGITS
C           DECODE SM DRAW NUMBER AND SET UP TRABUF(TEUW_SMWDN)
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = 0
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+7,BUF)
            TRABUF(TEUW_SMWDN) = I4TEMP                                         !SM DRAW NUMBER
C           DECODE SM DRAW YEAR (LAST TWO DIGITS) AND SET UP TRABUF(TEUW_SMWDY)
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = 0
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+8,BUF)
            TRABUF(TEUW_SMWDY) = I4TEMP                                         !SM DRAW YEAR
C           DECODE OFFSET TO SM DRAW CDC DATE AND SET UP TRABUF(TEUW_SMWOF)
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = 0
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+9,BUF)
            TRABUF(TEUW_SMWOF) = I4TEMP                                         !OFFSET TO SM DRAW CDC DATE
C           DECODE SM FIRST NUMBER AND SET UP TRABUF(TEUW_SMWB1) AND TRABUF(TEUW_SMWB2)
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+10,BUF)
            I1TEMP(2) = BPRO(BWRKTAB+ODATIND+11,BUF)
            I1TEMP(3) = BPRO(BWRKTAB+ODATIND+12,BUF)
            I1TEMP(4) = BPRO(BWRKTAB+ODATIND+13,BUF)
            TRABUF(TEUW_SMWB1) = I4TEMP                                         !SM WAGER FIRST NUMBER
            I1TEMP(4) = 0
            I1TEMP(3) = BPRO(BWRKTAB+ODATIND+14,BUF)
            I1TEMP(2) = BPRO(BWRKTAB+ODATIND+15,BUF)
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+16,BUF)
            TRABUF(TEUW_SMWB2) = I4TEMP
C           DECODE SM LAST NUMBER AND SET UP TRABUF(TEUW_SMWE1) AND TRABUF(TEUW_SMWE2)
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+17,BUF)
            I1TEMP(2) = BPRO(BWRKTAB+ODATIND+18,BUF)
            I1TEMP(3) = BPRO(BWRKTAB+ODATIND+19,BUF)
            I1TEMP(4) = BPRO(BWRKTAB+ODATIND+20,BUF)
            TRABUF(TEUW_SMWE1) = I4TEMP                                         !SM WAGER LAST NUMBER
            I1TEMP(4) = 0
            I1TEMP(3) = BPRO(BWRKTAB+ODATIND+21,BUF)
            I1TEMP(2) = BPRO(BWRKTAB+ODATIND+22,BUF)
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+23,BUF)
            TRABUF(TEUW_SMWE2) = I4TEMP
            ODATIND = ODATIND + 23                                              !UPDATE OPTION DATA INDEX
          ENDIF
C         CHECK SoM DATA PRESENT FLAG
          IF(IAND(OPTIONS,'04'X).NE.0) THEN
C           SET UP TRABUF(TEUW_SHWFL)
            TRABUF(TEUW_SHWFL) = 1                                              !SoM WAGER DATA IS PRESENT
C           DECODE SoM TOTAL BETS AND SET UP TRABUF(TEUW_SHWTB)
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = BPRO(BWRKTAB+ODATIND+1,BUF)
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+2,BUF)
            TRABUF(TEUW_SHWTB) = I4TEMP                                         !SoM TOTAL BETS
C           DECODE SoM DRAW NUMBER AND SET UP TRABUF(TEUW_SHWDN)
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = 0
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+3,BUF)
            TRABUF(TEUW_SHWDN) = I4TEMP                                         !SoM DRAW NUMBER
C           DECODE SoM DRAW YEAR (LAST TWO DIGITS) AND SET UP TRABUF(TEUW_SHWDY)
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = 0
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+4,BUF)
            TRABUF(TEUW_SHWDY) = I4TEMP                                         !SoM DRAW YEAR
C           DECODE OFFSET TO SoM DRAW CDC DATE AND SET UP TRABUF(TEUW_SHWOF)
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = 0
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+5,BUF)
            TRABUF(TEUW_SHWOF) = I4TEMP                                         !OFFSET TO SoM DRAW CDC DATE
C           DECODE SoM FIRST NUMBER AND SET UP TRABUF(TEUW_SHWB1) AND TRABUF(TEUW_SHWB2)
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+6,BUF)
            I1TEMP(2) = BPRO(BWRKTAB+ODATIND+7,BUF)
            I1TEMP(3) = BPRO(BWRKTAB+ODATIND+8,BUF)
            I1TEMP(4) = BPRO(BWRKTAB+ODATIND+9,BUF)
            TRABUF(TEUW_SHWB1) = I4TEMP                                         !SoM WAGER FIRST NUMBER
            I1TEMP(4) = 0
            I1TEMP(3) = BPRO(BWRKTAB+ODATIND+10,BUF)
            I1TEMP(2) = BPRO(BWRKTAB+ODATIND+11,BUF)
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+12,BUF)
            TRABUF(TEUW_SHWB2) = I4TEMP
C           DECODE SoM LAST NUMBER AND SET UP TRABUF(TEUW_SHWE1) AND TRABUF(TEUW_SHWE2)
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+13,BUF)
            I1TEMP(2) = BPRO(BWRKTAB+ODATIND+14,BUF)
            I1TEMP(3) = BPRO(BWRKTAB+ODATIND+15,BUF)
            I1TEMP(4) = BPRO(BWRKTAB+ODATIND+16,BUF)
            TRABUF(TEUW_SHWE1) = I4TEMP                                         !SoM WAGER LAST NUMBER
            I1TEMP(4) = 0
            I1TEMP(3) = BPRO(BWRKTAB+ODATIND+17,BUF)
            I1TEMP(2) = BPRO(BWRKTAB+ODATIND+18,BUF)
            I1TEMP(1) = BPRO(BWRKTAB+ODATIND+19,BUF)
            TRABUF(TEUW_SHWE2) = I4TEMP
            ODATIND = ODATIND + 19                                              !UPDATE OPTION DATA INDEX
          ENDIF
C----+---+-------------+------------------------------------------------
C V09|END| M16 PROJECT | WAGER: SET UP NEW TRABUF FIELDS
C----+---+-------------+------------------------------------------------
C
C  CREATE OUTPUT MESSAGE...
C
C----+------------------------------------------------------------------
C V07| Retry Fix
C----+------------------------------------------------------------------
C          IF (TRABUF(STATUS) .EQ. GOOD) AGTTAB(ALSWAG,TER) = TRABUF(TSER)
          IF (TRABUF(TSTAT) .EQ. GOOD) AGTTAB(ALSWAG,TER) = TRABUF(TSER)
C----+------------------------------------------------------------------
C V07| Retry Fix
C----+------------------------------------------------------------------
100       CONTINUE
          IND = 0
          DO I=15, MES_LEN
C          DO I=15, 36
             BPRO(BOUTTAB+I-15,BUF) = ZEXT (BPRO(WRKTAB*4-3+I,BUF))
             IND = IND + 1
          ENDDO
          HPRO(OUTLEN,BUF) = IND 

C Ricardo           
C           CALL OPS('VALOR DE BYTES: ',IND,0)
C           I4CCITT   = TRABUF(TCHK)
C           BPRO(BOUTTAB+2,BUF) = I1CCITT(2)
C           BPRO(BOUTTAB+3,BUF) = I1CCITT(1)
C           CALL GETCCITT(BPRO(BOUTTAB,BUF),1,IND-1,MYCHKSUM)
C           I4CCITT   = MYCHKSUM
C           BPRO(BOUTTAB+2,BUF) = I1CCITT(2)
C           BPRO(BOUTTAB+3,BUF) = I1CCITT(1)
C           CALL OPS('MYCHKSUM',MYCHKSUM,MYCHKSUM)
C           CALL OPS('TAMANHO DA MNS',IND-1,IND)
C           CALL OPS('TRABUF(TCHK)',TRABUF(TCHK),TRABUF(TCHK))
C
CC          DO I=0, IND
C          DO I=15, 36
C             TYPE *,'VALOR DA MENSAGEM ',I,' ', BPRO(BOUTTAB+I,BUF)
C             CALL OPS('Byte->', I, BPRO(BOUTTAB+I,BUF))
C          ENDDO
C Ricardo           
        
C
C DECODE EURO MILHOES - IF TCAN = CANCELATION
C        
        ELSE IF (TRABUF(TEUTYP) .EQ. TCAN) THEN
          MES_LEN = ZEXT (HPRO(OUTLEN,BUF))
C          CALL OPS('TAMANHO DA MENSAGEM',MES_LEN,ZEXT (HPRO(OUTLEN,BUF)))
          MTYPE = ZEXT(BPRO(WRKTAB*4-3+16,BUF))
          MTYPE = ISHFT(MTYPE,-4)

C
C UPDATE SERIAL NUMBER IN AGENT TABLE
C          
          IF ((MTYPE .EQ. 2)) THEN
             AGTTAB(ALSTRA,TER) = TRABUF(TSER)
C             AGTTAB(ALSCAN,TER) = TRABUF(TSER)
          ENDIF
C
C  DECODE MESSAGE SEQ # AND SET UP TRABUF(TEUMESSQ)
C 
          I1TEMP(4) = ZEXT (BPRO(WRKTAB*4-3+5,BUF))
          I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+6,BUF))
          I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+7,BUF))
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+8,BUF))
          TRABUF(TEUMESSQ) = I4TEMP
C          CALL OPS('VALOR DO MESSAGEQ SEQ #',TRABUF(TEUMESSQ),I4TEMP)
          IF (MTYPE .EQ. 9) THEN
             TRABUF(TSTAT) = REJT
             TRABUF(TERR) = INVL
             GOTO 101
          ENDIF          
C
C  DECODE SERIAL NUMBER AND SET UP TRABUF(TEUSER)
C                    
          I1TEMP(4) = 0
          I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+22,BUF))
          I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+23,BUF))
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+24,BUF))
          TRABUF(TEUSER) = I4TEMP
C          CALL OPS('VALOR DO SERIAL NUMBER',TRABUF(TEUSER),I4TEMP)

C
C  DECODE CHECK DIGITS AND SET UP TRABUF(TEUCHK)
C                    
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+25,BUF))
          TRABUF(TEUCHK) = I4TEMP          
C          CALL OPS('VALOR DO CHECK DIGITS',TRABUF(TEUCHK),I4TEMP)

C
C DECODE TIME: HOUR
C
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+19,BUF))
          TRABUF(TEUCTIMEH) = I4TEMP          
C
C DECODE TIME: MIN 
C
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+20,BUF))
          TRABUF(TEUCTIMEM) = I4TEMP
C
C DECODE TIME: SEC
C
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+21,BUF))
          TRABUF(TEUCTIMES) = I4TEMP      
C----+---+-------------+------------------------------------------------
C V09|BEG| M16 PROJECT | CANCELLATION: SET UP OF GAME TYPE AND GAME 
C        |             |               INDEX CHANGED FROM INMGR TO HERE
C----+---+-------------+------------------------------------------------
C
C DECODE GAME TYPE
C
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = BPRO(BWRKTAB+26,BUF)
          TRABUF(TGAMTYP) = I4TEMP
C          CALL OPS('VALOR DO GAME TYPE',TRABUF(TGAMTYP),I4TEMP)
C
C DECODE GAME INDEX
C
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = BPRO(BWRKTAB+27,BUF)
          TRABUF(TGAMIND) = I4TEMP
C          CALL OPS('VALOR DO GAME INDEX',TRABUF(TGAMIND),I4TEMP)
C----+---+-------------+------------------------------------------------
C V09|END| M16 PROJECT | CANCELLATION: SET UP OF GAME TYPE AND GAME
C        |             |               INDEX CHANGED FROM INMGR TO HERE
C----+---+-------------+------------------------------------------------
C
C DECODE CANCEL STATUS
C
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+28,BUF))
          TRABUF(TEUCST) = I4TEMP  
                    
          IF (TRABUF(TEUCST) .EQ. 0) THEN  !IF STATUS = 0 THEN CAN CANCEL WAGGER
C
C DECODE CANCEL AMOUNT
C
             I1TEMP(4) = ZEXT (BPRO(WRKTAB*4-3+29,BUF))
             I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+30,BUF))
             I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+31,BUF))
             I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+32,BUF))
             TRABUF(TEUCAM) = I4TEMP
C----+---+-------------+------------------------------------------------
C V09|BEG| M16 PROJECT | CANCELLATION: SET UP NEW TRABUF FIELDS
C----+---+-------------+------------------------------------------------
C
C DECODE MONDAY DRAW INDICATOR
C
C     SKIP MONDAY DRAW INDICATOR DATA (1 BYTE)
C
C DECODE OPTION FLAGS
C
             I1TEMP(4) = 0
             I1TEMP(3) = 0
             I1TEMP(2) = 0
             I1TEMP(1) = BPRO(BWRKTAB+34,BUF)
             OPTIONS   = I4TEMP
             ODATIND   = 34                                                     !SAVE OPTION DATA INDEX
C
C DECODE OPTION DATA
C
C            CHECK SM DATA PRESENT FLAG
             IF(IAND(OPTIONS,'80'X).NE.0) THEN
C              SET UP TRABUF(TEUC_SMCFL)
               TRABUF(TEUC_SMCFL) = 1                                           !SM CANCELLATION DATA IS PRESENT
C              DECODE SM WAGER EXTERNAL SERIAL NUMBER AND SET UP TRABUF(TEUC_SMWSN)
               I1TEMP(4) = 0
               I1TEMP(3) = BPRO(BWRKTAB+ODATIND+1,BUF)
               I1TEMP(2) = BPRO(BWRKTAB+ODATIND+2,BUF)
               I1TEMP(1) = BPRO(BWRKTAB+ODATIND+3,BUF)
               TRABUF(TEUC_SMWSN) = I4TEMP                                      !SM WAGER EXTERNAL SERIAL NUMBER
C              DECODE SM WAGER CHECK DIGITS AND SET UP TRABUF(TEUC_SMWCD)
               I1TEMP(4) = 0
               I1TEMP(3) = 0
               I1TEMP(2) = 0
               I1TEMP(1) = BPRO(BWRKTAB+ODATIND+4,BUF)
               TRABUF(TEUC_SMWCD) = I4TEMP                                      !SM WAGER CHECK DIGITS
C              DECODE SM CANCELLATION EXTERNAL SERIAL NUMBER AND SET UP TRABUF(TEUC_SMCSN)
               I1TEMP(4) = 0
               I1TEMP(3) = BPRO(BWRKTAB+ODATIND+5,BUF)
               I1TEMP(2) = BPRO(BWRKTAB+ODATIND+6,BUF)
               I1TEMP(1) = BPRO(BWRKTAB+ODATIND+7,BUF)
               TRABUF(TEUC_SMCSN) = I4TEMP                                      !SM CANCELLATION EXTERNAL SERIAL NUMBER
C              DECODE SM CANCELLATION CHECK DIGITS AND SET UP TRABUF(TEUC_SMCCD)
               I1TEMP(4) = 0
               I1TEMP(3) = 0
               I1TEMP(2) = 0
               I1TEMP(1) = BPRO(BWRKTAB+ODATIND+8,BUF)
               TRABUF(TEUC_SMCCD) = I4TEMP                                      !SM CANCELLATION CHECK DIGITS
C              DECODE SM CANCEL AMOUNT (WAGER UNITS) AND SET UP TRABUF(TEUC_SMWCA)
               I1TEMP(4) = BPRO(BWRKTAB+ODATIND+ 9,BUF)
               I1TEMP(3) = BPRO(BWRKTAB+ODATIND+10,BUF)
               I1TEMP(2) = BPRO(BWRKTAB+ODATIND+11,BUF)
               I1TEMP(1) = BPRO(BWRKTAB+ODATIND+12,BUF)
               TRABUF(TEUC_SMWCA) = I4TEMP                                      !SM CANCEL AMOUNT (WAGER UNITS)
               ODATIND = ODATIND + 12
             ENDIF
C            CHECK SoM CANCELLATION FLAG
             IF(IAND(OPTIONS,'40'X).NE.0) THEN
C              SET UP TRABUF(TEUC_SHCFL)
               TRABUF(TEUC_SHCFL) = 1                                           !SoM CANCELLATION FLAG
             ENDIF
C----+---+-------------+------------------------------------------------
C V09|END| M16 PROJECT | CANCELLATION: SET UP NEW TRABUF FIELDS
C----+---+-------------+------------------------------------------------
          ENDIF
C
C  CREATE OUTPUT MESSAGE...
C
          IF(TRABUF(TEUCST) .EQ. 0 .AND. TRABUF(TEUCAM) .GT. 0 ) AGTTAB(ALSCAN,TER) = TRABUF(TSER)

101       CONTINUE
          IND = 0
          DO I=15, MES_LEN
             BPRO(BOUTTAB+I-15,BUF) = ZEXT (BPRO(WRKTAB*4-3+I,BUF))
             IND = IND + 1
          ENDDO
          HPRO(OUTLEN,BUF) = IND           
C
C DECODE EURO MILHOES - IF TVAL = VALIDATION
C        
        ELSE IF (TRABUF(TEUTYP) .EQ. TVAL) THEN
          MES_LEN = ZEXT (HPRO(OUTLEN,BUF))
c          CALL OPS('TAMANHO DA MENSAGEM',MES_LEN,ZEXT (HPRO(OUTLEN,BUF)))
          MTYPE = ZEXT(BPRO(WRKTAB*4-3+16,BUF))
          MTYPE = ISHFT(MTYPE,-4)
C
C GET VALIDATION SUBTYPE
C
C           TEMP1=ZEXT(BPRO(BINPTAB+1,BUF))
          TRABUF(TEUVSBT)=IAND(ZEXT(BPRO(WRKTAB*4-3+16,BUF)),'0F'X)
C
C UPDATE SERIAL NUMBER IN AGENT TABLE
C          
          IF (MTYPE .EQ. 1) THEN
             AGTTAB(ALSTRA,TER) = TRABUF(TSER)
C             AGTTAB(ALSVAL,TER) = TRABUF(TSER)    
          ENDIF
C
C  DECODE MESSAGE SEQ # AND SET UP TRABUF(TEUMESSQ)
C 
          I1TEMP(4) = ZEXT (BPRO(WRKTAB*4-3+5,BUF))
          I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+6,BUF))
          I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+7,BUF))
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+8,BUF))
          TRABUF(TEUMESSQ) = I4TEMP
C          CALL OPS('VALOR DO MESSAGEQ SEQ #',TRABUF(TEUMESSQ),I4TEMP)
          IF (MTYPE .EQ. 9) THEN
             TRABUF(TSTAT) = REJT
             TRABUF(TERR) = INVL
             GOTO 102
          ENDIF                    
C
C  DECODE SERIAL NUMBER AND SET UP TRABUF(TEUSER)
C                    
          I1TEMP(4) = 0
          I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+22,BUF))
          I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+23,BUF))
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+24,BUF))
          TRABUF(TEUSER) = I4TEMP
C          CALL OPS('VALOR DO SERIAL NUMBER',TRABUF(TEUSER),I4TEMP)

C
C  DECODE CHECK DIGITS AND SET UP TRABUF(TEUCHK)
C                    
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+25,BUF))
          TRABUF(TEUCHK) = I4TEMP          
C          CALL OPS('VALOR DO CHECK DIGITS',TRABUF(TEUCHK),I4TEMP)
C----+---+-------------+------------------------------------------------
C V09|BEG| M16 PROJECT | VALIDATION: SET UP OF GAME TYPE AND GAME INDEX
C        |             |             CHANGED FROM INMGR TO HERE
C----+---+-------------+------------------------------------------------
C
C  DECODE GAME TYPE
C
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = BPRO(BWRKTAB+26,BUF)
          TRABUF(TGAMTYP) = I4TEMP          
C          CALL OPS('VALOR DO GAME TYPE',TRABUF(TGAMTYP),I4TEMP)
C
C  DECODE GAME INDEX
C
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = BPRO(BWRKTAB+27,BUF)
          TRABUF(TGAMIND) = I4TEMP          
C          CALL OPS('VALOR DO GAME INDEX',TRABUF(TGAMIND),I4TEMP)
C----+---+-------------+------------------------------------------------
C V09|BEG| M16 PROJECT | VALIDATION: SET UP OF GAME TYPE AND GAME INDEX
C        |             |             CHANGED FROM INMGR TO HERE
C----+---+-------------+------------------------------------------------
C           
C GET VALIDATION SUBTYPE
C
          I1TEMP(4)=ZEXT(BPRO(WRKTAB*4-3+16,BUF))
          TRABUF(TEUVSBT)=IAND(I1TEMP(4),'0F'X)
           
C
C DECODE TIME: HOUR
C
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+19,BUF))
          TRABUF(TEUVTIMEH) = I4TEMP          
C
C DECODE TIME: MIN 
C
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+20,BUF))
          TRABUF(TEUVTIMEM) = I4TEMP
C
C DECODE TIME: SEC
C
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+21,BUF))
          TRABUF(TEUVTIMES) = I4TEMP
C
C DECODE VALIDATION STATUS
C
          I1TEMP(4) = 0
          I1TEMP(3) = 0
          I1TEMP(2) = 0
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+28,BUF))
          TRABUF(TEUVST) = I4TEMP
          
C
C DECODE VALIDATION CASH AMOUNT
C 
          I1TEMP(4) = ZEXT (BPRO(WRKTAB*4-3+29,BUF))
          I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+30,BUF))
          I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+31,BUF))
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+32,BUF))
          TRABUF(TEUVCAM) = I4TEMP

C
C DECODE VALIDATION REFUND AMOUNT
C 
          I1TEMP(4) = ZEXT (BPRO(WRKTAB*4-3+33,BUF))
          I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+34,BUF))
          I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+35,BUF))
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+36,BUF))
          TRABUF(TEUVRAM) = I4TEMP

! V02 EUROMILLIONS VALIDATION PRIZE OVER 42M ...

C
C DECODE VALIDATION CASH AMOUNT HIGH HALF WORD
C 
          I1TEMP(4) = ZEXT (BPRO(WRKTAB*4-3+37,BUF))
          I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+38,BUF))
          I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+39,BUF))
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+40,BUF))
          TRABUF(TEUVCAMH) = I4TEMP

C
C DECODE VALIDATION REFUND AMOUNT HIGH HALF WORD
C 
          I1TEMP(4) = ZEXT (BPRO(WRKTAB*4-3+41,BUF))
          I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+42,BUF))
          I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+43,BUF))
          I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+44,BUF))
          TRABUF(TEUVRAMH) = I4TEMP

! ... V02 EUROMILLIONS VALIDATION PRIZE OVER 42M
C----+---+-------------+------------------------------------------------
C V09|BEG| M16 PROJECT | VALIDATION: SET UP NEW TRABUF FIELDS
C----+---+-------------+------------------------------------------------
C
C DECODE OPTION FLAGS
C
          IF(TRABUF(TEUVSBT).EQ.VNREG .OR. TRABUF(TEUVSBT).EQ.VNINQ .OR.
     *       TRABUF(TEUVSBT).EQ.VNDON .OR. TRABUF(TEUVSBT).EQ.VNBNK .OR.
     *       TRABUF(TEUVSBT).EQ.VNIBO) THEN
            I1TEMP(4) = 0
            I1TEMP(3) = 0
            I1TEMP(2) = 0
            I1TEMP(1) = BPRO(BWRKTAB+45,BUF)
            OPTIONS   = I4TEMP
            ODATIND   = 45                                                      !SAVE OPTION DATA INDEX
C
C DECODE OPTION DATA
C
C           CHECK PLAYER NIF PRESENT FLAG
            IF(IAND(OPTIONS,'80'X).NE.0) THEN
C             SET UP TRABUF(TEUV_NIFFL)
              TRABUF(TEUV_NIFFL) = 1                                            !PLAYER NIF IS PRESENT
C             DECODE PLAYER NIF AND SET UP TRABUF(TEUV_PLNIF)
              I1TEMP(4) = BPRO(BWRKTAB+ODATIND+1,BUF)
              I1TEMP(3) = BPRO(BWRKTAB+ODATIND+2,BUF)
              I1TEMP(2) = BPRO(BWRKTAB+ODATIND+3,BUF)
              I1TEMP(1) = BPRO(BWRKTAB+ODATIND+4,BUF)
              TRABUF(TEUV_PLNIF) = I4TEMP                                       !PLAYER NIF
              ODATIND   = ODATIND + 4
            ENDIF
C            CHECK PLAYER NIF CONFIRMATION REQUIRED FLAG
            IF(IAND(OPTIONS,'40'X).NE.0) THEN
C             SET UP TRABUF(TEUV_NIFCF)
              TRABUF(TEUV_NIFCF) = 1                                            !PLAYER NIF CONFIRMATION IS REQUIRED
            ENDIF
C           CHECK SoM VALIDATION FLAG
            IF(IAND(OPTIONS,'20'X).NE.0) THEN
C             SET UP TRABUF(TEUV_SHVFL)
              TRABUF(TEUV_SHVFL) = 1                                            !SoM WAS VALIDATED ALONG EM
            ENDIF
          ENDIF
C----+---+-------------+------------------------------------------------
C V09|END| M16 PROJECT | VALIDATION: SET UP NEW TRABUF FIELDS
C----+---+-------------+------------------------------------------------
C
C IF IS ONE VALIDATION WITH EXCANGE TICKET THEN SAVE WAGGER 
C
          IF ((TRABUF(TEUVST) .EQ. 11) .AND. (TRABUF(TEUVSBT) .EQ. 0)) THEN
C
C  DECODE SERIAL NUMBER AND SET UP TRABUF(TEUSER)
C                    
             I1TEMP(4) = 0
             I1TEMP(3) = ZEXT (BPRO(WRKTAB*4-3+39,BUF))
             I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+40,BUF))
             I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+41,BUF))
             TRABUF(TEUEVWSER) = I4TEMP
C          CALL OPS('VALOR DO SERIAL NUMBER',TRABUF(TEUSER),I4TEMP)

C
C  DECODE CHECK DIGITS AND SET UP TRABUF(TEUCHK)
C                    
             I1TEMP(4) = 0
             I1TEMP(3) = 0
             I1TEMP(2) = 0
             I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+42,BUF))
             TRABUF(TEUEVWCKD) = I4TEMP         
C          CALL OPS('VALOR DO CHECK DIGITS',TRABUF(TEUCHK),I4TEMP)
C
C DECODE OFFSETS (FIRST AND SECOND)
C
             I1TEMP(4) = 0
             I1TEMP(3) = 0
             I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+43,BUF))
             I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+44,BUF))
             TRABUF(TEUVEOFS1) = I4TEMP         

             I1TEMP(4) = 0
             I1TEMP(3) = 0
             I1TEMP(2) = ZEXT (BPRO(WRKTAB*4-3+45,BUF))
             I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+46,BUF))
             TRABUF(TEUVEOFS2) = I4TEMP         
          
C
C  DECODE FIRST WEEK DRAW DATE 
C
             I1TEMP(4) = 0
             I1TEMP(3) = 0
             I1TEMP(2) = 0
             I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+47,BUF))
             TRABUF(TEUVEBEGW) = I4TEMP
C
C  DECODE FIRST YEAR DRAW DATE 
C
             I1TEMP(4) = 0
             I1TEMP(3) = 0
             I1TEMP(2) = 0
             I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+48,BUF))
             TRABUF(TEUVEBEGY) = I4TEMP
C
C  DECODE SECOND WEEK DRAW DATE
C
             I1TEMP(4) = 0
             I1TEMP(3) = 0
             I1TEMP(2) = 0
             I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+49,BUF))
             TRABUF(TEUVEENDW) = I4TEMP
C
C  DECODE SECOND YEAR DRAW DATE
C          
             I1TEMP(4) = 0
             I1TEMP(3) = 0
             I1TEMP(2) = 0
             I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+50,BUF))
             TRABUF(TEUVEENDY) = I4TEMP 
C
C  DECODE TIME: HOUR
C          
             I1TEMP(4) = 0
             I1TEMP(3) = 0
             I1TEMP(2) = 0
             I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+51,BUF))
             TRABUF(TEUVETIMEH) = I4TEMP
C
C  DECODE TIME: MIN
C                    
             I1TEMP(4) = 0
             I1TEMP(3) = 0
             I1TEMP(2) = 0
             I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+52,BUF))
             TRABUF(TEUVETIMEM) = I4TEMP
C
C  DECODE TIME: SEC
C          
             I1TEMP(4) = 0
             I1TEMP(3) = 0
             I1TEMP(2) = 0
             I1TEMP(1) = ZEXT (BPRO(WRKTAB*4-3+53,BUF))
             TRABUF(TEUVETIMES) = I4TEMP
C
C GET DURATION AND NUMBER OF BOARDS
C           
             I4TEMP = ZEXT(BPRO(WRKTAB*4-3+56,BUF))
             TRABUF(TEUVENBET) = IAND(I4TEMP,15)
             TRABUF(TEUVEDUR)  = ISHFT(I4TEMP,-4)
C           CALL OPS('NBET E DUR',TRABUF(TEUWNBET),TRABUF(TEUWDUR))
C
C GET OPTIONS FIELD
C
           TEMP1 = ZEXT(BPRO(WRKTAB*4-3+57,BUF))
           TEMP2 = ZEXT(BPRO(WRKTAB*4-3+58,BUF))
           OPTIONS = ISHFT(TEMP1,8) + TEMP2
           IND=59           
C
C CHECK OPTIONS
C
           IF(IAND(OPTIONS,'8000'X) .NE. 0) THEN
           	IND = IND + 2
           ENDIF
           IF(IAND(OPTIONS,'4000'X) .NE. 0) THEN
           	IND = IND + 2
           ENDIF
           IF(IAND(OPTIONS,'2000'X) .NE. 0) THEN
           	IND = IND + 2
           ENDIF
           IF(IAND(OPTIONS,'0800'X) .NE. 0) THEN
           	IND = IND + 2
           ENDIF
           IF(IAND(OPTIONS,'0400'X) .NE. 0) THEN
           	IND = IND + 2
           ENDIF

C
C CHECK FOR QP IF TRUE THEN GET QUICK PICK FLAGS
C
           IF(IAND(OPTIONS,'0200'X) .NE. 0) THEN
             TEMP1 = ZEXT(BPRO(WRKTAB*4-3+IND+0,BUF))
             TEMP2 = ZEXT(BPRO(WRKTAB*4-3+IND+1,BUF))
             TRABUF(TEUVEQP) = ISHFT(TEMP1,8) + TEMP2
             IND=IND+2
C             CALL OPS('QUICK PICK FLAGS',TRABUF(TEUWQP),TRABUF(TEUWQP))
           ENDIF
C
C CHECK FOR SYSTEM BET FLAG IF TRUE GET SYSTEM NUMBER (2 bytes)
C
           IF(IAND(OPTIONS,'0100'X) .NE. 0) THEN
             TEMP1 = ZEXT(BPRO(WRKTAB*4-3+IND+0,BUF))
             TEMP2 = ZEXT(BPRO(WRKTAB*4-3+IND+1,BUF))
             TRABUF(TEUVENMK) = TEMP1
             TRABUF(TEUVENST) = TEMP2
             IND=IND+2
C             CALL OPS('SYSTEM NUMBER MARK E STARS',TRABUF(TEUVENMK),TRABUF(TEUVENST))
           ENDIF          
C
C CHECK OPTIONS
C
           IF(IAND(OPTIONS,'0008'X) .NE. 0) THEN
           	IND = IND + 8
           ENDIF
           IF(IAND(OPTIONS,'0004'X) .NE. 0) THEN
           	IND = IND + 1
           ENDIF
           IF(IAND(OPTIONS,'0002'X) .NE. 0) THEN
           	IND = IND + 4
           ENDIF
           IF(IAND(OPTIONS,'0001'X) .NE. 0) THEN
           	IND = IND + 4
           ENDIF
C
C
C
           IND = IND + 1 
C           MESLEN = ZEXT(HPRO(INPLEN,BUF))
           LEN = MES_LEN - IND + 1
           CALL LIB$MOVC3(LEN, BPRO(WRKTAB*4-3+IND,BUF), TRABUF(TEUVEBOARD))          
C
C APAGAR
C
C   	     TYPE *,'BILHETE DE TROCA'
C   	     DO I=0,MES_LEN
C                 TYPE 9997,I,BPRO(WRKTAB*4-3+I,BUF) 
C             ENDDO
C             TYPE *,' ' 
9997    FORMAT(' OUTMGR TROCA: ',I3.1,' - ', Z3.2) 

          ENDIF
C
C  CREATE OUTPUT MESSAGE...
C
C
C-------->>V04 ---------------------------------------------------------
!          IF ((TRABUF(TEUVSBT) .EQ. 0) .AND. 
!     *      (TRABUF(TEUVST) .EQ. 11 .OR. TRABUF(TEUVST) .EQ. 10) .AND.
!     *      (TRABUF(TEUVCAM) .GT. 0 ) ) AGTTAB(ALSVAL,TER) = TRABUF(TSER)
C----+------------------------------------------------------------------
C V06| Fix overflow problem in validation last transaction update
C----+------------------------------------------------------------------
        I4AUX_AMOUNT(1) = TRABUF(TEUVCAM)
        I4AUX_AMOUNT(2) = TRABUF(TEUVCAMH)
C----+------------------------------------------------------------------
C V06| Fix overflow problem in validation last transaction update
C----+------------------------------------------------------------------
          IF ((TRABUF(TEUVSBT) .EQ. 0 
     *        .OR. TRABUF(TEUVSBT) .EQ. VNDON  !NEW VALIDATION CASH ACCEPTED
     *        .OR. TRABUF(TEUVSBT) .EQ. VNBNK) !NEW VALIDATION BANK TRANSFER ACCEPTED
     *        .AND. (TRABUF(TEUVST) .EQ. 11 .OR. TRABUF(TEUVST) .EQ. 10) 
C----+------------------------------------------------------------------
C V06| Fix overflow problem in validation last transaction update
C----+------------------------------------------------------------------
     *        .AND. (I8AUX_AMOUNT .GT. 0 ) ) AGTTAB(ALSVAL,TER) = TRABUF(TSER)
C----+------------------------------------------------------------------
C V06| Fix overflow problem in validation last transaction update
C----+------------------------------------------------------------------
C-------->>V04 ---------------------------------------------------------
C
102       CONTINUE
          IND = 0
          DO I=15, MES_LEN
             BPRO(BOUTTAB+I-15,BUF) = ZEXT (BPRO(WRKTAB*4-3+I,BUF))
             IND = IND + 1
          ENDDO
          HPRO(OUTLEN,BUF) = IND           
C
C SPECIAL FUNCTIONS
C
        ELSE IF (TRABUF(TTYP) .EQ. TSPE) THEN
        
          MES_LEN = ZEXT (HPRO(OUTLEN,BUF))
C          call ops('MES_LEN',MES_LEN,MES_LEN)
          
          IF (TRABUF(TSFUN) .EQ. TGREP ) THEN
C----+---+-------------+------------------------------------------------
C V09|BEG| M16 PROJECT | GAME RESULTS: SET UP TRABUF FROM INPUT MESSAGE
C----+---+-------------+------------------------------------------------

!             TRABUF(TSDT1) = TEUM
!             TRABUF(TSDT2) = 1
             TRABUF(TSDT1) = ZEXT(BPRO(BINPTAB+4,BUF))                          !GAME TYPE
             TRABUF(TSDT2) = ZEXT(BPRO(BINPTAB+5,BUF))                          !GAME INDEX
             I1TEMP(4) = ZEXT (BPRO(BWRKTAB+5,BUF))
             I1TEMP(3) = ZEXT (BPRO(BWRKTAB+6,BUF))
             I1TEMP(2) = ZEXT (BPRO(BWRKTAB+7,BUF))
             I1TEMP(1) = ZEXT (BPRO(BWRKTAB+8,BUF))
             TRABUF(TSDT3) = I4TEMP                                             !MESSAGE QUEUE SEQUENCE NUMBER
C----+---+-------------+------------------------------------------------
C V09|END| M16 PROJECT | GAME RESULTS: SET UP TRABUF FROM INPUT MESSAGE
C----+---+-------------+------------------------------------------------
             MTYPE = ZEXT(BPRO(WRKTAB*4-3+16,BUF))
             MTYPE = ISHFT(MTYPE,-4)

             IF (MTYPE .EQ. 9) THEN
                TRABUF(TSTAT) = REJT
                TRABUF(TERR) = INVL
             ENDIF              
             
          ENDIF
C----+------------------------------------------------------------------
C V05| Marking transaction data for new accounting report
C----+------------------------------------------------------------------
          IF (TRABUF(TSFUN) .EQ. TNAP ) THEN
          
             MES_IND = WRKTAB*4-3+14
             
             ! All the new accounting report messages have the same header
             ! layout, so we will get our info from that header:
             ! Check documentation for messages of type 0x6A
             
             ! MESSAGE TYPE/SUBTYPE : BYTE 2
             MTYPE     = ZEXT(BPRO(MES_IND + 2,BUF))
             MTYPE     = ISHFT(MTYPE,-4)
             SUBTYP    = ZEXT(BPRO(MES_IND + 2,BUF))
             SUBTYP    = IAND(SUBTYP,15)
C----+------------------------------------------------------------------
C V08| Bugfix
C----+------------------------------------------------------------------
             IF (MTYPE .EQ. 6 .AND. SUBTYP .EQ. 10) THEN
    
                 ! CLASS : BYTE 8
                 I4TEMP    = ZEXT(BPRO(MES_IND + 8,BUF))
                 TRABUF(TSDT1) = I4TEMP
    
                 ! SUBCLASS : BYTE 9
                 I4TEMP    = ZEXT(BPRO(MES_IND + 9,BUF))
                 TRABUF(TSDT2) = I4TEMP
    
                 ! AGENT NUMBER : BYTES 13,14,15,16
                 I4TEMP    = 0
                 I1TEMP(4) = ZEXT(BPRO(MES_IND + 13,BUF))
                 I1TEMP(3) = ZEXT(BPRO(MES_IND + 14,BUF))
                 I1TEMP(2) = ZEXT(BPRO(MES_IND + 15,BUF))
                 I1TEMP(1) = ZEXT(BPRO(MES_IND + 16,BUF))
                 TRABUF(TSDT3) = I4TEMP
    
                 ! ACCOUNTABILITY YEAR : BYTE 10
                 I4TEMP    = ZEXT(BPRO(MES_IND + 10,BUF))
                 TRABUF(TSDT4) = I4TEMP
    
                 ! ACCOUNTABILITY ROUND : BYTES 11,12
                 I4TEMP    = 0
                 I1TEMP(2) = ZEXT(BPRO(MES_IND + 11,BUF))
                 I1TEMP(1) = ZEXT(BPRO(MES_IND + 12,BUF))
                 TRABUF(TSDT5) = I4TEMP
C
                 !MESSAGE QUEUE SEQUENCE NUMBER
                 I4TEMP    = 0
                 I1TEMP(4) = ZEXT (BPRO(BWRKTAB+5,BUF))
                 I1TEMP(3) = ZEXT (BPRO(BWRKTAB+6,BUF))
                 I1TEMP(2) = ZEXT (BPRO(BWRKTAB+7,BUF))
                 I1TEMP(1) = ZEXT (BPRO(BWRKTAB+8,BUF))
                 TRABUF(TSDT6) = I4TEMP                                         !MESSAGE QUEUE SEQUENCE NUMBER !V09
C
             ELSE IF (MTYPE .EQ. 6 .AND. SUBTYP .EQ. 15) THEN
                TRABUF(TSTAT) = REJT
                TRABUF(TERR) = INVL
C
                I4TEMP    = 0
                I1TEMP(4) = ZEXT (BPRO(BWRKTAB+5,BUF))
                I1TEMP(3) = ZEXT (BPRO(BWRKTAB+6,BUF))
                I1TEMP(2) = ZEXT (BPRO(BWRKTAB+7,BUF))
                I1TEMP(1) = ZEXT (BPRO(BWRKTAB+8,BUF))
                TRABUF(TSDT6) = I4TEMP                                          !MESSAGE QUEUE SEQUENCE NUMBER !V09
             ELSE IF (MTYPE .EQ. 9) THEN
                TRABUF(TSTAT) = REJT
                TRABUF(TERR) = INVL
C
                I4TEMP    = 0
                I1TEMP(4) = ZEXT (BPRO(BWRKTAB+5,BUF))
                I1TEMP(3) = ZEXT (BPRO(BWRKTAB+6,BUF))
                I1TEMP(2) = ZEXT (BPRO(BWRKTAB+7,BUF))
                I1TEMP(1) = ZEXT (BPRO(BWRKTAB+8,BUF))
                TRABUF(TSDT6) = I4TEMP                                          !MESSAGE QUEUE SEQUENCE NUMBER !V09
             ELSE
                TRABUF(TSTAT) = REJT
                TRABUF(TERR) = INVL
C
                I4TEMP    = 0
                I1TEMP(4) = ZEXT (BPRO(BWRKTAB+5,BUF))
                I1TEMP(3) = ZEXT (BPRO(BWRKTAB+6,BUF))
                I1TEMP(2) = ZEXT (BPRO(BWRKTAB+7,BUF))
                I1TEMP(1) = ZEXT (BPRO(BWRKTAB+8,BUF))
                TRABUF(TSDT6) = I4TEMP                                          !MESSAGE QUEUE SEQUENCE NUMBER !V09
             ENDIF 
C----+------------------------------------------------------------------
C V08| Bugfix
C----+------------------------------------------------------------------
          ENDIF
C----+------------------------------------------------------------------
C V05| Marking transaction data for new accounting report
C----+------------------------------------------------------------------
C----+------------------------------------------------------------------
C V08| Bugfix
C----+------------------------------------------------------------------
C          IF ((ZEXT(BPRO(WRKTAB*4-3+16,BUF)) .EQ. 99) .AND. 
C     *       ((PRO(REMSTS,BUF) .EQ. RMDOWN) .OR. (PRO(REMSTS,BUF) .EQ. RMTMOT))) THEN
C             IF (ZEXT(BPRO(WRKTAB*4-3+22,BUF)) .EQ. 1) BPRO(WRKTAB*4-3+22,BUF) = 17
C          ENDIF
          IF ((ZEXT(BPRO(WRKTAB*4-3+16,BUF)) .EQ. 99) .AND. 
     *       ((HPRO(REMSTS,BUF) .EQ. RMDOWN) .OR. (HPRO(REMSTS,BUF) .EQ. RMTMOT))) THEN
             IF (ZEXT(BPRO(WRKTAB*4-3+22,BUF)) .EQ. 1) BPRO(WRKTAB*4-3+22,BUF) = 17
          ENDIF
C----+------------------------------------------------------------------
C V08| Bugfix
C----+------------------------------------------------------------------

C
C  CREATE OUTPUT MESSAGE...
C
          IND = 0
          DO I=15, MES_LEN
             BPRO(BOUTTAB+I-15,BUF) = ZEXT (BPRO(WRKTAB*4-3+I,BUF))
             IND = IND + 1
          ENDDO
          I = 5
          IF (TRABUF(TSTAT) .NE. REJT) CALL PUTIME(TRABUF(TTIM), BPRO(BOUTTAB,BUF), I)
          HPRO(OUTLEN,BUF) = IND
C          CALL OPS('VALOR DA RESPOSTA',BPRO(BOUTTAB+1,BUF),BPRO(BOUTTAB+1,BUF))
          I4CCITT   = TRABUF(TCHK)                         
          BPRO(BOUTTAB+2,BUF) = I1CCITT(2)                 
          BPRO(BOUTTAB+3,BUF) = I1CCITT(1)                 
          CALL GETCCITT(BPRO(BOUTTAB,BUF),1,IND-1,MYCHKSUM)
          I4CCITT   = MYCHKSUM                             
          BPRO(BOUTTAB+2,BUF) = I1CCITT(2)                 
          BPRO(BOUTTAB+3,BUF) = I1CCITT(1)                 
C          CALL OPS('MYCHKSUM',MYCHKSUM,MYCHKSUM)            
C          CALL OPS('TAMANHO DA MNS',IND-1,IND)              
C          CALL OPS('TRABUF(TCHK)',TRABUF(TCHK),TRABUF(TCHK))
C          CALL OPS('nUMBER OF GAMES',0,ZEXT(BPRO(BOUTTAB+17,BUF)))
C	  DO I=0,IND
C              TYPE 9998,I,BPRO(BOUTTAB+I,BUF) 
C          ENDDO
C          TYPE *,' ' 
        ENDIF
        

2000    CONTINUE
C
C PUT GOOD TRANSACTION INTO TRABUF
C
C----+------------------------------------------------------------------
C V08| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------
        IF(IGSDEBUG(IA_OUTMGR)) THEN
            CALL OPSTXT('OUTMGR - OUTTAB - MESSAGE DUMP:')
            CALL DUMP_MESSAGE(1365,BUF,BPRO(BOUTTAB*4-3+1,BUF),ZEXT(HPRO(OUTLEN,BUF)))
            CALL OPSTXT('OUTMGR - WRKTAB - MESSAGE DUMP:')
            CALL DUMP_MESSAGE(1367,BUF,BPRO(WRKTAB*4-3+1,BUF),MES_LEN)
        ENDIF
C----+------------------------------------------------------------------
C V08| Added support to PLACARD Project - IGS
C----+------------------------------------------------------------------

        AGTHTB(ATRNUM,TER)=TRABUF(TTRN)
C         CALL CLEARBUF(0,PRO(WRKTAB,BUF),128)
        CALL TRALOG(TRABUF,PRO(WRKTAB,BUF))
        HPRO(TRCODE,BUF)=TYPREG
C        CALL OPSTXT(' ******************* OUTMGR PARA APULOG ****************')

        CALL QUETRA(APU, BUF)
        CALL DQUTRA(TASK,BUF)
        GOTO 20
9998    FORMAT(' OUTMGR SPEC: ',I3.1,' - ', Z3.2)         
        END
C**************************************************
C SUBROUTINE para verificar o estado dos processos COMMGR e OUTMGR
C INPUT:
C
C OUTPUT:
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CHECKPROCESS()
        IMPLICIT NONE
C**************************************************
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INTEGER*4   TSKSTS,STATUS
C
C IF COMMGR DO NOT RUN THEN RESTART COMMGR
C       
        STATUS = 0 
        CALL STTSK(8HCOMMGR  ,TSKSTS,STATUS) !VERIFY IF COMMGR IS OK
        IF (STATUS .EQ. 4) THEN
           CALL OPSTXT('ERROR!!!!! COMMGR IS NOT RUNNING')
           CALL OPSTXT('STARTING COMMGR AND TRY TO PROCESS ALL TRANSACTIONS ')
           CALL START(TSKNAM(32)) ! COMMGR
        ENDIF
C
C IF INMGR DO NOT RUN THEN RESTART INMGR
C       
        STATUS = 0 
        CALL STTSK(8HINMGR   ,TSKSTS,STATUS) ! VERIFY IF INMGR IS OK
        IF (STATUS .EQ. 4) THEN
           CALL OPSTXT('ERROR!!!!! INMGR IS NOT RUNNING')
           CALL OPSTXT('STARTING INMGR AND TRY TO PROCESS ALL TRANSACTIONS ')
           CALL START(TSKNAM(30)) !INMGR
        ENDIF
        END
C
C PUT TIME INTO REPORTS
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE PUTIME(TIME,OUTTAB,IND)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C 
        BYTE      OUTTAB(*)			!Output Message table.
C
	INTEGER*4 IND				!Index into Output Table
	INTEGER*4 TIME				!Time in ms since midnight
	INTEGER*4 HOURS
	INTEGER*4 MINS
	INTEGER*4 SECS
C
	BYTE	  I1TEMP(4)
	INTEGER*4 TEMP
	EQUIVALENCE(TEMP,I1TEMP)
C
	LOGICAL   HH_MM_SS_FLAG/.TRUE./		!Indicate what format.
C
	IF(HH_MM_SS_FLAG) THEN
	   HOURS = TIME/3600
	   MINS = (TIME-HOURS*3600) / 60
	   SECS = TIME - HOURS*3600 - MINS*60
C
	   OUTTAB(IND+0) = HOURS
	   OUTTAB(IND+1) = MINS
	   OUTTAB(IND+2) = SECS
	ELSE
	   TEMP = TIME
	   OUTTAB(IND+0) = I1TEMP(3)
	   OUTTAB(IND+1) = I1TEMP(2)
	   OUTTAB(IND+2) = I1TEMP(1)
	ENDIF
	IND = IND + 3
C	   
	RETURN
	END
C
C CLEAR BUFFER
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CLEARBUF(VALUE,OUARY,LEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INTEGER*4   VALUE
	INTEGER*4   OUARY(*)
	INTEGER*4   LEN
C
	INTEGER*4   XLEN
	INTEGER*4   K
C
C
C
	IF(VALUE.NE.0)THEN
	  DO 1100 K = 1, LEN
	    OUARY(K) = VALUE
1100	  CONTINUE
	  GOTO 9000
	ENDIF
C
C Special case for 0 value (most common)
C
	XLEN = LEN
	K = 1
2000	CONTINUE
	IF(XLEN.GT.64000)THEN
	  CALL LIB$MOVC5(1,0,0,64000,OUARY(K))
	  XLEN = XLEN-64000
	  K    = K + 16000
	  GOTO 2000
	ENDIF
C
	IF(XLEN.GT.0)THEN
	  CALL LIB$MOVC5(1,0,0,XLEN,OUARY(K))
	ENDIF
C
9000	CONTINUE
	RETURN
	END









        SUBROUTINE DUMP_MESSAGE(MESSAGE_ID, LINE_ID, OUTBUF, MESLEN)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        BYTE OUTBUF(*)
        INTEGER*4 MESLEN
        INTEGER*4 MESSAGE_ID, LINE_ID

        CHARACTER*255 BUF
        CHARACTER*3 ARR(16)
        INTEGER*4 I, J, K, DIV, REMAIN, OFFSET
        
        DO I = 1, 255
            BUF(I:I) = CHAR(0)
        ENDDO
        
        DIV = MESLEN / 16
        REMAIN = MOD(MESLEN,16)
        
        WRITE(BUF, 900) MESSAGE_ID, LINE_ID, MESLEN
        TYPE *, IAM(), '', TRIM(BUF)
        CALL OPSTXT(TRIM(BUF))
        
        DO K = 1, DIV
           DO I = 1, 16
               DO J = 1, 2
                   ARR(I)(J:J) = ' '
               ENDDO
               ARR(I)(3:3) = CHAR(0)
           ENDDO
           DO I = 1, 16
               OFFSET = ((K - 1) * 16) + I
               WRITE(ARR(I), 901) OUTBUF(OFFSET)
           ENDDO
           OFFSET = ((K - 1) * 16)
           WRITE(BUF, 902) OFFSET + 1, OFFSET + 16,( ARR(I), I = 1, 16)
           TYPE *, '', TRIM(BUF)
           CALL OPSTXT(TRIM(BUF))
        ENDDO
        IF(REMAIN .NE. 0) THEN
           DO I = 1, 16
               DO J = 1, 2
                   ARR(I)(J:J) = ' '
               ENDDO
               ARR(I)(3:3) = CHAR(0)
           ENDDO
           DO I = 1, REMAIN
               OFFSET = ((K - 1) * 16) + I
               WRITE(ARR(I), 901) OUTBUF(OFFSET)
           ENDDO
           OFFSET = ((K - 1) * 16)
           WRITE(BUF, 902) OFFSET + 1, OFFSET + REMAIN, (ARR(I), I = 1, 16)
           TYPE *, '', TRIM(BUF)
           CALL OPSTXT(TRIM(BUF))
        ENDIF
        TYPE *, ''

900     FORMAT('PARSED MESSAGE #',I8,' (@ LINE #',I8,') : LEN = ', I8)
901     FORMAT(Z2.2)
902     FORMAT('[',I4,':',I4,'] = ',16(A2,1X))

        RETURN
        END

        
	
