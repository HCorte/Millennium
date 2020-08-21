C
C SUBROUTINE INFOREP
C
C INFOREP.FOR
C
C V40 24-JAN-2000 OXK GIND=SUBCLASS, not hardcoded 1 for Sport. (Vakio changes)
C V39 29-NOV-1999 RXK Table of percents added to the popularity report (Ravi). 
C V38 14-MAY-1999 UXN Super Triple added.
C V37 05-FEB-1997 HXK Added seperate GVT news message
C V36 15-MAR-1996 HXK Putting Rita's fixes (RXK) into PVCS archive
C V35 12-MAR-1996 RXK Fix for the odds sent (Super Double) 
C V34 05-MAR-1996 RXK Crash avoided (the case when report is asked before 
C                     odds are in)
C V33 23-JAN-1996 HXK Chnage for Double / Couple Odds reports
C V32 23-NOV-1995 PXB Couple and Double games added
C V31 13-NOV-1995 HXK Include Quick Picked horses in historical popularities
C V30 10-AUG-1995 HXK Fix for selectinf draw number for Ravi Popularity
C V29 04-AUG-1995 HXK Batch of fixes for Ravi V5 installation
C V28 24-JUL-1995 HXK Popularity list can now be obtained for any previous draw
C V27 29-MAY-1995 HXK Popularity list: i) with QP; ii) without QP;
C                     also addition of prognosis maych 4 capability.
C V26 30-MAR-1995 HXK Fix for end sales date instead of close date
C V25 21-MAR-1995 HXK Fix for incorrect closing time on Odds reports
C V24 09-DEC-1994 HXK Fixed double date bug
C V23 05-DEC-1994 HXK Merging from 25-Nov -> 5 Dec
C V22 29-NOV-1994 HXK Added times for Score, Wintip
C V21 31-JAN-1994 HXK CHANGED TIME.
C V20 23-JAN-1994 HXK PUT IN DRAW DATE ON ODDS REPORT.
C V19 22-JAN-1994 HXK ADDED ERROR FOR GAMES CLOSED FOR ODDS REPORTS.
C V18 18-JAN-1994 HXK CLEAR ROWS_BITMAP.
C V17 13-JAN-1994 HXK TIDIED UP SCORE ODDS REPORT.
C V16 08-JAN-1994 HXK ADDED ODDS REPORTS FOR SCORE, WINNERTIP.
C V15 12-AUG-1993 SXH Put in code for zero TOTAL
C V14 11-AUG-1993 SXH Made VAKIO stats percentages to send to terminal
C V13 11-AUG-1993 SXH Removed debugging TYPE statements
C V12 10-AUG-1993 HXK PUT IN TVLST FOR TERMINAL PEOPLE
C V11 10-AUG-1993 SXH Fixed RAVI popularity list
C V10 10-AUG-1993 HXK DEBUG OF VAKIO MATCH LIST
C V09 06-AUG-1993 HXK CHANGED TVLST TO TGLST
C V08 30-JUL-1993 HXK FIXED BUG (GIND NOT SET FOR V65 REPORT)
C V07 16-JUL-1993 HXK ADDED VAKIO STATS MATCH LIST
C V06 05-JUL-1993 HXK ADDED POPULARITY LIST
C V05 28-JUN-1993 HXK changed err message length from 5 to 6
C V04 10-JUN-1993 HXK sorted out agtinf,agtcom
C V03 21-JAN.1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 14-NOV-91 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V01 01-AUG-90 XXX  RELEASED FOR VAX
C
C CALLING SEQUENCE:
C     CALL INFOREP(TRABUF,MESTAB,MESLEN,MESNUM)
C INPUT
C     TRABUF - INTERNAL TRANSACTION FORMAT
C     MESTAB - TERMINAL INPUT MESSAGE
C OUTPUT
C     MESTAB - TERMINAL OUTPUT MESSAGE
C     MESLEN - OUTPUT MESSAGE LENGTH
C     MESNUM - OUTPUT MESSAGE NUMBER
C
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
        SUBROUTINE INFOREP(TRABUF,MESTAB,MESLEN,MESNUM)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SPECOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:PRMDLL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:WITCOM.DEF'
        INCLUDE 'INCLIB:SCRCOM.DEF'
        INCLUDE 'INCLIB:STACOM.DEF'
        INCLUDE 'INCLIB:DBLCOM.DEF'
        INCLUDE 'INCLIB:CPLCOM.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:SSCCOM.DEF'
        INCLUDE 'INCLIB:SSPCOM.DEF'
        INCLUDE 'INCLIB:TRPCOM.DEF'
        INCLUDE 'INCLIB:TROCOM.DEF'
        INCLUDE 'INCLIB:STRCOM.DEF'
        INCLUDE 'INCLIB:STROCOM.DEF'
C
        !arguments
        BYTE       MESTAB(*)       !

        INTEGER*2  MESLEN          !
        INTEGER*2  MESNUM          !

        INTEGER*4  TEMP            !
        INTEGER*4  GTYP            !
        INTEGER*4  GIND            !
        INTEGER*4  I,J
        INTEGER*4  I4TEMP          !
        INTEGER*4  MYCHKSUM        !
        INTEGER*4  CHKLEN          !
        INTEGER*4  IND             !
        INTEGER*4  SUBCLASS        !
        INTEGER*4  CLASS           !
        INTEGER*4  ERRTYP          !
        INTEGER*4  WEEK            !
        INTEGER*4  YEAR            !
        INTEGER*4  TIME            !
        INTEGER*4  HOURS           ! time (hours)
        INTEGER*4  MINS            ! time (minutes)
        INTEGER*4  SECS            ! time (seconds)
        INTEGER*4  TOTAL           !
        INTEGER*4  ROWS_IND        !
        INTEGER*4  I4ODDS          !
        INTEGER*4  DSCORES(2,MAXD) !
        INTEGER*4  NUM_SCR_ROWS    !
        INTEGER*4  CNT
        INTEGER*4  IND_NUM_SSC_ROWS
        INTEGER*4  BUCKET
        INTEGER*4  ENTR
        INTEGER*4  IND_NUM_TRP_ROWS
        INTEGER*4  UCID
        INTEGER*4  EVE(3)
        INTEGER*4  NETAMT,EVENT_CNT,I1,I2,I3

        INTEGER*4  YEAR2           !Year in 4 digits
        INTEGER*2  SUBTYP          !
        INTEGER*2  I2TEMP(2)       !
        INTEGER*2  I2DATE(VDATE)   !

        BYTE       I1TEMP(4)       !
        BYTE       I1ODDS(4)       !
        BYTE       ROWS_BITMAP(6)  !


        REAL*8     POOL

        EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)
        EQUIVALENCE(I4ODDS,I1ODDS)


        DATA ERRTYP /Z90/
        INTEGER*4 RW1,RW2,RW3
C
        LOGICAL     NOTHING_PLAYED
C
C USE SUBTYP TO DECIDE WHICH ROUTINE TO USE
C
        SUBTYP = MESTAB(2)
        SUBTYP = IAND(SUBTYP,'000F'X)
C
C GET INFORMATION REPORT OPTIONS
C
        CLASS    = ZEXT( MESTAB(5) )  
        SUBCLASS = ZEXT( MESTAB(6) )
C
        TRABUF(TSDT1)=CLASS
        TRABUF(TSDT2)=SUBCLASS
C
C CHECK IF ANY PARAMETERS FOR REPORTS ARE SUPRESSED
C
        IF(TSBIT(P(SUPRPT),INFREP)) THEN    !MAIN INFO REP FLAG
          TRABUF(TERR)=SUPR
          GOTO 3000
        ENDIF
C
C CHECK IF NEWS MESSAGE FLAG IS SUPPRESSED
C
        IF(TSBIT(P(SUPRPT),NEWREP)) THEN
          TRABUF(TERR)=SUPR
          GOTO 3000
        ENDIF
        IND=5
C
C GET WEEK AND YEAR FROM INPUT MESSAGE
C
        WEEK    = ZEXT( MESTAB(7) )  
        YEAR    = ZEXT( MESTAB(8) )
C
C PUT TIME INTO OUTPUT MESSAGE
C
        I4TEMP = TRABUF(TTIM)
        MESTAB(IND+0) = I1TEMP(3)
        MESTAB(IND+1) = I1TEMP(2)
        MESTAB(IND+2) = I1TEMP(1)
        IND=IND+3

C
C PROCESS ODDS REPORTS
C

        IF(SUBTYP.EQ.7) THEN

           ! GTYPE/GIND
           GTYP = CLASS
           GIND = SUBCLASS
           MESTAB(IND+0) = GTYP
           IND=IND+1  
           MESTAB(IND+0) = GIND
           IND=IND+1

           ! WINNERS TIP
           IF(GTYP.EQ.TWIT) THEN
              IF(WITSTS(GIND).LT.GAMOPN.OR.WITSTS(GIND).GT.GAMENV) THEN
                 TRABUF(TERR)    = GREV
                 TRABUF(TSUBERR) = GREV_GAMCLT
                 GOTO 3000
              ENDIF
              I2DATE(VCDC) = WITESD(GIND)
              CALL CDATE(I2DATE)
              MESTAB(IND+0)=I2DATE(VDAY)
              MESTAB(IND+1)=I2DATE(VMON)
              MESTAB(IND+2)=I2DATE(VYEAR)
              IND=IND+3

              TIME  = WITTIM(GIND)
              IF(TIME.GT.'40000000'X) TIME=TIME-'40000000'X
              HOURS = TIME/3600
              MINS  = (TIME-HOURS*3600)/60
              SECS  = TIME - HOURS*3600 - MINS*60
              MESTAB(IND+0)=HOURS
              MESTAB(IND+1)=MINS
              MESTAB(IND+2)=SECS
              IND=IND+3

              !ROWS BITMAP / ODDS   6 BYTES
              ROWS_IND = IND
              IND=IND+6
              DO I=1,6
                 ROWS_BITMAP(I) = 0
              ENDDO
              DO 1000 I=1,MAXWRW
                 IF(WITSTA(I,GIND).LT.GAMOPN.OR.
     *              WITSTA(I,GIND).GT.GFINAL) GOTO 1000
                 I4ODDS = WTPOOL(I,1,2,GIND)
                 CALL BSETN(ROWS_BITMAP,I-1)
                 MESTAB(IND+0) = I1ODDS(3)
                 MESTAB(IND+1) = I1ODDS(2)
                 MESTAB(IND+2) = I1ODDS(1)
                 IND=IND+3
1000          ENDDO
              DO J=1,6
                 MESTAB(ROWS_IND+J-1)=ROWS_BITMAP(J)
              ENDDO
              MESLEN=IND-1
              GOTO 9000
           ENDIF

           !* SCORE
           IF(GTYP.EQ.TSCR) THEN
              IF(SCRSTS(GIND).LT.GAMOPN.OR.SCRSTS(GIND).GT.GAMENV) THEN
                 TRABUF(TERR)    = GREV
                 TRABUF(TSUBERR) = GREV_GAMCLT
                 GOTO 3000
              ENDIF
              I2DATE(VCDC) = SCRESD(GIND)
              CALL CDATE(I2DATE)
              MESTAB(IND+0)=I2DATE(VDAY)
              MESTAB(IND+1)=I2DATE(VMON)
              MESTAB(IND+2)=I2DATE(VYEAR)
              IND=IND+3

              TIME  = SCRTIM(GIND)
              IF(TIME.GT.'40000000'X) TIME=TIME-'40000000'X
              HOURS = TIME/3600
              MINS  = (TIME-HOURS*3600)/60
              SECS  = TIME - HOURS*3600 - MINS*60
              MESTAB(IND+0)=HOURS
              MESTAB(IND+1)=MINS
              MESTAB(IND+2)=SECS
              IND=IND+3

              !#ROWS SENT   1 BYTE
              NUM_SCR_ROWS = MAXD - 6  
              MESTAB(IND) = NUM_SCR_ROWS
              IND = IND +1
              DO 1100 I=1,MAXD
                 IF(I.GT.21.AND.I.LT.31) GOTO 1100
                 CALL INDPOL(DSCORES(1,I),DSCORES(2,I),DPOOL(I,SPSCOR,GIND))
                 !HOME SCORE   1 BYTE
                 MESTAB(IND) = DSCORES(1,I)
                 IND = IND + 1
                 !AWAY SCORE   1 BYTE
                 MESTAB(IND) = DSCORES(2,I)
                 IND=IND + 1
                 !ODDS         3 BYTES
                 I4ODDS = DPOOL(I,SPDODS,GIND)
                 MESTAB(IND+0) = I1ODDS(3)
                 MESTAB(IND+1) = I1ODDS(2)
                 MESTAB(IND+2) = I1ODDS(1)
                 IND = IND + 3
                 IF(I.EQ.15.OR.I.EQ.21.OR.I.EQ.45) THEN
                    MESTAB(IND) = 'FF'X
                    IND = IND + 1
                    MESTAB(IND) = 'FF'X
                    IND =IND +1
                    IF(I.EQ.15) I4ODDS = OPOOL(1,SPDODS,GIND)
                    IF(I.EQ.21) I4ODDS = OPOOL(2,SPDODS,GIND)
                    IF(I.EQ.45) I4ODDS = OPOOL(3,SPDODS,GIND)
                    MESTAB(IND+0) = I1ODDS(3)
                    MESTAB(IND+1) = I1ODDS(2)
                    MESTAB(IND+2) = I1ODDS(1)
                    IND = IND + 3
                 ENDIF
1100          CONTINUE
              MESLEN=IND-1
              GOTO 9000
           ENDIF

           ! SUPERSCORE

           IF(GTYP.EQ.TSSC) THEN
              NOTHING_PLAYED = .FALSE.
              IF(SSCSTS(GIND).LT.GAMOPN.OR.SSCSTS(GIND).GT.GAMENV) THEN
                 TRABUF(TERR)    = GREV
                 TRABUF(TSUBERR) = GREV_GAMCLT
                 GOTO 3000
              ENDIF
              J=SSPFEL(GIND)
              IF(J.NE.0) THEN         
                 DO WHILE(SSPTOPC(SSGNEL,J,GIND).NE.0)
                   ENTR = SSPTOPC(SSGAME,J,GIND)
                   CALL MOVBYT(SSPTOPC(SSGAMB,J,GIND),1,BUCKET,1,2)
                   IF(SSPCAMT(ENTR,BUCKET,GIND).NE.0) GOTO 1110
                   J=SSPTOPC(SSGNEL,J,GIND)
                 ENDDO
                 IF(J.EQ.SSPFEL(GIND)) GOTO 1110 ! ONLY ONE ENTRY....
              ENDIF
C
C No combinations played....(played, but cancelled)
C
              NOTHING_PLAYED = .TRUE.
C
1110          CONTINUE
              I2DATE(VCDC) = SSCESD(GIND)
              CALL CDATE(I2DATE)
              MESTAB(IND+0)=I2DATE(VDAY)
              MESTAB(IND+1)=I2DATE(VMON)
              MESTAB(IND+2)=I2DATE(VYEAR)
              IND=IND+3

              TIME  = SSCTIM(GIND)
              IF(TIME.GT.'40000000'X) TIME=TIME-'40000000'X
              HOURS = TIME/3600
              MINS  = (TIME-HOURS*3600)/60
              SECS  = TIME - HOURS*3600 - MINS*60
              MESTAB(IND+0)=HOURS
              MESTAB(IND+1)=MINS
              MESTAB(IND+2)=SECS
              IND=IND+3

              !#ROWS SENT   1 BYTE
              IF(NOTHING_PLAYED) THEN
                 MESTAB(IND) = 0
                 MESLEN=IND
                 GOTO 9000
              ENDIF
              MESTAB(IND) = SSMAXD
              IND_NUM_SSC_ROWS = IND            
              IND = IND +1
              J=SSPFEL(GIND)
              POOL = SSCSAL(GIND) * CALPER(SSCSPR(GIND)) + 
     *                              DFLOAT(SSCPOL(1,GIND))
              CNT=0
C
C MAIN LOOP. DON'T SEND THE COMBINATIONS WITH 0 MKS
C
1115          CONTINUE
              ENTR = SSPTOPC(SSGAME,J,GIND)
              CALL MOVBYT(SSPTOPC(SSGAMB,J,GIND),1,BUCKET,1,2)
              IF(SSPCAMT(ENTR,BUCKET,GIND).EQ.0) GOTO 1120
C 
              MESTAB(IND) = SSPTOPC(SSGCID+2,J,GIND)
              IND = IND +1
              MESTAB(IND) = SSPTOPC(SSGCID+1,J,GIND)
              IND = IND +1
              MESTAB(IND) = SSPTOPC(SSGCID+0,J,GIND)
              IND = IND +1
              I4ODDS = IDNINT(100.D0*POOL/DFLOAT(SSPCAMT(ENTR,BUCKET,GIND)))
              MESTAB(IND+0) = I1ODDS(4)
              MESTAB(IND+1) = I1ODDS(3)
              MESTAB(IND+2) = I1ODDS(2)
              MESTAB(IND+3) = I1ODDS(1)
              IND = IND + 4

              CNT = CNT+1
1120          CONTINUE
              J=SSPTOPC(SSGNEL,J,GIND)
              IF(J.EQ.0) GOTO 1150
C
              IF(CNT.LT.SSMAXD) GOTO 1115

1150          CONTINUE
              MESTAB(IND_NUM_SSC_ROWS) = CNT
              MESLEN=IND-1
              GOTO 9000
           ENDIF

           ! SUPER DOUBLE

           IF(GTYP.EQ.TDBL) THEN
              IF(DBLSTS(GIND).LT.GAMOPN.OR.DBLSTS(GIND).GT.GAMENV) THEN
                 TRABUF(TERR)    = GREV
                 TRABUF(TSUBERR) = GREV_GAMCLT
                 GOTO 3000
              ENDIF
              I2DATE(VCDC) = DBLESD(GIND)
              CALL CDATE(I2DATE)
              MESTAB(IND+0)=I2DATE(VDAY)
              MESTAB(IND+1)=I2DATE(VMON)
              MESTAB(IND+2)=I2DATE(VYEAR)
              IND=IND+3

              TIME  = DBLTIM(GIND)
              IF(TIME.GT.'40000000'X) TIME=TIME-'40000000'X
              HOURS = TIME/3600
              MINS  = (TIME-HOURS*3600)/60
              SECS  = TIME - HOURS*3600 - MINS*60
              MESTAB(IND+0)=HOURS
              MESTAB(IND+1)=MINS
              MESTAB(IND+2)=SECS
              IND=IND+3

              !#ROWS SENT
              MESTAB(IND) = DBLMAXD
              IND=IND+1
              
              DO 1200 I=1,DBLMAXD
                 MESTAB(IND+0) =((DBPSORT(2,I,GIND)-1)/MAXDBLRW)+1
                 IF(MESTAB(IND+0) .GT. MAXDBLRW) MESTAB(IND+0) = MAXDBLRW
                 MESTAB(IND+1) =(MOD(DBPSORT(2,I,GIND)-1,MAXDBLRW))+1
                 IF(MESTAB(IND+1) .EQ. 0) MESTAB(IND+1) = MAXDBLRW
                 IND=IND+2
                 IF(DBPSORT(2,I,GIND).NE.0) I4ODDS = 
     *              DBPOOL(DBPSORT(2,I,GIND),DBLPODDS,DBLPSTAT,GIND)
                 MESTAB(IND+0) = I1ODDS(3)
                 MESTAB(IND+1) = I1ODDS(2)
                 MESTAB(IND+2) = I1ODDS(1)
                 IND=IND+3
1200          ENDDO

              MESLEN=IND-1
              GOTO 9000
           ENDIF

           !* TODAYS COUPLE
           IF(GTYP.EQ.TCPL) THEN
              IF(CPLSTS(GIND).LT.GAMOPN.OR.CPLSTS(GIND).GT.GAMENV) THEN
                 TRABUF(TERR)    = GREV
                 TRABUF(TSUBERR) = GREV_GAMCLT
                 GOTO 3000
              ENDIF
              I2DATE(VCDC) = CPLESD(GIND)
              CALL CDATE(I2DATE)
              MESTAB(IND+0)=I2DATE(VDAY)
              MESTAB(IND+1)=I2DATE(VMON)
              MESTAB(IND+2)=I2DATE(VYEAR)
              IND=IND+3

              TIME  = CPLTIM(GIND)
              IF(TIME.GT.'40000000'X) TIME=TIME-'40000000'X
              HOURS = TIME/3600
              MINS  = (TIME-HOURS*3600)/60
              SECS  = TIME - HOURS*3600 - MINS*60
              MESTAB(IND+0)=HOURS
              MESTAB(IND+1)=MINS
              MESTAB(IND+2)=SECS
              IND=IND+3

              !#ROWS SENT
              MESTAB(IND) = CPLMAXD
              IND=IND+1
              
              DO 1300 I=1,CPLMAXD
                 MESTAB(IND+0) =(CPPSORT(2,I,GIND)/(MAXCPLRW/2))+1
                 IF(MESTAB(IND+0) .GT. MAXCPLRW/2) MESTAB(IND+0) = MAXCPLRW/2
                 MESTAB(IND+1) = MOD(CPPSORT(2,I,GIND),MAXCPLRW/2)
                 IF(MESTAB(IND+1) .EQ. 0) MESTAB(IND+1) = MAXCPLRW/2
                 IND=IND+2
                 IF(CPPSORT(2,I,GIND).NE.0) I4ODDS = 
     *              CPPOOL(CPPSORT(2,I,GIND),CPLPODDS,CPLPSTAT,GIND)
                 MESTAB(IND+0) = I1ODDS(3)
                 MESTAB(IND+1) = I1ODDS(2)
                 MESTAB(IND+2) = I1ODDS(1)
                 IND=IND+3
1300          ENDDO

              MESLEN=IND-1
              GOTO 9000
           ENDIF

           ! TODAY'S TRIO

           IF(GTYP.EQ.TTRP) THEN
              IF(TRPSTS(GIND).LT.GAMOPN.OR.TRPSTS(GIND).GT.GAMENV) THEN
                 TRABUF(TERR)    = GREV
                 TRABUF(TSUBERR) = GREV_GAMCLT
                 GOTO 3000
              ENDIF
C
              I2DATE(VCDC) = TRPESD(GIND)
              CALL CDATE(I2DATE)
              MESTAB(IND+0)=I2DATE(VDAY)
              MESTAB(IND+1)=I2DATE(VMON)
              MESTAB(IND+2)=I2DATE(VYEAR)
              IND=IND+3

              TIME  = TRPTIM(GIND)
              IF(TIME.GT.'40000000'X) TIME=TIME-'40000000'X
              HOURS = TIME/3600
              MINS  = (TIME-HOURS*3600)/60
              SECS  = TIME - HOURS*3600 - MINS*60
              MESTAB(IND+0)=HOURS
              MESTAB(IND+1)=MINS
              MESTAB(IND+2)=SECS
              IND=IND+3

              !REMEMBER #ROWS SENT   1 BYTE
C
C IF NOTHING PLAYED THEN SET NUMBER OF ROWS TO 0
C
              IF(TROTNUM(GIND).LE.0) THEN
                  MESTAB(IND) = 0
                  MESLEN = IND
                  GOTO 9000
              ENDIF
                
              IND_NUM_TRP_ROWS = IND            
              IND = IND +1
        
              J=TROFEL(GIND)
C
C GET NET AMOUNT (WITHOUT CANCELLED ROWS)
C
              EVENT_CNT = 0
              IF(TRPEST(1,GIND).NE.GAMNUL) EVENT_CNT=EVENT_CNT+1
              IF(TRPEST(2,GIND).NE.GAMNUL) EVENT_CNT=EVENT_CNT+1
              IF(TRPEST(3,GIND).NE.GAMNUL) EVENT_CNT=EVENT_CNT+1
              NETAMT=0
              IF(EVENT_CNT.EQ.3) THEN
                DO 1320 I1=1,MAXTRPRW
                  IF(TRPSTA(I1,1,GIND).NE.GAMOPN) GOTO 1320
                  DO 1310 I2=1,MAXTRPRW
                    IF(TRPSTA(I2,2,GIND).NE.GAMOPN) GOTO 1310
                    DO 1305 I3=1,MAXTRPRW
                      IF(TRPSTA(I3,3,GIND).NE.GAMOPN) GOTO 1305
                      UCID= I1 + (I2-1)*MAXTRPRW + (I3-1)*MAXTRPRW*MAXTRPRW
                      NETAMT = NETAMT + TRODDS(TRGAMT,UCID,GIND)
1305                 CONTINUE
1310               CONTINUE
1320            CONTINUE
              ELSEIF(EVENT_CNT.EQ.2) THEN
                DO 1340 I1=1,MAXTRPRW
                  IF(TRPSTA(I1,1,GIND).NE.GAMOPN) GOTO 1340
                  DO 1330 I2=1,MAXTRPRW
                   IF(TRPSTA(I2,2,GIND).NE.GAMOPN) GOTO 1330
                   UCID= I1 + (I2-1)*MAXTRPRW 
                   NETAMT = NETAMT + TRODDS(TRGAMT,UCID,GIND)
1330              CONTINUE
1340            CONTINUE
              ELSEIF(EVENT_CNT.EQ.1) THEN
              DO 1350 I1=1,MAXTRPRW
                IF(TRPSTA(I1,1,GIND).NE.GAMOPN) GOTO 1350
                UCID= I1
                NETAMT = NETAMT + TRODDS(TRGAMT,UCID,GIND)
1350          CONTINUE
             ENDIF
C
             POOL = NETAMT*CALPER(TRPSPR(GIND))+DFLOAT(TRPPOL(1,GIND))
             CNT=0

1400          CONTINUE

              UCID=J
              IF(TRODDS(TRGAMT,J,GIND).EQ.0) GOTO 1420
              IF(TRPEST(3,GIND).EQ.GAMOPN) THEN
                 EVE(3)=(UCID-1)/(MAXTRPRW*MAXTRPRW)+1
                 IF(TRPSTA(EVE(3),3,GIND).EQ.GAMCAN) GOTO 1420   
                 UCID=UCID-(EVE(3)-1)*MAXTRPRW*MAXTRPRW
              ELSE
                 EVE(3)=0
              ENDIF
              IF(TRPEST(2,GIND).EQ.GAMOPN) THEN
                 EVE(2)=(UCID-1)/MAXTRPRW +1
                 IF(TRPSTA(EVE(2),2,GIND).EQ.GAMCAN) GOTO 1420   
                 UCID=UCID-(EVE(2)-1)*MAXTRPRW
              ELSE
                 EVE(2)=0
              ENDIF
              EVE(1) =UCID
              IF(TRPSTA(EVE(1),1,GIND).EQ.GAMCAN) GOTO 1420   

              MESTAB(IND) = EVE(1)
              IND = IND +1
              MESTAB(IND) = EVE(2)
              IND = IND +1
              MESTAB(IND) = EVE(3)
              IND = IND +1
              I4ODDS = IDNINT(100.D0*POOL/DFLOAT(TRODDS(TRGAMT,J,GIND)))
              MESTAB(IND+0) = I1ODDS(4)
              MESTAB(IND+1) = I1ODDS(3)
              MESTAB(IND+2) = I1ODDS(2)
              MESTAB(IND+3) = I1ODDS(1)
              IND = IND + 4

              CNT = CNT+1
              IF(CNT.EQ.TRPMAXD) GOTO 1450
   
1420          CONTINUE
              J=TRODDS2(TRGNEL,J,GIND)
              IF(J.EQ.0) GOTO 1450
              GOTO 1400  

1450          CONTINUE
              MESTAB(IND_NUM_TRP_ROWS) = CNT
              MESLEN=IND-1
              GOTO 9000
           ENDIF

           ! SUPER TRIPLE

           IF(GTYP.EQ.TSTR) THEN
              IF(STRSTS(GIND).LT.GAMOPN.OR.STRSTS(GIND).GT.GAMENV) THEN
                 TRABUF(TERR)    = GREV
                 TRABUF(TSUBERR) = GREV_GAMCLT
                 GOTO 3000
              ENDIF
C
              I2DATE(VCDC) = STRESD(GIND)
              CALL CDATE(I2DATE)
              MESTAB(IND+0)=I2DATE(VDAY)
              MESTAB(IND+1)=I2DATE(VMON)
              MESTAB(IND+2)=I2DATE(VYEAR)
              IND=IND+3

              TIME  = STRTIM(GIND)
              IF(TIME.GT.'40000000'X) TIME=TIME-'40000000'X
              HOURS = TIME/3600
              MINS  = (TIME-HOURS*3600)/60
              SECS  = TIME - HOURS*3600 - MINS*60
              MESTAB(IND+0)=HOURS
              MESTAB(IND+1)=MINS
              MESTAB(IND+2)=SECS
              IND=IND+3

              !REMEMBER #ROWS SENT   1 BYTE
C
C IF NOTHING PLAYED THEN SET NUMBER OF ROWS TO 0
C
              IF(STROTNUM(GIND).LE.0) THEN
                  MESTAB(IND) = 0
                  MESLEN = IND
                  GOTO 9000
              ENDIF
                
              IND_NUM_TRP_ROWS = IND            
              IND = IND +1
        
C
C GET NET AMOUNT (WITHOUT CANCELLED ROWS)
C
              NETAMT=0
              DO 1520 I1=1,MAXSTRRW
                  IF(STRSTA(I1,GIND).NE.GAMOPN) GOTO 1520
                  DO 1510 I2=1,MAXSTRRW
                    IF(STRSTA(I2,GIND).NE.GAMOPN.OR.I2.EQ.I1) GOTO 1510
                    DO 1505 I3=1,MAXSTRRW
                      IF(STRSTA(I3,GIND).NE.GAMOPN.OR.
     *                   I1.EQ.I3.OR.I2.EQ.I3) GOTO 1505
                      UCID= I1 + (I2-1)*MAXSTRRW + (I3-1)*MAXSTRRW*MAXSTRRW
                      NETAMT = NETAMT + STRODDS(STRGAMT,UCID,GIND)
1505                CONTINUE
1510              CONTINUE
1520          CONTINUE
C
             POOL = NETAMT*CALPER(STRSPR(GIND))+DFLOAT(STRPOL(1,GIND))
             CNT=0
             J=STROFEL(GIND)

1600         CONTINUE

              UCID=J
              IF(STRODDS(STRGAMT,J,GIND).EQ.0) GOTO 1620

              RW3 = UCID/(MAXSTRRW*MAXSTRRW)+1
              IF(STRSTA(RW3,GIND).EQ.GAMCAN) GOTO 1620
              UCID = UCID-(RW3-1)*MAXSTRRW*MAXSTRRW

              RW2 = UCID/MAXSTRRW+1
              IF(STRSTA(RW2,GIND).EQ.GAMCAN) GOTO 1620
              UCID = UCID-(RW2-1)*MAXSTRRW
              RW1 = UCID
              IF(STRSTA(RW1,GIND).EQ.GAMCAN) GOTO 1620

              MESTAB(IND) = RW1
              IND = IND +1
              MESTAB(IND) = RW2
              IND = IND +1
              MESTAB(IND) = RW3
              IND = IND +1
              I4ODDS = IDNINT(100.D0*POOL/DFLOAT(STRODDS(STRGAMT,J,GIND)))
              MESTAB(IND+0) = I1ODDS(4)
              MESTAB(IND+1) = I1ODDS(3)
              MESTAB(IND+2) = I1ODDS(2)
              MESTAB(IND+3) = I1ODDS(1)
              IND = IND + 4

              CNT = CNT+1
              IF(CNT.EQ.STRMAXD) GOTO 1650
   
1620          CONTINUE
              J=STRODDS2(STRGNEL,J,GIND)
              IF(J.EQ.0) GOTO 1650
              GOTO 1600  

1650          CONTINUE
              MESTAB(IND_NUM_TRP_ROWS) = CNT
              MESLEN=IND-1
              GOTO 9000
           ENDIF

        ENDIF


C
C PUT CLASS AND SUBCLASS INTO OUTPUT MESSAGE
C
        MESTAB(IND) = CLASS
        IND=IND+1
        MESTAB(IND) = SUBCLASS
        IND=IND+1


C
C PROCESS NEWS MESSAGE REQUEST
C
        IF(TRABUF(TSFUN).EQ.TINFO) THEN
          MESNUM=MNEWS
          IF(SUBCLASS.EQ.1) MESNUM = TVNEWS
          IF(SUBCLASS.EQ.2) MESNUM = GVTNEWS
          MESLEN = IND - 1 
          GOTO 9000
        ENDIF

C
C PROCESS VAKIO MATCH STATS LIST
C
        IF(TRABUF(TSFUN).EQ.TVLST) THEN
                GIND=SUBCLASS
                I4TEMP=SPTDAT(1,GIND)
                MESTAB(IND+0) = I1TEMP(2)
                MESTAB(IND+1) = I1TEMP(1)
                !CALL MOVBYT(TEMP,3,MESTAB,IND,2)
                IND=IND+2

                TEMP=I4TEMP
                CALL FIGWEK(TEMP,WEEK,YEAR2)
                MESTAB(IND+0) = MOD(YEAR2,100)
                MESTAB(IND+1) = WEEK
                IND = IND + 2

                I4TEMP = SPTMAX(GIND)
                MESTAB(IND)=I1TEMP(1)
                IND=IND+1
                DO I=1,SPTMAX(GIND)
                    TOTAL = STASPT_TAB2(I,1,GIND) +
     *                      STASPT_TAB2(I,2,GIND) +
     *                      STASPT_TAB2(I,3,GIND) 
                    IF (TOTAL .NE. 0) THEN
                        MESTAB(IND)   = STASPT_TAB2(I,1,GIND)*100 / TOTAL
                        MESTAB(IND+1) = STASPT_TAB2(I,2,GIND)*100 / TOTAL
                        MESTAB(IND+2) = STASPT_TAB2(I,3,GIND)*100 / TOTAL
                        IND=IND+3
                    ELSE
                        MESTAB(IND)   = 0
                        MESTAB(IND+1) = 0
                        MESTAB(IND+2) = 0
                        IND=IND+3
                    END IF
                ENDDO

                MESLEN=IND-1
                GOTO 9000
         ENDIF
C
C ERROR IN INFO REQUEST FROM TERMINAL
C
3000    CONTINUE
        TRABUF(TSTAT)=REJT
        MESTAB(2) = ERRTYP
        MESTAB(5) = TRABUF(TERR)
        IF(TRABUF(TERR).EQ.GREV) THEN
           MESTAB(6) = TRABUF(TSUBERR)
        ELSE
           MESTAB(6) = 0
        ENDIF
        MESLEN=6
C
C CALCULATE CHECKSUM FOR MESSAGE BACK TO TERMINAL
C
9000    CONTINUE
        I4CCITT = TRABUF(TCHK)
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)
        CHKLEN=MESLEN-1
        CALL GETCCITT(MESTAB,1,CHKLEN,MYCHKSUM)
        I4CCITT = MYCHKSUM
        MESTAB(3) = I1CCITT(2)
        MESTAB(4) = I1CCITT(1)
C
        RETURN

        END
