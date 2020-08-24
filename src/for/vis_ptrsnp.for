C SUBROUTINE PTRSNP
C
C V06 23-SEP-1999 UXN Format statement changed.
C V05 02-JUN-1999 UXN Take data from memory only if DAYSTS is DSOPEN
C V04 28-MAY-1999 UXN DTRWIN changed. MAXTRPRW changed to 18.
C V03 17-MAY-1999 UXN Name changed to PTRSNP
C V02 27-JAN-1998 UXN Screens for final results added.
C V01 20-JAN-1998 RXK Initial release.  
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE PTRSNP(DRAW,GIND,PAGE,CLINE)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:VISCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:TRPCOM.DEF'
        INCLUDE 'INCLIB:TROCOM.DEF'
        INCLUDE 'INCLIB:DTRREC.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:TRFREC.DEF'
C
        ! arguments
        INTEGER*4  DRAW                     !
        INTEGER*4  GIND                     !
        INTEGER*4  CLINE(20)                !
        INTEGER*4  PAGE        

        INTEGER*2 DBUF(LDATE_LEN)   !data buffer
        INTEGER*2 BEGSAL(LDATE_LEN) !beginning sales date
        INTEGER*2 ENDSAL(LDATE_LEN) !ending sales date
        INTEGER*4 BUF(CDLEN)        !command buffer
        INTEGER*4 AMOUNT(MAXTRPRW,3)!amount bet on row (net,without cancels)
        INTEGER*4 GNUM              !game number
        INTEGER*4 POS               !
        INTEGER*4 KEYNUM            !
        INTEGER*4 VALUE             !
        INTEGER*4 VALUE1            !
        INTEGER*4 TOTAMT
        REAL*8    RTOTAMT           !total amount bet
        INTEGER*4 NETAMT            !
        INTEGER*4 TAX               !not used
        REAL*8    NETPOL            !net pool
        REAL*8    TOTPOL            !tot pool, includes roll over
        INTEGER*4 RNDPOL            !rounded pool
        INTEGER*4 NETSAL            !net sales
        INTEGER*4 PRG_AMT
        INTEGER*4 WINSHR            !winners share
        INTEGER*4 I                 !misc counter
        INTEGER*4 J                 !,,      ,,
        INTEGER*4 K                 !,,      ,,
        INTEGER*4 L                 !,,      ,,
        INTEGER*4 R                 !,,      ,,
        INTEGER*4 I1,I2,I3
        INTEGER*4 UCID
        INTEGER*4 ST                !state indicator (after call)
        INTEGER*4 LIN               !lin counter
        INTEGER*4 FDB(7)
        INTEGER*4 CANROWS(MAXTRPRW)

        CHARACTER*17 POLSTS(11)     !pool status
        DATA POLSTS/'Not initialized  ','No drawing       ',
     *              'Info entered     ','Game open        ',
     *              'End of game      ','Results entered  ',
     *              'Results verified ','Drawing completed',
     *              'Results are final','Cancelled/Ref dis',
     *              'Cancelled/Ref ena'/

        CHARACTER*3 RWSTS(11)       !row status
        DATA RWSTS/'   ','cls','ent','opn','cls','rin','ver',
     *             'drw','fin','can','ref'/

        REAL*8    CMDCHG(3)         !command
        INTEGER*4  EVENT_CNT
        DATA CMDCHG/'OPN     ','CLS     ','CAN     '/
        INTEGER*4   WIN_TOT,WIN_CNT(3),WINMAP(3,MAXTRPTI)
        INTEGER*4   FRST_WIN,LAST_WIN
        CHARACTER*80 TEMPLINE

C----------------------------- Start of Code -----------------------------

        IF(GIND.LE.0) GIND = 1
        IF (GIND .GT. MAXIND) THEN
          WRITE(CLIN23,3000) GTNAMES(TTRP)
          RETURN
        END IF

        GNUM = GTNTAB(TTRP,GIND)

        IF (GNUM .LT. 1) THEN
          WRITE(CLIN23,3010) GTNAMES(TTRP),GIND
          RETURN
        END IF

        IF(DRAW.LT.1) DRAW=DAYDRW(GNUM)
        IF(DRAW.EQ.0) DRAW=DAYHDR(GNUM)
	IF(DRAW.LE.0) RETURN

        IF(PAGE.LE.0) PAGE = 1
   
C---- Check for command input.

        POS = 1

        CALL KEY(CLINE,CMDCHG,3,POS,KEYNUM)

        IF (POS .GT. 40) GOTO 100                    !-- NO INPUT

        IF (KEYNUM .EQ. 0) GOTO 70                   !-- INPUT ERROR

        CALL NUMB(CLINE,POS,VALUE)                   !-- GET VALUE

        IF (VALUE .GE. 101 .AND. VALUE .LE.(100+MAXTRPRW)) THEN
           VALUE=VALUE-100
           VALUE1=1  
        ELSEIF (VALUE .GE. 201 .AND. VALUE .LE.(200+MAXTRPRW)) THEN
           VALUE=VALUE-200
           VALUE1=2  
        ELSEIF (VALUE .GE. 301 .AND. VALUE .LE.(300+MAXTRPRW)) THEN
           VALUE=VALUE-300
           VALUE1=3  
        ELSE
           GOTO 80
        ENDIF

        CALL FASTSET(0,BUF,CDLEN)

        GOTO(10,20,30) KEYNUM

        GOTO 70

C---- Open row command

10      CONTINUE
        IF (TRPSTA(VALUE,VALUE1,GIND) .EQ. GAMNUL) GOTO 90
        IF (TRPSTA(VALUE,VALUE1,GIND) .EQ. GAMOPN) GOTO 80
        BUF(1)=2
        BUF(2)=GAMOPN
        BUF(3)=TCTRP
        BUF(6)=IDNUM
        BUF(8)=GIND
        BUF(9)=VALUE
        BUF(10)=VALUE1
        GOTO 60

C---- Close row command

20      CONTINUE
        IF (TRPSTA(VALUE,VALUE1,GIND) .EQ. GAMNUL) GOTO 90
        IF (TRPSTA(VALUE,VALUE1,GIND) .EQ. GAMBFD) GOTO 80
        BUF(1)=2
        BUF(2)=GAMBFD
        BUF(3)=TCTRP
        BUF(6)=IDNUM
        BUF(8)=GIND
        BUF(9)=VALUE
        BUF(10)=VALUE1
        GOTO 60

C---- Cancell row command

30      CONTINUE
        IF (TRPSTA(VALUE,VALUE1,GIND) .EQ. GAMNUL) GOTO 90
        IF (TRPSTA(VALUE,VALUE1,GIND) .EQ. GAMCAN) GOTO 80
        IF (TRPSTA(VALUE,VALUE1,GIND) .EQ. GAMREF) GOTO 80
        BUF(1)=2
        BUF(2)=GAMCAN
        BUF(3)=TCTRP
        BUF(6)=IDNUM
        BUF(8)=GIND
        BUF(9)=VALUE
        BUF(10)=VALUE1
        GOTO 60

C---- Queue command to system input queue

60      CONTINUE
        CALL VISCMD(BUF,ST)
        CALL XWAIT(2,2,ST)
        GOTO 100

C---- Input errors

70      CONTINUE
        WRITE(CLIN23,800)
        RETURN

80      CONTINUE
        WRITE(CLIN23,810)
        RETURN

90      CONTINUE
        WRITE(CLIN23,820)
        RETURN

C---- Set up Screen.

100     CONTINUE

C---- Get data from memory.

110     CONTINUE
        DO I = 3,22
           WRITE(XNEW(I),3050)
        ENDDO

        IF(DRAW.EQ.DAYDRW(GNUM) .AND. DAYSTS.EQ.DSOPEN) THEN
           CALL GAMLOG(TTRP,GIND,DTRREC,TRPSTS)
           CALL FASTMOV(TRODDS(1,1,GIND),TRFODDS,2*TRGPOL)
           GOTO 300
        ENDIF

C---- Get game data from file.

        SMODE = .TRUE.

        CALL OPENW(1,GFNAMES(1,GNUM),0,0,0,ST)
        CALL IOINIT(FDB,1,DTRSEC*256)

        IF (ST .NE. 0) THEN
          WRITE(CLIN23,3020) (GFNAMES(K,GNUM),K=1,5),ST
          CALL USRCLOS1(1)
          RETURN
        END IF

        CALL READW(FDB,DRAW,DTRREC,ST)

        IF (ST .NE. 0) THEN
          WRITE(CLIN23,3030) (GFNAMES(K,GNUM),K=1,5),ST,DRAW
          CALL USRCLOS1(1)
          RETURN
        END IF

        CALL USRCLOS1(1)

        IF (DTRSTS .EQ. 0) THEN
            WRITE(CLIN23,3040) GTNAMES(TTRP),GIND,DRAW
            RETURN
        END IF

C---- Get pool data from file.

        IF (DTRSTS .LT. GFINAL) THEN
           CALL OPENQW(4,DTRPFN,4,0,0,ST)
           IF(ST.NE.0) THEN
              WRITE(CLIN23,3060) GTNAMES(TTRP),GIND,DRAW
              RETURN
           ENDIF
           CALL IOQINIT(FDB,4,TRFSEC*256)
           CALL READQW(FDB,1,TRFREC,ST)
           CALL CLOSEQFIL(FDB)
        ENDIF

C---- Work out amount and total of cancellatios.

300     CONTINUE
        
        EVENT_CNT = 0
        IF(DTREST(1).NE.GAMNUL) EVENT_CNT=EVENT_CNT+1
        IF(DTREST(2).NE.GAMNUL) EVENT_CNT=EVENT_CNT+1
        IF(DTREST(3).NE.GAMNUL) EVENT_CNT=EVENT_CNT+1
        IF(EVENT_CNT.EQ.0) RETURN
        IF (DTRSTS .GE. GFINAL) GOTO 470

C---- Calculate row amounts

        CALL FASTSET(0,AMOUNT,3*MAXTRPRW)
        IF(EVENT_CNT.EQ.3) THEN
           DO 420 I1=1,MAXTRPRW
              IF(DTRSTA(I1,1).NE.GAMOPN) GOTO 420
              DO 410 I2=1,MAXTRPRW
                 IF(DTRSTA(I2,2).NE.GAMOPN) GOTO 410
                 DO 400 I3=1,MAXTRPRW
                    IF(DTRSTA(I3,3).NE.GAMOPN) GOTO 400
                    UCID= I1 + (I2-1)*MAXTRPRW + (I3-1)*MAXTRPRW*MAXTRPRW
                    AMOUNT(I1,1) = AMOUNT(I1,1) + TRFODDS(TRGAMT,UCID)
                    AMOUNT(I2,2) = AMOUNT(I2,2) + TRFODDS(TRGAMT,UCID)
                    AMOUNT(I3,3) = AMOUNT(I3,3) + TRFODDS(TRGAMT,UCID)
400              CONTINUE
410           CONTINUE
420        CONTINUE
        ELSEIF(EVENT_CNT.EQ.2) THEN
           DO 440 I1=1,MAXTRPRW
              IF(DTRSTA(I1,1).NE.GAMOPN) GOTO 440
              DO 430 I2=1,MAXTRPRW
                 IF(DTRSTA(I2,2).NE.GAMOPN) GOTO 430
                 UCID= I1 + (I2-1)*MAXTRPRW 
                 AMOUNT(I1,1) = AMOUNT(I1,1) + TRFODDS(TRGAMT,UCID)
                 AMOUNT(I2,2) = AMOUNT(I2,2) + TRFODDS(TRGAMT,UCID)
430           CONTINUE
440        CONTINUE
        ELSEIF(EVENT_CNT.EQ.3) THEN
           DO 450 I1=1,MAXTRPRW
              IF(DTRSTA(I1,1).NE.GAMOPN) GOTO 450
              UCID= I1
              AMOUNT(I1,1) = AMOUNT(I1,1) + TRFODDS(TRGAMT,UCID)
450        CONTINUE
        ENDIF
        NETAMT = 0
        DO I=1,MAXTRPRW
          DO J=1,3
            NETAMT = NETAMT + AMOUNT(I,J)
          ENDDO
        ENDDO
        TOTAMT = NETAMT
C---- Calculate taxes and net pool.

        TAX = 0
        RNDPOL = DTRBRK(1)
        NETPOL = NETAMT * CALPER(DTRSPR)
        TOTPOL = NETPOL + DFLOAT(TAX) + DFLOAT(RNDPOL)

        !---- Header Info first.

470     CONTINUE
        IF (DTRSTS .EQ. GAMNUL) THEN
          WRITE(CLIN1,900) GTNAMES(TTRP),GIND,(DBUF(I),I=7,13)
          WRITE(CLIN2,9221) DRAW,POLSTS(DTRSTS+1)
          WRITE(CLIN23,3040) GTNAMES(TTRP),GIND,DRAW
          RETURN
        ELSE
          DBUF(5)=DTRDAT
          BEGSAL(5) = DTRBSD
          ENDSAL(5) = DTRESD
          CALL LCDATE(DBUF)
          CALL LCDATE(BEGSAL)
          CALL LCDATE(ENDSAL)
          WRITE(CLIN1,901) GTNAMES(TTRP),GIND,
     *                     (BEGSAL(I),I=7,13),(ENDSAL(I),I=7,13)
          WRITE(CLIN2,902) (DTRMNM(I),I=1,TRPENM_LEN/4),DRAW,
     *                     POLSTS(DTRSTS+1),(DBUF(I),I=7,13)
        END IF
        IF (DTRSTS .LT. GFINAL) THEN
          DO I = 3,22
             WRITE(XNEW(I),3050)
          ENDDO
          WRITE(CLIN3,904) (DTRENM(I,1),I=1,6),
     *                     (DTRENM(I,2),I=1,6),
     *                     (DTRENM(I,3),I=1,4),
     *                     CMONY(DTRPRC,4,BETUNIT)
          LIN = 4
        ELSE
          WIN_TOT = DTRCMB
        END IF
C
C Game Info.
C
        IF (DTRSTS .LT. GFINAL) THEN
          DO R = 1,MAXTRPRW
	    IF(DTRNMS(1,R,1).EQ.0) 
     *         CALL LIB$MOVC5(0,0,ICHAR(' '),TRPNMS_LEN,DTRNMS(1,R,1))
	    IF(DTRNMS(1,R,2).EQ.0) 
     *         CALL LIB$MOVC5(0,0,ICHAR(' '),TRPNMS_LEN,DTRNMS(1,R,2))
	    IF(DTRNMS(1,R,3).EQ.0) 
     *         CALL LIB$MOVC5(0,0,ICHAR(' '),TRPNMS_LEN,DTRNMS(1,R,3))

            WRITE (XNEW(LIN),906)  R,
     *                (DTRNMS(I,R,1),I=1,2),
     *                DTRSBR(R,1)/(DFLOAT(EVENT_CNT*DOLL_BASE/DYN_BETUNIT)),
     *                RWSTS(DTRSTA(R,1)+1),
     *                R,
     *                (DTRNMS(I,R,2),I=1,2),
     *                DTRSBR(R,2)/(DFLOAT(EVENT_CNT*DOLL_BASE/DYN_BETUNIT)),
     *                RWSTS(DTRSTA(R,2)+1),
     *                R,
     *                (DTRNMS(I,R,3),I=1,2),
     *                DTRSBR(R,3)/(DFLOAT(EVENT_CNT*DOLL_BASE/DYN_BETUNIT)),
     *                RWSTS(DTRSTA(R,3)+1)
            LIN=LIN+1
          END DO
          RTOTAMT = DFLOAT(TOTAMT)/(DFLOAT(EVENT_CNT)*DFLOAT(DOLL_BASE/DYN_BETUNIT))
          NETPOL = NETPOL/(DFLOAT(EVENT_CNT)*DFLOAT(DOLL_BASE/DYN_BETUNIT))
          TOTPOL = (TOTPOL/DFLOAT(EVENT_CNT)+DFLOAT(DTRPOL(1)))/
     *             DFLOAT(DOLL_BASE/DYN_BETUNIT)

          WRITE(CLIN22,922) RTOTAMT,
     *                      NETPOL,
     *                      DFLOAT(DTRPOL(1))/DFLOAT(DOLL_BASE/DYN_BETUNIT),
     *                      TOTPOL
        ELSE
          NETSAL = DTRSAL(DOLAMT)-DTRREF
          WINSHR = NETSAL*CALPER(DTRSPR)  
	  CALL FASTSET(0,WINMAP,3*MAXTRPTI) 
	  CALL FASTSET(0,WIN_CNT,3)
	  DO I = 1,3
	     DO J=1,MAXTRPTI
		IF(DTRWIN(I,J).GT.0) WINMAP(I,DTRWIN(I,J)) = 1
	     ENDDO
	     DO J=1,MAXTRPTI
	        IF(WINMAP(I,J).GT.0) WIN_CNT(I) = WIN_CNT(I)+1
	     ENDDO
	  ENDDO
          DO I=1,3
             J=0
             DO L=1,MAXTRPRW
                IF(DTRSTA(L,I).EQ.GAMCAN) THEN
                   J=J+1
                   CANROWS(J)=L
                ENDIF
             ENDDO   
             IF(J.GT.0) THEN 
                WRITE(XNEW(3+I),9301) I,(DTRENM(K,I),K=1,4),WIN_CNT(I),
     *            (CANROWS(R),R=1,J)
             ELSE
                WRITE(XNEW(3+I),930) I,(DTRENM(K,I),K=1,4),WIN_CNT(I)
             ENDIF
          ENDDO
          WRITE(CLIN7,931) (BEGSAL(K),K=7,13)
          WRITE(CLIN8,932) (ENDSAL(K),K=7,13)
          WRITE(CLIN9,933) (DBUF(K),K=7,13)
          WRITE(CLIN11,934),'Total Sales    ',CSMONY(DTRSAL(2),12,BETUNIT)
          WRITE(CLIN12,934),'Total Refunds  ',CSMONY(DTRREF,12,BETUNIT)
          WRITE(CLIN13,934),'Net Sales      ',CSMONY(NETSAL,12,BETUNIT)
          WRITE(CLIN14,934),'Winners Share  ',CSMONY(WINSHR,12,BETUNIT)
          WRITE(CLIN15,934),'Extra Amount   ',CSMONY(DTRPOL(1),12,BETUNIT)
          WRITE(CLIN16,934),'Winning amount ',CSMONY(DTRWON-DTRREF,12,BETUNIT)
          WRITE(CLIN17,934),'Rounding Pot   ',CSMONY(DTRBRK(1),12,BETUNIT)
          WRITE(CLIN18,934),'Roll Pot       ',CSMONY(DTRPOL(2),12,BETUNIT)
          WRITE(CLIN19,934),'Winners Paid   ',CSMONY(DTRPAD-DTRPRF,12,VALUNIT)
          WRITE(CLIN20,934),'Refunds Paid   ',CSMONY(DTRPRF,12,BETUNIT)
          IF(DTRPUP.NE.0) THEN
            PRG_AMT = DTRWON-DTRPAD
          ELSE
            PRG_AMT = 0
          ENDIF
          WRITE(CLIN21,934),'Amount Purged  ',CSMONY(PRG_AMT,12,BETUNIT)
          WRITE(CLIN22,934),'Min.stake      ',CSMONY(DTRPRC,12,BETUNIT)
C
C Prepare printout for winners.
C
          IF((PAGE-1)*3.GE.WIN_TOT) THEN
            FRST_WIN = 1
          ELSE
            FRST_WIN = (PAGE-1)*3+1
          ENDIF
          LAST_WIN = MIN(FRST_WIN+2,WIN_TOT)
          LIN = 10
          DO 500 I=FRST_WIN,LAST_WIN
            IF(LIN.EQ.10.OR.LIN.EQ.14.OR.LIN.EQ.18) THEN
                TEMPLINE = XNEW(LIN)
                WRITE(TEMPLINE(1:10),3051) I
                XNEW(LIN) = TEMPLINE
                LIN = LIN + 1
            ENDIF
            TEMPLINE = XNEW(LIN)
            WRITE(TEMPLINE(26:40),3055) DTRODS(I)/100,MOD(DTRODS(I),100) 
            XNEW(LIN)= TEMPLINE
            TEMPLINE = XNEW(LIN+1)
            WRITE(TEMPLINE(26:40),3056) DTRWBT(TRACNT,I)
            XNEW(LIN+1)= TEMPLINE
            DO J=1,3
              IF(DTRWIN(J,I).GT.0) THEN
                  TEMPLINE = XNEW(LIN)
                  WRITE(TEMPLINE(1:25),3052) J,DTRWIN(J,I),
     *              (DTRNMS(K,DTRWIN(J,I),J),K=1,4)
                  XNEW(LIN) = TEMPLINE
                  LIN = LIN + 1
              ENDIF
            ENDDO
500     CONTINUE
        ENDIF

        RETURN

C------------------- Formating statements ------------------------------

800     FORMAT ('Input error')
810     FORMAT ('Value error')
820     FORMAT ('Sorry, that row not activated, no change allowed')

900     FORMAT ('* ',A8,1X,I1,1X,7A2,' *',30(' '))
901     FORMAT (A8,1X,I1,7A2,' -',7A2)
902     FORMAT (<TRPENM_LEN/4>A4,'*Event code',I3,'*',A17,'*Draw',7A2)
903     FORMAT ('Min.stake ',A6)
9221    FORMAT (' ','Event code -',I4,20(' '),'* ',A17,' *')
904     FORMAT (6A4,3X,6A4,3X,4A4,'Stake ',A4)
905     FORMAT ('No Name         Amount Sts',1X,'No Name         Amount Sts',
     *          1X,'No Name         Amount Sts')
906     FORMAT (3(I2.2,1X,2A4,F11.02,1X,A3,1X))
922     FORMAT ('Sales ',F12.02,1(' '),'Net pool ',F12.02,1(' '),
     *          'Extra ',F10.02,1(' '),'Tot pool ',F13.02)
930     FORMAT(1X,I1,'.',1X,4A4,1X,I2,' winner(s)')
9301    FORMAT(1X,I1,'.',1X,4A4,1X,I2,' winner(s),  Cancelled ',<J>I3)
931     FORMAT(4X,T41,'Beginning sales ',3X,7A2)
932     FORMAT(4X,'Row Winner',19X,'Odds',3X,'Ending sales    ',3X,7A2)
933     FORMAT(29X,'#winners',3X,'Draw date       ',3X,7A2)
934     FORMAT(39X,A16,4X,A12)
3000    FORMAT ('Enter !',A8,' game index ')
3010    FORMAT (A8,1X,I1,' game not active')
3020    FORMAT (5A4,' open error ',I4)
3030    FORMAT (5A4,' read error ',I4,' record > ',I4)
3040    FORMAT (A8,1X,I1,' game not initialized event > ',I4)
3050    FORMAT (80(' '))
3051    FORMAT(1X,'(',I2.2,')')
3052    FORMAT(2X,I1,'.',1X,I2.2,1X,4A4)
3055    FORMAT(I9,'.',I2.2)
3056    FORMAT(I12)
3060    FORMAT (A8,1X,I1,' pool file read error, event > ',I4)
        END

