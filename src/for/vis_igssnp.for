C
C SUBROUTINE IGSSNP
C
C
C VIS_IGSSNP.FOR
C
C V02 20-JUL-2015 SCML Adding support for IGS internal cancel flags
C V01 08-APR-2014 SCML Placard Project
C
C
C IGS CONTROL SNAPSHOT
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE IGSSNP(CLINE,GAM)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:VISCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:ENCCOM.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:X2XQUE.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
C
C
        ! arguments
        INTEGER*4  CLINE(20)                     !
        INTEGER*4  GAM                           !

        ! variables
        INTEGER*4  SVAL                          !
        INTEGER*4  SCAN                          !
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
        INTEGER*4  SICAN                         !
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
        INTEGER*4  SWAG                          !
        INTEGER*4  SREP                          !
C        INTEGER*4  SACA                          !
        INTEGER*4  ST                            !
        INTEGER*4  BITMAP(2)                    !
        INTEGER*4  BITMAP1                        !
        INTEGER*4  I                             !
        INTEGER*4  KEYNUM                        !
        INTEGER*4  POS                           !
        INTEGER*4  GIND                          !
        INTEGER*4  GTYP                          !
        INTEGER*4  BUF(CDLEN)                    !
        INTEGER*4  TEMP                          !
        INTEGER*4  VALUE                         !

!        INTEGER*4  Z,X                          !
        INTEGER*4 IGSQUE_LIST(3)
        INTEGER*4 ACTIVE(NUMTSK)
        INTEGER*4 QUE
        INTEGER*4  MESS(EDLEN)       !

        INTEGER*4 MAXPRM
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C       PARAMETER (MAXPRM=13)
        PARAMETER (MAXPRM=14)
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
        REAL*8    K(MAXPRM)                      !

        CHARACTER*9 GAMNAM                       !
        CHARACTER   CTEMP(4)                     !
        CHARACTER*20  PASPAS
        CHARACTER*7   DEFTPASS
        CHARACTER*7   PASSENT                 !

        EQUIVALENCE(TEMP,CTEMP)
        EQUIVALENCE(PASPAS,PASSENT)
C
        DATA   K/'INIGS   ','COMIGS  ','OUTIGS  ','IGSCOn  ',
     *           'SUPPLac ','IGSTMo  ','SUPWAg  ','SUPCAn  ',
     *           'SUPVAl  ','SUPREp  ','SUPFIn  ','SUPRNt  ',
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C    *           'FINTMo  '/
     *           'FINTMo  ','SUPICan '/
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
        DATA DEFTPASS/'SUPORTE'/
C
        MESS(1) = 63
        MESS(2) = TEIGS
C
C GET GAME NUMBER
C
!        CALL OPS('GAM[',GAM,GAM)
!        CALL OPS('MAXGAM[',MAXGAM,MAXGAM)
        IF(GAM.NE.1) THEN !GAM = 1 = PLACARD
          IF(GAM.GT.1) THEN
            WRITE(CLIN23,923) GAM
            RETURN
          ENDIF
            GAM=0
        ELSE
!           CALL OPS('2-GAM[',GAM,GAM)
!           CALL OPS('GAMTYP[',GAMTYP,GAMTYP)
!           CALL OPS('GNTTAB[',GNTTAB,GNTTAB)
!           Z=1
!           X=1
!            DO WHILE (X<MAXGAM)
!             CALL OPS ('GTTAB - X GAM',X,X)
!             CALL OPS ('VALOR',GNTTAB(GAMTYP,X),0)
!             X=X+1
!            ENDDO
           
C            GTYP=GNTTAB(GAMTYP,GAM)
            GTYP=TODS
C            GIND=GNTTAB(GAMIDX,GAM)
            GIND=1
!           CALL OPS('GTYP[',GTYP,GTYP)
!           CALL OPS('GIND[',GIND,GIND)
C            IF(GTYP.LT.1.OR.GTYP.GT.MAXTYP) THEN
            IF(GAM.GT.1) THEN
                WRITE(CLIN23,923) GAM
                RETURN
            ENDIF

            WRITE (GAMNAM,1600) GTNAMES(GTYP), GIND
        ENDIF
        
C
C IGSSNP INPUT
C
        VALUE = 0
        TEMP  = 0
        POS   = 1
        CALL KEY(CLINE,K,MAXPRM,POS,KEYNUM)
C
        IF(POS.GT.40) GOTO 300                     !NO INPUT
        IF(KEYNUM.EQ.0)GOTO 200                    !INPUT ERROR
        IF(KEYNUM.EQ.13.AND.GAM.NE.1) GOTO 300

        CALL NUMB(CLINE,POS,VALUE)                 !GET VALUE
        IF(VALUE.LT.0)  GOTO 205
C
C CLEAR COMMAND MESSAGE BUFFER
C
2       CONTINUE
        CALL FASTSET(0,BUF,CDLEN)
        GOTO(200,200,200,504,
     *       505,506,507,508,
     *       509,510,512,513,
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C    *       514) KEYNUM   
     *       514,515) KEYNUM   
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
        GOTO 200
C
C IGSCon CHANGE
C
504     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        BUF(1)=IGSCONF
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C PLACard CHANGE
C
505     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        BUF(1)=IGSPPLA
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C IGSTMo CHANGE
C
506     CONTINUE
        CALL PASSWORD(5,PASPAS)
        IF (PASSENT .NE. DEFTPASS) THEN
          MESS(3) = 5
          CALL QUEMES(MESS)
          GOTO 206
        ENDIF
        IF(VALUE.LT.1.OR.VALUE.GT.50) GOTO 2051
        BUF(1)=IGSTOUT
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250
C
C SUPWAg CHANGE
C
507     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        IF(GAM.EQ.0) THEN
          BUF(1)=IGSPWAG
          BUF(2)=VALUE
        ELSE
          BITMAP1=P(IGSPGWAG)
          IF(VALUE.EQ.0) THEN
            CALL BCLR(BITMAP1,GAM-1)
          ELSE
            CALL BSET(BITMAP1,GAM-1)
          ENDIF
          BUF(1)=IGSPGWAG
          BUF(2)=BITMAP1
        ENDIF
        BUF(3)=TCPAR
        GOTO 250
C
C SUPCAn CHANGE
C
508     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        IF(GAM.EQ.0) THEN
          BUF(1)=IGSPCAN
          BUF(2)=VALUE
        ELSE
          BITMAP1=P(IGSPGCAN)
          IF(VALUE.EQ.0) THEN
            CALL BCLR(BITMAP1,GAM-1)
          ELSE
            CALL BSET(BITMAP1,GAM-1)
          ENDIF
          BUF(1)=IGSPGCAN
          BUF(2)=BITMAP1
        ENDIF
        BUF(3)=TCPAR
        GOTO 250
C
C SUPVAl CHANGE
C
509     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        IF(GAM.EQ.0) THEN
          BUF(1)=IGSPVAL
          BUF(2)=VALUE
        ELSE
          BITMAP1=P(IGSPGVAL)
          IF(VALUE.EQ.0) THEN
            CALL BCLR(BITMAP1,GAM-1)
          ELSE
            CALL BSET(BITMAP1,GAM-1)
          ENDIF
          BUF(1)=IGSPGVAL
          BUF(2)=BITMAP1
        ENDIF
        BUF(3)=TCPAR
        GOTO 250
C
C SUPREp CHANG
C
510     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        IF(GAM.EQ.0) THEN
          BUF(1)=IGSPREP
          BUF(2)=VALUE
        ELSE
          BITMAP1=P(IGSPGREP)
          IF(VALUE.EQ.0) THEN
            CALL BCLR(BITMAP1,GAM-1)
          ELSE
            CALL BSET(BITMAP1,GAM-1)
          ENDIF
          BUF(1)=IGSPGREP
          BUF(2)=BITMAP1
        ENDIF
        BUF(3)=TCPAR
        GOTO 250
C
C SUPACa CHANGE
C
C511     CONTINUE
C        CALL OPS('11-VALUE:',VALUE, VALUE) ! DEBUG - REMOVER
C        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
C        IF(GAM.EQ.0) THEN
C          BUF(1)=IGSPACAN
C          BUF(2)=VALUE
C        ELSE
C          BITMAP1=P(IGSPAGCA)
C          IF(VALUE.EQ.0) THEN
C            CALL BCLR(BITMAP1,GAM-1)
C          ELSE
C            CALL BSET(BITMAP1,GAM-1)
C          ENDIF
C          BUF(1)=IGSPAGCA
C          BUF(2)=BITMAP1
C        ENDIF
C        BUF(3)=TCPAR
C        GOTO 250
C
C SUPFIn CHANGE
C
512     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        IF(GAM.EQ.0) THEN
          BUF(1)=IGSPFIN
          BUF(2)=VALUE
        ELSE
          BITMAP1=P(IGSPGFIN)
          IF(VALUE.EQ.0) THEN
            CALL BCLR(BITMAP1,GAM-1)
          ELSE
            CALL BSET(BITMAP1,GAM-1)
          ENDIF
          BUF(1)=IGSPGFIN
          BUF(2)=BITMAP1
        ENDIF
        BUF(3)=TCPAR
        GOTO 250
C
C SUPRNt CHANGE
C
513     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        IF(GAM.EQ.0) THEN
          BUF(1)=IGSPRNT
          BUF(2)=VALUE
        ELSE
          BITMAP1=P(IGSPGRNT)
          IF(VALUE.EQ.0) THEN
            CALL BCLR(BITMAP1,GAM-1)
          ELSE
            CALL BSET(BITMAP1,GAM-1)
          ENDIF
          BUF(1)=IGSPGRNT
          BUF(2)=BITMAP1
        ENDIF
        BUF(3)=TCPAR
        GOTO 250


C
C FINTMo CHANGE
C
514     CONTINUE
        CALL PASSWORD(5,PASPAS)
        IF (PASSENT .NE. DEFTPASS) THEN
          MESS(3) = 6
          CALL QUEMES(MESS)
          GOTO 206
        ENDIF
        IF(VALUE.LT.1.OR.VALUE.GT.50) GOTO 2052
        IF(GAM.EQ.1) THEN
          BUF(1)=PLAFINTO
          BUF(2)=VALUE
          BUF(3)=TCPAR
          GOTO 250
        ENDIF
        GOTO 300

C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C
C SUPICan CHANGE
C
515     CONTINUE
        IF(VALUE.NE.0.AND.VALUE.NE.1) GOTO 205
        IF(GAM.EQ.0) THEN
          BUF(1)=IGSPICAN
          BUF(2)=VALUE
        ELSE
          BITMAP1=P(IGSPGICAN)
          IF(VALUE.EQ.0) THEN
            CALL BCLR(BITMAP1,GAM-1)
          ELSE
            CALL BSET(BITMAP1,GAM-1)
          ENDIF
          BUF(1)=IGSPGICAN
          BUF(2)=BITMAP1
        ENDIF
        BUF(3)=TCPAR
        GOTO 250
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------



C
C INPUT ERROR
C
200     CONTINUE
        WRITE(CLIN23,800)
800     FORMAT('Input error')

        RETURN
C
C VALUE ERROR
C
205     CONTINUE
        WRITE(CLIN23,801)
801     FORMAT('Value error')

        RETURN
C
C VALUE ERROR IGSTMo
C
2051    CONTINUE
        WRITE(CLIN23,8011)
8011    FORMAT('Value error - Does not fall within interval 1 to 50')

        RETURN
C
C VALUE ERROR FINTMo
C
2052    CONTINUE
        WRITE(CLIN23,8012)
8012    FORMAT('Value error - Does not fall within interval 1 to 50')

        RETURN
C
C INVALID PASSWORD
C
206     CONTINUE
        WRITE(CLIN23,802)
802     FORMAT('Invalid Password')

        RETURN
C
C QUEUE COMMAND BUFFER TO SYSTEM INPUT INPUT QUEUE
C
250     CONTINUE
        BUF(6)=IDNUM
        CALL VISCMD(BUF,ST)
        CALL XWAIT(2,1,ST)
C
C BUILD IGSSNP SCREEN IMAGE
C
300     CONTINUE

C
C GET BUFFER UTILIZATION INFORMATION
C
        CALL LISTSIZE(QUETAB(1,IGI),IGSQUE_LIST(1)) !INIGS
        CALL LISTSIZE(COMIGSQUE(1),IGSQUE_LIST(2)) !COMIGS
        CALL LISTSIZE(QUETAB(1,IGO),IGSQUE_LIST(3)) !OUTIGS
C
C GET GAME FLAGS
C
        IF(GAM.NE.0) THEN
            SWAG=0
            SCAN=0
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
            SICAN = 0
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
            SVAL=0         
            SREP=0
C            SACA=0
            IF(TSBIT(P(IGSPGWAG),GAM-1)) SWAG=1
            IF(TSBIT(P(IGSPGCAN),GAM-1)) SCAN=1
            IF(TSBIT(P(IGSPGVAL),GAM-1)) SVAL=1
            IF(TSBIT(P(IGSPGREP),GAM-1)) SREP=1
C            IF(TSBIT(P(IGSPAGCA),GAM-1)) SACA=1
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
            IF(TSBIT(P(IGSPGICAN),GAM-1)) SICAN=1
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
        ENDIF

        WRITE(CLIN1,901)

C---- System 

        WRITE(CLIN3,903) K(1),IGSQUE_LIST(1),K(2),IGSQUE_LIST(2),
     *                   K(3),IGSQUE_LIST(3)

C---- Supress

        WRITE(CLIN6,910) K(4),P(IGSCONF),K(5),P(IGSPPLA)
        WRITE(CLIN7,911) K(6),P(IGSTOUT)

C---- Game
        
        IF(GAM.EQ.0) THEN
            WRITE(CLIN12,912)  K(7),P(IGSPWAG),K(8),P(IGSPCAN),
     *                         K(9),P(IGSPVAL),K(10),P(IGSPREP)
                           
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C           WRITE(CLIN13,9124) K(11),P(IGSPFIN),K(12),P(IGSPRNT)
            WRITE(CLIN13,9124) K(11),P(IGSPFIN),K(12),P(IGSPRNT),K(14),P(IGSPICAN)
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
        ELSE
            IF(GTYP.EQ.18) THEN
                WRITE(CLIN12,9121) GAMNAM,K(7),SWAG,K(8),SCAN,K(9),SVAL,K(10),SREP
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C               WRITE(CLIN13,9123) K(11),P(IGSPGFIN),K(12),P(IGSPGRNT),K(13),P(PLAFINTO)
                WRITE(CLIN13,9123) K(11),P(IGSPGFIN),K(12),P(IGSPGRNT),K(13),P(PLAFINTO)
     *                            ,K(14),SICAN
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------

                WRITE(CLIN15,900)
                WRITE(CLIN16,900)
            ENDIF
        ENDIF

        RETURN
C
C----- FORMAT STATEMENTS
C
900     FORMAT(80(' '))
901     FORMAT('**** IGS control snapshot ****')
903     FORMAT('QUEUES   >  ',3('',A7,I7,3X))
910     FORMAT('         >  ',2('*',A7,I6,3X))
911     FORMAT('            ',1('*',A7,I6,3X))
912     FORMAT('IGS      >  ',4('*',A7,I6,3X))
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C9124    FORMAT('            ',2('*',A7,I6,3X))
9124    FORMAT('            ',3('*',A7,I6,3X))
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
9121    FORMAT(A9,'>  ',4('*',A7,I6,3X))
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
C9123    FORMAT('            ','*',A7,I6,3X,'*',A7,I6,3X,'*',A7,I6,3X)
9123    FORMAT('            ',4('*',A7,I6,3X))
C----+------------------------------------------------------------------
C V02| Added support for IGS internal cancel flags
C----+------------------------------------------------------------------
923     FORMAT('Sorry, IGS game ',I2,' not active ')
1600    FORMAT(A8,I1)
1601    FORMAT(1X,'Unable to Send buffer, no buffer available')
        END
