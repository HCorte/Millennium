CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C File      : M_ISLN_PJMC.FOR
C Change Log:
C
C Ver Date       Author  Comment
C --- ---------- ------- ----------------------------------------------
C V01 2013.02.01 SCML    Created - module that contains additional
C                        functionality for processing Stamp Tax in
C                        national lottery
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C      THIS ITEM IS THE PROPERTY OF SCML.
C
C      COPYRIGHT 2013 SCML. ALL RIGHTS RESERVED.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SUBROUTINE DUMP_STRUCT_TAX_SHARES(TX_SHRS)
C 
C Dumps the TAX_SHRS structure onto screen
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE DUMP_STRUCT_TAX_SHARES(TX_SHRS)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:M_ISLN_PJMC.DEF'

        INTEGER*4 I
        RECORD /TAX_SHARES/ TX_SHRS
        
        TYPE *, IAM(), '-----------------------------------------------'
        TYPE *, IAM(), 'TX_SHRS.TAXPER    = ', TX_SHRS.TAXPER
        TYPE *, IAM(), 'TX_SHRS.BASAMT    = ', TX_SHRS.BASAMT
        TYPE *, IAM(), 'TX_SHRS.RFNAMT    = ', TX_SHRS.RFNAMT
        TYPE *, IAM(), 'TX_SHRS.POPWSER   = ', TX_SHRS.POPWSER
        TYPE *, IAM(), 'TX_SHRS.GNUM      = ', TX_SHRS.GNUM
        TYPE *, IAM(), 'TX_SHRS.EEAAAA    = ''', TX_SHRS.EEAAAA, ''''
        TYPE *, IAM(), 'TX_SHRS.DRWN      = ', TX_SHRS.DRWN
        TYPE *, IAM(), 'TX_SHRS.NUMDIVS   = ', TX_SHRS.NUMDIVS
        TYPE *, IAM(), 'TX_SHRS.NUMXDIVS  = ', TX_SHRS.NUMXDIVS
        TYPE *, IAM(), 'TX_SHRS.DRWDT     = ', TX_SHRS.DRWDT
        TYPE *, IAM(), 'TX_SHRS.NOFFRAC   = ', TX_SHRS.NOFFRAC
        TYPE *, IAM(), 'TX_SHRS.HASTAX    = ', TX_SHRS.HASTAX
        TYPE *, IAM(), 'TX_SHRS.PRZAMT    = ', TX_SHRS.PRZAMT
        TYPE *, IAM(), 'TX_SHRS.NETPRZAMT = ', TX_SHRS.NETPRZAMT
        TYPE *, IAM(), 'TX_SHRS.MAX_PRZAMT    = ', TX_SHRS.MAX_PRZAMT
        TYPE *, IAM(), 'TX_SHRS.MAX_NETPRZAMT = ', TX_SHRS.MAX_NETPRZAMT
        TYPE *, IAM(), 'TX_SHRS.MIN_PRZAMT    = ', TX_SHRS.MIN_PRZAMT
        TYPE *, IAM(), 'TX_SHRS.MIN_NETPRZAMT = ', TX_SHRS.MIN_NETPRZAMT
        TYPE *, IAM(), '...............................................'        
        DO I = 1, TAX_SHARES_MAXDIV
            TYPE *, IAM(), 'TX_SHRS.GAMSHV(',I,')= '
     *            , TX_SHRS.GAMSHV(I)
        END DO
        TYPE *, IAM(), '...............................................'        
        DO I = 1, TAX_SHARES_MAXDIV
            TYPE *, IAM(), 'TX_SHRS.GAMEXSHV(',I,')= '
     *            , TX_SHRS.GAMEXSHV(I)
        END DO
        TYPE *, IAM(), '...............................................'        
        DO I = 1, TAX_SHARES_MAXDIV
            TYPE *, IAM(), 'TX_SHRS.TOTDIVAMT(',I,')= '
     *            , TX_SHRS.TOTDIVAMT(I)
        END DO
        TYPE *, IAM(), '...............................................'        
        DO I = 1, TAX_SHARES_MAXDIV
            TYPE *, IAM(), 'TX_SHRS.NETDIVAMT(',I,')= '
     *            , TX_SHRS.NETDIVAMT(I)
        END DO
        TYPE *, IAM(), '...............................................'        
        DO I = 1, TAX_SHARES_MAXDIV
            TYPE *, IAM(), 'TX_SHRS.TOTDIVPRZ(',I,')= '
     *            , TX_SHRS.TOTDIVPRZ(I)
        END DO
        TYPE *, IAM(), '...............................................'        
        DO I = 1, TAX_SHARES_LDATE_LEN
            TYPE *, IAM(), 'TX_SHRS.DRDAT(',I,')= '
     *            , TX_SHRS.DRDAT(I)
        END DO
        TYPE *, IAM(), '-----------------------------------------------'
        
        END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SUBROUTINE INIT_STRUCT_TAX_SHARES(TX_SHRS)
C 
C Initializes the TAX_SHRS structure onto screen
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE INIT_STRUCT_TAX_SHARES(TX_SHRS)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:M_ISLN_PJMC.DEF'

        INTEGER*4 I
        RECORD /TAX_SHARES/ TX_SHRS
        
        TX_SHRS.TAXPER = 0
        TX_SHRS.BASAMT = 0
        TX_SHRS.RFNAMT = 0
        TX_SHRS.POPWSER = 0
        TX_SHRS.GNUM = 0
        TX_SHRS.EEAAAA = '      '
        TX_SHRS.DRWN = 0
        TX_SHRS.NUMDIVS = 0
        TX_SHRS.NUMXDIVS = 0
        TX_SHRS.DRWDT = 0
        TX_SHRS.NOFFRAC = 0
        TX_SHRS.HASTAX = .FALSE.
        TX_SHRS.PRZAMT = 0
        TX_SHRS.NETPRZAMT = 0
        TX_SHRS.MAX_PRZAMT = - 9223372036854775807
        TX_SHRS.MAX_NETPRZAMT = - 9223372036854775807
        TX_SHRS.MIN_PRZAMT = 9223372036854775807
        TX_SHRS.MIN_NETPRZAMT = 9223372036854775807
        DO I = 1, TAX_SHARES_MAXDIV
            TX_SHRS.GAMSHV(I) = 0
        END DO
        DO I = 1, TAX_SHARES_MAXDIV
            TX_SHRS.GAMEXSHV(I) = 0
        END DO
        DO I = 1, TAX_SHARES_MAXDIV
            TX_SHRS.TOTDIVAMT(I) = 0
        END DO
        DO I = 1, TAX_SHARES_MAXDIV
            TX_SHRS.NETDIVAMT(I) = 0
        END DO
        DO I = 1, TAX_SHARES_MAXDIV
            TX_SHRS.TOTDIVPRZ(I) = 0
        END DO
        DO I = 1, TAX_SHARES_LDATE_LEN
            TX_SHRS.DRDAT(I) = 0
        END DO
        
        END
        
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SUBROUTINE GET_PASSHV(GNUM,DRW,GAMSHV,GAMEXSHV,NUMDIVS,DRDAT,
C                       NOFFRAC)
C
C THIS SUBROUTINE GETS THE SHARE VALUES FOR THE GIVEN PASSIVE GAME
C AND DRAW
C
C INPUTS:
C  GNUM        GAME NUMBER (CLASSICA OR POPULAR)
C  DRW         DRAW NUMBER
C
C OUTPUTS:
C  GAMSHV      SHARE VALUES (IF POPULAR THIS IS THE WINNING SERIE SHARE VALUES)
C  GAMEXSHV    NOT WINNING SERIES SHARE VALUES (POPULAR ONLY)
C  NUMDIVS     TOTAL DIVISIONS
C  DRDAT       DRAW DATE
C  NOFFRAC     NUMBER OF FRACTIONS PER TICKET (FOR POPULAR THE VALUE
C              IS SET TO ONE; FOR CLASSICA THE VALUE IS TAKEN FROM
C              GAME FILE)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE GET_PASSHV(GNUM,DRW,GAMSHV,GAMEXSHV,NUMDIVS,DRDAT,
     *                        NOFFRAC)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
C
        INTEGER*4 MAXDIV
        PARAMETER(MAXDIV = 40)
C
        INTEGER*4 ST
        INTEGER*8 GAMSHV(MAXDIV), GAMEXSHV(MAXDIV)
        INTEGER*4 DRW, NUMDIVS, DRWDT, NOFFRAC
        INTEGER*4 GLUN, FDB(7), GNUM, DIV, GTYP
        INTEGER*2 DRDAT(LDATE_LEN) ! DRAWING DATE
C
        CALL FASTSET(0,GAMSHV,MAXDIV)
        CALL FASTSET(0,GAMEXSHV,MAXDIV)
        NUMDIVS = 0
C
        GTYP = GNTTAB(GAMTYP,GNUM)
        IF (DRW .GT. 0 .AND. GTYP .EQ. TPAS) THEN
          CALL FIND_AVAILABLE_LUN(GLUN,ST)
          IF (ST .NE. 0) THEN
            TYPE*, IAM(), 'Erro a obter uma LUN para o jogo: ',GNUM
            CALL GPAUSE()
            CALL GSTOP(GEXIT_FATAL)
          ENDIF
          CALL OPENW(GLUN,GFNAMES(1,GNUM),4,0,0,ST)
C
          IF (GTYP .EQ. TPAS) THEN
            CALL IOINIT(FDB,GLUN,DPASEC*256)
          ENDIF
          IF (ST .NE. 0) THEN
            CALL FILERR(GFNAMES(1,GNUM),1,ST,0)
            CALL GSTOP(GEXIT_FATAL)
          ENDIF
C
          IF (GTYP .EQ. TPAS) THEN
            CALL READW(FDB,DRW,DPAREC,ST)
            DRDAT(VCDC) = DPAESD
            CALL LCDATE(DRDAT(1))
C
            IF (GNUM .EQ. 8 ) THEN ! CLASSICA
              NOFFRAC = DPANOFFRA
            ELSEIF (GNUM .EQ. 9 ) THEN ! POPULAR
              NOFFRAC = 1
            ENDIF
C
            DO DIV=1,PAGDIV
              ! GET SHARE VALUES
              ! IF POPULAR ALL THIS SHARES BELONGS TO THE WINNING SERIE
              ! (SOME OF THEM ALSO BELONG TO ALL NOT WINNING SERIES)
              GAMSHV(DIV) = DPASHV(DIV)
            ENDDO
            NUMDIVS = MIN(PAGDIV,DPADIV)
C
            IF (GNUM .EQ. 9) THEN ! POPULAR
              ! FIRST, GET SHARE VALUES OF NOT WINNING SERIES
              DO DIV=1,PAGEDV
                GAMEXSHV(DIV) = DPAEXSHV(DIV)
              ENDDO
              ! THEN, FILL THE REMAINING SHARES OF NOT WINNING SERIES
              ! WITH VALUES OF THE WINNING SERIE (THERE ARE VALUES IN
              ! COMMON WITH ALL SERIES)
              DO DIV=1,NUMDIVS
                IF (GAMEXSHV(DIV) .EQ. 0) GAMEXSHV(DIV) = GAMSHV(DIV)
              ENDDO
            ENDIF
          ENDIF
          IF (ST .NE. 0) THEN
            CALL FILERR(GFNAMES(1,GNUM),2,ST,DRW)
          ENDIF
          CALL CLOSEFIL(FDB)
        ENDIF
C
        RETURN
        END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SUBROUTINE CHECKPASTAX(VALREC,BASAMT,RFNAMT,TAXPER,GAMSHV,DRWN,
C                        NOFFRAC,GAMEXSHV,POPWSER,
C                        TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,PRZAMT,
C                        NETPRZAMT,HASTAX)
C
C THIS SUBROUTINE CALCULATES THE NET AMOUNT VALUE FOR EACH PRIZE 
C DIVISION OF PASSIVE GAMES.
C FOR EACH PRIZE DIVISION VALUE GREATER THAN <BASAMT> THE VALUE IS 
C TAXED, ACCORDINGLY TO THE PERCENTAGE VALUE <TAXPER>.
C THE TAX AMOUNT IS TRUNCATED (NOT ROUNDED).
C
C INPUTS:
C  VALREC           VALIDATION RECORD TO PROCESS
C  BASAMT           MINIMUM AMOUNT TO WHICH THE TAX APPLIES
C  RFNAMT           TAX REFUND AMOUNT
C  TAXPER           TAX PERCENTAGE
C  GAMSHR           GAME SHARE VALUES
C  DRWN             DRAW NUMBER
C  NOFFRAC          NUMBER OF FRACTIONS PER TICKET
C  GAMEXSHV         NOT WINNING SERIES SHARE VALUES (POPULAR ONLY)
C  POPWSER          WINNING SERIE (POPULAR ONLY)
C
C OUTPUTS:
C  TOTDIVAMT        TOTAL AMOUNT BY DIVISION FOR MAIN GAME 
C                   (INTEGER*4 * MAXDIV)
C  NETDIVAMT        TOTAL NET AMOUNT BY DIVISION FOR MAIN GAME 
C                   (INTEGER*4 * MAXDIV)
C  TOTDIVPRZ        TOTAL PRIZES BY DIVISION FOR MAIN GAME 
C                   (INTEGER*4 * MAXDIV)
C  PRZAMT           TOTAL PRIZE AMOUNT
C  NETPRZAMT        TOTAL NET PRIZE AMOUNT
C  HASTAX           TRUE IF THE TOTAL PRIZE AMOUNT HAS BEEN TAXED,
C                   FALSE OTHERWISE
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C====== OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CHECKPASTAX (VALREC,BASAMT,RFNAMT,TAXPER,GAMSHV,DRWN,
     *                          NOFFRAC,GAMEXSHV,POPWSER,
     *                          TOTDIVAMT,NETDIVAMT,TOTDIVPRZ,
     *                          PRZAMT,NETPRZAMT,HASTAX)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
C
        INCLUDE 'INCLIB:VALFIL.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
        INCLUDE 'INCLIB:WINUPD.DEF'
C
C=======================================================================
C       VARIABLES
C=======================================================================
C
        INTEGER*4 I
        INTEGER*4 BASAMT, RFNAMT, TAXPER
        INTEGER*4 SHRAMT, TAXAMT
        INTEGER*4 MAXDIV
        PARAMETER(MAXDIV = 40)
        INTEGER*8 GAMSHV(MAXDIV), GAMEXSHV(MAXDIV)
        INTEGER*4 DRWN
        INTEGER*4 NOFFRAC
        INTEGER*4 POPWSER
C
C       DATA STRUCTURES FOR:
C          CLASSICA AND POPULAR

C IT MUST BE NOT LESS THAN THE MAXIMUM NUMBER OF DIVISIONS        
        INTEGER*4 TOTDIVAMT(MAXDIV)
        INTEGER*4 NETDIVAMT(MAXDIV) !
        INTEGER*4 TOTDIVPRZ(MAXDIV) !
        INTEGER*4 PRZAMT, NETPRZAMT
C
        INTEGER*4 PAS_ROUND_VALUE
        INTEGER*4 GTYP, GIND, KGAM, GAM, FRCS
        INTEGER*4 DDRW, DDIV, DSHR
C
        LOGICAL   HASTAX
C
        CALL FASTSET(0,TOTDIVAMT,MAXDIV)
        CALL FASTSET(0,NETDIVAMT,MAXDIV)
        CALL FASTSET(0,TOTDIVPRZ,MAXDIV)
C
        HASTAX = .FALSE.
        TAXAMT = 0
        PRZAMT = 0
        NETPRZAMT = 0
C
        GTYP = VALREC(VGTYP)
        GIND = VALREC(VGIND)
        GAM  = VALREC(VGAM)
C
        IF (GTYP .EQ. TPAS) THEN
          CALL DLOGPAS(VALREC,VDETAIL)
          DO 100 I = 1,VALREC(VPZOFF)
            DDRW = VDETAIL(VDRW,I) ! DRAW
            DDIV = VDETAIL(VDIV,I) ! DIVISION
            DSHR = VDETAIL(VSHR,I) ! NUMBER OF SHARES IN DIVISION
            IF (DDRW .LE. 0) GOTO 100
            IF (GIND .NE. 1 .AND. GIND .NE. 2) GOTO 100
            IF (DDRW .NE. DRWN) GOTO 100
            SHRAMT = 0
            TAXAMT = 0
C
            IF (GIND .EQ. 2) THEN ! POPULAR
              IF (VALREC(VPFRAC) .EQ. POPWSER) THEN
                ! WINNER SERIE
                SHRAMT = PAS_ROUND_VALUE(GAMSHV(DDIV)) / NOFFRAC
              ELSE
                ! NOT A WINNER SERIE
                SHRAMT = PAS_ROUND_VALUE(GAMEXSHV(DDIV)) / NOFFRAC
              ENDIF
            ELSE
              ! CLASSICA
              SHRAMT = PAS_ROUND_VALUE(GAMSHV(DDIV)) / NOFFRAC
            ENDIF
C
            IF (SHRAMT .GT. BASAMT) THEN
              HASTAX = .TRUE.
C TAX AMOUNT (ADD 0.50 TO GET THE ROUNDED VALUE; 
C             REMOVE IT IF YOU WANT THE TRUNCATED VALUE)
C TAXAMT = INT((DFLOAT(SHRAMT - RFNAMT) 
C             * CALPER(TAXPER * 10) + 0.50D0))
              TAXAMT = 
     *          INT((DFLOAT(SHRAMT - RFNAMT) * CALPER(TAXPER * 10)))
            ENDIF
            ! TOTAL AMOUNT
            TOTDIVAMT(DDIV) = TOTDIVAMT(DDIV) + SHRAMT * DSHR
            ! NET AMOUNT
            NETDIVAMT(DDIV) = 
     *          NETDIVAMT(DDIV) + SHRAMT * DSHR - TAXAMT * DSHR
            ! TOTAL PRIZES
            TOTDIVPRZ(DDIV) = TOTDIVPRZ(DDIV) + DSHR
            ! TOTAL AMOUNT AND TOTAL NET AMOUNT
            NETPRZAMT = NETPRZAMT + SHRAMT * DSHR - TAXAMT * DSHR
            PRZAMT = PRZAMT + SHRAMT * DSHR
            GOTO 100
C
100       CONTINUE
C
        ENDIF
C
        RETURN
        END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SUBROUTINE READ_TAX_FIL( TX_SHRS )
C
C This subroutine reads the tax configuration file (adapted from 
C TAXMNG.FOR - lines 2986:3024)
C
C INPUT/OUTPUT:
C   TX_SHRS     Structure holding the various fields necessary
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE READ_TAX_FIL (TX_SHRS)
        IMPLICIT NONE

        INCLUDE '(LIB$ROUTINES)'
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:TAXCONFIG.DEF'
        INCLUDE 'INCLIB:M_ISLN_PJMC.DEF'
        
        RECORD /TAX_SHARES/ TX_SHRS
        INTEGER*4 TAXPER, BASAMT, RFNAMT
        
        INTEGER*4   ST, FDB(7)
        LOGICAL     ISTHERE
C=======================================================================
C       IF FILE TAXCONF.FIL DOESN'T EXIST, EXIT THE PROGRAM
C=======================================================================
C
        TYPE*, IAM(), '       '
        TYPE*, IAM(), 'A obter a configuracao do imposto de selo '
     *     // 'a aplicar...'
C
        INQUIRE(FILE='FILE:TAXCONF.FIL', EXIST=ISTHERE)
        IF (.NOT. ISTHERE) THEN
          TYPE*, IAM(),'       '
          TYPE*, IAM(), 'READ_TAX_FIL - Nao foi encontrado o ficheiro '
     *     // 'TAXCONF.FIL'
          TYPE*, IAM(),'       '
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C=======================================================================
C       OPEN TAX CONFIG FILE AND LOAD CONFIGURATION
C=======================================================================
C
        CALL OPENX(1,'FILE:TAXCONF.FIL',4,0,0,ST)
        CALL IOINIT(FDB,1,TXCF_SEC*256)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'READ_TAX_FIL - Nao foi possivel abrir o '
     *      // 'ficheiro TAXCONF.FIL'
          TYPE*, IAM(), '       '
          CALL CLOSEFIL(FDB)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
        CALL READW(FDB,1,TXCF_REC,ST)
        IF (ST .NE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'READ_TAX_FIL - Nao foi possivel ler o '
     *      // 'ficheiro TAXCONF.FIL'
          TYPE*, IAM(), '       '
          CALL CLOSEFIL(FDB)
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
        CALL CLOSEFIL(FDB)        
C
C=======================================================================
C       SET THE TAX PERCENTAGE, BASE AMOUNT AND REFUND AMOUNT
C=======================================================================
C
        TAXPER = TXCF_LNTAX
        BASAMT = TXCF_LNBSAMNT
        RFNAMT = TXCF_LNTAXRFN
        
        TX_SHRS.TAXPER = TAXPER
        TX_SHRS.BASAMT = BASAMT
        TX_SHRS.RFNAMT = RFNAMT
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SUBROUTINE READ_SHR_FIL ( TX_SHRS )
C
C This subroutine reads the vpf configuration file (adapted from 
C TAXMNG.FOR - lines 3184:3313)
C
C INPUT/OUTPUT:
C   TX_SHRS     Structure holding the various fields necessary
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE READ_SHR_FIL (TX_SHRS)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:VALPASFIL.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:PASIOSUBS.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:M_ISLN_PJMC.DEF'

        RECORD /TAX_SHARES/ TX_SHRS
        
        INTEGER*4 TAXPER, BASAMT, RFNAMT
C
        CHARACTER*4 GSHDSC
C
        CHARACTER*6 EEAAAA, EXTRACAOANO
        INTEGER*4   EXTRACAO, ANO, SZ, ST
        INTEGER*4   YESNO
        LOGICAL     FINREP /.FALSE./      ! FINAL REPORTING?
        LOGICAL     ISTHERE
        INTEGER*4   CTIM(2), CDAT(8), FDB(7)
        INTEGER*4   INDEMIS, EMIOFF, IPAS
        INTEGER*4   DRWN, GETDRW, GNUM
        INTEGER*2   DATEBUF(12)
C
        INTEGER*4 TUBSIZ
        PARAMETER (TUBSIZ=I4BUCSIZ*7)
        INTEGER*4 VPFBUF(TUBSIZ)
C
C
        INTEGER*4 REP_LUN
        INTEGER*4 SAP_LUN
        INTEGER*4 VPF_LUN
C
        CHARACTER*43 REPFILNAM ! REPORT FILE NAME
        CHARACTER*25 SAPFILNAM ! INTERFACE FILE FILE NAME FOR SAP
        CHARACTER*18 VPFFILNAM ! VPF FILE NAME
C
        CHARACTER*20 VPF_CFILNAM   !
        INTEGER*4    VPF_IFILNAM(5)
        EQUIVALENCE (VPF_IFILNAM, VPF_CFILNAM)
C
        INTEGER*4 MAXDIV
        PARAMETER(MAXDIV = 40)
        INTEGER*4 NUMDIVS
        INTEGER*8 GAMSHV(MAXDIV)
! NOT WINNING SERIES TOTAL DIVISIONS (POPULAR ONLY)
        INTEGER*4 NUMEXDIVS        
! NOT WINNING SERIES SHARE VALUES (POPULAR ONLY)
        INTEGER*8 GAMEXSHV(MAXDIV) 
! WINNING SERIE (POPULAR ONLY)
        INTEGER*4 POPWSER          
        INTEGER*4 I
        CHARACTER*50 C50DIV
        INTEGER*4 TOTSAPREC
        INTEGER*4 TOTREADVPF, TOTVALREC, TOTINVREC, TOTNOTAX, TOTTAX
        INTEGER*2 DRDAT(LDATE_LEN) ! DRAWING DATE
C
        INTEGER*4 TOTDIVAMT(MAXDIV), NETDIVAMT(MAXDIV)
     *          , TOTDIVPRZ(MAXDIV)
        INTEGER*4 PRZAMT, NETPRZAMT, TAXBLAMT
        INTEGER*4 TOTPRZAMT, TOTNETPRZAMT, TOTTAXBLAMT
        INTEGER*4 PAS_ROUND_VALUE
        INTEGER*4 DRWDT, NOFFRAC
        CHARACTER*9 TCKT /'         '/
        CHARACTER*132 LIBCMD
C
        LOGICAL VALDRW /.FALSE./
        LOGICAL HASTAX /.FALSE./
C
        RECORD /STPASFDB/  PASFDB
        RECORD /STPASREC/  PASREC
C
        CHARACTER*11 CKEY


        TAXPER = TX_SHRS.TAXPER
        BASAMT = TX_SHRS.BASAMT
        RFNAMT = TX_SHRS.RFNAMT
        EEAAAA = TX_SHRS.EEAAAA
        GNUM = TX_SHRS.GNUM
        DRWN = TX_SHRS.DRWN
C
C=======================================================================
C       CHECK IF EMISSION IS IN MEMORY
C=======================================================================
C
        IF (GNUM .EQ. 8) THEN
          IPAS = 1 ! CLASSICA
        ELSEIF(GNUM .EQ. 9) THEN
          IPAS = 2 ! POPULAR
        ENDIF
C
        INDEMIS = -1
        DO EMIOFF=1, PAGEMI
          IF (PASEMIS(EMIOFF,IPAS) .EQ. DRWN) THEN
            INDEMIS = EMIOFF
            EXIT
          ENDIF
        ENDDO
C
        IF (INDEMIS .LE. 0) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'VPASPJMC - EXTRACAO nao esta em memoria'
          ST = -1
          RETURN
        ENDIF
C
C=======================================================================
C       CHECK IF RESULTS ARE FINAL
C=======================================================================
C
        IF (PASSTS(INDEMIS,IPAS) .NE. GFINAL) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'VPASPJMC - Premios nao apurados para a '
     *      // 'EXTRACAO ', EEAAAA
          TYPE*, IAM(), '       '
          ST = -1
          RETURN
        ENDIF

C
C=======================================================================
C       LOAD SHARE VALUES FOR THE CHOSEN GAME AND DRAW FROM DISK
C=======================================================================
C
        TYPE*, IAM(), '       '
        TYPE*, IAM(), 'A obter o valor das shares da EXTRACAO' 
     *      // ' ' // EEAAAA
        IF (GNUM .EQ. 8) THEN ! CLASSICA
            TYPE*, IAM(), 'da LOT. CLASSICA...'
        ENDIF
        IF (GNUM .EQ. 9) THEN ! POPULAR
            TYPE*, IAM(), 'da LOT. POPULAR...'
        ENDIF
        CALL GET_PASSHV(GNUM,DRWN,GAMSHV,GAMEXSHV,NUMDIVS,DRDAT,NOFFRAC)
C
        DO I = 1, LDATE_LEN
            TX_SHRS.DRDAT(I) = DRDAT(I)
        END DO
        
        IF (GNUM .EQ. 8) THEN ! CLASSICA
          DO I=1,NUMDIVS
            WRITE(C50DIV,'(A,I2,A)') ' Divisao ',I,': '
            TYPE*, IAM(), TRIM(C50DIV), ' '
     *           , CMONY(PAS_ROUND_VALUE(GAMSHV(I)),13,VALUNIT)
            TX_SHRS.GAMSHV(I) = GAMSHV(I)
          ENDDO
          TX_SHRS.NUMDIVS = NUMDIVS
          TX_SHRS.NOFFRAC = NOFFRAC
        ENDIF
C
        IF (GNUM .EQ. 9) THEN ! POPULAR
          WRITE(C50DIV,'(A,I0,A)') '             Serie Sorteada ('
     *      , POPWSER, ')   Restantes Series'
          TYPE*, IAM(), TRIM(C50DIV)
          DO I=1,NUMDIVS
            WRITE(5,'(1X,A,A,I2,A,A13,A,A13)') IAM(), ' Divisao '
     *          , I, ':      ',
     *         CMONY(PAS_ROUND_VALUE(GAMSHV(I)),13,VALUNIT),'      ',
     *         CMONY(PAS_ROUND_VALUE(GAMEXSHV(I)),13,VALUNIT)
            TX_SHRS.GAMSHV(I) = GAMSHV(I)
            TX_SHRS.GAMEXSHV(I) = GAMEXSHV(I)
          ENDDO
          TX_SHRS.NUMDIVS = NUMDIVS
          TX_SHRS.NOFFRAC = NOFFRAC
        ENDIF
        
C
C=======================================================================
C       LOAD WINNING SERIE FROM MEMORY (POPULAR)
C=======================================================================
C
        IF (GNUM .EQ. 9) THEN
          POPWSER = PASWSER(INDEMIS,IPAS)
          IF (POPWSER .EQ. 0) THEN
            TYPE*, IAM(), '       '
            TYPE*, IAM(),'VPASPJMC - Serie sorteada igual a zero!'
            TYPE*, IAM(), '       '
            CALL GSTOP(GEXIT_FATAL)
          ENDIF
        ENDIF 
        TX_SHRS.POPWSER = POPWSER
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C SUBROUTINE STRUCT_TAX_SHARES_CHECKPASTAX ( TX_SHRS )
C
C Calls the subroutine CHECKPASTAX using the values stored at the 
C TAX_SHRS structure 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE STRUCT_TAX_SHARES_CHECKPASTAX ( TX_SHRS, VALREC )
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:VALPASFIL.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
        INCLUDE 'INCLIB:DESVAL.DEF'
        INCLUDE 'INCLIB:VDETAIL.DEF'
        INCLUDE 'INCLIB:PASIOSUBS.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:TAXCONFIG.DEF'

        INCLUDE 'INCLIB:M_ISLN_PJMC.DEF'
        
        RECORD /TAX_SHARES/ TX_SHRS

        TX_SHRS.HASTAX = .FALSE.

C
C=======================================================================
C       CHECK IF VALIDATION RECORD SHOULD BE SUBJECT TO TAX
C=======================================================================
C
        IF (VALREC(VPAMT) .LE. TX_SHRS.BASAMT) THEN
            TX_SHRS.NETPRZAMT = VALREC(VPAMT) 
            TX_SHRS.PRZAMT = VALREC(VPAMT)
            
            TX_SHRS.MAX_NETPRZAMT = 
     *          MAX (TX_SHRS.MAX_NETPRZAMT, TX_SHRS.NETPRZAMT)
            TX_SHRS.MIN_NETPRZAMT = 
     *          MIN (TX_SHRS.MIN_NETPRZAMT, TX_SHRS.NETPRZAMT)

            TX_SHRS.MAX_PRZAMT = 
     *          MAX (TX_SHRS.MAX_PRZAMT, TX_SHRS.PRZAMT)
            TX_SHRS.MIN_PRZAMT = 
     *          MIN (TX_SHRS.MIN_PRZAMT, TX_SHRS.PRZAMT)
            RETURN
        ENDIF

C=======================================================================
C       CHECK IF PRIZE HAS TO BE TAXED AND IF SO CALCULATE TAX VALUES
C=======================================================================
C
        CALL CHECKPASTAX (
     *        VALREC
     *      , TX_SHRS.BASAMT
     *      , TX_SHRS.RFNAMT
     *      , TX_SHRS.TAXPER
     *      , TX_SHRS.GAMSHV
     *      , TX_SHRS.DRWN
     *      , TX_SHRS.NOFFRAC
     *      , TX_SHRS.GAMEXSHV
     *      , TX_SHRS.POPWSER
     *      , TX_SHRS.TOTDIVAMT
     *      , TX_SHRS.NETDIVAMT
     *      , TX_SHRS.TOTDIVPRZ
     *      , TX_SHRS.PRZAMT
     *      , TX_SHRS.NETPRZAMT
     *      , TX_SHRS.HASTAX)

C
C=======================================================================
C       CHECK IF THE PRZAMT VALUE EQUALS VALREC(VPAMT) VALUE.
C       IF THEY DIFFER, IT MEANS THAT SOME PROBLEM OCCURRED
C       DURING THE CALCULATION OF NET PRIZES
C=======================================================================
C
        IF (TX_SHRS.PRZAMT .NE. VALREC(VPAMT)) THEN
          TYPE*, IAM(), '       '
          TYPE*, IAM(), 'VPASPJMC - Erro no calculo do premio liquido!'
          TYPE*, IAM(), '         Valor liquido do premio recalculado'
     *          // 'com'
          TYPE*, IAM(), '         base nas divisoes premiadas difere do'
          TYPE*, IAM(), '         valor liquido do premio previamente '
     *          // 'apurado!'
          TYPE*, IAM(), '       '
          CALL GPAUSE
        ENDIF
     
            TX_SHRS.MAX_NETPRZAMT = 
     *          MAX (TX_SHRS.MAX_NETPRZAMT, TX_SHRS.NETPRZAMT)
            TX_SHRS.MIN_NETPRZAMT = 
     *          MIN (TX_SHRS.MIN_NETPRZAMT, TX_SHRS.NETPRZAMT)

            TX_SHRS.MAX_PRZAMT = 
     *          MAX (TX_SHRS.MAX_PRZAMT, TX_SHRS.PRZAMT)
            TX_SHRS.MIN_PRZAMT = 
     *          MIN (TX_SHRS.MIN_PRZAMT, TX_SHRS.PRZAMT)

        END 
        
