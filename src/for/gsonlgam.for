C
C     FILE   : GSONLGAM.FOR
C     AUTHOR : J.H.R
C     VERSION: 01            DATE: 09 / 02 / 2001
C
C
C V01 JHR INITIAL RELEASE FOR PORTUGAL PROJECT
C
C GSALER ON LINE GAMES, THIS PROGRAM RUNS GSALES LOADER FOR ON LINE GAMES
C
C     **************************************************************************
C
C        THIS ITEM IS THE PROPERTY OF GTECH CORPORATION, POVIDENCE, RHODE
C     ISLAND, AND CONTAINS CONFIDENTIAL AND TRADE SECRET INFORMATION. IT MAY
C     NOT BE TRANSFERRED FROM THE CUSTODY OR CONTROL OF GTECH EXCEPT AS AUTO -
C     RIZED IN WRITING BY AN OFFICER OF GTECH. NEITHER THIS ITEM NOR THE
C     INFORMATION IT CONTAINS MAY BE USED, TRANSFERRED, REPRODUCED, PUBLISHED
C     OR DISCLOSED, IN WHOLE OR IN PART, AND DIRECTLY OR INDIRECTLY, EXCEPT AS
C     EXPRESSLY AUTHORIZED BY AN OFFICER OR GTECH, PURSUANT TO WRITTEN AGREEMENT
C
C     Copyright 2000 GTECH Corporation. All Rigth Reserved
C
C     **************************************************************************
C
C THIS PROGRAM RUNS GSALES LOADERS FOR ON LINE GAMES ( INCLUDING PASSIVE )
C
C=======OPTIONS /CHECK = NOOVERFLOW /EXT
      PROGRAM GSONLGAM
      IMPLICIT NONE
C
C INCLUDES DEFINITION TO RUN GSALES ON LINE GAMES LOADER
C
      INCLUDE '(LIB$ROUTINES)'
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:GSALES.DEF'
C
C CONSTANT PARAMETER DEFINITION TO RUN GSALES ON LINE GAMES LOADER
C
      INTEGER * 4 VERSION           ! ON LINE GAMES VERSION
C
C INITIATE CONSTANT PARAMETER DEFINITION TO RUN GSALES ON LINE GAMES LOADER
C
      PARAMETER(VERSION = 1)        ! ON LINE GAMES VERSION
C
C VARIABLES DEFINITION TO RUN GSALES ON LINE GAMES LOADER
C
      INTEGER * 4 CNTA              ! COUNTER A
      INTEGER * 4 GNUM              ! GAME NUMBER
      INTEGER * 4 GIND              ! GAME INDEX
      INTEGER * 4 GTYP              ! GAME TYPE
      INTEGER * 4 PSONLG            ! PASSIVE ON LINE GAME TYPE
C
      CHARACTER * 23 FILENAME       ! GSALES FILE NAME LOADER
C
C FUNCTIONS DEFINITION TO RUN GSALES ON LINE GAMES LOADER
C
      CHARACTER * 06 GET_FRACTION_TICKET   ! NUMBER OF FRACTIONS BY TICKET
      CHARACTER * 12 GET_BET_PRIZE         ! SIMPLE BET PRIZE
C
C DISPLAY USER INFORMATION ( RUNNING ON LINE GAME LOADER )
C
      CALL COPYRITE
      TYPE *, IAM()
      TYPE *, IAM(), 'Running Gsales On Line Games Loader'
      TYPE *, IAM()
C
C INITIATE VARIABLES TO RUN GSALES ON LINE GAMES LOADER
C
      FILRECCNT = 0
C
C SET ON LINE GSALES FILE NAME ( YYYYDDMM_OGAME.FIL )
C
      WRITE(FILENAME, 100) GET_YYYYMMDD_CDC(DAYCDC)
C
C IF ON LINE GSALES FILE GAME EXIST, DELETE IT
C
      CALL DELETE_FILE_NAME(FILENAME)
C
C GET FREE IDENTIFICATION FILE NUMBER
C
      IDFIL = GET_FREE_IDFIL(0)
C
C OPEN YYYYDDMM_OGAME.FIL ( ASCII FILE, ALTHOUGH IT'S EXTENSION IS '.FIL' )
C
      CALL OPEN_FILE_NAME(IDFIL, FILENAME)
C
C WRITE ON LINE GAME HEADER
C
      CALL GSHEADER(VERSION)
C
C LOOP TO GET ACTIVE GAMES AND WRITE INFORMATION IN FILE
C
      DO 1000 GNUM = 1, MAXGAM
C
C GET GAME TYPE AND GAME INDEX, IF IT'S NOT ACTIVE GAME, GO TO CHECK NEXT GAME 
C
        GTYP = GNTTAB(GAMTYP, GNUM)
        GIND = GNTTAB(GAMIDX, GNUM)
        IF(GTYP .LT. 1 .OR. GTYP .GT. MAXTYP) GOTO 1000
        IF(GIND .LT. 1 .OR. GIND .GT. MAXIND) GOTO 1000
C
C SET IF ACTIVE GAME IT'S ON LINE GAME OR PASSIVE GAME
C
        PSONLG = 2
        IF(GTYP .EQ. TPAS) PSONLG = 1
C
C WRITE INFORMATION IN FILE REGISTER 
C
        WRITE(FILREC, 200) 
     *
     *        PSONLG,                                ! GAME TYPE
     *        GNUM,                                  ! GAME NUMBER
     *        GET_FRACTION_TICKET(GTYP, GIND),       ! FRACTION / TICKET
     *        GET_BET_PRIZE(GTYP, GIND),             ! SIMPLE BET PRIZE
     *        GSNAMES(GNUM),                         ! SHORT NAME
     *        (GLNAMES(CNTA, GNUM), CNTA = 1, 4)     ! LONG NAME
C
        FILRECLNG =1 + 2 + 6 + 12 + 20 + 40
C
C WRITE INFORMATION IN GSALES FILE AND INCREASE NUMBER OF FILE RECORDS WRITEN
C
        WRITE(IDFIL, 400) FILREC(1:FILRECLNG)
        FILRECCNT = FILRECCNT + 1
C
C END OF LOOP TO GET ACTIVE GAMES AND WRITE INFORMATION IN FILE
C
1000    CONTINUE
C
C WRITE ON LINE GAME FOOTER
C
      CALL GSFOOTER
C
C CLOSE YYYYDDMM_OGAME.FIL GSALES LOADER FILE
C
      CALL USRCLOS1(IDFIL)     
C
C DISPLAY MESSAGE TO USER
C
      TYPE *, IAM()
      TYPE *, IAM(), 'Generated On Line GSales File: ', FILENAME
      TYPE *, IAM()
C
C CREATE GSALES END FILE ( TO GSALES MEANS END OF PROCEDURE )
C
      WRITE(FILENAME, 300) GET_YYYYMMDD_CDC(DAYCDC)
      CALL CREATE_GSALES_END_FILE(FILENAME)
C 
C GSALES ON LINE LOADER ENDS OK
C
      CALL GSTOP(GEXIT_SUCCESS)
C
C FORMATS DEFINITION TO RUN GSALES ON LINE GAMES LOADER
C
100   FORMAT('valx:', A8, '_ogame.fil')
200   FORMAT(I1.1, I2.2, A6, A12, A4, 16(' '), 4A4, 24(' '))
300   FORMAT('valx:', A8, '_ogame.fin')
400   FORMAT(A)
C
C THIS IS THE END TO RUN GSALES ON LINE GAMES LOADER
C
      END


C ******************************************************************************
C
C     SUBROUTINE: GET_FRACTION_TICKET
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 19 / 05 / 2001
C
C ******************************************************************************
C
C FUNCTION TO GET NUMBER OF FRACTION BY TICKET FOR PASSIVE GAMES
C
C=======OPTIONS /CHECK = NOOVERFLOW /EXT
      CHARACTER * 06 FUNCTION GET_FRACTION_TICKET(GTYP, GIND)
      IMPLICIT NONE
C
C INCLUDES DEFINITION TO GET NUMBER OF FRACTION BY TICKET FOR PASSIVE GAMES
C
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:PASCOM.DEF'
C
C PARAMETERS DEFINITION TO GET NUMBER OF FRACTION BY TICKET FOR PASSIVE GAMES
C
      INTEGER * 4 GTYP              ! GAME TYPE
      INTEGER * 4 GIND              ! GAME INDEX
C
C VARIABLES DEFINITION TO GET NUMBER OF FRACTION BY TICKET FOR PASSIVE GAMES
C
      INTEGER * 4 NUMFRACT          ! NUMBER OF FRACTIONS
C
C IF GAMES IS NOT PASSIVE GAME SET BLANKS AND RETURN
C
      IF(GTYP .NE. TPAS) THEN
        WRITE(GET_FRACTION_TICKET, 100)
        RETURN
      ENDIF
C
C SET NUMBER OF FRACTION BY TICKET FOR PASSIVE GAMES
C
      NUMFRACT = PASNOFFRA(CURDRW, GIND)
      WRITE(GET_FRACTION_TICKET, 200) NUMFRACT
      RETURN
C
C FORMATS DEFINITION TO GET NUMBER OF FRACTION BY TICKET FOR PASSIVE GAMES
C
100   FORMAT(6(' '))
200   FORMAT(I6.6)
C
C THIS IS THE END TO GET NUMBER OF FRACTION BY TICKET FOR PASSIVE GAMES
C
      END


C ******************************************************************************
C
C     SUBROUTINE: GET_BET_PRIZE
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 19 / 05 / 2001
C
C ******************************************************************************
C
C FUNCTION TO GET SIMPLE BASE PRIZE FOR EACH GAME ( GAME TYPE / INDEX )
C
C=======OPTIONS /CHECK = NOOVERFLOW /EXT
      CHARACTER * 12 FUNCTION GET_BET_PRIZE(GTYP, GIND)
      IMPLICIT NONE
C
C INCLUDES DEFINITION TO GET SIMPLE BASE PRIZE FOR EACH GAME
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:LTOCOM.DEF'
      INCLUDE 'INCLIB:SPTCOM.DEF'
      INCLUDE 'INCLIB:TGLCOM.DEF'
      INCLUDE 'INCLIB:KIKCOM.DEF'
      INCLUDE 'INCLIB:PASCOM.DEF'
      INCLUDE 'INCLIB:GSALES.DEF'
C
C PARAMETERS DEFINITION TO GET SIMPLE BASE PRIZE FOR EACH GAME
C
      INTEGER * 4 GTYP              ! GAME TYPE
      INTEGER * 4 GIND              ! GAME INDEX
C
C VARIABLES DEFINITION TO GET SIMPLE BASE PRIZE FOR EACH GAME
C
      INTEGER * 4 EMISPRZ           ! EMISION PRIZE
C
C FUNCTIONS DEFINITION TO GET SIMPLE BASE PRIZE FOR EACH GAME
C
      INTEGER * 4 GET_EMISION_PRIZE ! GET EMISION BASE PRIZE
C
C SET BASE PRIZE WITH BLANCKS FOR NOT ACTIVE GAMES 
C
C ( ACTIVE GAMES: LOTTO, SPORTS, JOKER, RESULTS AND PASSIVE )
C
      WRITE(GET_BET_PRIZE, 100)
C
C GO TO EACH GAME TYPE TO GO GET BASE PRIZE
C
      IF(GTYP .EQ. TLTO) GOTO 1000
      IF(GTYP .EQ. TSPT) GOTO 2000
      IF(GTYP .EQ. TKIK) GOTO 3000
      IF(GTYP .EQ. TTGL) GOTO 4000
      IF(GTYP .EQ. TPAS) GOTO 5000
C
C SET MESSAGE TO USER, WE HAVE ONE ACTIVE GAME NOT IMPLEMENTED IN GSALES
C
      TYPE *, IAM()
      TYPE *, IAM(), 'Atention, We Have An Active Game That'
      TYPE *, IAM(), 'Is Not Implemented In Gsales Loaders'
      TYPE *, IAM()
      TYPE *, IAM(), 'Game Type : ', GTYP
      TYPE *, IAM(), 'Game Index: ', GIND
      TYPE *, IAM()
      RETURN
C
C SET BASE PRIZE FOR LOTTO GAMES
C
1000  CONTINUE
      WRITE(GET_BET_PRIZE, 200) PRNT_AMT(LTOPRC(GIND), 4)
      RETURN
C
C SET BASE PRIZE FOR SPORTS GAMES
C
2000  CONTINUE
      WRITE(GET_BET_PRIZE, 200)  PRNT_AMT(SPTPRC(GIND), 4)
      RETURN
C
C SET BASE PRIZE FOR JOKER ( KIKER ) GAMES 
C
3000  CONTINUE
      WRITE(GET_BET_PRIZE, 200) PRNT_AMT(KIKPRC(GIND), 4)
      RETURN
C
C SET BASE PRIZE FOR RESULTS GAMES
C
4000  CONTINUE
      WRITE(GET_BET_PRIZE, 200) PRNT_AMT(TGLPRC(GIND), 4)
      RETURN
C
C SET BASE PRIZE FOR PASSIVE GAMES
C
5000  CONTINUE
      WRITE(GET_BET_PRIZE, 200) PRNT_AMT(PASPRC(CURDRW, GIND), 2)
      IF(PASESD(CURDRW, GIND) .EQ. DAYCDC) THEN
        EMISPRZ = GET_EMISION_PRIZE(PASEMIS(CURDRW, GIND) - 1, GIND)
        WRITE(GET_BET_PRIZE, 200) PRNT_AMT(EMISPRZ, 2)
      ENDIF
      RETURN
C
C FORMATS DEFINITION TO GET SIMPLE BASE PRIZE FOR EACH GAME
C
100    FORMAT(12(' '))
200    FORMAT(A12)
C
C THIS IS THE END TO GET SIMPLE BASE PRIZE FOR EACH GAME
C
       END


C ******************************************************************************
C
C     SUBROUTINE: GET_EMISION_PRIZE
C     AUTHOR    : J.H.R
C     VERSION   : 01            DATE: 21 / 05 / 2001
C
C ******************************************************************************
C
C FUNCTION TO GET EMISION BASE PRIZE FOR SELECTED EMISION NUMBER
C
C=======OPTIONS /CHECK = NOOVERFLOW /EXT
      INTEGER * 4 FUNCTION GET_EMISION_PRIZE(EMINUM, GIND)
      IMPLICIT NONE
C
C INCLUDES DEFINITION GET EMISION BASE PRIZE FOR SELECTED EMISION NUMBER
C
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:PASCOM.DEF'
C
C PARAMETERS DEFINITION TO GET EMISION BASE PRIZE FOR SELECTED EMISION NUMBER
C
      INTEGER * 4 EMINUM            ! EMISION NUMBER
      INTEGER * 4 GIND              ! GAME INDEX
C
C VARIABLES DEFINITION TO GET EMISION BASE PRIZE FOR SELECTED EMISION NUMBER
C
      INTEGER * 4 EMISCNT           ! EMISION COUNTER
C
C SET START EMISION BASE PRIZE TO ZERO
C
      GET_EMISION_PRIZE = 0
C
C IF EMISION NUMBER IS LESS THAN ZERO RETURN BASE PRIZE ZERO
C
      IF(EMINUM .LE. 0) RETURN
C
C SEARCH EMISION NUMBER IN MEMORY
C
      DO EMISCNT = CURDRW, PAGEMI
        IF(PASEMIS(EMISCNT, GIND) .EQ. EMINUM) THEN
          GET_EMISION_PRIZE = PASPRC(EMISCNT, GIND)
          RETURN                   
        ENDIF
      ENDDO
C
C FUNCTION DOES NOT FOUND EMISION NUMBER IN MEMORY
C
      RETURN
C
C THIS IS THE END TO GET EMISION BASE PRIZE FOR SELECTED EMISION NUMBER
C
      END



