C
C     FILE   : SRTFLD2.FOR
C     AUTHOR : J.H.R AND E.P.H.
C     VERSION: 01            DATE: 01 / 10 / 2001
C
C
C PROCEDURE TO SORT AGENT FILE CONFIGURATION USING NEXT CODES: DISTRIBUTION
C LINE ( LD ), RECEPTION CENTER ( CR ), AND AGENT NUMBER ( AGT )
C
C NOTE:
C       THIS FUNCTION GET AGENT CONFIGURED IN AGT_LOOKUP_TER TABLE
C
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
C THIS FUNCTION SORT AGENT TERMINALS TABLE BY DISTRIBUTION LINE, RECEPTION
C CENTER AND AGENT NUMBER
C
C=======OPTIONS /CHECK = NOOVERFLOW /EXT
      SUBROUTINE SRTFLD2(NUM_SORT_ITEMS, AGTSRTTBL)
      IMPLICIT NONE
C
C INCLUDES DEFINITION TO SORT AGENT TABLE BY LD, CR AND AGENT NUMBER
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:AGTINF.DEF'
      INCLUDE 'INCLIB:AGTCOM.DEF'
      INCLUDE 'INCLIB:RECAGT.DEF'
C
C PARAMETERS DEFINITION TO SORT AGENT TABLE BY LD, CR AND AGENT NUMBER
C
      INTEGER * 4 NUM_SORT_ITEMS          ! NUMBER OF SORT ITEMS IN THE TABLE
      INTEGER * 4 AGTSRTTBL(NUMAGT)       ! AGENT SORTED TABLE
C
C VARIABLES DEFINITION TO SORT AGENT TABLE BY LD, CR AND AGENT NUMBER
C
      INTEGER * 4 LOOP_CNT                ! LOOP COUNTER
      INTEGER * 4 AGT                     ! AGENT NUMBER
      INTEGER * 4 TERM                    ! TERMINAL NUMBER
      INTEGER * 4 DIS_LINE                ! DISTRIBUTION LINE
      INTEGER * 4 REP_CENTER              ! RECEPTION CENTER
      INTEGER * 4 SRTDTA(3, NUMAGT)       ! SORTED INFORMATION DATA
      INTEGER * 4 FSTS                    ! FUNCTION STATUS
C
C INITIATE VARIABLES TO SORT AGENT TABLE BY LD, CR AND AGENT NUMBER
C
      LOOP_CNT = 1
      CALL FASTSET(0, AGTSRTTBL, NUMAGT)
      CALL FASTSET(0, SRTDTA, 3 * NUMAGT)
C
C DISPLAY USER INFORMATION ( SORTING AGENT SALES FILE )
C
      TYPE *, IAM()
      TYPE *, IAM(), 'Sorting Agent Sales File Reports ...'
      TYPE *, IAM()
C
C GET TERMINAL NUMBER AND GET INFORMATION FORM THE ASF.FIL FILE
C
1000  CONTINUE
      TERM = AGT_LOOKUP_TER(LOOP_CNT)
      CALL READASF(TERM, ASFREC, FSTS)
      IF(FSTS .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Error Reading Agent File For Terminal: ', TERM
        TYPE *, IAM()
        CALL GPAUSE 
      ENDIF
C
C CHECK IF ACTIVE AGENT ( IF NOT GO TO READ NEXT TERMINAL AGENT NUMBER )
C
      AGT = 0
      CALL ASCBIN(ASFINF, SAGNO, LAGNO, AGT, FSTS)
      IF(FSTS .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Error Getting Agent Number For Terminal: ', TERM
        TYPE *, IAM()
        CALL GPAUSE 
       ENDIF
       IF(AGT .LE. 0) GOTO 1000
C
C GET INFORMATION FROM ASF.FIL FILE TO BE SORTED ( DISTRIBUTION LINE )
C
      CALL ASCBIN(ASFINF, SLIND, LLIND, DIS_LINE, FSTS)
      IF(FSTS .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Error Getting Distribution Line For Terminal: ', TERM
        TYPE *, IAM()
        DIS_LINE = 0
      ENDIF
C
C GET INFORMATION FROM ASF.FIL FILE TO BE SORTED ()
C
      CALL ASCBIN(ASFINF, SCENR, LCENR, REP_CENTER, FSTS)
      IF(FSTS .NE. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Error Getting Reception Center For Terminal: ', TERM
        TYPE *, IAM()
        REP_CENTER = 0
      ENDIF
C
C PREPARE INFORMATION TO BE SORTED ( PREPARE SORT TABLES )
C
      AGTSRTTBL(LOOP_CNT) = TERM
      SRTDTA(1, LOOP_CNT) = DIS_LINE
      SRTDTA(2, LOOP_CNT) = REP_CENTER
      SRTDTA(3, LOOP_CNT) = AGT
C
C GO TO READ NEXT AGENT INFORMATION
C
      LOOP_CNT = LOOP_CNT + 1
      IF(LOOP_CNT .GT. AGT_LOOKUP_CNT) GOTO 2000
      GOTO 1000
C
C UPDATED OUTPUT VARIABLES AND EXIT SORT FUNCTION
C
2000  CONTINUE
      NUM_SORT_ITEMS = LOOP_CNT - 1
C
C SORT ALL INFORMATION USING QSORT 
C
      CALL I4SHELL(AGTSRTTBL, NUM_SORT_ITEMS, SRTDTA, 3)
C
C DISPLAY USER INFORMATION ( SORTING AGENT SALES HAS BEEN SORTED )
C
      TYPE *, IAM()
      TYPE *, IAM(), 'Agent Sales File Reports Has Been Sorted ...'
      TYPE *, IAM()
C
C THIS IS THE END TO SORT AGENT TABLE BY LD, CR AND AGENT NUMBER
C
      END

