C
C V02 14-NOV-2003 FRP Modify for Batch2 Totobola Changes.
C
C     FILE   : SPROLEXTDRW.FOR
C     AUTHOR : J.H.R
C     VERSION: 01            DATE: 19 / 12 / 2000
C
C     THIS FUNCTION ALLOW ADD ADITIONAL MONEY TO POOLS CARRIED OVER FROM
C     POOLS CARRIED OVER OF ANOTHER GAME ( THIS GAME SHOULD BE SAME TYPE )
C     THIS FUNCTION IT'S PREPARE TO SUPORT TOTOBOLA ( SPORTS ) GAME TYPES
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
C FUNCTION TO ADD ADITIONAL POOLS FROM EXTRAORDINARY GAME FOR SPORTS GAME TYPE 
C
C=======OPTIONS /CHECK = NOOVERFLOW /EXTEND
      SUBROUTINE SPROLEXTDRW(FGNUM, FDRAW, FILREC)
      IMPLICIT NONE
C
C INCLUDES DEFINITION TO ADD ADITIONAL POOLS FROM EXTRAORDINARY GAME
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:RECSCF.DEF'
      INCLUDE 'INCLIB:GTNAMES.DEF'
      INCLUDE 'INCLIB:DSPREC.DEF'
C
C FUNCTION PARAMETERS TO ADD ADITIONAL POOLS FROM EXTRAORDINARY GAME
C
      INTEGER * 4 FGNUM               ! ACTUAL GAME NUMBER
      INTEGER * 4 FDRAW               ! ACTUAL DRAW NUMBER
      INTEGER * 4 FILREC(*)           ! FILE REGISTER
C
C CONSTANT PARAMETERS TO ADD ADITIONAL POOLS FROM EXTRAORDINARY GAME
C
      INTEGER * 4 IDFIL              ! IDENTIFICATION FILE
C
C INITIATE CONSTANT PARAMETERS TO ADD ADITIONAL POOLS FROM EXTRAORDINARY GAME
C
      PARAMETER(IDFIL = 11)          ! IDENTIFICATION FILE
C
C VARIABLES DEFINITION TO ADD ADITIONAL POOLS FROM EXTRAORDINARY GAME 
C
      INTEGER * 4 FSTS               ! FUNCTION STATUS
      INTEGER * 4 FGTYPE             ! ACTUAL GAME TYPE
      INTEGER * 4 SGNUM              ! EXTRAORDINARY GAME NUMBER
      INTEGER * 4 SDRAW              ! EXTRAORDINARY DRAW NUMBER
      INTEGER * 4 SGTYPE             ! EXTRAORDINARY GAME TYPE
      INTEGER * 4 CNTA               ! COUNTER A
      INTEGER * 4 USEL               ! USER SELECTION
      INTEGER * 4 FDB(7)             ! FILE DESCRIPTOR BLOCK
C
      INTEGER * 4 EXTRA1_GIND
      INTEGER * 4 EXTRA1_GNUM
      INTEGER * 4 EXTRA2_GIND
      INTEGER * 4 EXTRA2_GNUM
C
      LOGICAL ITSMTBLEQ              ! IT'S MATCH TABLE EQUAL
C
C FILE REGISTER VARIABLES DEFINITON
C
      INTEGER * 4 EXTDSPREC(DSPLEN)  ! EXTRAORDINARY REGISTER 
      INTEGER * 4 EXTDSPSTS          ! EXTRAORDINARY DRAW STATUS
      INTEGER * 4 EXTDSPDIV          ! EXTRAORDINARY NUMBER OF DIVISIONS
      INTEGER * 4 EXTDSPPOL(SPGDIV)  ! EXTRAORDINARY POOLS AMOUNT
      INTEGER * 4 EXTDSPMAT(SPGDIV)  ! EXTRAORDINARY MATCH TABLE
      INTEGER * 4 EXTDSPRGT(2)       ! EXTRAORDINARY ROLLOVED TO
C
C EQUIVALENCES FILE REGISTER VARIABLES DEFINITON
C
      EQUIVALENCE(EXTDSPSTS, EXTDSPREC(DSPSTS_OFF))
      EQUIVALENCE(EXTDSPDIV, EXTDSPREC(DSPDIV_OFF))
      EQUIVALENCE(EXTDSPPOL, EXTDSPREC(DSPPOL_OFF))
      EQUIVALENCE(EXTDSPMAT, EXTDSPREC(DSPMAT_OFF))
      EQUIVALENCE(EXTDSPRGT, EXTDSPREC(DSPRGT_OFF))
C
C READ SYSTEM CONFIGURATION FILE
C
      CALL GETSCONF(SCFREC, FSTS)
      IF(FSTS .NE. 0) CALL GSTOP(GEXIT_FATAL)
C
C INITIATE VARIABLES TO ADD ADITIONAL POOLS FROM EXTRAORDINARY GAME
C
      CALL FASTMOV(FILREC, DSPREC, DSPLEN)
      FGTYPE = SCFGNT(GAMTYP, FGNUM)
C
C CHECK IF THIS IS A NORMAL NAME ( FOR EXTRAORDINARY GAMES NOT ALLOW ROLLOVER )
C V02: NOW ALLOW ROLLOVER NOT ONLY FOR NORMAL GAME, BUT ALSO FOR EXTRA GAMES
C
C     IF(DSPEXT .NE. 0) RETURN
C
C DISPLAY INFORMATION TO USER ( DRAW NUMBER / GAME NAME )
C
3000  CONTINUE
      TYPE *, IAM()
      TYPE *, IAM(), '* * * * * * * * * * * * * * * * * * * * *'
      TYPE *, IAM(), '* TotoBola: Extraordinary Draws Manager *'
      TYPE *, IAM(), '* * * * * * * * * * * * * * * * * * * * *'
      TYPE *, IAM()
      WRITE(6, 100) IAM(), FDRAW, FGNUM, GTNAMES(FGTYPE)
      TYPE *, IAM()
C
C ASK TO USER IF HE/SHE WANTS ADD EXTRA MONEY TO ACTUAL GAME
C
      CALL PRMYESNO('Do You Want Add Money From Extraordinay Draw [Y/N] ?', USEL)
      IF(USEL .NE. 1) GOTO 2000
C
C ASK TO USER FOR SECOND GAME ( EXTRAORDINARY GAME )
C
1000  CONTINUE
      CALL PRMNUM('Enter Extraordinary Game Number:', USEL, 1, MAXGAM, FSTS)
      IF(FSTS .LT. 0) GOTO 2000
      SGNUM = USEL
      SGTYPE = SCFGNT(GAMTYP, SGNUM)
C
C ASK TO USER FOR SECOND GAME ( DRAW NUMBER )
C
      CALL PRMNUM('Enter Extraordinary Draw Number:', USEL, 1, 999999, FSTS)
      IF(FSTS .LT. 0) GOTO 2000
      SDRAW = USEL
C
C IF ACTUAL GAME NUMBER IT'S IDENTICAL TO ENTERED GAME NUMBER IT'S AN ERROR
C
      IF(FGNUM .EQ. SGNUM) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Error, Entered / Actual Game Should Be Different'
        TYPE *, IAM()
        GOTO 1000
      ENDIF
C
C CHECK IF ENTERD GAME TYPE IT'S IDENTICAL TO ACTUAL GAME NUMBER
C
      IF(FGTYPE .NE. SGTYPE) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Error, Entered / Actual Game Type Should Be Equal'
        TYPE *, IAM()
        GOTO 1000
      ENDIF
C
C CHECK IF ENTERED GAME NUMBER IT'S ACTIVE
C
      IF(SGTYPE .EQ. 0) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Error, Entered Game Number It''s Not Active'
        TYPE *, IAM()
        GOTO 1000
      ENDIF
C
C CHECK IF ENTERED GAME NUMBER CORRESPONDS TO TOTOBOLA EXTRA1 OR TOTOBOLA EXTRA2
C
      EXTRA1_GIND = 3
      EXTRA1_GNUM = SCFGTN(FGTYPE,3)
      EXTRA2_GIND = 2
      EXTRA2_GNUM = SCFGTN(FGTYPE,2)
      IF(SGNUM .NE. EXTRA1_GNUM .AND. SGNUM .NE. EXTRA2_GNUM) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Error, Entered Extraordinary Game Number Does Not Exist'
        TYPE *, IAM()
        GOTO 1000
      ENDIF
C
C CHECK IF ACTUAL GAME WAS ALREADY UPDATED
C V02: NOW TOTOBOLA NORMAL CAN TAKE MONEY FROM EXTRA1 AND EXTRA2
C      (IT WILL BE THEN UPDATED MORE THAN ONCE)
C
C     IF(DSPROD(1) .NE. 0) THEN
C       TYPE *, IAM()
C       TYPE *, IAM(), 'Error, Actual Game Was Already Updated'
C       TYPE *, IAM()
C       GOTO 1000
C     ENDIF
C
C OPEN ENTERED GAME NUMBER FILE ( FOR TOTOBOLA GAMES )
C
      CALL OPENW(IDFIL, SCFGFN(1, SGNUM), 4, 0, 0, FSTS)
      CALL IOINIT(FDB, IDFIL, DSPSEC * 256)
      IF(FSTS .NE. 0) THEN
        TYPE *, IAM()
        WRITE(6, 101) IAM(), (SCFGFN(CNTA, SGNUM), CNTA = 1, 5)
        TYPE *, IAM()
        GOTO 1000
      ENDIF
C
C READ ENTERED DRAW, PLUS 1, NUMBER GAME FILE: IS DRAW+1 READ, AND NOT DRAW,
C BECAUSE SCML ENTERS THE DRAW FROM WHERE THE MONEY WAS ROLLOVED TO THE DRAW+1
C (SCML DOES NOT ENTER THE DRAW+1, TO WHERE THE MONEY WAS ROLLOVED FROM DRAW)
C
      CALL READW(FDB, SDRAW+1, EXTDSPREC, FSTS)
      IF(FSTS .NE. 0) THEN
        TYPE *, IAM()
        WRITE(6, 102) IAM(), (SCFGFN(CNTA, SGNUM), CNTA = 1, 5)
        TYPE *, IAM()
        CALL CLOSEFIL(FDB)
        GOTO 1000
      ENDIF
C
C CHECK IF DIVISION NUMBER IT'S IDENTICAL FOR NORMAL AND EXTRAORDINARY GAME
C
      IF(DSPDIV .NE. EXTDSPDIV) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Error, Number Of Winner Divisions Should Be Identical'
        TYPE *, IAM()
        CALL CLOSEFIL(FDB)
        GOTO 1000
      ENDIF
C
C CHECK IF DIVISION MATCH TABLE ARE IDENTICAL FOR NORMAL AND ESTRAORDINARY GAME
C
      ITSMTBLEQ = .FALSE.
      DO CNTA = 1, DSPDIV
        IF(DSPMAT(CNTA) .NE. EXTDSPMAT(CNTA)) ITSMTBLEQ = .TRUE.
      ENDDO
C
C IF DIVISION MATCH TABLE ARE NOT IDENTICA DISPLAY MESSAGE ERROR
C
      IF(ITSMTBLEQ .EQ. .TRUE.) THEN
        TYPE *, IAM()
        TYPE *, IAM(), 'Error, Division Match Table Should Be Identical'
        TYPE *, IAM()
        CALL CLOSEFIL(FDB)
        GOTO 1000
      ENDIF
C
C CHECK IF EXTRAORDINARY DRAW STATUS IT'S OPEN ( SHARE DONE, DON'T CHANGE POOL )
C
C      IF(EXTDSPSTS .GT. GAMOPN) THEN
C        TYPE *, IAM()
C        TYPE *, IAM(), 'Error, Extraordinary Game Status It''s Not Open For Sales'
C        TYPE *, IAM()
C        CALL CLOSEFIL(FDB)
C        GOTO 1000
C      ENDIF
C
C DISPLAY TITLE MESSAGES ( DISPALY INFORMATION )
C
      TYPE *, IAM()
      TYPE *, IAM(), 'Division     Actuals Pools     Extraordinary Pools'
      TYPE *, IAM(), '--------------------------------------------------'
      TYPE *, IAM()
C
C DISPLAY INFORMATION ( ABOUT ACTUAL / EXTRAORDINARY POOLS )
C
      DO CNTA = 1, DSPDIV
        WRITE(6, 104) IAM(), 
     *                 DSPMAT(CNTA), 
     *                 CMONY(DSPPOL(CNTA), 14, BETUNIT),
     *                 CMONY(EXTDSPPOL(CNTA), 14, BETUNIT)
      ENDDO
C
C ASK TO USER IF HE/SHE WANTS UPDATE EXTRA MONEY FROM EXTRAORDINARY DRAW
C
      TYPE *, IAM()
      CALL PRMYESNO('Do Yo Want Update Money From Extra Draw [Y/N] ?', USEL)
      IF(USEL .NE. 1) THEN
        CALL CLOSEFIL(FDB)
        GOTO 3000
      ENDIF
C
C ADD TO ACTUAL POOLS EXTRAORDINARY POOLS AMOUNT AND RESET EXTRA POOLS
C
      DO CNTA = 1, DSPDIV
        DSPPOL(CNTA) = DSPPOL(CNTA) + EXTDSPPOL(CNTA)
        EXTDSPPOL(CNTA) = 0
      ENDDO
C
C UPDATE WHERE ROLLOVER GO TO ( GAME NUMBER AND DRAW NUMBER )
C
      EXTDSPRGT(1) = FGNUM
      EXTDSPRGT(2) = FDRAW
C
C SAVE EXTRAORDINARY DRAW FILE INFORMATION ( POOLS HAS BEEN SET TO ZERO )
C
      CALL WRITEW(FDB, SDRAW+1, EXTDSPREC, FSTS)
      IF(FSTS .NE. 0) THEN
        TYPE *, IAM()
        WRITE(6, 103) IAM(), (SCFGFN(CNTA, SGNUM), CNTA = 1, 5)
        TYPE *, IAM()
        CALL CLOSEFIL(FDB)
        GOTO 1000
      ENDIF
C
C UPDATE WHERE ROLLOVER CAME FROM ( GAME NUMBER AND DRAW NUMBER )
C
      DSPROD(1) = SGNUM
      DSPROD(2) = SDRAW+1
C
C UPDATE FILE REGISTER INFORMATION
C
      CALL FASTMOV(DSPREC, FILREC, DSPLEN)
C
C CLOSE ENTERED DRAW NUMBER GAME FILE AND ASK AGAIN IF WANTED TO ADD MONEY
C
      CALL CLOSEFIL(FDB)
      GOTO 3000
C
C DO NOT ASK MORE IF WANTED TO ADD MONEY
C
2000  CONTINUE
      RETURN
C
C FORMATS DEFINITION TO ADD ADITIONAL POOLS FROM EXTRAORDINARY GAME
C
100   FORMAT(X, A, 'Draw: ', I6.6, ' Game Number: ', I2.2, ' Game Type: ', A8)
101   FORMAT(X, A, 'Error Opening File: ', 5A4)
102   FORMAT(X, A, 'Error Reading File: ', 5A4)
103   FORMAT(X, A, 'Error Writing File: ', 5A4)
104   FORMAT(X, A, 6X, I2.2, 4X, A14, 10X, A14)
C
C THIS IS THE END TO ADD ADITIONAL POOLS FROM EXTRAORDINARY GAME
C
      END
