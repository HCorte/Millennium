C
C SUBROUTINE BNGVER
C  
C V04 20-JAN-2000 RXK AB part removed, FH part changed for subphases.
C V03 13-JAN-2000 RXK Def-file for Bingo division names added
C V02 24_NOV-1994 HXK Fixed formatting and input bug
C V01 17-OCT-1994 HXK Initial revision.
C  
C
C BNGVER.FOR
C
C
C SUBROUTINE TO PROCESS LOTTERY ENTRY OF BINGO RESULTS.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE BNGVER(GNUM,GIND,DRAW,ST)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:RESCOM.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:BNGDNAM.DEF'


        ! arguments
        INTEGER*4  GNUM                          !
        INTEGER*4  GIND                          !
        INTEGER*4  DRAW                          !
        INTEGER*4  ST                            !

        ! variables
        INTEGER*4  FLAG                          !
        INTEGER*4  K                             !
        INTEGER*4  EXT                           !
        INTEGER*4  NUM                           !
        INTEGER*4  I                             !
        INTEGER*4  BDROFF                        !
        INTEGER*4  CBUF(CDLEN)                   !
        INTEGER*4  OFF                           !
        INTEGER*4  SUBGAME
        INTEGER*4  VER_DBNSPH(BGOSPH)
        INTEGER*4  VER_DBNWST
        INTEGER*4  VER_DBNWS2
        INTEGER*4  VER_DBNNDF(BGODIV) 

        CHARACTER*23  INPUT_TEXT
        CHARACTER*16  BINGO_SUBGAME(BGOSUB)
        CHARACTER*40  STRING

        DATA  BINGO_SUBGAME/'Bingo A,B       ',
     *                      'Full House      ',
     *                      'Lucky Number    '/
C
C
        WRITE(5,901) IAM(),GTNAMES(TBNG),GIND,DRAW

C FULL HOUSE
C ----------
        IF(DBNDIV(BGOFHS).EQ.0) GOTO 300
200     CONTINUE
        WRITE(5,904) BINGO_SUBGAME(BGOFHS)
        DO I=1,BGONBR
           WRITE(INPUT_TEXT,903) I
           CALL INPNUM(INPUT_TEXT,NUM,1,BGONBR,EXT)
           IF(EXT.LT.0) GOTO 200
           DBNHLD(I)=NUM
        ENDDO
C
        SUBGAME = BGOFHS
        CALL VER_BNGCHK(SUBGAME,ST)
C
        IF(ST.NE.0) THEN
           TYPE*,IAM(),' Duplicate numbers entered, please re-enter'
           GOTO 200
        ENDIF
C
        CALL INPNUM('Enter Phase 1 numbers drawn:',NUM,1,BGONBR,EXT)
        IF(EXT.LT.0) GOTO 200
        VER_DBNSPH(1)=NUM
        CALL INPNUM('Enter Phase 2 numbers drawn:',NUM,1,BGONBR,EXT)
        IF(EXT.LT.0) GOTO 200
        VER_DBNSPH(2)=NUM
        CALL INPNUM('Enter Phase 3 numbers drawn:',NUM,1,BGONBR,EXT)
        IF(EXT.LT.0) GOTO 200
        VER_DBNSPH(3)=NUM
        CALL INPNUM('Enter Phase 4 numbers drawn:',NUM,1,BGONBR,EXT)
        IF(EXT.LT.0) GOTO 200
        VER_DBNSPH(4)=NUM
        CALL INPNUM('Enter Phase 5 numbers drawn:',NUM,1,BGONBR,EXT)
        IF(EXT.LT.0) GOTO 200
        VER_DBNSPH(5)=NUM
C 
        IF(DBNMAT(BGOMAXMAP+1,BGOFHS).NE.0) THEN
           CALL INPNUM('Enter Worst numbers drawn:',NUM,1,BGONBR,EXT)
           IF(EXT.LT.0) GOTO 200
           VER_DBNWST=NUM
        ENDIF
C
        IF(DBNMAT(BGOMAXMAP+2,BGOFHS).NE.0) THEN
           CALL INPNUM('Enter Second Worst numbers drawn:',NUM,1,BGONBR,EXT)
           IF(EXT.LT.0) GOTO 200
           VER_DBNWS2=NUM
        ENDIF
C
        DO I=1,BGODIV
           IF(DBNFST(I,SUBGAME).NE.0) THEN
              WRITE(STRING,908) BNGDNAMES(DBNDNR(I))
              CALL INPNUM(STRING,NUM,1,BGONBR,EXT)
              VER_DBNNDF(I)= NUM
           ENDIF
        ENDDO

C
        WRITE(5,905) IAM(),(DBNHLD(K),K=1,BGONBR)
        WRITE(5,906) IAM(),(VER_DBNSPH(K),K=1,DBNNSP)
        IF(DBNMAT(BGOMAXMAP+1,BGOFHS).NE.0) WRITE(5,910) VER_DBNWST
        IF(DBNMAT(BGOMAXMAP+2,BGOFHS).NE.0) WRITE(5,911) VER_DBNWS2

        DO I=1,BGODIV
           IF(DBNFST(I,SUBGAME).NE.0)
     *        WRITE(5,909) BNGDNAMES(DBNDNR(I)),VER_DBNNDF(I)
        ENDDO
        CALL WIMG(5,'Are the numbers entered correct [Y/N] ')
        CALL YESNO(FLAG)
        IF(FLAG.NE.1) GOTO 200

C LUCKY NUMBER
C ------------
        IF(DBNDIV(BGOLNM).EQ.0) GOTO 400
300     CONTINUE
        WRITE(5,904) BINGO_SUBGAME(BGOLNM)
        DO I=1,BGONLN
           WRITE(INPUT_TEXT,903) I
           CALL INPNUM(INPUT_TEXT,NUM,1,9999999,EXT)
           IF(EXT.LT.0) GOTO 300
           DBNHLN(I)=NUM
        ENDDO
C
C
        WRITE(5,907) IAM(),(DBNHLN(K),K=1,BGONLN)
        CALL WIMG(5,'Are the numbers entered correct [Y/N] ')
        CALL YESNO(FLAG)
        IF(FLAG.NE.1) GOTO 300

400     CONTINUE

C
C CHECK AGAINST OPERATOR ENTRY
C
        IF(DBNDIV(BGOFHS).NE.0) THEN
            SUBGAME=BGOFHS
            DO I=1,BGONBR
                IF(DBNWIN(I).NE.DBNHLD(I)) THEN
                    TYPE*,IAM(),' Verification error, please re-enter'
                    OPDONE=0
                    DBNSTS=GAMBFD
                    ST=-1
                    RETURN
                ENDIF
            ENDDO
            DO I=1,DBNNSP
                IF(DBNSPH(I).NE.VER_DBNSPH(I)) THEN
                    TYPE*,IAM(),' Verification error, please re-enter'
                    OPDONE=0
                    DBNSTS=GAMBFD
                    ST=-1
                    RETURN
                ENDIF
            ENDDO
            IF(DBNWST.NE.VER_DBNWST) THEN
                TYPE*,IAM(),' Verification error, please re-enter'
                OPDONE=0
                DBNSTS=GAMBFD
                ST=-1
                RETURN
            ENDIF
            IF(DBNWS2.NE.VER_DBNWS2) THEN
                TYPE*,IAM(),' Verification error, please re-enter'
                OPDONE=0
                DBNSTS=GAMBFD
                ST=-1
                RETURN
            ENDIF
            DO I=1,BGODIV
               IF(DBNNDF(I).NE.VER_DBNNDF(I)) THEN
                  TYPE*,IAM(),' Verification error, please re-enter'
                  OPDONE=0
                  DBNSTS=GAMBFD
                  ST=-1
                  RETURN
               ENDIF
            ENDDO
        ENDIF
C
        IF(DBNDIV(BGOLNM).NE.0) THEN
            DO I=1,BGONLN
                IF(DBNWLN(I).NE.DBNHLN(I)) THEN
                    TYPE*,IAM(),' Verification error, please re-enter'
                    OPDONE=0
                    DBNSTS=GAMBFD
                    ST=-1
                    RETURN
                ENDIF
            ENDDO
        ENDIF
C
C
        ST=0
        DBNSTS=GAMENV
        IF(DBNSPR.EQ.0) DBNSTS=GAMDON
        RETURN

C
C
901     FORMAT(1X,A,1X,A8,I1,' draw ',I4)
902     FORMAT(1X,A,' Numbers entered: ',10(I3.3,1X),/,
     *         27X,10(I3.3,1X),/,
     *         27X,5(I3.3,1X))
903     FORMAT(1X,'Enter drawn number ',I2,':')
904     FORMAT(1X,A,1X,'Subgame ',A16)
905     FORMAT(1X,A,' Numbers entered: ',15(I2.2,1X),/,
     *         27X,15(I2.2,1X),/,
     *         27X,15(I2.2,1X),/,
     *         27X,15(I2.2,1X),/,
     *         27X,15(I2.2,1X))
906     FORMAT(1X,A,'Phase 1(bingos)    : ',I2,/
     *         18X,' Phase 2(doubles)   : ',I2,/
     *         18X,' Phase 3(triples)   : ',I2,/
     *         18X,' Phase 4(quadruples): ',I2,/
     *         18X,' Phase 5(fullhouse) : ',I2)
907     FORMAT(1X,A,1X,' Numbers entered: '/,5(I7.7,1X),/,
     *         27X,5(I7.7,1X))
908     FORMAT('Enter numbers drawn for first ',A8)
909     FORMAT(18X,' For first ',A8,':',I2)
910     FORMAT(18X,' Worst              : ',I2)
911     FORMAT(18X,' Second Worst       : ',I2)

        END
