C
C SUBROUTINE BNGENT
C  
C V04 20-JAN-2000 RXK AB part removed, FH part changed for subphases. 
C V03 13-JAN-2000 RXK Def-file for Bingo division names added.
C V02 14-NOV-1994 HXK Fixed input range bug
C V01 17-OCT-1994 HXK Initial revision.
C  
C
C BNGENT.FOR
C
C SUBROUTINE TO PROCESS OPERATOR ENTRY OF BINGO RESULTS.
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
        SUBROUTINE BNGENT(GNUM,GIND,DRAW)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:RESCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:BNGDNAM.DEF'

        ! arguments
        INTEGER*4  GNUM                         !
        INTEGER*4  GIND                         !
        INTEGER*4  DRAW                         !

        ! variables
        INTEGER*4  FDB(7)                       !
        INTEGER*4  FLAG                         !
        INTEGER*4  K                            !
        INTEGER*4  EXT                          !
        INTEGER*4  NUM                          !
        INTEGER*4  I                            !
        INTEGER*4  ST                           !
        INTEGER*4  BDROFF                       !
        INTEGER*4  CBUF(CDLEN)                  !
        INTEGER*4  OFF                          !
        INTEGER*4  SUBGAME
        CHARACTER*23  INPUT_TEXT
        CHARACTER*16  BINGO_SUBGAME(BGOSUB)
        CHARACTER*40  STRING

        DATA  BINGO_SUBGAME/'Bingo A,B       ',
     *                      'Full House      ',
     *                      'Lucky Number    '/       


        IF(ONLINE) THEN
            CALL GAMLOG(TBNG,GIND,DBNREC,BNGBLK)
        ELSE
            CALL OPENW(2,SCFGFN(1,GNUM),4,0,0,ST)
            CALL IOINIT(FDB,2,DBNSEC*256)
            IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
            CALL READW(FDB,DRAW,DBNREC,ST)
            IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
        ENDIF
        IF(DBNSTS.NE.GAMBFD) THEN
            WRITE(5,900) IAM(),GTNAMES(TBNG),GIND,DRAW,DBNSTS
            CALL GPAUSE
        ENDIF
        WRITE(5,901) IAM(),GTNAMES(TBNG),GIND,DRAW

100     CONTINUE

C FULL HOUSE
C ----------
        IF(DBNDIV(BGOFHS).EQ.0) GOTO 300
200     CONTINUE
        WRITE(5,904) BINGO_SUBGAME(BGOFHS)
        DO I=1,BGONBR
           WRITE(INPUT_TEXT,903) I
           CALL PRMNUM(INPUT_TEXT,NUM,1,BGONBR,EXT)
           IF(EXT.LT.0) GOTO 200
           DBNWIN(I)=NUM
        ENDDO
        SUBGAME = BGOFHS
        CALL BNGCHK(SUBGAME,ST)

        IF(ST.NE.0) THEN
           TYPE*,IAM(),' Duplicate numbers entered, please re-enter'
           GOTO 200
        ENDIF

        CALL PRMNUM('Enter Phase 1 numbers drawn:',NUM,1,BGONBR,EXT)
        IF(EXT.LT.0) GOTO 200
        DBNSPH(1)=NUM
        CALL PRMNUM('Enter Phase 2 numbers drawn:',NUM,1,BGONBR,EXT)
        IF(EXT.LT.0) GOTO 200
        DBNSPH(2)=NUM
        CALL PRMNUM('Enter Phase 3 numbers drawn:',NUM,1,BGONBR,EXT)
        IF(EXT.LT.0) GOTO 200
        DBNSPH(3)=NUM
        CALL PRMNUM('Enter Phase 4 numbers drawn:',NUM,1,BGONBR,EXT)
        IF(EXT.LT.0) GOTO 200
        DBNSPH(4)=NUM
        CALL PRMNUM('Enter Phase 5 (FH) numbers drawn:',NUM,1,BGONBR,EXT)
        IF(EXT.LT.0) GOTO 200
        DBNSPH(5)=NUM
        DBNPHS(BGOPHS)=NUM

        IF(DBNMAT(BGOMAXMAP+1,BGOFHS).NE.0) THEN
           CALL PRMNUM
     *         ('Enter Worst numbers drawn:',NUM,1,BGONBR,EXT)
           IF(EXT.LT.0) GOTO 200
           DBNWST=NUM
        ENDIF

        IF(DBNMAT(BGOMAXMAP+2,BGOFHS).NE.0) THEN
           CALL PRMNUM
     *         ('Enter Second Worst numbers drawn:',NUM,1,BGONBR,EXT)
           IF(EXT.LT.0) GOTO 200
           DBNWS2=NUM
        ENDIF

        DO I=1,BGODIV
           IF(DBNFST(I,SUBGAME).NE.0) THEN
              WRITE(STRING,908) BNGDNAMES(DBNDNR(I))
              CALL PRMNUM(STRING,NUM,1,BGONBR,EXT)
              DBNNDF(I)=NUM
           ENDIF
        ENDDO

        WRITE(5,905) IAM(),(DBNWIN(K),K=1,BGONBR)
        WRITE(5,906) IAM(),(DBNSPH(K),K=1,DBNNSP)
        IF(DBNMAT(BGOMAXMAP+1,BGOFHS).NE.0) WRITE(5,910) DBNWST
        IF(DBNMAT(BGOMAXMAP+2,BGOFHS).NE.0) WRITE(5,911) DBNWS2
        
        DO I=1,BGODIV
           IF(DBNFST(I,SUBGAME).NE.0)
     *        WRITE(5,909) BNGDNAMES(DBNDNR(I)),DBNNDF(I)
        ENDDO
        CALL WIMG(5,'Are the numbers entered correct [Y/N] ')
        CALL YESNO(FLAG)
        IF(FLAG.NE.1) GOTO 200
C
C LUCKY NUMBER
C ------------
        IF(DBNDIV(BGOLNM).EQ.0) GOTO 400
300     CONTINUE
        WRITE(5,904) BINGO_SUBGAME(BGOLNM)
        DO I=1,BGONLN
           WRITE(INPUT_TEXT,903) I
           CALL PRMNUM(INPUT_TEXT,NUM,1,9999999,EXT)
           IF(EXT.LT.0) GOTO 300
           DBNWLN(I)=NUM
        ENDDO

        WRITE(5,907) IAM(),(DBNWLN(K),K=1,BGONLN)
        CALL WIMG(5,'Are the numbers entered correct [Y/N] ')
        CALL YESNO(FLAG)
        IF(FLAG.NE.1) GOTO 300

400     CONTINUE

        DBNSTS = GAMEN1
        OPDONE = 1
C
C WAIT FOR VERIFICATION FROM REMOTE TERMINAL.
C
500     CONTINUE

        TYPE*,IAM(),' Waiting for verification from remote terminal'
        IF(DBNSTS.GE.GAMENV) THEN

            IF(ONLINE) THEN
               DO I=1,5        ! 5 portions of winning numbers, 15numbers each
                  CALL FASTSET(0,CBUF,CDLEN)
                  CBUF(1) = 2
                  CBUF(2) = 1+15*(I-1)
                  CBUF(3) = TCBNG
                  CBUF(6) = 'SYS '
                  CBUF(8) = GIND
                  OFF = 1

                  IF(DBNDIV(BGOFHS).NE.0) THEN
                     DO K=1+15*(I-1),15+15*(I-1)
                        CALL ISBYTE(DBNWIN(K),CBUF(9),OFF)
                        OFF=OFF+1
                     ENDDO
                  ENDIF
                  CALL RESCMD(CBUF)
               ENDDO
               
               CALL FASTSET(0,CBUF,CDLEN)           !numbers for subphases
               CBUF(1) = 7
               CBUF(3) = TCBNG
               CBUF(6) = 'SYS '
               CBUF(8) = GIND
               OFF = 1
               DO I=1,DBNNSP
                  CALL ISBYTE(DBNSPH(I),CBUF(9),OFF)
                  OFF=OFF+1
               ENDDO
               CALL RESCMD(CBUF)

               CALL FASTSET(0,CBUF,CDLEN)           !game status
               CBUF(1) = 1
               CBUF(2) = DBNSTS
               CBUF(3) = TCBNG
               CBUF(6) = 'SYS '
               CBUF(8) = GIND
               CALL RESCMD(CBUF)
            ELSE
               CALL WRITEW(FDB,DRAW,DBNREC,ST)
               IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
            ENDIF

            RETURN

        ENDIF

        CALL XWAIT(5,2,ST)
        IF(DBNSTS.EQ.GAMBFD) THEN
            TYPE*,IAM(),' Verification error, please re-enter '
            GOTO 100
        ENDIF
        GOTO 500
C
C
900     FORMAT(1X,A,1X,A8,I1,' draw ',I4,' invalid game status> ',I4)
901     FORMAT(1X,A,1X,A8,I1,' draw ',I4)
902     FORMAT(1X,A,' Numbers entered: ',7(I3.3,1X),/,
     *         27X,9(I3.3,1X),/,
     *         27X,9(I3.3,1X))
903     FORMAT(1X,'Enter drawn number ',I2,':')
904     FORMAT(1X,A,1X,'Subgame ',A16)
905     FORMAT(1X,A,'Numbers entered: ',15(I2.2,1X),/,
     *         27X,15(I2.2,1X),/,
     *         27X,15(I2.2,1X),/,
     *         27X,15(I2.2,1X),/,
     *         27X,15(I2.2,1X))
906     FORMAT(1X,A,'Phase 1(bingos)    : ',I2,/
     *         18X,' Phase 2(doubles)   : ',I2,/
     *         18X,' Phase 3(triples)   : ',I2,/
     *         18X,' Phase 4(quadruples): ',I2,/
     *         18X,' Phase 5(fullhouse) : ',I2)
907     FORMAT(1X,A,1X,' Numbers emtered: '/,5(I7.7,1X),/,
     *         27X,5(I7.7,1X))
908     FORMAT('Enter numbers drawn for first ',A8)
909     FORMAT(18X,' For first ',A8,':',I2)
910     FORMAT(18X,' Worst             : ',I2)
911     FORMAT(18X,' Second Worst      : ',I2)
        END
