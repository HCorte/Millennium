C
C SUBROUTINE BSHARE
C  
C  
C V04 02-MAR-2000 RXK Bug fixed in entry of values.
C V03 29-APR-1999 RXK Stopsys optimization (PRMYESNO,PRMNUM,PRMMONY).
C V02 08-JAN-1995 HXK Lengthened STRING variable
C V01 19-DEC-1994 HXK Initial revision.
C  
C
C BSHARE.FOR
C
C
C SUBROUTINE TO PROCESS OPERATOR ENTRY OF BINGO SHARE VALUES.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE BSHARE(GNUM,DRAW)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DBNREC.DEF'
C
        INTEGER*4   CONLU
        PARAMETER   (CONLU=5)               !Default Lu to the Console
C
        BYTE        BELL                    !Bell Character (BEEP)
        INTEGER*4   GFDB(7)                 !Game File Descriptor Block
        INTEGER*4   GNUM                    !Game Number to Update
        INTEGER*4   DRAW                    !Draw Number to Update
        INTEGER*4   ST                      !Subroutine Return Status
        INTEGER*4   EXT                     !Exit Value (INP***)
        INTEGER*4   FLAG                    !YESNO Answer Flag
        INTEGER*4   I, K                    !Loop Variables
        INTEGER*4   FINAL_PROG              !Final Prognosis Flag
C
        CHARACTER   STRING*60               !Prompt String
C
        DATA        BELL/07/
C
        COMMON SCFREC
C
C
        CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
        CALL IOINIT(GFDB,3,DBNSEC*256)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
        CALL READW(GFDB,DRAW,DBNREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
        TYPE*,IAM()
        TYPE*,IAM(),'HUOM!! Do NOT use for BINGO unless authorised !'
        TYPE*,IAM()
C
        WRITE(CONLU,800) IAM()
        IF(DBNSTS.NE.GAMENV.AND.DBNSTS.NE.GAMDON) THEN
          WRITE(CONLU,900) IAM(),(SCFLGN(K,GNUM),K=1,4),
     *                     DRAW,DBNSTS,BELL
          CALL GPAUSE
        ENDIF
C
C CHECK IF GAME HAVE FIXED PRIZES (WARN BUT LET THEM OVERRIDE)
C
        IF(DBNSPR.LE.0) THEN
          WRITE(CONLU,904) IAM(),BELL,BELL
          CALL PRMYESNO('Do you want to OVERRIDE (Y/N) ',FLAG)
          IF(FLAG.NE.1) THEN
            CALL CLOSEFIL(GFDB)
            RETURN
          ENDIF
          WRITE(CONLU,905) IAM()
        ENDIF
C
        WRITE(CONLU,901) IAM(),(SCFLGN(K,GNUM),K=1,4),DRAW
C
C GET SHARE VALUES BY DIVISION
C
100     CONTINUE
        FINAL_PROG=0
        CALL PRMYESNO('Is this final prognosis ? ',FINAL_PROG)
C       TYPE*,IAM(),' Bingo A,B share values '
C       DO 110 I=1,DBNDIV(BGOBAB)
C         WRITE (STRING,902) IAM(),I
C         CALL PRMMONY(STRING,DBNSHV(I,BGOBAB),VALUNIT,EXT)
C         IF(EXT.LT.0) GOTO 100
C         WRITE(STRING,906) IAM(),I
C         CALL PRMNUM(STRING,DBNTSR(I,BGOBAB),0,99999999,EXT)
C         DBNSHR(I,BGOBAB) = DBNTSR(I,BGOBAB)
C         IF(EXT.LT.0) GOTO 100
C110    CONTINUE
C       WRITE(CONLU,800) IAM()
C       DO 120 I=1,DBNDIV(BGOBAB)
C         WRITE(CONLU,903) IAM(),I,CMONY(DBNSHV(I,BGOBAB),11,VALUNIT)
C         WRITE(CONLU,907) IAM(),I,DBNSHR(I,BGOBAB)
C120    CONTINUE
C       WRITE(CONLU,800) IAM()
C       CALL PRMYESNO('Are these values correct (Y/N) ',FLAG)
C       IF(FLAG.NE.1) GOTO 100

        IF(FINAL_PROG.EQ.1) THEN
           WRITE(CONLU,800) IAM()
           TYPE*,IAM(),' Bingo Full House share values '
           TYPE*,IAM(),' as they are in game file : '
           WRITE(CONLU,800) IAM()
           DO  I=1,DBNDIV(BGOFHS)
              WRITE(CONLU,903) IAM(),I,CMONY(DBNSHV(I,BGOFHS),11,VALUNIT)
              WRITE(CONLU,907) IAM(),I,DBNSHR(I,BGOFHS)
           ENDDO
           WRITE(CONLU,800) IAM()
           CALL PRMYESNO('Are these values correct (Y/N) ',FLAG)
           IF(FLAG.EQ.1) GOTO 300
        ENDIF
C
200     CONTINUE
        TYPE*,IAM(),' Enter Bingo Full House share values '
        DO 210 I=1,DBNDIV(BGOFHS)
          WRITE (STRING,902) IAM(),I
          CALL PRMMONY(STRING,DBNSHV(I,BGOFHS),VALUNIT,EXT)
          IF(EXT.LT.0) GOTO 200
          WRITE(STRING,906) IAM(),I
          CALL PRMNUM(STRING,DBNTSR(I,BGOFHS),0,99999999,EXT)
          DBNSHR(I,BGOFHS) = DBNTSR(I,BGOFHS)
          IF(EXT.LT.0) GOTO 200
210     CONTINUE
        WRITE(CONLU,800) IAM()
        DO 220 I=1,DBNDIV(BGOFHS)
          WRITE(CONLU,903) IAM(),I,CMONY(DBNSHV(I,BGOFHS),11,VALUNIT)
          WRITE(CONLU,907) IAM(),I,DBNSHR(I,BGOFHS)
220     CONTINUE
        WRITE(CONLU,800) IAM()
        CALL PRMYESNO('Are these values correct (Y/N) ',FLAG)
        IF(FLAG.NE.1) GOTO 200

C
300     CONTINUE 
        IF(FINAL_PROG.EQ.1) THEN
          DBNSTS=GAMDON
          TYPE*,IAM(),'Share values are NOW set after Final Prognosis'
        ENDIF
        CALL WRITEW(GFDB,DRAW,DBNREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
        CALL CLOSEFIL(GFDB)
        RETURN
C
C
C
800     FORMAT(1X,A)
900     FORMAT(1X,A,4A4,' event ',I4,
     *         ' invalid game status> ',I4,A1)
901     FORMAT(/,1X,A,4A4,' draw ',I4,/)
902     FORMAT(1X,A,1X,'Enter division ',I2,' share value')
903     FORMAT(1X,A,'Division ',I2,' share value ',A11)
904     FORMAT(1X,A,'This game have fixed share values!',A1,A1)
905     FORMAT(//,1X,A,'*** OVERRIDE OF FIXED SHARE VALUES ***',/)
906     FORMAT(1X,A,1X,'Enter # total shares for division ',I2)
907     FORMAT(1X,A,'Division ',I2,' TOTAL   shares ',I8)
        END
