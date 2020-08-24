C
C SUBROUTINE KSHARE
C  
C V06 29-APR-99 RXK Stopsys optimization (PRMYESNO,PRMNUM,PRMMONY).
C V05 16-AUG-95 HXK Fix for Joker shares, now without offline
C V04 31-AUG-93 HXK lengthened string
C V03 01-JUL-93 HXK Released for Finland Vax Conversion
C V02 21-JAN-93 DAB Initial Release Based on Netherlands Bible, DEC Baseline
C V01 21-NOV-91 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C
C KSHARE.FOR
C
C
C SUBROUTINE TO PROCESS OPERATOR ENTRY OF KICKER SHARE VALUES.
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
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE KSHARE(GNUM,GIND,DRAW)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
C
        INTEGER*4   CONLU
        PARAMETER   (CONLU=5)               !Default Lu to the Console
C
        BYTE        BELL                    !Bell Character (BEEP)
        INTEGER*4   GFDB(7)                 !Game File Descriptor Block
        INTEGER*4   GNUM                    !Game Number to Update
        INTEGER*4   GIND                    !Game Index to Update
        INTEGER*4   DRAW                    !Draw Number to Update
        INTEGER*4   ST                      !Subroutine Return Status
        INTEGER*4   EXT                     !Exit Value (INP***)
        INTEGER*4   FLAG                    !YESNO Answer Flag
        INTEGER*4   I, K                    !Loop Variables
        INTEGER*4   FINAL_PROG              !Final Prognosis Flag
C
        CHARACTER   STRING*40               !Prompt String
C
        DATA        BELL/07/
C
        COMMON SCFREC
C
C
        CALL OPENW(3,SCFGFN(1,GNUM),4,0,0,ST)
        CALL IOINIT(GFDB,3,DKKSEC*256)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
        CALL READW(GFDB,DRAW,DKKREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
C
C
        WRITE(CONLU,800) IAM()
        IF(DKKSTS.NE.GAMENV.AND.DKKSTS.NE.GAMDON) THEN
          WRITE(CONLU,900) IAM(),(SCFLGN(K,GNUM),K=1,4),
     *                     DRAW,DKKSTS,BELL
          CALL GPAUSE
        ENDIF
C
C CHECK IF GAME HAVE FIXED PRIZES (WARN BUT LET THEM OVERRIDE)
C
        WRITE(CONLU,800) IAM()
        DO 20 I=1,DKKDIV
          WRITE(CONLU,903) IAM(),I,CMONY(DKKSHV(I),11,VALUNIT)
          WRITE(CONLU,907) IAM(),I,DKKSHR(I)
          DKKTSR(I) = DKKSHR(I)
20      CONTINUE
        IF(DKKSPR.LE.0) THEN
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
C GET SHARE VALUES AND TOTAL SHARES BY DIVISION
C
        FINAL_PROG=0
        CALL PRMYESNO('Is this final prognosis ? ',FINAL_PROG)
        IF(FINAL_PROG.EQ.1) THEN
           WRITE(CONLU,800) IAM()
           TYPE*,IAM(),' Jokeri share values as they are in game file : '
           DO I=1,DKKDIV
              WRITE(CONLU,903) IAM(),I,CMONY(DKKSHV(I),11,VALUNIT)
              WRITE(CONLU,907) IAM(),I,DKKSHR(I)
              DKKTSR(I) = DKKSHR(I)
           ENDDO
           WRITE(CONLU,800) IAM()
           CALL PRMYESNO('Are these values correct (Y/N) ',FLAG)
           IF(FLAG.EQ.1) GOTO 200
        ENDIF

100     CONTINUE
        TYPE*,IAM(),' Enter Jokeri share values '
        DO 110 I=1,DKKDIV
          WRITE (STRING,902) I
          CALL PRMMONY(STRING,DKKSHV(I),VALUNIT,EXT)
          IF(EXT.LT.0) GOTO 100
          WRITE(STRING,906) I
          CALL PRMNUM(STRING,DKKSHR(I),0,99999999,EXT)
          IF(EXT.LT.0) GOTO 100
110     CONTINUE
        WRITE(CONLU,800) IAM()
        DO 120 I=1,DKKDIV
          WRITE(CONLU,903) IAM(),I,CMONY(DKKSHV(I),11,VALUNIT)
          WRITE(CONLU,907) IAM(),I,DKKSHR(I)
          DKKTSR(I) = DKKSHR(I)
120     CONTINUE
        WRITE(CONLU,800) IAM()
        CALL PRMYESNO('Are these values correct (Y/N) ',FLAG)
        IF(FLAG.NE.1) GOTO 100
C
C
200     CONTINUE
        IF(FINAL_PROG.EQ.1) THEN 
          DKKSTS=GAMDON
          TYPE*,IAM(),'Share Values set after Final Prognosis'
        ENDIF 
        CALL WRITEW(GFDB,DRAW,DKKREC,ST)
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
902     FORMAT(1X,'Enter division ',I1,' share value')
903     FORMAT(1X,A,'Division ',I1,' share value ',A11)
904     FORMAT(1X,A,'This game have fixed share values!',A1,A1)
905     FORMAT(//,1X,A,'*** OVERRIDE OF FIXED SHARE VALUES ***',/)
906     FORMAT('Enter # total shares for division ',I1)
907     FORMAT(1X,A,'Division ',I1,'   shares ',I8)
        END
