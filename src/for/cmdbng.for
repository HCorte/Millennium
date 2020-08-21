C
C SUBROUTINE CMDBNG
C
C V04 20-JAN-2000 RXK Quadruples added.
C V03 13-JAN-2000 RXK Amount for quadruple can be changed.
C V02 18-OCT-1994 HXK Changed dimensioning of BNGSHV for subgames
C V01 09-OCT-1994 HXK Initial revision.
C  
C CMDBNG.FOR
C
C SUBROUTINE TO PROCESS BNG GAME COMMANDS
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
        SUBROUTINE CMDBNG(TRABUF,MESS)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'

        ! argument
        INTEGER*4  MESS(EDLEN)     !

        ! variables
        INTEGER*4  I4TEMP          !
        INTEGER*4  IND             !
        INTEGER*4  OFF             !
        INTEGER*4  GIND            !
        INTEGER*4  GNUM            !
        INTEGER*4  CMDNUM          !
        INTEGER*4  CNT             !
        INTEGER*4  I               !
        INTEGER*4  GAMMAP          !

        INTEGER*2 I2TEMP(2)        !

        EQUIVALENCE (I4TEMP,I2TEMP)

        CMDNUM=TRABUF(TCMNUM)
        GOTO (10,20,30,40,50,60,70,80,90,100) CMDNUM
        GOTO 1000
C
C CHANGE BINGO GAME STATUS
C
10      CONTINUE
        GIND=TRABUF(TCMDT1)
        TRABUF(TCMOLD)=BNGSTS(GIND)
        BNGSTS(GIND)=TRABUF(TCMNEW)
        IF(TRABUF(TCMNEW).EQ.GAMBFD) THEN
          BNGCTM(GIND)=TRABUF(TTIM)
          CALL BSET(BNGTIM(GIND),1)
          CALL CLRSUM
          I4TEMP=BNGREV(GIND)
          I2TEMP(1)=I2TEMP(1)+1
          BNGREV(GIND)=I4TEMP
        ENDIF
        MESS(2)=TECMD
        MESS(3)=3
        MESS(6)=GIND
        MESS(9)=TRABUF(TCMOLD)
        MESS(10)=TRABUF(TCMNEW)
        RETURN
C
C SET BINGO FH WINNING NUMBERS
C
20      CONTINUE
        GIND=TRABUF(TCMDT1)
        IND=TRABUF(TCMNEW)
        OFF=1
        DO OFF=1,15
           CALL ILBYTE(I4TEMP,TRABUF(TCMDT2),OFF)
           BNGWIN(IND,GIND)=I4TEMP
           IND=IND+1
        ENDDO
        MESS(2)=TECMD
        MESS(3)=4
        MESS(6)=GIND
        RETURN
C
C CHANGE BINGO FH FULL HOUSE PRIZE
C
30    CONTINUE
      GIND=TRABUF(TCMDT1)
      DO I=1,BGODIV
         IF(BNGDNR(I,GIND).EQ.13) THEN 
            TRABUF(TCMOLD)=BNGSHV(I,BGOFHS,GIND)
            BNGSHV(I,BGOFHS,GIND)=TRABUF(TCMNEW)
            BNGPER(I,BGOFHS,GIND)=0
            MESS(2)=TECMD
            MESS(3)=3
            MESS(6)=GIND
            MESS(9)=TRABUF(TCMOLD)
            MESS(10)=TRABUF(TCMNEW)
            RETURN
         ENDIF
      ENDDO 
      RETURN
C
C CHANGE BINGO FH HIT 24 PRIZE
C
40    CONTINUE
      GIND=TRABUF(TCMDT1)
      DO I=1,BGODIV
         IF(BNGDNR(I,GIND).EQ.14) THEN 
            TRABUF(TCMOLD)=BNGSHV(I,BGOFHS,GIND)
            BNGSHV(I,BGOFHS,GIND)=TRABUF(TCMNEW)
            BNGPER(I,BGOFHS,GIND)=0
            MESS(2)=TECMD
            MESS(3)=3
            MESS(6)=GIND
            MESS(9)=TRABUF(TCMOLD)
            MESS(10)=TRABUF(TCMNEW)
            RETURN
         ENDIF
      ENDDO
      RETURN 
C
C CHANGE BINGO FH TRIPLE("OTHER") PRIZE
C
50    CONTINUE
      GIND=TRABUF(TCMDT1)
      DO I=1,BGODIV
         IF(BNGDNR(I,GIND).EQ.6) THEN 
            TRABUF(TCMOLD)=BNGSHV(I,BGOFHS,GIND)
            BNGSHV(I,BGOFHS,GIND)=TRABUF(TCMNEW)
            BNGPER(I,BGOFHS,GIND)=0
            MESS(2)=TECMD
            MESS(3)=3
            MESS(6)=GIND
            MESS(9)=TRABUF(TCMOLD)
            MESS(10)=TRABUF(TCMNEW)
            RETURN
         ENDIF
      ENDDO
      RETURN 
C
C CHANGE BINGO FH WORST PRIZE
C
60    CONTINUE
      GIND=TRABUF(TCMDT1)
      DO I=1,BGODIV
         IF(BNGDNR(I,GIND).EQ.19) THEN 
            TRABUF(TCMOLD)=BNGSHV(I,BGOFHS,GIND)
            BNGSHV(I,BGOFHS,GIND)=TRABUF(TCMNEW)
            BNGPER(I,BGOFHS,GIND)=0
            MESS(2)=TECMD
            MESS(3)=3
            MESS(6)=GIND
            MESS(9)=TRABUF(TCMOLD)
            MESS(10)=TRABUF(TCMNEW)
            RETURN
         ENDIF
      ENDDO
      RETURN 
C
C CHANGE BINGO FH SUBPHASES 
C
70    CONTINUE
      GIND=TRABUF(TCMDT1)
      MESS(9)=BNGSPH(BNGNSP(GIND),GIND)
      DO OFF=1,BNGNSP(GIND)
         CALL ILBYTE(I4TEMP,TRABUF(TCMDT2),OFF)
         BNGSPH(OFF,GIND)=I4TEMP
      ENDDO
      MESS(2)=TECMD
      MESS(3)=3
      MESS(6)=GIND
      MESS(10)=BNGSPH(BNGNSP(GIND),GIND)    !msg for FH only
      RETURN
C
C CHANGE BINGO FH PHASE 2 NUMBER
C
80    CONTINUE
      GIND=TRABUF(TCMDT1)
      TRABUF(TCMOLD)=BNGPHS(2,GIND)
      BNGPHS(2,GIND)=TRABUF(TCMNEW)
      MESS(2)=TECMD
      MESS(3)=3
      MESS(6)=GIND
      MESS(9)=TRABUF(TCMOLD)
      MESS(10)=TRABUF(TCMNEW)
      RETURN
C
C CHANGE BINGO FH QUADRUPLE("OTHER") PRIZE
C
90    CONTINUE
      GIND=TRABUF(TCMDT1)
      DO I=1,BGODIV
         IF(BNGDNR(I,GIND).EQ.8) THEN 
            TRABUF(TCMOLD)=BNGSHV(I,BGOFHS,GIND)
            BNGSHV(I,BGOFHS,GIND)=TRABUF(TCMNEW)
            BNGPER(I,BGOFHS,GIND)=0
            MESS(2)=TECMD
            MESS(3)=3
            MESS(6)=GIND
            MESS(9)=TRABUF(TCMOLD)
            MESS(10)=TRABUF(TCMNEW)
            RETURN
         ENDIF
      ENDDO
      RETURN 
C
C PUT NEXT BINGO COMMAND HERE
C
100     CONTINUE

C
C INVALID COMMAND NUMBER
C
1000    CONTINUE
        TRABUF(TSTAT)=REJT
        TRABUF(TERR)=INVL
        MESS(2)=TECMD
        MESS(3)=1
        MESS(4)=TRABUF(TCMTYP)
        MESS(5)=TRABUF(TCMNUM)
        RETURN
        END
