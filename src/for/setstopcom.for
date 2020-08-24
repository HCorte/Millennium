C
C V03 21-DEC-2010 MAC LOTTO3 AND LOTTO4
C V02 01-DEC-2000 UXN TOTOGOLO ADDED.
C V01 01-MAR-2000 UXN Separated from MULSUBS.
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
      SUBROUTINE SETSTOPCOM
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
C
      INTEGER*4 I
C
C  CLEAR STOPCOM FOR MULTIWIN
C
      CALL FASTSET(0,TSKCNT ,MAXWTSK)
      CALL FASTSET(0,TSKGAM ,3*MAXIND*MAXWTSK)
      CALL FASTSET(0,TSKVOL ,MAXWTSK)
      CALL FASTSET(0,WINWTSK,MAXWTSK)
      CALL FASTSET(0,VLWNBR ,MAXWTSK)
      CALL FASTSET(0,TCWNBR ,MAXWTSK)
      CALL FASTSET(0,FCNT   ,MAXWTSK)
      CALL FASTSET(0,BUFTYP ,MAXF1*MAXWTSK)
      CALL FASTSET(0,STSTSK ,MAXWTSK)
      CALL FASTSET(0,TSKTCFSTS,MAXWTSK)
      CALL FASTSET(0,VLWSTS ,MAXMVLF)
      CALL FASTSET(0,TCWSTS ,MAXMTCF)
      CALL FASTSET(0,MRGTYP ,MAXWTSK)
      WINCNT=0
      STOPMOD=WINMANUAL
      ASFLOCK=0
      VLCSTS=WUSE
      TCCSTS=WUSE
C
C
C                            WINTSK
      TSKCNT( 1)=3+NUMSPT+NUMTGL      
        TSKGAM(1,1)=GTNTAB(TLTO,3)     !V03

       TSKGAM(2,1)=GTNTAB(TLTO,4)      !V03
        TSKGAM(3,1)=GTNTAB(TKIK,1)
        DO I=1,NUMSPT
            TSKGAM(3+I,1)= GTNTAB(TSPT,I)
        ENDDO
        DO I=1,NUMTGL
            TSKGAM(3+NUMSPT+I,1)= GTNTAB(TTGL,I)
        ENDDO


C                            TWINTSK
      TSKCNT( 4)=1
        TSKGAM( 1, 4)= GTNTAB(TTSL,1)
C                            SWINTSK
      TSKCNT( 5)=MAXIND
        DO I=1,MAXIND
            TSKGAM( I, 5)= GTNTAB(TSCR,I)
        ENDDO

C                            WWINTSK
      TSKCNT( 6)=MAXIND
        DO I=1,MAXIND
            TSKGAM( I, 6)= GTNTAB(TWIT,I)
        ENDDO

C                            BWINTSK
      TSKCNT( 7)=1
        TSKGAM( 1, 7)= GTNTAB(TBNG,1)

C                            DWINTSK
      TSKCNT( 8)=MAXIND
        DO I=1,MAXIND
            TSKGAM( I, 8)= GTNTAB(TDBL,I)
        ENDDO

C                            CWINTSK
      TSKCNT( 9)=MAXIND
        DO I=1,MAXIND
            TSKGAM( I, 9)= GTNTAB(TCPL,I)
        ENDDO

C                            SSWINTSK
      TSKCNT(10)=MAXIND
        DO I=1,MAXIND
            TSKGAM( I,10)= GTNTAB(TSSC,I)
        ENDDO

C                            TRWINTSK
      TSKCNT(11)=MAXIND
        DO I=1,MAXIND
            TSKGAM( I,11)= GTNTAB(TTRP,I)
        ENDDO

C                            STWINTSK
      TSKCNT(12)=MAXIND
        DO I=1,MAXIND
            TSKGAM( I,12)= GTNTAB(TSTR,I)
        ENDDO
C
C
      DO 1 I=1,MAXWTSK
         MRGTYP(I)=MRGVL
1     CONTINUE
C
      MRGTYP(1)=MRGALL       !WINTSK
C
      CALL VLTCNAM           ! SET VLWnn & TCWnn NAMES
C
C     CALL CHKWINS           ! Set games for MULTIWIN
C
      RETURN
      END
