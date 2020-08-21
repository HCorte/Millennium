C TERSIMINIT.FOR
C
C V06 17-DEC-2010 HXK LOTTO 2 CHANGES (LOTTO 3 AND LOTTO 4 - 5/49)
C V05 03-DEC-2000 UXN Totogolo added.
C V04 31-JAN-2000 OXK Changed parameters for TSPT (Vakio changes)
C V03 13-OCT-1999 RXK World Tour added.
C V02 17-MAY-1999 UXN SUPER TRIPLE ADDED.
C V01 XX-XXX-XXXX XXX INITIAL RELEASE.
C
C INITIALIZE LOCAL GAME PARAMETERS' ARRAY FOR TERSIM
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
C Copyright 2010,1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS	/CHECK=NOOVERFLOW
	SUBROUTINE TERSIMINIT
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:RECSCF.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
C
        INTEGER*4 GAM,ST
C
C
	CALL FASTSET(0,PAR(1,1),MXPAR*MAXGAM)
C
C GET SYSTEM CONTROL CONFIGURATION INFO.
C
        CALL GETSCONF(SCFREC,ST)
        IF(ST.NE.0) THEN
           WRITE(6,901) IAM(),ST
           CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
        DO 100 GAM= 1,MAXGAM
          GTYP = SCFGNT(GAMTYP,GAM)
          GIND = SCFGNT(GAMIDX,GAM)
          IF(GTYP .LT. 1 .OR. GTYP .GT. MAXTYP) GOTO 100
          IF(GIND .LT. 1 .OR. GIND .GT. MAXIND) GOTO 100
C
C LOTTO 5/49
C
          IF(GTYP.EQ.TLTO) THEN  
             PAR(GMTYP,GAM)=TLTO
             PAR(GMIND,GAM)=GIND
             PAR(GMMAX,GAM)=49
             PAR(GMBET,GAM)=5
             PAR(GMBRD,GAM)=10
             PAR(GMDRW,GAM)=1
             PAR(GMBON,GAM)=0
             PAR(GMMIN,GAM)=1
             PAR(GMJOK,GAM)=1
             IF(SCFKGN(GAM) .EQ. 0) PAR(GMJOK, GAM) = 0 ! NO JOKER CONFIGURED
          ENDIF
C
C SPORTS GAME
C
          IF(GTYP.EQ.TSPT) THEN  
             PAR(GMTYP,GAM)=TSPT
             PAR(GMIND,GAM)=GIND
             PAR(GMMAX,GAM)=3*14
             PAR(GMBET,GAM)=14
             PAR(GMBRD,GAM)=10
             PAR(GMDRW,GAM)=1
             PAR(GMBON,GAM)=0
             PAR(GMMIN,GAM)=2
             PAR(GMJOK,GAM)=1
          ENDIF
C
C TOTOGOLO GAME
C
          IF(GTYP.EQ.TTGL) THEN  
             PAR(GMTYP,GAM)=TTGL
             PAR(GMIND,GAM)=GIND
             PAR(GMMAX,GAM)=3*14
             PAR(GMBET,GAM)=14
             PAR(GMBRD,GAM)=10
             PAR(GMDRW,GAM)=1
             PAR(GMBON,GAM)=0
             PAR(GMMIN,GAM)=1
             PAR(GMJOK,GAM)=1
          ENDIF
C
C JOKER GAME
C
          IF(GTYP.EQ.TKIK) THEN  
             PAR(GMTYP,GAM)=TKIK
             PAR(GMIND,GAM)=GIND
             PAR(GMMAX,GAM)=10
             PAR(GMBET,GAM)=10
             PAR(GMBRD,GAM)=10
             PAR(GMDRW,GAM)=10
             PAR(GMBON,GAM)=0
             PAR(GMMIN,GAM)=0
             PAR(GMJOK,GAM)=3
          ENDIF
C
C TOTO SELECT GAME
C
          IF(GTYP.EQ.TTSL) THEN  
             PAR(GMTYP,GAM)=TTSL
             PAR(GMIND,GAM)=GIND
             PAR(GMMAX,GAM)=6
             PAR(GMBET,GAM)=6
             PAR(GMBRD,GAM)=1
             PAR(GMDRW,GAM)=1
             PAR(GMBON,GAM)=0
             PAR(GMMIN,GAM)=1
             PAR(GMJOK,GAM)=0
          ENDIF
C
C SCORE GAME
C
          IF(GTYP.EQ.TSCR) THEN  
             PAR(GMTYP,GAM)=TSCR
             PAR(GMIND,GAM)=GIND
             PAR(GMMAX,GAM)=20
             PAR(GMBET,GAM)=2
             PAR(GMBRD,GAM)=8
             PAR(GMDRW,GAM)=1
             PAR(GMBON,GAM)=0
             PAR(GMMIN,GAM)=1
             PAR(GMJOK,GAM)=0
          ENDIF
C
C WINTIP GAME
C
          IF(GTYP.EQ.TWIT) THEN  
             PAR(GMTYP,GAM)=TWIT
             PAR(GMIND,GAM)=GIND
             PAR(GMMAX,GAM)=10
             PAR(GMBET,GAM)=1
             PAR(GMBRD,GAM)=10
             PAR(GMDRW,GAM)=1
             PAR(GMBON,GAM)=0
             PAR(GMMIN,GAM)=1
             PAR(GMJOK,GAM)=0
          ENDIF
C
C SUPER DOUBLE GAME
C
          IF(GTYP.EQ.TDBL) THEN  
             PAR(GMTYP,GAM)=TDBL
             PAR(GMIND,GAM)=GIND
             PAR(GMMAX,GAM)=10
             PAR(GMBET,GAM)=1
             PAR(GMBRD,GAM)=10
             PAR(GMDRW,GAM)=1
             PAR(GMBON,GAM)=0
             PAR(GMMIN,GAM)=1
             PAR(GMJOK,GAM)=0
          ENDIF
C
C TODAYS COUPLE GAME
C
          IF(GTYP.EQ.TCPL) THEN  
             PAR(GMTYP,GAM)=TCPL
             PAR(GMIND,GAM)=GIND
             PAR(GMMAX,GAM)=10
             PAR(GMBET,GAM)=1
             PAR(GMBRD,GAM)=10
             PAR(GMDRW,GAM)=1
             PAR(GMBON,GAM)=0
             PAR(GMMIN,GAM)=1
             PAR(GMJOK,GAM)=0
          ENDIF
C
C TODAYS TRIO GAME
C
          IF(GTYP.EQ.TTRP) THEN  
             PAR(GMTYP,GAM)=TTRP
             PAR(GMIND,GAM)=GIND
             PAR(GMMAX,GAM)=3
             PAR(GMBET,GAM)=18
             PAR(GMBRD,GAM)=1
             PAR(GMDRW,GAM)=1
             PAR(GMBON,GAM)=0
             PAR(GMMIN,GAM)=1
             PAR(GMJOK,GAM)=0
          ENDIF
C
C SUPER TRIPLE GAME
C
          IF(GTYP.EQ.TSTR) THEN  
             PAR(GMTYP,GAM)=TSTR
             PAR(GMIND,GAM)=GIND
             PAR(GMMAX,GAM)=3
             PAR(GMBET,GAM)=18
             PAR(GMBRD,GAM)=1
             PAR(GMDRW,GAM)=1
             PAR(GMBON,GAM)=0
             PAR(GMMIN,GAM)=1
             PAR(GMJOK,GAM)=0
          ENDIF
C
C SUPERSCOR GAME
C
          IF(GTYP.EQ.TSSC) THEN  
             PAR(GMTYP,GAM)=TSSC
             PAR(GMIND,GAM)=GIND
             PAR(GMMAX,GAM)=3
             PAR(GMBET,GAM)=30
             PAR(GMBRD,GAM)=1
             PAR(GMDRW,GAM)=1
             PAR(GMBON,GAM)=0
             PAR(GMMIN,GAM)=1
             PAR(GMJOK,GAM)=0
          ENDIF
C
C BINGO LOTO
C
          IF(GTYP.EQ.TBNG) THEN  
             PAR(GMTYP,GAM)=TBNG
             PAR(GMIND,GAM)=GIND
             PAR(GMMAX,GAM)=75
             PAR(GMBET,GAM)=25
             PAR(GMBRD,GAM)=3
             PAR(GMDRW,GAM)=1
             PAR(GMBON,GAM)=0
             PAR(GMMIN,GAM)=1
             PAR(GMJOK,GAM)=0
          ENDIF
C
C PASSIVE GAMES
C
          IF(GTYP .EQ. TPAS) THEN  
             PAR(GMTYP,GAM) = TPAS
             PAR(GMIND,GAM) = GIND
             PAR(GMMAX,GAM) = 0
             PAR(GMBET,GAM) = 0
             PAR(GMBRD,GAM) = 0
             PAR(GMDRW,GAM) = 0
             PAR(GMBON,GAM) = 0
             PAR(GMMIN,GAM) = 0
             PAR(GMJOK,GAM) = 0
          ENDIF

100     CONTINUE
C
	RETURN

901     FORMAT(1X,A,'Unable to get System Control Information,',
     *         '  Status: ',I4)
	END
