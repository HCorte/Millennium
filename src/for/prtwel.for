C PRTWEL.FOR
C
C V02 18-JUN-2014 SCML PLACARD Project - Added IGS transaction
C V01 06-FEB-2014 FRP  RELEASED FOR VAX
C
C THIS SUBROUTINE WILL PRINT THE CONTENTS OF
C A PROCOM BUFFER (IN HEX).
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE PRTWEL(BUF)
      IMPLICIT NONE

      INCLUDE '(LIB$ROUTINES)'
      INCLUDE '($FORIOSDEF)'
      INCLUDE 'INCLIB:SYSDEFINE.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:TNAMES.DEF'
      INCLUDE 'INCLIB:GTNAMES.DEF'
      INCLUDE 'INCLIB:DESTRA.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:PROCOM.DEF'

*     Arguments: 

      INTEGER * 4  BUF       ! INPUT: Buffer number 

*     Locals: 

      INTEGER * 4  J, K      ! Loop counters
      INTEGER * 4  ST        ! I/O status
      INTEGER * 4  WST
      INTEGER * 4  LU        ! Logical unit
      INTEGER * 4  INITIM,FINTIM,DIFTIM
      INTEGER * 4  WRKBUF(TRALEN),GTYP
      INTEGER * 4  TSUBTYP, TRXTYP !V02


*     Implementation: 

      IF(PRO(TIMINOFF,BUF).EQ.0) RETURN

      CALL LOGTRA(WRKBUF,PRO(WRKTAB,BUF))

      IF(WRKBUF(TTYP).EQ.TSPE) THEN
        GTYP = WRKBUF(TGAMTYP)
        TRXTYP = WRKBUF(TTYP) !V02
        TSUBTYP = WRKBUF(TSFUN) !V02
        FINTIM = PRO(TSTAMP,BUF)
        INITIM = PRO(TIMINOFF,BUF)
      ELSEIF(WRKBUF(TTYP).EQ.TEUR) THEN
        GTYP = WRKBUF(TGAMTYP)
        TRXTYP = WRKBUF(TTYP) !V02
        TSUBTYP = WRKBUF(TEUTYP) !V02
        FINTIM = PRO(TSTAMP,BUF)
        INITIM = PRO(TIMINOFF,BUF)
      ELSEIF(WRKBUF(TTYP).EQ.TIGS) THEN !V02
        GTYP = WRKBUF(TGAMTYP)
        TRXTYP = WRKBUF(TTYP)
        TSUBTYP = WRKBUF(TIGS_TTYP)
        FINTIM = PRO(TSTAMP,BUF)
        INITIM = PRO(TIMINOFF,BUF)
      ELSEIF(WRKBUF(TTYP).GE.TWAG .AND. WRKBUF(TTYP).LE.TREF) THEN
        GTYP = WRKBUF(TGAMTYP)
        TRXTYP = WRKBUF(TTYP) !V02
        TSUBTYP = 0 !V02
        CALL GETTIM(FINTIM)
        INITIM = PRO(TSTAMP,BUF)
      ELSEIF(WRKBUF(TTYP).NE.TCMD .AND. WRKBUF(TTYP).NE.TGUI) THEN
        GTYP = TINS
        TRXTYP = WRKBUF(TTYP) !V02
        TSUBTYP = WRKBUF(TITYP) !V02
C        FINTIM = PRO(TSTAMP,BUF)
C        INITIM = PRO(TIMINOFF,BUF)
        CALL GETTIM(FINTIM)
        INITIM = PRO(TSTAMP,BUF)
      ENDIF

      DIFTIM = FINTIM - INITIM
      IF(DIFTIM .LE. 2) RETURN

      ST = LIB$GET_LUN(LU)

      ST = FOR$IOS_OPEFAI
      DO WHILE(ST.EQ.FOR$IOS_OPEFAI)
         OPEN (UNIT        = LU, 
     .        FILE        = 'GTECH$SUPWEL', 
     .        ACCESS      = 'APPEND', 
     .        STATUS      = 'UNKNOWN', 
     .        SHARED,
     .        IOSTAT      = ST)    
	 CALL XWAIT(50,1,WST)
      ENDDO

C      WRITE(LU,1000) PRO(SERIAL,BUF),GTNAMES(GTYP),DISTIM(INITIM),DISTIM(FINTIM),DIFTIM
C1000  FORMAT(1X,I10,3(',',A8),',',I6)

      WRITE(LU,1000) PRO(SERIAL,BUF),GTNAMES(GTYP),TTYPE(TRXTYP),TSUBTYP,DISTIM(INITIM),DISTIM(FINTIM),DIFTIM !V02
1000  FORMAT(1X,I10,',',A8,',',A4,',',I0,2(',',A8),',',I6) !V02

      CLOSE (LU)

      ST = LIB$FREE_LUN(LU)

      RETURN
      END
