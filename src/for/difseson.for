C  GXSRC:DIFSESON.FOR
C
C V02 06-OCT-2000 UXN AlphaIPS release.
C V01 07-NOV-1997 DXA INITIAL RELEASE FOR UK NATIONAL LOTTERY
C
C SUBROUTINE TO DECODE INSTANT FSE SIGN-ON/OFF MESSAGE FROM TERMINAL
C
C NOTE : THERE ARE THREE TYPES :-
C
C 1. TRABUF(TIFSETYP) = 0 > FSE SIGN-ON  WITH    PASSWORD CHECKING (SEND TO IPS)
C 2. TRABUF(TIFSETYP) = 1 > FSE SIGN-ON  WITHOUT PASSWORD CHECKING (DO NOT SEND TO IPS)
C 3. TRABUF(TIFSETYP) = 2 > FSE SIGN-OFF WITHOUT PASSWORD CHECKING (DO NOT SEND TO IPS)
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXTEND_SOURCE
C
      SUBROUTINE DIFSESON(TERMES,TRABUF,MESLEN,SALPAS)
C
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:AGTCOM.DEF'
      INCLUDE 'INCLIB:DESTRA.DEF'
      INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
      BYTE TERMES(*)
C
      INTEGER*2 MESLEN
C
      INTEGER*4 SALPAS,
     +          TEMP,
     +          CHKLEN,
     +          MYCHKSUM,
     +          ENCMES,
     +          OPTIONS,
     +          ENCACT,
     +          IND,
     +          TER,
     +          TYP
C
      LOGICAL*4 ONLINE
C
C GET SEQUENCE NUMBER
C
      TEMP = ZEXT(TERMES(1))
      TRABUF(TTRN)=IAND(TEMP,15)
C
C GET CHECKSUM
C
      CALL TERM_TO_HOST(TERMES(3),TRABUF(TCHK),2)
C
C GET STATISTICS
C
      TRABUF(TTSTCS) = ZEXT(TERMES(5))
C
C GET OPTION FLAG
C
      TER = TRABUF(TTER)
      TYP = AGTTAB(AGTTYP,TER)
C
      ONLINE = (TSBIT(TYP,AGTTOI))
C
      CALL TERM_TO_HOST(TERMES(6),OPTIONS,2)
C
      IF ((OPTIONS.NE.0).AND.(ONLINE)) THEN
C
        TRABUF(TERR) = SYNT
        SYNTERRCOD   = 85
        GOTO 8000
C
      ENDIF
C
      IND = 8
C
C CHECK FOR NODE NUMBER (NOT USED IN TEXAS)
C
      IF (IAND(OPTIONS,'01'X).NE.0) THEN
        IND = IND + 4
      ENDIF
C
C CHECK FOR CLERK NUMBER (NOT USED IN TEXAS)
C
      IF (IAND(OPTIONS,'02'X).NE.0) THEN
        IND = IND + 4
      ENDIF
C
C CHECK FOR PASSWORD NUMBER (NOT USED IN TEXAS)
C
      IF (IAND(OPTIONS,'04'X).NE.0) THEN
        IND = IND + 2
      ENDIF
C
C CHECK FOR ORIGINATOR NUMBER (NOT USED IN TEXAS)
C
      IF (IAND(OPTIONS,'08'X).NE.0) THEN
        IND = IND + 2
      ENDIF
C
C CHECK FOR LOCATION NUMBER (NOT USED IN TEXAS)
C
      IF (IAND(OPTIONS,'10'X).NE.0) THEN
        IND = IND + 4
      ENDIF
C
C CHECK FOR USER ID (NOT USED IN TEXAS)
C
      IF (IAND(OPTIONS,'20'X).NE.0) THEN
        IND = IND + 4
      ENDIF
C
C CHECK FOR OPERATOR PASSWORD (NOT USED IN TEXAS)
C
      IF (IAND(OPTIONS,'40'X).NE.0) THEN
        IND = IND + 4
      ENDIF
C
C GET SIGN-ON/OFF TYPE
C
      TRABUF(TIFSETYP) = ZEXT(TERMES(IND))
C
      IND = IND + 1
C
      IF (TRABUF(TIFSETYP).NE.0) THEN !SIGN-OFF
C
         TRABUF(TIFSETYP) = 2
C
      ELSEIF (P(FSE_SNON).NE.FSE_SNON_ON) THEN !SIGN-ON WITHOUT PASSWORD CHECKING
C
         TRABUF(TIFSETYP) = 1
C
      ENDIF
C
C GET FSE NUMBER
C
      CALL TERM_TO_HOST(TERMES(IND),TRABUF(TIFSEREP),4)
      IND = IND + 4
C
C GET FSE PASS NUMBER
C
      CALL TERM_TO_HOST(TERMES(IND),SALPAS,2)
      IND = IND + 2
C
C GET FSE CLASS NUMBER
C
      TRABUF(TIFSECLS) = ZEXT(TERMES(IND))
      IND = IND + 1
C
C CHECK MESSAGE CHECKSUM
C
      IF (P(SUPSUM).EQ.0) THEN
        I4CCITT=IAND(BASECHKSUM+TRABUF(TTER),'FFFF'X)
	CALL HOST_TO_TERM(TERMES(3),I4CCITT,2)
        CHKLEN=MESLEN-1
        CALL GETCCITT(TERMES,1,CHKLEN,MYCHKSUM)
        IF (MYCHKSUM.NE.TRABUF(TCHK)) TRABUF(TERR)=CBAD
      ENDIF
C
C CHECK FOR DES ERROR
C
      IF (P(DESACT).EQ.0) THEN
        ENCMES = ZEXT(TERMES(1))
        ENCMES = IAND(ENCMES,'08'X)
        IF ((P(DESFLG).EQ.0).AND.
     +      (BTEST(AGTTAB(AGTTYP,TRABUF(TTER)),AGTDES))) THEN
          ENCACT = '08'X
        ELSE
          ENCACT = 0
        ENDIF
        IF (ENCMES.NE.ENCACT) TRABUF(TERR) = DESMOD
      ENDIF
C
8000  CONTINUE
C
      IF (TRABUF(TERR).NE.NOER) TRABUF(TSTAT) = REJT
C
      RETURN
      END
