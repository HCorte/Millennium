C SPESIM.FOR
C
C V02 11-SEP-95 SLK ESTONIAN MESSAGE FORMAT APPLIED TO SERVICE/ORDER
C V01 12-JUN-95 WXM INITIAL RELEASE
C
C PREPARE A SIMULATED SPECIAL TRANSACTIONS
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS	/CHECK=NOOVERFLOW
	SUBROUTINE SONSIM(EXT)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
C
	INTEGER*4 I
	INTEGER*4 EXT
	INTEGER*4 TEMP
	BYTE	  TEMP1(4)
	EQUIVALENCE (TEMP,TEMP1)
C
	BYTE	SN1DAT(109)
C
	DATA (SN1DAT(I),I=1,73)/
     *                   Z20,   ! 1  control and sequence
     *                   Z30,   ! 2  type and SUBtype
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z80,   ! 5  statistics (simulator transaction)
     *                   Z00,   ! 6  agent #
     *                   Z00,   ! 7  agent #
     *                   Z00,   ! 8  agent #
     *                   Z00,   ! 9  agent #
     *                   Z00,   ! 10  agent pass # (n/a)
     *                   Z00,   ! 11 agent pass # (n/a)
     *                   Z00,   ! 12 pass #
     *                   Z00,   ! 13 pass #
     *                   Z41,   ! 14 ter hrd id	  A
     *                   Z42,   ! 15 ter hrd id	  B
     *                   Z43,   ! 16 ter hrd id	  C
     *                   Z44,   ! 17 ter hrd id	  D
     *                   Z31,   ! 18 ter hrd id	  1
     *                   Z32,   ! 19 ter hrd id	  2
     *                   Z33,   ! 20 ter hrd id	  3
     *                   Z34,   ! 21 ter hrd id	  4
     *                   Z45,   ! 22 prt hrd id	  E
     *                   Z46,   ! 23 prt hrd id	  F
     *                   Z47,   ! 24 prt hrd id	  G
     *                   Z48,   ! 25 prt hrd id	  H
     *                   Z35,   ! 26 prt hrd id	  5
     *                   Z36,   ! 27 prt hrd id	  6
     *                   Z37,   ! 28 prt hrd id	  7
     *                   Z38,   ! 29 prt hrd id	  8
     *                   Z49,   ! 30 rdr hrd id	  I
     *                   Z4A,   ! 31 rdr hrd id	  J
     *                   Z4B,   ! 32 rdr hrd id	  K
     *                   Z4C,   ! 33 rdr hrd id	  L
     *                   Z39,   ! 34 rdr hrd id	  9
     *                   Z30,   ! 35 rdr hrd id	  0
     *                   Z31,   ! 36 rdr hrd id	  1
     *                   Z32,   ! 37 rdr hrd id	  2
     *                 8*Z00,   ! 38-45  unused hrd id  
     *                 8*Z00,   ! 46-53  unused hrd id  
     *                 8*Z00,   ! 54-61  unused hrd id  
     *                   Z41,   ! 62 cpu rom rev  A
     *                   Z41,   ! 63 cpu rom rev  A
     *                   Z31,   ! 64 cpu rom rev  1
     *                   Z31,   ! 65 cpu rom rev  1
     *                   Z42,   ! 66 prt rom rev  B
     *                   Z42,   ! 67 prt rom rev  B
     *                   Z32,   ! 68 prt rom rev  2
     *                   Z32,   ! 69 prt rom rev  2
     *                   Z43,   ! 70 rdr rom rev  C
     *                   Z43,   ! 71 rdr rom rev  C
     *                   Z33,   ! 72 rdr rom rev  3 
     *                   Z33/   ! 73 rdr rom rev  3
CCC	DATA (SN1DAT(I),I=74,109)/
CCC     *                   Z41,   ! 74 trm W-b id   A
CCC     *                   Z41,   ! 75 trm W-b id   A
CCC     *                   Z31,   ! 76 trm W-b id   1 
CCC     *                   Z31/   ! 77 trm W-b id   1
CCC     *                20*Z00,   ! 78  unused
CCC     *                   Z00/   ! 109 unused
C
	CALL CLRSCR(5)
	EXT=0
	TYPE*
C
	CALL MOVBYT(SN1DAT,1,MESBUF1,1,109)
C
	CALL INPNUM('Enter Agent ID',TEMP,1,99999999,EXT)
        IF(EXT.LT.0) RETURN
	MESBUF1(6)=TEMP1(4)
	MESBUF1(7)=TEMP1(3)
	MESBUF1(8)=TEMP1(2)
	MESBUF1(9)=TEMP1(1)
C
        CALL INPNUM('Enter Agent password',TEMP,1,9999,EXT)
        IF(EXT.LT.0) RETURN
	MESBUF1(10)=TEMP1(2)
	MESBUF1(11)=TEMP1(1)
C
        CALL INPNUM('Enter Terminal password',TEMP,1,9999,EXT)
        IF(EXT.LT.0) RETURN
	MESBUF1(12)=TEMP1(2)
	MESBUF1(13)=TEMP1(1)
C
	MESLEN=109
C
	MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)
	CALL SETCHKSUM(MESBUF1,MESLEN)
C
	IF(MANCHG) CALL PERTUR(EXT)
	IF(EXT.LT.0) RETURN
C
	IF(NODISP) RETURN
C
	TYPE 900, (MESBUF1(I),I=1,109)
C
	RETURN
C
900     FORMAT(' Signon     mes: ',/,(11X,20Z3.2))
	END 
C
C
C=======OPTIONS	/CHECK=NOOVERFLOW
	SUBROUTINE SOFSIM(EXT)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
C
	INTEGER*4 I
	INTEGER*4 EXT
C
	BYTE	SF1DAT(48)
C
	DATA (SF1DAT(I),I=1,4)/
     *                   Z20,   ! 1  control and sequence
     *                   Z3F,   ! 2  type and SUBtype
     *                   Z00,   ! 3  checksum
     *                   Z00/   ! 4  checksum
C
	EXT=0
	TYPE*
C
	CALL MOVBYT(SF1DAT,1,MESBUF1,1,4)
C
	MESLEN=4
C
	MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)
	CALL SETCHKSUM(MESBUF1,MESLEN)
C
	IF(MANCHG) CALL PERTUR(EXT)
	IF(EXT.LT.0) RETURN
C
	IF(NODISP) RETURN
C
	TYPE 900, (MESBUF1(I),I=1,4)
C
	RETURN
C
900    FORMAT(' Signoff'/ '     mes: ',4Z3.2)
	END 
C
C
C=======OPTIONS	/CHECK=NOOVERFLOW
	SUBROUTINE NEWSIM(EXT)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
C
C
	INTEGER*4 EXT
C
	INTEGER*4 I
C
	BYTE	  NW1DAT(6)
	DATA     (NW1DAT(I),I=1,6)/
     *                   Z20,   ! 1  control and sequence
     *                   Z64,   ! 2  type and subtype
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z00,   ! 5  class
     *                   Z00/   ! 6  subclass
C
	CALL MOVBYT(NW1DAT,1,MESBUF1,1,6)
C
	MESLEN=6
C
	MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)
	CALL SETCHKSUM(MESBUF1,MESLEN)
C
	IF(MANCHG) CALL PERTUR(EXT)
	IF(EXT.LT.0) RETURN
C
	IF(NODISP) RETURN
C
	TYPE 900, (MESBUF1(I),I=1,6)		    
C
	RETURN
C
900	FORMAT(' News Message'/ '     mes: ',6Z3.2)
	END 
C
C
C=======OPTIONS	/CHECK=NOOVERFLOW
	SUBROUTINE ORDSRVSIM(EXT)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
C
	INTEGER*4 I, FUNTYP, SUPTYP
	INTEGER*4 EXT
	CHARACTER STAR(0:1)
	DATA (STAR(I),I=0,1)/' ','*'/
C
	BYTE	  TEMP1(8)
C
	BYTE	  OR1DAT(6)
	DATA (OR1DAT(I),I=1,6)/
     *                   Z20,   ! 1  control and sequence
     *                   ZD0,   ! 2  type and SUBtype
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z00,   ! 5  order/service status
     *                   Z00/   ! 6  order/service bitmap
C
        CALL MOVBYT(OR1DAT,1,MESBUF1,1,6)
	CALL CLRSCR(5)
	EXT=0
	TYPE*
       	TYPE*, '     CHOOSE ONE OF THE FOLLOWING:'
	TYPE*, '     ----------------------------'
	TYPE*, '        (1) SUPPLY  ORDER'
	TYPE*, '        (2) SUPPLY  CONFIRMED'
	TYPE*, '        ---------------------    '
	TYPE*, '        (3) SERVICE REQUEST'
	TYPE*, '        (4) SERVICE COMPLETED'
	TYPE*, '     ----------------------------'
	TYPE*
1	CONTINUE
	CALL INPNUM('Enter option',FUNTYP,1,4,EXT)
	IF(EXT.EQ.-5) GOTO 1
        IF(EXT.LT.0) RETURN

	DO 2 I=1,8
	    TEMP1(I)=0
2	CONTINUE
	IF(FUNTYP.LE.2) THEN
	    MESBUF1(2)=MESBUF1(2)+1
	    IF(FUNTYP.EQ.1) THEN
		MESBUF1(5)='01'X
	    ELSE
		MESBUF1(5)='02'X
	    ENDIF
5	    CONTINUE
	    CALL CLRSCR(5)
	    TYPE*
	    IF(FUNTYP.EQ.1) THEN
		TYPE*, '   CHOOSE SUPPLY TYPE (ORDERED):'
	    ELSE
		TYPE*, '   CHOOSE SUPPLY TYPE (CONFIRMED):'
	    ENDIF
	    TYPE*
	    TYPE * , '      (1) PAPER           '//STAR(TEMP1(1))
	    TYPE * , '      (2) VR48 BETSLIPS   '//STAR(TEMP1(2))
	    TYPE * , '      (3) VREX BETSLIPS   '//STAR(TEMP1(3))
	    TYPE * , '      (4) KENO BETSLIPS   '//STAR(TEMP1(4))
	    TYPE *,  '      (5) PRINTER RIBBON  '//STAR(TEMP1(5))
	    TYPE *,  '      (6) OTHERS          '//STAR(TEMP1(6))
	    TYPE*
	    CALL INPNUM('Enter option or ''C'' to continue',SUPTYP,1,6,EXT)
	    IF(EXT.EQ.-1) RETURN
	    IF(EXT.LT.0) GOTO 10
	    IF(SUPTYP.EQ.1) TEMP1(1)=1
	    IF(SUPTYP.EQ.2) TEMP1(2)=1
	    IF(SUPTYP.EQ.3) TEMP1(3)=1
	    IF(SUPTYP.EQ.4) TEMP1(4)=1 
	    IF(SUPTYP.EQ.5) TEMP1(5)=1 
	    IF(SUPTYP.EQ.6) TEMP1(6)=1 
	    GOTO 5

10	    CONTINUE	    
	    MESBUF1(6)=TEMP1(1)*'01'X+TEMP1(2)*'02'X+TEMP1(3)*'04'X+
     *		      TEMP1(4)*'08'X+TEMP1(5)*'10'X+TEMP1(6)*'20'X
	    IF(MESBUF1(6).EQ.0) GOTO 5
	ELSE
	    MESBUF1(2)=MESBUF1(2)+2
	    IF(FUNTYP.EQ.3) THEN
		MESBUF1(5)='01'X
	    ELSE
		MESBUF1(5)='03'X
	    ENDIF

15          CONTINUE
            CALL CLRSCR(5)
	    TYPE*
            IF(FUNTYP.EQ.3) THEN
	      TYPE*, '   CHOOSE SERVICE TYPE (REQUESTED):'
	    ELSE
	      TYPE*, '   CHOOSE SERVICE TYPE (COMPLETED):'
	    ENDIF
            TYPE*
            TYPE * , '      (1) PRINTER         '//STAR(TEMP1(1))
            TYPE * , '      (2) VIDEO           '//STAR(TEMP1(2))
            TYPE *,  '      (3) READER          '//STAR(TEMP1(3))
            TYPE *,  '      (4) OTHERS          '//STAR(TEMP1(4))
            TYPE*
	    CALL INPNUM('Enter option or ''C'' to continue',SUPTYP,1,4,EXT)
	    IF(EXT.EQ.-1) RETURN
            IF(EXT.LT.0) GOTO 20
            IF(SUPTYP.EQ.1) TEMP1(1)=1
            IF(SUPTYP.EQ.2) TEMP1(2)=1
            IF(SUPTYP.EQ.3) TEMP1(3)=1
            IF(SUPTYP.EQ.4) TEMP1(4)=1
            GOTO 15

20          CONTINUE
            MESBUF1(6)=TEMP1(1)*'01'X+TEMP1(2)*'02'X+
     *                TEMP1(3)*'04'X+TEMP1(4)*'08'X
	    IF(MESBUF1(6).EQ.0) GOTO 15
	ENDIF
	MESLEN=6
C
	MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)
	CALL SETCHKSUM(MESBUF1,MESLEN)
C
	IF(MANCHG) CALL PERTUR(EXT)
	IF(EXT.LT.0) RETURN
C
	IF(NODISP) RETURN
C
	TYPE 900, (MESBUF1(I),I=1,6)
C
	RETURN
C
900     FORMAT(' Order/service'/ '     mes: ',6Z3.2)
	END 
C
