C FRACSIM.FOR
C
C PREPARE A SIMULATED BET FRACTION
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS	/CHECK=NOOVERFLOW
	SUBROUTINE FRACSIM(EXT)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
C
	INTEGER*4 I
	INTEGER*4 CCDC, SER, JUL, SCRAM, CHECK, EXT
	INTEGER*2 DBUF(12)
C
	INTEGER*4 TEMP
	BYTE	  TEMP1(4)
	EQUIVALENCE (TEMP,TEMP1)
C
	BYTE	FN1DAT(12)
	BYTE	UN1DAT(11)
        INTEGER*4 NUMFRAC
C
	DATA (FN1DAT(I),I=1,12)/	
     *                   Z20,   ! 1  control and sequence
     *                   ZF1,   ! 2  type and SUBtype
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z80,   ! 5  statistics (simulator transaction)
     *                   Z00,   ! 6  julian date
     *                   Z00,   ! 7  julian date
     *                   Z00,   ! 8  serial number
     *                   Z00,   ! 9  serial number
     *                   Z00,   ! 10 serial number
     *                   Z00,   ! 11 check digits
     *                   Z00/   ! 12 number of tickets
	DATA (UN1DAT(I),I=1,11)/	
     *                   Z20,   ! 1  control and sequence
     *                   ZFF,   ! 2  type and SUBtype
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z80,   ! 5  statistics (simulator transaction)
     *                   Z00,   ! 6  julian date
     *                   Z00,   ! 7  julian date
     *                   Z00,   ! 8  serial number
     *                   Z00,   ! 9  serial number
     *                   Z00,   ! 10 serial number
     *                   Z00/   ! 11 check digits
C
	CALL CLRSCR(5)
	EXT=0
	TYPE*
        CALL INPNUM('Enter CDC date       ',CCDC,1,9999,EXT)
        IF(EXT.LT.0) RETURN
        CALL INPNUM('Enter internal SERIAL number  ',SER,1,99999999,EXT)
        IF(EXT.LT.0) RETURN
        CALL OUTGEN(CCDC,SER,SCRAM,CHECK)
        DBUF(VCDC)=CCDC
        CALL CDATE(DBUF)
        JUL = DBUF(VJUL)
        CALL INPNUM('Enter # of fracs (0-unfrac or 1,2,5,10)'
     *              ,NUMFRAC,0,10,EXT)
        IF(EXT.LT.0) RETURN
	IF(NUMFRAC.EQ.0) THEN
           CALL MOVBYT(UN1DAT,1,MESBUF1,1,11)
           MESLEN=11
        ELSE
           CALL MOVBYT(FN1DAT,1,MESBUF1,1,12)
           MESLEN=12
           TEMP=NUMFRAC
  	   MESBUF1(12)=TEMP1(1)		
        ENDIF
	TEMP=JUL
	MESBUF1(6)=TEMP1(2)		
	MESBUF1(7)=TEMP1(1)		
	TEMP=SCRAM
        MESBUF1( 8)=TEMP1(3)		
        MESBUF1( 9)=TEMP1(2)		
        MESBUF1(10)=TEMP1(1)		
	TEMP=CHECK
	MESBUF1(11)=TEMP1(1)		
C
	MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)
	CALL SETCHKSUM(MESBUF1,MESLEN)
C
	IF(MANCHG) CALL PERTUR(EXT)
	IF(EXT.LT.0) RETURN
C
	IF(NODISP) RETURN
C
	IF(NUMFRAC.EQ.0) THEN
  	   TYPE 900, (MESBUF1(I),I=1,11)
        ELSE
  	   TYPE 901, (MESBUF1(I),I=1,12)
        ENDIF
	RETURN
C
900     FORMAT(' Unfraction'/ '     mes: ',11Z3.2)
901     FORMAT(' Fraction'/ '       mes: ',12Z3.2)
	END 
C
C
