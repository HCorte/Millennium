C VALSIM.FOR
C
C
C PREPARE A SIMULATED VALIDATION
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
	SUBROUTINE VALSIM(EXT)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
C
	INTEGER*4 I,REG
        INTEGER*4 JUL, SCRAM, CHECK, EXT
C
	INTEGER*4 TEMP
	BYTE	  TEMP1(4)
	EQUIVALENCE (TEMP,TEMP1)
C
	BYTE	VL1DAT(19)
	DATA (VL1DAT(I),I=1,19)/
     *     Z20,   ! 1  control and sequence
     *     Z10,   ! 2  type and SUBtype   !0-regular,1-midtier,5-bank
     *     Z00,   ! 3  checksum
     *     Z00,   ! 4  checksum
     *     Z80,   ! 5  statistics (simulator transaction)
     *     Z00,   ! 6  julian date   (subtyp 0,1,5)
     *     Z00,   ! 7  julian date   (subtyp 0,1,5)
     *     Z00,   ! 8  serial number (subtyp 0,1,5)
     *     Z00,   ! 9  serial number (subtyp 0,1,5)
     *     Z00,   ! 10 serial number (subtyp 0,1,5)
     *     Z00,   ! 11 check digits  (subtyp 0,1,5)
     *     Z00,   ! 12 bank id (subtyp 5)
     *     Z00,   ! 13 bank id (subtyp 5)
     *     Z00,   ! 14 bank id (subtyp 5)
     *     Z00,   ! 15 bank id (subtyp 5)
     *     Z00,   ! 16 bank account (subtyp 5)
     *     Z00,   ! 17 bank account (subtyp 5)
     *     Z00,   ! 18 bank account (subtyp 5)
     *     Z00/   ! 19 bank account (subtyp 5)
C
	CALL CLRSCR(5)
	EXT=0
	TYPE*
        CALL INPNUM('Enter Julian Sales Date:', JUL, 1, 999, EXT)
        IF(EXT.LT.0) RETURN
        CALL INPNUM('Enter External Serial Number:', SCRAM, 1, 99999999, EXT)
        IF(EXT.LT.0) RETURN
        CALL INPNUM('Enter External Check Digits Number:', CHECK, 1, 999, EXT)
        IF(EXT.LT.0) RETURN
        CALL INPNUM('0: Regular, 1: Midtier, 2: Bank',REG,0,2,EXT)
        IF(EXT.LT.0) RETURN
        CALL MOVBYT(VL1DAT,1,MESBUF1,1,5)    ! subtype 0,5
C
	TEMP=JUL
        IF(REG .EQ. 1) MESBUF1(2) = MESBUF1(2) .OR. '01'X
	MESBUF1(6)=TEMP1(2)		
	MESBUF1(7)=TEMP1(1)		
	TEMP=SCRAM
        MESBUF1( 8)=TEMP1(3)		
        MESBUF1(9)=TEMP1(2)		
        MESBUF1(10)=TEMP1(1)		
	TEMP=CHECK
	MESBUF1(11)=TEMP1(1)		
 	MESLEN=11
c
	IF(REG.EQ.2) THEN
           MESBUF1(2)=MESBUF1(2) .OR. '05'X
           TEMP=BANKID
	   MESBUF1(12)=TEMP1(4)		
	   MESBUF1(13)=TEMP1(3)		
	   MESBUF1(14)=TEMP1(2)		
	   MESBUF1(15)=TEMP1(1)		
           TEMP=BANKACC
	   MESBUF1(16)=TEMP1(4)		
	   MESBUF1(17)=TEMP1(3)		
	   MESBUF1(18)=TEMP1(2)		
	   MESBUF1(19)=TEMP1(1)		
      	   MESLEN=19
        ENDIF       
C
	MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)
	CALL SETCHKSUM(MESBUF1,MESLEN)
C
	IF(MANCHG) CALL PERTUR(EXT)
	IF(EXT.LT.0) RETURN
C
	IF(NODISP) RETURN
C
	TYPE 900, (MESBUF1(I),I=1,MESLEN)		    
C
	RETURN
C
900     FORMAT(' Validation'/ '     mes: ',<MESLEN>Z3.2)
	END 
