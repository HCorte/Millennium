C CANSIM.FOR
C
C
C PREPARE A SIMULATED CANCELLATION
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
	SUBROUTINE CANSIM(EXT)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
C
	INTEGER*4 I
	INTEGER*4 CCDC, SER, JUL, SCRAM, CHECK,  EXT
	INTEGER*2 DBUF(12)
C
	INTEGER*4 TEMP
	BYTE	  TEMP1(4)
	EQUIVALENCE (TEMP,TEMP1)
C
	BYTE	CN1DAT(11)					   
C
	DATA (CN1DAT(I),I=1,11)/				   
     *                   Z20,   ! 1  control and sequence
     *                   Z20,   ! 2  type and SUBtype
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
C
C PROJECT EUROMIL - ASK FOR JULIAN DATE 
C
        CALL INPNUM('Enter Julian Sales Date:', JUL, 1, 999, EXT)
        IF(EXT.LT.0) RETURN
        IF (JUL .GE. 500) THEN
           CALL INPNUM('Enter External serial date       ',SCRAM,1,99999999,EXT)
           IF(EXT.LT.0) RETURN
           CALL INPNUM('Enter Check digits  ',CHECK,1,255,EXT)
           IF(EXT.LT.0) RETURN           
        ELSE 
           CALL INPNUM('Enter CDC date       ',CCDC,1,9999,EXT)
           IF(EXT.LT.0) RETURN
           CALL INPNUM('Enter internal SERIAL number  ',SER,1,2147483647,EXT)
           IF(EXT.LT.0) RETURN
           CALL OUTGEN(CCDC,SER,SCRAM,CHECK)
           DBUF(VCDC)=CCDC
           CALL CDATE(DBUF)
C        JUL = DBUF(VJUL) ! COMMENT FOR EUROMIL
        ENDIF
	CALL MOVBYT(CN1DAT,1,MESBUF1,1,11)	
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
	MESLEN=11				
C
	MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)
	CALL SETCHKSUM(MESBUF1,MESLEN)
C
	IF(MANCHG) CALL PERTUR(EXT)
	IF(EXT.LT.0) RETURN
C
	IF(NODISP) RETURN
C
	TYPE 900, (MESBUF1(I),I=1,11)
	RETURN
C
900     FORMAT(' Cancellation'/ '     mes: ',11Z3.2)
	END 
C
C
