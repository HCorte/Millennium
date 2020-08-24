C SRPSIM.FOR
C
C
C V01 12-JUN-95 WXM INITIAL RELEASE
C
C
C PREPARE A SIMULATED GAME RESULTS REPORT
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
	SUBROUTINE GAMSIM(EXT)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:TERSIM.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
C
	INTEGER*4 I, RCDC
	INTEGER*4 EXT
C
	INTEGER*4 TEMP
	INTEGER*2 TEMP2(2)
	BYTE	  TEMP1(4)
	EQUIVALENCE (TEMP,TEMP2,TEMP1)
C
	INTEGER*2 DATE(12)
C
	BYTE	GN1DAT(9)
C
	DATA (GN1DAT(I),I=1,9)/
     *                   Z20,   ! 1  control and sequence
     *                   Z61,   ! 2  type and subtype
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z00,   ! 5  class
     *                   Z00,   ! 6  subclass
     *                   Z00,   ! 7  month
     *                   Z00,   ! 8  day
     *                   Z00/   ! 9  year
C
	CALL MOVBYT(GN1DAT,1,MESBUF1,1,9)
C
	CALL CLRSCR(5)
	EXT=0
	TYPE*
	WRITE(5,901)
	DO 10 I=1,MAXTYP
	  WRITE(5,902) I,GTNAMES(I)
10	CONTINUE
	TYPE*
        CALL INPNUM('Enter game type',TEMP,1,MAXTYP,EXT)
        IF(EXT.LT.0) RETURN
	MESBUF1(5)=TEMP1(1)
        CALL INPNUM('Enter game index:',TEMP,1,MAXIND,EXT)
        IF(EXT.LT.0) RETURN
	MESBUF1(6)=TEMP1(1)
C
        CALL INPDAT(RCDC,EXT)
        IF(EXT.LT.0) RETURN
C
	DATE(VCDC)=RCDC
	CALL CDATE(DATE)
	MESBUF1(7)=DATE(VMON)
	MESBUF1(8)=DATE(VDAY)
	MESBUF1(9)=DATE(VYEAR)
C
	MESLEN=9
C
	MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)
	CALL SETCHKSUM(MESBUF1,MESLEN)
C
	IF(MANCHG) CALL PERTUR(EXT)
	IF(EXT.LT.0) RETURN
C
	IF(NODISP) RETURN
C
	TYPE 903, (MESBUF1(I),I=1,9)		    
C
	RETURN
C
901	FORMAT(1X,'Available Game Types:',/)
902	FORMAT(1X,6X,I2,' - ',A8)
903     FORMAT(' Game Report'/ '     mes: ',9Z3.2)
C
	END 
