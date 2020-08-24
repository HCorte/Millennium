C SRPSIM.FOR
C
C
C V01 12-JUN-95 WXM INITIAL RELEASE
C
C
C PREPARE A SIMULATED SALES REPORT
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
	SUBROUTINE FINSIM(EXT)
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
	INTEGER*4 TEMP
	BYTE	  TEMP1(4)
	EQUIVALENCE (TEMP,TEMP1)
C
	BYTE	FN1DAT(14)
C
	DATA (FN1DAT(I),I=1,14)/
     *                   Z20,   ! 1  control and sequence
     *                   Z63,   ! 2  type and subtype
     *                   Z00,   ! 3  checksum
     *                   Z00,   ! 4  checksum
     *                   Z00,   ! 5  class
     *                   Z00,   ! 6  subclass
     *                   Z00,   ! 7  report type
     *                   Z00,   ! 8  report pass # (n/a)
     *                   Z00,   ! 9  report pass # (n/a)
     *                   Z00,   ! 10 agent/terminal # to report on
     *                   Z00,   ! 11 agent/terminal # to report on
     *                   Z00,   ! 12 agent/terminal # to report on
     *                   Z00,   ! 13 agent/terminal # to report on
     *                   Z00/   ! 14 lottery ID
C
	CALL MOVBYT(FN1DAT,1,MESBUF1,1,14)
C
	MESBUF1(2)='63'X
C
	CALL CLRSCR(5)
	EXT=0
	TYPE*
	WRITE(5,901)
        CALL INPNUM('Enter report class',TEMP,1,9,EXT)
        IF(EXT.LT.0) RETURN
	MESBUF1(5)=TEMP1(1)
	IF(TEMP1(1).EQ.9) MESBUF1(2)='69'X
C
	TYPE*
	WRITE(5,902)
        CALL INPNUM('Enter reported day/period',TEMP,0,8,EXT)
        IF(EXT.LT.0) RETURN
	MESBUF1(6)=TEMP1(1)
C
	TYPE*
	WRITE(5,903)
        CALL INPNUM('Enter report type',TEMP,0,1,EXT)
        IF(EXT.LT.0) RETURN
	MESBUF1(7)=TEMP1(1)
C
	IF(MESBUF1(6).EQ.0.AND.MESBUF1(7).EQ.2) THEN
          CALL INPNUM('Enter Lottery ID',TEMP,0,1,EXT)
          IF(EXT.LT.0) RETURN
	  MESBUF1(14)=TEMP1(1)
	ENDIF
C
	MESLEN=14
C
	MESBUF1(1) = (MESBUF1(1).AND.'F0'X) .OR. SEQNO(TER)
	CALL SETCHKSUM(MESBUF1,MESLEN)
C
	IF(MANCHG) CALL PERTUR(EXT)
	IF(EXT.LT.0) RETURN
C
	IF(NODISP) RETURN
C
	TYPE 904, (MESBUF1(I),I=1,14)		    
C
	RETURN
901	FORMAT(1X,'Available Financial Reports:',//,
     *	       1X,'      1 - Total Summary',/,
     *	       1X,'      2 - Online Sales',/,
     *	       1X,'      3 - n/a',/,
     *	       1X,'      4 - Online Validations',/,
     *	       1X,'      5 - n/a',/,
     *	       1X,'      6 - Monthly Commissions',/,
     *         1X,'      7 - n/a',/,
     *         1X,'      8 - Commissions',/,
     *         1X,'      9 - Invoice',/)
902	FORMAT(1X,'Day/Period of the Report:',//,
     *	       1X,'      0 - Today',/,
     *	       1X,'      1 - Monday',/,
     *	       1X,'      2 - Tuesday',/,
     *	       1X,'      3 - Wednesday',/,
     *	       1X,'      4 - Thursday',/,
     *	       1X,'      5 - Friday',/,
     *	       1X,'      6 - Saturday',/,
     *	       1X,'      7 - Sunday',/,
     *	       1X,'      8 - Weekly',/)
903	FORMAT(1X,'Available Report Types:',//,
     *	       1X,'      0 - Regular',/,
     *	       1X,'      1 - Specific Agent',/)
904     FORMAT(' Financial Report'/ '     mes: ',14Z3.2)
	END 
