C SUBROUTINE FULLANL
C  
C V03 03-MAR-2000 OXK Layout fix (Vakio changes)
C v02 28 Jun 1993 HXN Change in N1X2, 123 to 1X2.
C v01 21 Jan 1993 DAB Initial Release
C  			Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  			DEC Baseline
C
C     FULLANL(SYSBET,NUMROWS,TOTAL)
C     IN - SYSBET      - BET
C          NUMROWS     - NUMBER OF ROWS IN BET
C     OUT - TOTAL      - TOTAL NR OF CORRESPONDING SINGLE BOARDS
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
	SUBROUTINE FULLANL(SYSBET,NUMROWS,TOTAL)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INTEGER*4 BET, OFF, TOT, NUMROWS
C
	INTEGER*2 SYSBET(*)
	INTEGER*4 TOTAL
	INTEGER*4 N1X2(0:7) /'    ',' 1--',' -X-',' 1X-',' --2',' 1-2',
     *	                     ' -X2',' 1X2'/
	INTEGER*4 CNTMARK(0:15)      !COUNT OF BITS FOR VALS 0:15
	DATA CNTMARK/0,1,1,2,1,2,2,3,8*0/
	INTEGER*4 DISP(SPGNBR)
C
	TOT=1
	DO 10, OFF=1,NUMROWS
	   BET=SYSBET(OFF)
	   TOT=TOT*CNTMARK(BET)              !UPDATE CORRESPONDING
C                                          ;NR OF SINGLE BETS
	   DISP(OFF)=BET                     !DISPLAY TABLE
10	CONTINUE
	WRITE (6,950) TOT,(N1X2(DISP(OFF)),OFF=1,NUMROWS)
	TOTAL=TOTAL+TOT
	RETURN
C950	FORMAT(1H ,'Comb= ',I4,'  ',16(1X,A4))
950	FORMAT(1H ,'Comb= ',I4,' ',16(A4))
	END
