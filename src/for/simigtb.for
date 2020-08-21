C SIMIGTB.FOR
C  
C V12 03-AUG-2005 FRP Modify for IPS Distribution.
C V11 15-JUN-2000 OXK CLEANUP W/ WARNINGS=ALL
C V10 17 Apr 1996 HXK Release of Finland for X.25, Telephone Betting, 
C			Instant Pass Thru Phase 1
C V09 10 Jan 1995 SHEATHCOCK  Added support for new prize values.
C V08 06-JAN-1995 SMH NEW PRIZE VALUES
C V07 22 Sep 1994 MCM START INDEX AT 2 (BYTE 1 IS RESERVED)
C V06 08 Sep 1994 MCM SWAPPING BYTES IS NO LONGER NECESSARY ON THE DEC LMS
C V05 30 Mar 1994 MCM Initial revision.
C V04 03 Jan 1994 SYSTEM  Applying PVCS header for automatic revision history
C V03 21 Dec 1993 SYSTEM  Initial revision.
C V02 28-MAY-1992 TKO UPDATED FOR NEW MESSAGE FORMATS
C V01 13-NOV-1991 JPJ RELEASED FOR VAX (INSTANTS)
C
C SUBROUTINE TO FILL IN CROSS SYSTEM GAME TABLE MESSAGE.
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
	SUBROUTINE SIMIGTB(OUTTAB,TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CRSBUF.DEF'
C
        INTEGER*4 IND, I
	BYTE      OUTTAB(*)
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)

        INTEGER*4 GAMES(MAXGTA)
        DATA GAMES/34501,34502,34503,34504,34505,34506,34507,34508,34509,34510,
     *             34511,34512,34513,34514,34515,34516,34517,34518,34519,34520,
     *             34521,34522,34523,34524,34525,34526,34527,34528,34529,34530,
     *             34531,34532,34533,34534,34535,34536,34537,34538,34539,34540/
      
C
C       FILL IN LENGTH
C
	IND=2
        IF(TRABUF(TIGMC).EQ.1) THEN
          I4TEMP=174
        ELSE
          I4TEMP=98
        ENDIF
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
C
C     FILL IN ERROR CODE
C
	IND=12    !!! SMH WAS 14
	I4TEMP=99
	OUTTAB(IND+0) = I1TEMP(1)
	OUTTAB(IND+1) = I1TEMP(2)
	IND=IND+2
C
C       FILL IN CLASS NUMBER (NOT USED)
C
        IND=IND+2
C
C       FILL IN INFORMATION FOR REQUEST 1
C
        IF(TRABUF(TIGMC).EQ.1) THEN
          DO 100 I=1, MAXGTA
             I4TEMP=GAMES (I)
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     OUTTAB(IND+2) = I1TEMP(3)
             OUTTAB(IND+3) = I1TEMP(4)
	     IND=IND+4
100       CONTINUE
        ELSE
C
C         FILL IN INFORMATION FOR REQUEST 2
C
          DO 200 I=1,MAXTBL
C
C            FILL IN GAME NUMBER
C
             I4TEMP=I
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE ONE INFO   - SEND IN CENTS
C
             I4TEMP=1 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE TWO INFO
C
             I4TEMP=2 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE THREE INFO
C
             I4TEMP=3 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE FOUR INFO
C
             I4TEMP=4 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE FIVE INFO
C
             I4TEMP=5 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE SIX INFO
C
             I4TEMP=6 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE SEVEN INFO
C
             I4TEMP=7 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE EIGHT INFO
C
             I4TEMP=8 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE NINE INFO
C
             I4TEMP=9 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE TEN INFO
C
             I4TEMP=10 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE ELEVEN INFO
C
             I4TEMP=11 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE TWELVE INFO
C
             I4TEMP=12 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE THIRTEEN INFO
C
             I4TEMP=13 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE FOURTEEN INFO
C
             I4TEMP=14 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE FIFTEEN INFO
C
             I4TEMP=15 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE SIXTEEN INFO
C
             I4TEMP=16 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE SEVENTEEN INFO
C
             I4TEMP=17 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE EIGHTEEN INFO
C
             I4TEMP=18 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE NINETEEN INFO
C
             I4TEMP=19 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
C
C            FILL IN PRIZE TWENTY INFO
C
             I4TEMP=20 * 100
	     OUTTAB(IND+0) = I1TEMP(1)
             OUTTAB(IND+1) = I1TEMP(2)
	     IND=IND+2
200       CONTINUE
        ENDIF
C
C
	RETURN
	END
