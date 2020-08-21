C
C SUBROUTINE BLDJAK
C
C V03 29-NOV-2000 UXN TOTGOLA ADDED
C V02 12-JUN-2000 UXN Cleaned up.
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C
C BLDJAK
C
C SUBROUTINE TO BUILD JACKPOT MESSAGE TO BROADCAST TO ALL TERMINALS.
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
	SUBROUTINE BLDJAK(OUTLEN,MESNUM,MESTAB)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
	INCLUDE 'INCLIB:TGLCOM.DEF'
	INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
	BYTE	    MESTAB(*)			!Output Table
	BYTE	    I1TEMP(4)			!Temp. Variable
C
        INTEGER*2   OUTLEN                      !Message Length (incl Contrl).
C
	INTEGER*4   IND				!Index into the Output Table.
	INTEGER*4   MESNUM			!Message Number (Not used).
	INTEGER*4   GNUM			!Game Number.
	INTEGER*4   GIND			!Game Index.
	INTEGER*4   PLFLAG			!Pool Flag (Game Status).
	INTEGER*4   POOL			!Total Pool.
	INTEGER*4   LTOT			!Lotto Total Sales.
	INTEGER*4   STOT			!Sport Total Sales.
	INTEGER*4   TTOT			!TOTOGOLA.
	INTEGER*4   GAMCNT			!# Games Sent in Message.
	INTEGER*4   TIME(3)			!Holds Time from ICLOCK.
	INTEGER*4   DRWCDC			!Cdc date of Draw.
	INTEGER*4   GNMIND			!Index for # games.
	INTEGER*4   I				!Loop Variable(s)
	INTEGER*4   I4TEMP			!Temp. Variable
C
	EQUIVALENCE (I4TEMP,I1TEMP(1))
C
C SET / INITIALIZE VARIABLES
C
	IND = 1
	LTOT = 0
	STOT = 0
	TTOT = 0
	GAMCNT = 0
	MESNUM = 0				!Not Used
	OUTLEN = 0
C
C CHECK IF JACKPOT REPORTS ARE SUPPRESED GLOBALY
C
	IF(TSBIT(P(SUPRPT),INFREP)) RETURN
C
C STORE CONTROL/SEQUENCE AND TYPE/SUBTYPE
C
	MESTAB(IND+0) = '20'X
	MESTAB(IND+1) = 'B3'X
	IND = IND + 2
C
C SAVE SPACE FOR THE CHECKSUM
C
	IND = IND + 2
C
C SAVE SPACE FOR # GAMES SENT IN MESSAGE.
C
	GNMIND = IND
	IND = IND + 1
C
C GET TIME
C
	CALL ICLOCK(0,TIME)
	MESTAB(IND+0) = TIME(1)
	MESTAB(IND+1) = TIME(2)
	MESTAB(IND+2) = TIME(3)
	IND = IND + 3
C
C GET POOL FOR ALL LOTTO GAMES
C
	DO 100 GIND = 1,NUMLTO
	   GNUM = GTNTAB(TLTO,GIND)
	   IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) GOTO 100
	   IF(TSBIT(P(SUPRPT),GNUM)) GOTO 100        !Jackpot report suppressed.
	   GAMCNT = GAMCNT + 1
C
	   DO 110 I = 1,LTGENT
	      LTOT = LTOT + LTOSAL(I,GIND)
110	   CONTINUE
C
	   POOL = IDNINT(DFLOAT(LTOT) * CALPER(LTOSPR(GIND)))
	   POOL = IDNINT(DFLOAT(POOL) * CALPER(LTOPER(1,GIND)))
     *				        + LTOAPL(GIND)
	   IF(POOL.LT.LTOMIN(GIND)) POOL = LTOMIN(GIND)
	   IF(LTOOPA(GIND).NE.0)    POOL = LTOOPA(GIND)
	   PLFLAG = 1                                    !Closed
	   IF(LTOSTS(GIND).LE.GAMBFD) PLFLAG = 0         !Open
	   DRWCDC = LTODAT(1,GIND)
C
C BUILD MESSAGE FOR GAME
C
	   MESTAB(IND) = ISHFT(TLTO,4) + GIND		 !Game Type/Index
	   IND = IND + 1
	   I4TEMP = DRWCDC				 !Draw Cdc Date
	   MESTAB(IND+0) = I1TEMP(2)
	   MESTAB(IND+1) = I1TEMP(1)
	   IND = IND + 2
	   I4TEMP = POOL * DYN_BETUNIT				 !Pool Amount
	   MESTAB(IND+0) = I1TEMP(4)
	   MESTAB(IND+1) = I1TEMP(3)
	   MESTAB(IND+2) = I1TEMP(2)
	   MESTAB(IND+3) = I1TEMP(1)
	   IND = IND + 4
	   MESTAB(IND) = PLFLAG				 !Pool Flag
	   IND = IND + 1
100	CONTINUE
C
C GET POOL FOR ALL SPORT GAMES
C
        DO 200 GIND = 1,NUMSPT
           GNUM = GTNTAB(TSPT,GIND)
           IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) GOTO 200
	   IF(TSBIT(P(SUPRPT),GNUM)) GOTO 200	    !Jackpot report supressed.
           GAMCNT = GAMCNT + 1
C
           DO 210 I = 1,SPGENT
              STOT = STOT + SPTSAL(I,GIND)
210        CONTINUE
C
           POOL = IDNINT(DFLOAT(STOT) * CALPER(SPTSPR(GIND)))
           POOL = IDNINT(DFLOAT(POOL) * CALPER(SPTPER(1,GIND)))
     *                                  + SPTAPL(GIND)
           IF(POOL.LT.SPTMIN(GIND)) POOL = SPTMIN(GIND)
           IF(SPTOPA(GIND).NE.0)    POOL = SPTOPA(GIND)
           PLFLAG = 1                                    !Closed
           IF(SPTSTS(GIND).LE.GAMBFD) PLFLAG = 0         !Open
           DRWCDC = SPTDAT(1,GIND)
C
C BUILD MESSAGE FOR GAME
C
           MESTAB(IND) = ISHFT(TSPT,4) + GIND	  	 !Game Type/Index
           IND = IND + 1
           I4TEMP = DRWCDC                               !Draw Cdc Date
           MESTAB(IND+0) = I1TEMP(2)
           MESTAB(IND+1) = I1TEMP(1)
           IND = IND + 2
           I4TEMP = POOL * DYN_BETUNIT                   !Pool Amount
           MESTAB(IND+0) = I1TEMP(4)			 !Dolar Amount in Dolar.
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
           MESTAB(IND) = PLFLAG                          !Pool Flag
           IND = IND + 1
200     CONTINUE	
C
C GET POOL FOR ALL TOTOGOLA GAMES
C
        DO 300 GIND = 1,NUMTGL
           GNUM = GTNTAB(TTGL,GIND)
           IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) GOTO 300
	   IF(TSBIT(P(SUPRPT),GNUM)) GOTO 300	    !Jackpot report supressed.
           GAMCNT = GAMCNT + 1
C
           DO 310 I = 1,TGGENT
              TTOT = TTOT + TGLSAL(I,GIND)
310        CONTINUE
C
           POOL = IDNINT(DFLOAT(TTOT) * CALPER(TGLSPR(GIND)))
           POOL = IDNINT(DFLOAT(POOL) * CALPER(TGLPER(1,GIND)))
     *                                  + TGLAPL(GIND)
           IF(POOL.LT.TGLMIN(GIND)) POOL = TGLMIN(GIND)
           IF(TGLOPA(GIND).NE.0)    POOL = TGLOPA(GIND)
           PLFLAG = 1                                    !Closed
           IF(TGLSTS(GIND).LE.GAMBFD) PLFLAG = 0         !Open
           DRWCDC = TGLDAT(1,GIND)
C
C BUILD MESSAGE FOR GAME
C
           MESTAB(IND) = ISHFT(TTGL,4) + GIND	  	 !Game Type/Index
           IND = IND + 1
           I4TEMP = DRWCDC                               !Draw Cdc Date
           MESTAB(IND+0) = I1TEMP(2)
           MESTAB(IND+1) = I1TEMP(1)
           IND = IND + 2
           I4TEMP = POOL * DYN_BETUNIT                   !Pool Amount
           MESTAB(IND+0) = I1TEMP(4)			 !Dolar Amount in Dolar.
           MESTAB(IND+1) = I1TEMP(3)
           MESTAB(IND+2) = I1TEMP(2)
           MESTAB(IND+3) = I1TEMP(1)
           IND = IND + 4
           MESTAB(IND) = PLFLAG                          !Pool Flag
           IND = IND + 1
300     CONTINUE	
C
C UPDATE NUMBER OF GAMES TO SEND.
C IF # GAMES = ZERO SET OUTLEN TO ZERO IN ORDER NOT TO SEND MESSAGE TO TERMS.
C
	IF(GAMCNT.LE.0) IND = 1
	MESTAB(GNMIND) = GAMCNT
C
	OUTLEN = IND - 1
	RETURN
	END
