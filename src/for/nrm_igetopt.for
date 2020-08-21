C
C SUBROUTINE IGETOPT
C
C V07 30-NOV-2010 FJG TWEMSER/TWEMCHK replaced by TWLNKSER/TWLNKCHK
C V06 03-apr-2009 trg add euromillion addon on Joker
C V05 29-NOV-2000 UXN TOTOGOLO ADDED
C V04 21-AUG-1992 GCAN CHECK FOR KICKER OPTION ON TOTO SELECT BETS.
C V03 11-FEB-1992 GCAN FIX FOR CHECKING THE WEDNESDAY DRAW OPTION.
C V02 07-OCT-1991 MTK  INITIAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX  RELEASED FOR VAX
C
C SUBROUTINE TO GET WAGER OPTION FLAGS FOR
C REPRINT AND EXCHANGE TICKETS
C
C
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE IGETOPT(TRABUF,OPTION)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'


	INTEGER*4 OPTION




C-------------------------------------------------------

	OPTION=0

	IF(TRABUF(TWQPF).NE.0)   OPTION=OPTION+'0200'X    !Quick Pick flag
	IF(TRABUF(TWSYSN).NE.0)  OPTION=OPTION+'0100'X    !System Number

	IF (TRABUF(TGAMTYP).EQ.TLTO   .OR.
     * 	    TRABUF(TGAMTYP).EQ.TSPT   .OR.
     *      TRABUF(TGAMTYP).EQ.TTGL) THEN
                OPTION=OPTION+'0020'X    !Always set
	        IF(TRABUF(TWKFLG).NE.0)  OPTION=OPTION+'0080'X !Particip Joker 1
	        IF(TRABUF(TWKFLG2).NE.0) OPTION=OPTION+'0040'X !Particip Joker 2
                IF(TRABUF(TWKFLG2).NE.0) OPTION=OPTION+'0010'X
	ENDIF

	IF (TRABUF(TGAMTYP).EQ.TKIK) THEN
          OPTION=OPTION+'0020'X    !Always set
          IF(TRABUF(TWLNKSER) .NE. 0  .AND. TRABUF(TWLNKCHK) .NE. 0) OPTION=OPTION+'0004'X  ! v06
	ENDIF

        IF (TRABUF(TGAMTYP).EQ.TSPT   .AND.
     *      TRABUF(TWWEQP).NE.0)     THEN
                OPTION=OPTION+'0001'X
        ENDIF

	IF(TRABUF(TWBNKID).NE.0) OPTION=OPTION+'0008'X    !Bank Number

	RETURN
	END         !IGETOPT.FCC
