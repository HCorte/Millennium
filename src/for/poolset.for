C SUBROUTINE POOLSET
C
C V05 29-FEB-2000 OXK LTPOOLDRW checked before setting it
C V04 14-FEB-2000 UXN Vakio changes.
C V03 04-DEC-1995 HXK Made changes for LTPOOL_GAMENR not having MAXTYP as 
C                     array size!
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C SET CHANGEABLE LOTTO POOLS PARAMETERS
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
C Copyright 1996 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE POOLSET
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:LTOCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'
C
	INTEGER*4 GAMENR, GAME_IND, GAME_TYPE, DIV
C
C       CURRENT UPDATED GAME INFORMATION
C       --------------------------------
C
	DO 10, GAME_TYPE=1,LTPOOL_MAXTYP
	DO 10, GAME_IND=1,MAXIND
	   GAMENR=LTPOOL_GAMENR(GAME_TYPE,GAME_IND)
	   IF (GAMENR.EQ.0) GOTO 10
	   IF (GAME_TYPE.EQ.TLTO) THEN
	       IF(LTPOOLDRAW(GAMENR).NE.0) GOTO 10
	       LTPOOLDRAW(GAMENR)=LTODRW(GAME_IND)
	   ELSEIF (GAME_TYPE.EQ.TSPT) THEN
	       IF(SPTDRW(GAME_IND).LT.1) GOTO 10
	       IF(LTPOOLDRAW(GAMENR).NE.0.AND.
     *            LTPOOLDRAW(GAMENR).NE.SPTDRW(GAME_IND)) GOTO 10

	       LTPOOLDRAW(GAMENR) = SPTDRW(GAME_IND)
               LTPOOLNR(GAMENR)   = SPTMAX(GAME_IND)
               LTPOOLBET(GAMENR)  = SPTMAX(GAME_IND)

               LTPOOLFLAGS(GAMENR) = 0
               CALL FASTSET(0,LTPOOLFLAG(1,GAMENR),LTPOOL_MAXSHR)

      	       DO 20 DIV=1,SPGDIV
      		   IF(SPTMAT(DIV,GAME_IND).LE.0) GOTO 20
      		   LTPOOLFLAG(DIV*2-1,GAMENR) = 1
      		   LTPOOLFLAGS(GAMENR) = MAX(LTPOOLFLAGS(GAMENR),DIV*2-1)
20    	       CONTINUE
	   ENDIF
	   IF(LTPOOLDRAW(GAMENR).LT.0) LTPOOLDRAW(GAMENR)=0
10	CONTINUE
C
	RETURN
	END
