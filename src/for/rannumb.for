C
C SUBROUTINE RANNUMB.FOR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]RANNUMB.FOV                                  $
C  $Date::   17 Apr 1996 14:37:26                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C RANNUMB.FTN
C
C V01 19-FEB-89 MRM INITIAL RELEASE.
C
C This routine will return NUM_PERM permutations from a set
C of COMBINATIONS in the array NUMBERS.  PERM is a seed
C into the routine.
C
C                     perm=mod(stn,5040) or 2000 for example
C     Combinations
C    ---------------  quoe=mod(perm,7) or mod(2000,7) = 5 ---- (ind)
C     1 2 3 4 5 6 7   perm=perm/7      or perm=2000/7 = 285  |
C    ---------------  numbers(1)=comb(5)                     |
C ind 1 2 3 4 5 6 7   comb(5)=0 -----> numbers(1)=5          |
C             ^-----------------------------------------------
C     Combinations
C    ---------------  quoe=mod(perm,6) or mod(285,6) = 3 ----- (ind)
C     1 2 3 4 0 6 7   perm=perm/6      or perm=285/6 = 47    |
C    ---------------  numbers(2)=comb(3)                     |
C ind 1 2 3 4   5 6   comb(5)=0 -----> numbers(2)=3          |
C         ^---------------------------------------------------
C     Combinations
C    ---------------  quoe=mod(perm,5) or mod(47,5) = 2 ------ (ind)
C     1 2 0 4 0 6 7   perm=perm/5      or perm=47/5 = 9      |
C    ---------------  numbers(3)=comb(2)                     |
C ind 1 2   3   4 5   comb(2)=0 -----> numbers(3)=2          |
C       ^-----------------------------------------------------
C     Combinations
C    ---------------  quoe=mod(perm,4) or mod(9,4) = 2 ------- (ind)
C     1 0 0 4 0 6 7   perm=perm/4      or perm=9/4 = 2       |
C    ---------------  numbers(4)=comb(2)                     |
C ind 1     2   3 4   comb(2)=0 -----> numbers(4)=4          |
C           ^-------------------------------------------------
C
C      ....               ....            ....
C
C Calling sequence:
C
C   CALL RANNUMB(SEED,NUM_PERM,COMBINATIONS,PERMUTATIONS)
C
C Input parameters:
C
C     SEED            Int*4       Input seed
C     NUM_PERM        Int*4       Number of ports to assign
C     COMBINATIONS    Int*4(*)   Input numbers
C
C Output parameters:
C
C     PERMUTATIONS    Int*4(*)    Random selection of output numbers
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE RANNUMB(SEED,NUM_PERM,COMBINATIONS,PERMUTATIONS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*4   SEED
	INTEGER*4   NUM_PERM
	INTEGER*4   COMBINATIONS(*)
	INTEGER*4   PERMUTATIONS(*)
	INTEGER*4   I, PORT, OFF, QUOE, INDEX, SEED2, J
C
C LOOP THROUGH TABLE STARTING AT THE TOP.
C
	J=0
	SEED2=SEED
	DO 50 INDEX=NUM_PERM,1,-1
	  QUOE=MOD(SEED2,INDEX)
	  SEED2=SEED2/INDEX
	  OFF=0
	  PORT=0
	  DO 51 I=1,NUM_PERM
	    IF(COMBINATIONS(I).NE.0) THEN
	      IF(OFF.EQ.QUOE) THEN
	        PORT=COMBINATIONS(I)
	        COMBINATIONS(I)=0
	      ENDIF
	      OFF=OFF+1
	    ENDIF
51	  CONTINUE
	  J=J+1
	  PERMUTATIONS(J)=PORT
50	CONTINUE
C
	RETURN
	END
