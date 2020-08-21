C
C SUBROUTINE X2STNOAS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2STNOAS.FOV                                 $
C  $Date::   17 Apr 1996 16:36:34                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2loddef.for;1 **
C
C   X2STNOAS.FTN
C
C   V01    25-APR-90   WOL   INITIAL RELEASE
C
C   THIS SUBROUTINE WILL ASSIGN AN ORDERED SET OF NODES TO A
C   STATION IN A NETWORK PREVENTING CLUSTERING IN NODES
C
C   CALLING SEQUENCE:
C
C       CALL X2STNOAS(STIX, NODORD)
C
C   INPUT PARAMETERS:
C
C       STIX      INT*4                     STATION INDEX
C
C   OUTPUT PARAMETERS:
C
C       NODORD    INT*4(NUM_OF_NODES)       ORDERED TABLE OF
C                                           NODES ASSIGNED
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
	SUBROUTINE X2STNOAS(STIX, NODORD)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2NETDEF.DEF'
C
	INTEGER*4 NODORD(NUM_OF_NODES)
	INTEGER*4 ESNOOR(20),GENNUM,COUNT,I,K,L
	INTEGER*4 STIX
	REAL DEFRAN2
C
	L = MOD(STIX,NUM_OF_NODES)
	IF(L .EQ. 1)  THEN
	  DO 2010 K=1,NUM_OF_NODES
	    ESNOOR(K) = 0
2010	    CONTINUE
	  DO 2210 K=1,NUM_OF_NODES - 1
	    GENNUM = INT(DEFRAN2(IDUM)*(NUM_OF_NODES + 1 - K))
	    IF(GENNUM .LT. 0)  GENNUM = 0
	    GENNUM = 1 + GENNUM
	    COUNT = 0
	    I = 0
2110	    CONTINUE
	    I = I + 1
	    IF(ESNOOR(I) .EQ. 0)  COUNT = COUNT + 1
	    IF(COUNT .LT. GENNUM) GO TO 2110
	    ESNOOR(I) = 1
	    NODORD(K) = I
2210	    CONTINUE
	  I = 0
2310	  CONTINUE
	  I = I + 1
	  IF(ESNOOR(I) .NE. 0) GO TO 2310
	  NODORD(NUM_OF_NODES) = I
	ELSE IF(L .NE. 1) THEN
	  GENNUM = NODORD(1)
	  DO 2410 K=1,NUM_OF_NODES - 1
	    NODORD(K) = NODORD(K + 1)
2410	    CONTINUE
	  NODORD(NUM_OF_NODES)=GENNUM
	ENDIF
	RETURN
	END
