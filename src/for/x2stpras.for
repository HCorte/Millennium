C
C SUBROUTINE X2STPRAS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2STPRAS.FOV                                 $
C  $Date::   17 Apr 1996 16:37:18                                         $
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
C   X2STPRAS.FTN
C
C   V01   30-APL-90   WOL   INITIAL RELEASE
C
C   THIS SUBROUTINE WILL ASSIGN ORDERED SET OF PORTS
C   TO A STATION IN THE NETWORK TAKING INTO ACCOUNT
C   THE SET OF NODES PERTAINING TO THE STATION,
C   ASSIGNED BY X2STNOAS SUBROUTINE
C
C   PORTS ARE EVENLY DESTRIBUTED: PORTS FROM THE SAME NODE
C   HAVE ALMOST THE SAME NUMBER OF STATIONS CONNECTED
C   (MAXIMAL DIFFERENCE MIGHT BE ONE)
C
C   CALLING SEQUNCE:
C
C      X2STPRAS( NODORD )
C
C   INPUT PARAMETERS:
C
C      NODORD   INT*4(NUM_OF_NODES)    ORDERED SET OF NODES FOR S
C                                      THE STATION
C
C   INPUT PARAMETERS PASSED IN COMMON STATEMENT:
C
C      NUM_OF_STATIONS  INT*4  NUMBER OF STATIONS IN THE NETWORK
C      NUM_OF_PORTS     INT*4  NUMBER OF PORTS IN THE NETWORK
C      NUM_OF_NODES     INT*4  NUMBER OF NODES IN THE NETWORK
C
C   OUTPUT PARAMETERS:
C
C   NODORD      INT*4(NUM_OF_NODES )     ORERED SET OF PORTS FOR
C                                        THE STATION
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
	SUBROUTINE X2STPRAS( NODORD )
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2NETDEF.DEF'
C
	INTEGER*4 NODORD(NUM_OF_NODES)
	INTEGER*4 PRTIX(200), TEMP
	INTEGER*4 I,J,K,L,M,N,COUNT
	REAL DEFRAN2
C
	DO 3110 J = 1, NUM_OF_NODES
	  L = NODORD(J)
	  TEMP = TABFRQ(POASTONO(L,1), J)
	  COUNT = 1
	  M = POASTONO(L,0)
	  PRTIX(COUNT) = POASTONO(L,1)
	  DO 2910 I = 2, M
	  IF( TEMP .LT. TABFRQ(POASTONO(L,I), J) ) GO TO 2910
	  IF( TEMP .GT. TABFRQ(POASTONO(L,I), J) ) GO TO 2810
	  COUNT = COUNT + 1
	  PRTIX(COUNT) = POASTONO(L,I)
	  GO TO 2910
2810	  CONTINUE
	  COUNT = 1
	  PRTIX (COUNT) = POASTONO(L,I)
	  TEMP = TABFRQ(POASTONO(L,I), J)
2910	  CONTINUE
	IF( COUNT .GT. 1) THEN
	   K = 1 + INT(DEFRAN2(IDUM) * COUNT)
	   N = PRTIX(K)
	ELSE
	   N = PRTIX(COUNT)
	ENDIF
	NODORD(J) = N
	TABFRQ(N,J) = TABFRQ(N,J) + 1
3110	CONTINUE
	RETURN
	END
