C
C SUBROUTINE EVDSPRAS
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]EVDSPRAS.FOV                                 $
C  $Date::   17 Apr 1996 13:05:26                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2lodnet.for;1 **
C
C
C EVDSPRAS.FTN
C
C V02 02-MAY-92 JPJ UPDATED TO USE X2X_NETWORK_PORTS FOR LOCAL ARRAYS
C V01 08-JUN-90 WOL INITIAL RELEASE FOR SPAIN
C
C THIS SUBROUTINE WILL ASSIGN THE ORDERED SET OF PORTS
C TO THE STATIONS IN THE NETWORK
C PORTS ARE EVENLY DISTRIBUTED: EACH PORT HAS THE SAME
C NUMBER OF STATIONS CONNECTED (MAXIMAL DIFFERENCE MIGHT BE ONE)
C AND NEARLY THE SAME AMOUNT OF SERVED TRAFFIC (SUM OF ACTIVITY
C WEIGHTS OF CONNECTED STATIONS
C
C   CALLING SEQUENCE:
C
C     CALL EVDSPRAS(SITE,NUM_OF_ASSGN_PORTS)
C
C   INPUT PARAMETERS:
C
C     SITE                  INT*4      SITE NUMBER
C     NUM_OF_ASSGN_PORTS    INT*4      # OF PORTS IN THE SET ASSIGNED
C
C   OUTPUT PARAMETERS:
C
C     FILLS UP ALL_SETS_OF_PORTS TABLE FROM COMMON STATEMENT
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
	SUBROUTINE EVDSPRAS(SITE,NUM_OF_ASSGN_PORTS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XPRM.DEF'
	INCLUDE 'INCLIB:X2NETCOM.DEF'
C
	INTEGER*4 SITE, IDUM
	INTEGER*4 NUM_OF_ASSGN_PORTS
	INTEGER*4 PORTSIDX(X2X_NETWORK_PORTS)
	INTEGER*4 PROHPRT(X2X_NETWORK_PORTS)
	INTEGER*4 PAIRSTAB(X2X_NETWORK_PORTS,X2X_NETWORK_PORTS)
	INTEGER*4 I, J, JJ, K, KK, COUNT, MEAN_STN_PRT
	INTEGER*4 TEMP(3), PRTIDX, STIX
	REAL RAN2
C
C   SET THE TABLES
C
	DO 2330 K = 1, NUM_OF_PORTS
	  PROHPRT(K) = 0
	  DO 2110 J = 1, NUM_OF_ASSGN_PORTS
	    TABFRQ(K,J) = 0
	    TABWEI(K,J) = 0
2110	  CONTINUE
	  DO 2220 KK = 1, NUM_OF_PORTS
	    PAIRSTAB(K,KK) = 0
2220	  CONTINUE
2330	CONTINUE
	MEAN_STN_PRT=NUM_OF_STATIONS/NUM_OF_PORTS-NUM_OF_ASSGN_PORTS
C
C   THE ORDERED SETS OF PORTS TO STATIONS ASSIGNMENT
C
	DO 3540 J = 1, NUM_OF_ASSGN_PORTS
	  DO 3210 I = 1, NUM_OF_STATIONS
            STIX = STNWEIGHT(STNIDX,NUM_OF_STATIONS - I + 1)
	    IF (J .GT. 1) THEN
	      DO 2440 JJ = 1, J-1
	        PROHPRT(ALL_SETS_OF_PORTS(SITE,STIX,JJ)) = 1
2440	      CONTINUE
	      TEMP(3) = 1000000
	    ENDIF
	    TEMP(1) = 1000000
	    TEMP(2) = 1000000
	    COUNT = 0
	    DO 2770 K = 1, NUM_OF_PORTS
	      IF (PROHPRT(K) .GT. 0) GO TO 2770
	      IF (TEMP(1) .LT. MEAN_STN_PRT .AND. TABFRQ(K,J) .LT.
     *	      MEAN_STN_PRT) GO TO 2550
	      IF (TABFRQ(K,J) .GT. TEMP(1)) GO TO 2770
	      IF (TABFRQ(K,J) .LT. TEMP(1)) GO TO 2660
2550	      CONTINUE
	      IF (J .GT. 1) THEN
	        IF (PAIRSTAB(ALL_SETS_OF_PORTS(SITE,STIX,J-1),K) .GT.
     *	            TEMP(3)) GO TO 2770
	        IF (PAIRSTAB(ALL_SETS_OF_PORTS(SITE,STIX,J-1),K) .LT.
     *	            TEMP(3)) GO TO 2660
	      ENDIF
	      IF (TABWEI(K,J) .GT. TEMP(2)) GO TO 2770
	      IF (TABWEI(K,J) .LT. TEMP(2)) GO TO 2660
	      COUNT = COUNT + 1
	      PORTSIDX(COUNT) = K
	      GO TO 2770
2660	      CONTINUE
	      COUNT = 1
	      PORTSIDX(COUNT) = K
	      TEMP(1) = TABFRQ(K,J)
	      TEMP(2) = TABWEI(K,J)
	      IF (J .GT. 1) TEMP(3) = PAIRSTAB(ALL_SETS_OF_PORTS
     *	                    (SITE,STIX,J-1),K)
2770	    CONTINUE
	    IF (COUNT .GT. 1) THEN
	      PRTIDX = PORTSIDX(1 + INT(RAN2(IDUM)*COUNT))
	    ELSE
	      PRTIDX = PORTSIDX(COUNT)
	    ENDIF
	    ALL_SETS_OF_PORTS(SITE,STIX,J) = PRTIDX
	    TABFRQ(PRTIDX,J) = TABFRQ(PRTIDX,J) + 1
            TABWEI(PRTIDX,J) = TABWEI(PRTIDX,J) + STNWEIGHT(STNRNK,STIX)
	    IF (J .GT. 1) THEN
	      DO 2880 JJ = 1, J-1
	        PAIRSTAB(ALL_SETS_OF_PORTS(SITE,STIX,JJ),PRTIDX) =
     *	        PAIRSTAB(ALL_SETS_OF_PORTS(SITE,STIX,JJ),PRTIDX) + 1
	        PAIRSTAB(PRTIDX,ALL_SETS_OF_PORTS(SITE,STIX,JJ)) =
     *	        PAIRSTAB(PRTIDX,ALL_SETS_OF_PORTS(SITE,STIX,JJ)) + 1
	        PROHPRT(ALL_SETS_OF_PORTS(SITE,STIX,JJ)) = 0
2880	      CONTINUE
	    ENDIF
3210	  CONTINUE
3540	CONTINUE
	RETURN
	END
