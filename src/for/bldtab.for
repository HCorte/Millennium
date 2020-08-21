C
C SUBROUTINE BLDTAB
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]BLDTAB.FOV                                   $
C  $Date::   17 Apr 1996 12:19:40                                         $
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
C
C ==============================================================
C
C SUBROUTINE BLDTAB
C
C This routine will build the tables necessary to assign
C ports.
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
	SUBROUTINE BLDTAB(PRVSTN,ACTSTN,PRVPRT,ACTPRT,NETPORTS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2NETDEF.DEF'
C
	INTEGER*4   PRVSTN      !Previous number of stations
	INTEGER*4   ACTSTN      !Actual number of stations
	INTEGER*4   PRVPRT      !Previous number of ports
	INTEGER*4   ACTPRT      !Actual number of ports
	INTEGER*4   NETPORTS(X2X_NETWORK_PORTS)
	INTEGER*4   I, K, IDNUM
	REAL        DEFRAN2
C
C DISTRIBUTE THE PORTS BASED ON ANY NEW PORTS BEING ADDED.
C
	IDUM=-7
	K=1+INT(DEFRAN2(IDNUM)*NUM_OF_NODES)
	CALL DEF_PORTDIST(PRVPRT,ACTPRT)
C
C ASSIGN NODES TO THE STATIONS, AND GENERATE SETS OF PORTS.
C
	DO 500 I = PRVSTN+1,ACTSTN
	  CALL X2STNOAS(I,ONE_SET_OF_NODES)
	  DO 510 K = 1, NUM_OF_NODES
	    ONE_SET_OF_PORTS(K) = ONE_SET_OF_NODES(K)
510	  CONTINUE
	  CALL X2STPRAS(ONE_SET_OF_PORTS)
	  DO 520 K=1,NUM_OF_NODES
	    ALL_SETS_OF_PORTS(I,K) = NETPORTS(ONE_SET_OF_PORTS(K))
520	  CONTINUE
500	CONTINUE
	RETURN
	END
