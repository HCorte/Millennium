C
C SUBROUTINE X2LODNET
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2LODNET.FOV                                 $
C  $Date::   17 Apr 1996 16:21:40                                         $
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
C X2LODNET.FOR
C
C V05 15-JUN-00 UXN NUM_OF_ASS_PRT defined locally
C V04 05-SEO-95 DAS SELECT BY CONN. TYPE NOT CLASS
C V03 26-OCTP94 DXG ONLY SELECT STATIONS THAT ARE X.25 TYPE (CLASS
C 	            2) AND ARE DEFINED IN THE DATABASE
C V02 03-MAY-92 JPJ UPDATED EVDSPRAS TO USE X2X_NETWORK_PORTS TO
C		    DIMISION LOCAL ARRAYS
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C THIS SUBROUTINE WILL BUILD THE NETWORK PORT ASSIGNMENT
C TABLES.
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
	SUBROUTINE X2LODNET
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2NETCOM.DEF'
C
	INTEGER*4   I,J,STIX,IDUM             !LOCAL VARIABLES
	INTEGER*4   NETPORTS(X2X_NETWORK_PORTS) !TABLE OF PORTS
	INTEGER*4   LOCPRT, SITE					    !V03
	INTEGER*4   NUM_OF_ASS_PRT
C
C SEARCH FOR THE HIGHEST DEFINED STATION.
C
	NUM_OF_STATIONS=0
C
C LOAD THE STATION WEIGHT AND STATION INDEX
C ARRAYS AND SORT BY STATION RANK.
C
30	CONTINUE
        IDUM=-7
C
C **************** V03 *********************
C
	DO 32 I = 1, X2X_STATIONS
          IF (BX2XS_CONN_TYPE(I).NE.X2XSCT_X25SVC) GOTO 32
	  NUM_OF_STATIONS = NUM_OF_STATIONS + 1
          STNWEIGHT(STNIDX,NUM_OF_STATIONS)=I
          CALL ILBYTE(STNWEIGHT(STNRNK,NUM_OF_STATIONS),
     *	      IX2XS_SALES_RANK,I-1)
32      CONTINUE
C
        IF(NUM_OF_STATIONS.EQ.0) THEN
          TYPE *,IAM(),'WARNING: No X25 stations defined!'
          CALL GPAUSE
          RETURN
        ENDIF
C
C **************** V03 *********************
C
        IF(NUM_OF_STATIONS.GT.1)
     *     CALL I4XSORT(STNWEIGHT,2,NUM_OF_STATIONS,2,0,0)
C
C LOAD TABLES FOR BOTH SITES.
C
	DO 50 SITE=1,X2X_MAX_SITES
C
C BUILD A TABLE CONTAINING ONLY SPECIFIC SITE
C NETWORK PORTS (EXCLUDE DIALUP).
C
	  NUM_OF_PORTS = 0
	  DO 100 I=1,X2X_NETWORK_PORTS
	    LOCPRT=X2XPN_NETWORK_TO_LOCAL(I)
	    IF(LOCPRT.EQ.0) GOTO 100
	    IF(X2XPL_SITE(LOCPRT).NE.SITE) GOTO 100
	    IF(X2XPN_TYPE(I).EQ.X2XPT_DIALUP) GOTO 100
	    IF(X2XPN_OUTCALL(I).NE.0) GOTO 100
	    NUM_OF_PORTS=NUM_OF_PORTS+1
	    NETPORTS(NUM_OF_PORTS)=I
100	  CONTINUE
C
C BUILD INTERNAL TABLES.
C
	  IF(NUM_OF_PORTS.EQ.0) GOTO 50
          NUM_OF_ASS_PRT=MIN0(X2X_MAXPRT_ASSIGN+X2X_MAXX32_ASSIGN,
     *                        NUM_OF_PORTS)
	  CALL EVDSPRAS(SITE,NUM_OF_ASS_PRT)
C
C ASSIGN PORTS TO STATIONS.
C
	  DO 2411 I = 1, NUM_OF_STATIONS
            STIX = STNWEIGHT(STNIDX,NUM_OF_STATIONS - I + 1)
	    DO 2351 J = 1, NUM_OF_ASS_PRT
	      ALL_SETS_OF_PORTS(SITE,STIX,J)=
     *	        NETPORTS(ALL_SETS_OF_PORTS(SITE,STIX,J))
2351	    CONTINUE
2411	  CONTINUE
50	CONTINUE
C
C LOAD COMPLETE.
C
	RETURN
	END
