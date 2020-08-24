C
C SUBROUTINE X2CLRSAP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CLRSAP.FOV                                 $
C  $Date::   17 Apr 1996 16:13:58                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2sndbuf.for;2 **
C
C V02  7-MAR-94 JWE Clear CAPACITY and network type
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     X2CLRSAP(SAP)            ;CLEAR SAP INFO
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
	SUBROUTINE X2CLRSAP(SAP)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*4  NEXT_SAP, STATION_SAP, STATION, SAP
C
        X2XE_MAX_CAPACITY(SAP) = 0
        X2XE_CAPACITY(SAP) = 0
	X2XE_NETWORK_TYPE(SAP) = 0	
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	    TYPE *,'X2CLRSAP ',SAP
	IF (SAP.GT.0) THEN
	   DO 10, STATION=1,X2X_STATIONS
	      STATION_SAP= ZEXT (BX2XS_SAP(STATION))
	      IF (STATION_SAP.NE.SAP) GOTO 10
	      BX2XS_SAP(STATION)=0
              IF(BX2XS_CONN_TYPE(STATION).NE.X2XSCT_ASYPVC .AND.
     *           BX2XS_CONN_TYPE(STATION).NE.X2XSCT_USAT_PVC)
     *           X2XS_CONN_ID(STATION)=0
10	   CONTINUE
C
C
	   IF (X2XE_DEF_STATUS(SAP).NE.X2XES_NOTUP.AND.
     *	       X2XE_BUF(SAP).GT.0) THEN
	      CALL LANRELB(X2XE_BUF(SAP))
	      X2XE_BUF(SAP)=-1
	   ENDIF
	ELSE
	   CALL FASTSET(0,IX2XS_SAP,X2X_STATIONS/4)
	   DO 20, STATION=1,X2X_STATIONS
              IF(BX2XS_CONN_TYPE(STATION).NE.X2XSCT_ASYPVC .AND.
     *           BX2XS_CONN_TYPE(STATION).NE.X2XSCT_USAT_PVC)
     *           X2XS_CONN_ID(STATION)=0
20	   CONTINUE
C
	   DO 30, NEXT_SAP=1,X2X_SAP
	   IF (X2XE_DEF_STATUS(NEXT_SAP).NE.X2XES_NOTUP.AND.
     *	       X2XE_BUF(NEXT_SAP).GT.0) THEN
	      CALL LANRELB(X2XE_BUF(NEXT_SAP))
	      X2XE_BUF(NEXT_SAP)=-1
	   ENDIF
30	   CONTINUE
	ENDIF
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	    TYPE *,'RET X2CLRSAP'
	RETURN
	END
