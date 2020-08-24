C
C SUBROUTINE CMDXLPC
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CMDXLPC.FOV                                  $
C  $Date::   17 Apr 1996 12:39:42                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - cmdxsub.for;1 **
C
C
C
C =============================================================
C CMDXLPC
C
C This subroutine loads the local port information into
C common.
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
	SUBROUTINE CMDXLPC(FIELD,ALLREC,ADDFLG)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XLPC.DEF'
C
	INTEGER*4   FIELD           !Modified field
	INTEGER*4   ALLREC(128)     !Record buffer
	INTEGER*4   ADDFLG          !New network port
	INTEGER*4   PORT            !Local port number
        INTEGER*4   OLDPORT         !value of network port
C
C STORE THE INFORMATION INTO COMMON.
C
	CALL FASTMOV(ALLREC,X2XLPC_REC,128)
	PORT=X2XLPC_PORT
        IF(PORT.LE.0 .OR. PORT.GT.X2X_LOCAL_PORTS) GOTO 8000
	IF(FIELD.EQ.2) X2XPL_SAP(PORT)=X2XLPC_SAP
	IF(FIELD.EQ.3) X2XPL_SAP_PORT(PORT)=X2XLPC_SAP_PORT
	IF(FIELD.EQ.3) X2XE_LOCAL_PORT(X2XLPC_SAP_PORT,X2XLPC_SAP)=PORT
	IF(FIELD.EQ.4) X2XPL_STATE(PORT)=X2XLPC_STATE
	IF(FIELD.EQ.5) THEN
          OLDPORT = X2XPL_LOCAL_TO_NETWORK(PORT)
          X2XPL_LOCAL_TO_NETWORK(PORT)=X2XLPC_NETPORT
          IF(X2XLPC_NETPORT.GT.0) THEN
            X2XPN_NETWORK_TO_LOCAL(X2XLPC_NETPORT)=PORT
          ELSE 
            IF(OLDPORT.NE.0)
     *         X2XPN_NETWORK_TO_LOCAL(OLDPORT) = 0
          ENDIF
        ENDIF
        IF(FIELD.EQ.6) X2XPL_SITE(PORT)= X2XLPC_SITE
C
8000    CONTINUE
	RETURN
	END
