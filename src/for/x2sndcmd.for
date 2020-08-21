C
C SUBROUTINE X2SNDCMD
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2SNDCMD.FOV                                 $
C  $Date::   17 Apr 1996 16:34:34                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2xmgr.for;1 **
C
C V04 26-OCT-94 GPR USE MAIN OPTIONS FOR STOP CALLS AND CLEAR STATS -
C		    Integrate UK changes into X2X Baseline
C V03 05-MAY-94 GPR CORRECT MAINTENANCE LOOP SO THAT MSG TYPE 0 IS SENT
C V02 05-APR-94 GPR USE X2X_I4_STATION TO DETERMINE STATION AND TERNUM
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     X2SNDCMD(SAP,COMMAND)     ;SEND COMMAND
C     IN:
C     SAP   -  SAP TO SEND DATA TO
C     COMMAND- COMMAND TO SEND (TRANSPORT LAYER)
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
	SUBROUTINE X2SNDCMD(SAP,COMMAND)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2TDBH.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 DISCONNECT_FLAG, MAINTENANCE_NUMBER, PROBUF
	INTEGER*4 COMMAND, SAP
        INTEGER*4 MAXTRIES, TMPTYPE
        INTEGER*4 MAINTENANCE_LOOP(X2X_SAP)  /X2X_SAP*0/
	INTEGER*4 MAINTENANCE_TYPE(X2X_SAP)  /X2X_SAP*0/
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	   TYPE *,IAM(),'X2SNDCMD ',SAP,COMMAND
	CALL GETBUF(PROBUF)
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0.AND.PROBUF.LE.0)
     *	     TYPE *,IAM(),'RETURN X2SNDCMD, PROBUF ',PROBUF
	IF (PROBUF.LE.0) RETURN             !IF COULD NOT GET ANY
C
	HPRO(X2X_DEST,PROBUF)=X2DEST_TRANSPORT

C       ***** Start V02 changes *****

        IF (X2X_I4_STATION) THEN
	   PRO(LINENO,PROBUF)=SAP
        ELSE
	   HPRO(LINENO,PROBUF)=SAP
        ENDIF

C       ***** End V02 changes *****

	CALL ISBYTE(COMMAND,PRO(OUTTAB,PROBUF),0)
C
	IF (COMMAND.EQ.X2TDBHT_MAINTENANCE_REQ) THEN
	   CALL I4TOBUF4(X2XPN_OUT_RETRY_LIMIT,PRO(OUTTAB,PROBUF),
     *	              1+X2TDBHM_OUT_RETRY_LIMIT-1)
	   CALL I4TOBUF4(X2XPN_OUT_RETRY_INTERVAL,PRO(OUTTAB,PROBUF),
     *	              1+X2TDBHM_OUT_RETRY_INTERVAL-1)
	   CALL I4TOBUF4(X2XPN_OUT_DURATION_TIMOUT,PRO(OUTTAB,PROBUF),
     *	              1+X2TDBHM_OUT_DURATION_TIMOUT-1)
	   CALL I4TOBUF4(X2XPN_IN_DURATION_TIMOUT,PRO(OUTTAB,PROBUF),
     *	              1+X2TDBHM_IN_DURATION_TIMOUT-1)
	   CALL I4TOBUF4(X2XE_MAX_SIZE,PRO(OUTTAB,PROBUF),
     *	              1+X2TDBHM_MAX_SIZE-1)
	   CALL I4TOBUF4(X2XE_MAX_CNT,PRO(OUTTAB,PROBUF),
     *	              1+X2TDBHM_MAX_CNT-1)
	   CALL I4TOBUF4(X2XE_SEND_INTERVAL,PRO(OUTTAB,PROBUF),
     *	              1+X2TDBHM_SEND_INTERVAL-1)
CV04	   CALL I4TOBUF4(X2XE_FLOW_TIMOUT,PRO(OUTTAB,PROBUF),
CV04     *	              1+X2TDBHM_FLOW_TIMOUT-1)
	   CALL ISBYTE(X2XE_MAINT_OPTIONS,PRO(OUTTAB,PROBUF),		  !V04
     *	              1+X2TDBHM_MAINT_OPTIONS-1)
	   CALL I4TOBUF4(P(ACTTIM),PRO(OUTTAB,PROBUF),
     *	              1+X2TDBHM_CENTRAL_TIME-1)
           IF(MAINTENANCE_LOOP(SAP).LT.X2X_MAX_MAINTENANCE_TYPE+1) THEN	  ! V03
             MAINTENANCE_NUMBER=MOD(MAINTENANCE_TYPE(SAP)+1,
     *                      X2X_MAX_MAINTENANCE_TYPE+1)
             MAINTENANCE_LOOP(SAP)=MAINTENANCE_LOOP(SAP)+1
           ELSE
             MAXTRIES=0
             TMPTYPE=MAINTENANCE_TYPE(SAP)
100          CONTINUE
             MAINTENANCE_NUMBER=MOD(TMPTYPE+1,
     *                      X2X_MAX_MAINTENANCE_TYPE+1)
             IF(X2XE_REQ_MAINTENANCE(MAINTENANCE_NUMBER,SAP).EQ.0) THEN
               IF(MAXTRIES.LT.X2X_MAX_MAINTENANCE_TYPE+1) THEN		  ! V03
                 TMPTYPE=TMPTYPE+1
                 MAXTRIES=MAXTRIES+1
                 GOTO 100
               ELSE
                 MAINTENANCE_NUMBER=0
               ENDIF
             ENDIF
           ENDIF
	   MAINTENANCE_TYPE(SAP)=MAINTENANCE_NUMBER
	   CALL ISBYTE(MAINTENANCE_NUMBER,PRO(OUTTAB,PROBUF),
     *	              1+X2TDBHM_MAINTENANCE_TYPE_REQ-1)
	   DISCONNECT_FLAG=X2TDBHM_STATION_DISCONNECT_ON
	   IF (X2X_FE_SEND_DISCONNECT_FLAG.NE.0)
     *	       DISCONNECT_FLAG=X2TDBHM_STATION_DISCONNECT_OFF
	   CALL ISBYTE(DISCONNECT_FLAG,PRO(OUTTAB,PROBUF),
     *	              1+X2TDBHM_STATION_DISCONNECT-1)
	   CALL ISBYTE(X2X_FE_DISCONNECT_TIMOUT,PRO(OUTTAB,PROBUF),
     *	              1+X2TDBHM_STATION_DISCONNECT_TIMO  -1)
	   CALL ISBYTE(P(SYSNAM),PRO(OUTTAB,PROBUF),
     *	              1+X2TDBHM_CENTRAL_SYSID-1)
C
	   HPRO(OUTLEN,PROBUF)=X2TDBHM_CENTRAL_SYSID+1
C
	ELSE
	   HPRO(OUTLEN,PROBUF)=1
	ENDIF
	CALL X2ADDPRO(PROBUF)
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	       TYPE *,IAM(),'RETURN X2SNDCMD '
	RETURN
	END
