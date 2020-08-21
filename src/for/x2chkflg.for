C
C SUBROUTINE X2CHKFLG
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2CHKFLG.FOV                                 $
C  $Date::   17 Apr 1996 16:12:18                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2rcvbuf.for;1 **
C
C
C
C++++++++++++++++++++++++++++++++++++++++++++++
C     X2CHKFLG(SAP,FLAGS,STATUS)   ;CHECK TRANSPORT LAYER FLAGS
C
C     IN:
C        SAP   - SOURCE SAP
C        FLAGS - FLAGS IN TDBH
C     OUT:
C        STATUS- 0 IF EVERYTHING OK
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
	SUBROUTINE X2CHKFLG(SSAP,FLAGS,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2TDBH.DEF'
C
	INTEGER*4 FLAGS
	INTEGER*4 STATUS, STATE, OCE, ICE, ONLINE, SSAP
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	    TYPE *,'X2CHKFLAGS ',SSAP,FLAGS
C
	STATUS=0
	ONLINE=IAND(FLAGS,X2TDBHF_ONLINE)
	ICE=IAND(FLAGS,X2TDBHF_ICE)
	OCE=IAND(FLAGS,X2TDBHF_OCE)
C
	IF (ONLINE.NE.0.AND.X2XE_DEF_STATUS(SSAP).EQ.X2XES_NOTUP) THEN
	   STATUS=X2ERR_FATAL+X2ERR_FLAG
	   IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	       TYPE *,'RETURN X2CHKFLG ',STATUS
	   RETURN
	ENDIF
C
	IF (ONLINE.NE.0) THEN
	   X2XE_ACT_STATUS(SSAP)=X2XES_ONLINE
	ELSE
	   X2XE_ACT_STATUS(SSAP)=X2XES_IDLE
	ENDIF
C
	STATE=X2XE_ACT_STATE(SSAP)
	STATE=IOR(STATE,X2TDBHF_ICE)
	IF (ICE.EQ.0) STATE=IEOR(STATE,X2TDBHF_ICE)
	STATE=IOR(STATE,X2TDBHF_OCE)
	IF (OCE.EQ.0) STATE=IEOR(STATE,X2TDBHF_OCE)
	X2XE_ACT_STATE(SSAP)=STATE
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	   TYPE *,'RETURN X2CHKFLG ',STATUS
	RETURN
	END
