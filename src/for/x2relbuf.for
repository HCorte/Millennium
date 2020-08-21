C SUBROUTINE X2RELBUF
C
C V03 16-JUN-2000 OXK Cleanup w/ WARNINGS=ALL
C V02 05-APR-1994 GPR USE X2X_I4_STATION TO DETERMINE STATION AND TERNUM
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     CALL X2RELBUF(PROBUF)    ;RELEASE PROCOM BUFFER
C                              ;WILL CLEAR EXTRA FIELDS
C                              ;SEND TO GAME ANY TRANSACTION IN WAIT
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
	SUBROUTINE X2RELBUF(PROBUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*4 TERMINAL_NO, LINK, PROBUF
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	    TYPE *,'X2RELBUF ',PROBUF
	LINK=HPRO(X2X_LINK,PROBUF)
	IF (LINK.EQ.-1) THEN
C**************************************************************************
C                 NO MORE BUFFERS IN WAIT AS OF 30-APR-91 RRB             *
C**************************************************************************

C          ***** Start V02 changes *****

           IF (X2X_I4_STATION) THEN
	      TERMINAL_NO=PRO(TERNUM,PROBUF)
           ELSE
	      TERMINAL_NO=HPRO(TERNUM,PROBUF)
           ENDIF

C          ***** End V02 changes *****

C****	   IF (X2XT_PRO(TERMINAL_NO).NE.PROBUF .AND.
C**** *	       X2XT_PRO(TERMINAL_NO).NE.0) THEN
C****	      CALL X2REMLST(PROBUF1,X2XT_PRO(TERMINAL_NO))
C****	      HPRO(X2X_LINK,PROBUF1)=-1
C****	      CALL QUEINP(PROBUF1,ST)
C****	   ELSE
	      X2XT_PRO(TERMINAL_NO)=0
 	      X2X_IN_WAIT_ADD=X2X_IN_WAIT_ADD-1
C****	   ENDIF
C***  ELSEIF (LINK.EQ.-2) THEN
C***     STATION_NO=PRO(LINENO,PROBUF)
C***     IF (X2XS_PRO(STATION_NO).NE.PROBUF .AND.
C*** *       X2XS_PRO(STATION_NO).NE.0) THEN
C***        CALL X2REMLST(PROBUF1,X2XS_PRO(STATION_NO))
C***        HPRO(X2X_LINK,PROBUF1)=-2
C***        CALL QUEINP(PROBUF1,ST)
C***     ELSE
C***        X2XS_PRO(STATION_NO)=0
C***     ENDIF
	ENDIF
	HPRO(X2X_DEST,PROBUF)=0
	HPRO(X2X_CONNCTL_OVR,PROBUF)=0
	HPRO(X2X_LINK,PROBUF)=0
	HPRO(X2X_CONNCTL_OVR,PROBUF)=0
	HPRO(X2X_HOST_ID,PROBUF)=0
	CALL RELBUF(PROBUF)
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	   TYPE *,'RET X2RELBUF '
	RETURN
	END
