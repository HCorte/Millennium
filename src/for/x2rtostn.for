C
C SUBROUTINE X2RTOSTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RTOSTN.FOV                                 $
C  $Date::   17 Apr 1996 16:32:52                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2xrsubs.for;1 **
C
C V02 29-DEC-94 WJK MOVE UNSOLICITED STATION CONNECT AND DISCONNECT FROM GLOBAL
C                   TO STATION CLASS
C V01 05-APR-94 GPR USE LINENO2 WHICH WILL BE THE HALFWORD OFFSET FOR LINENO
C                   IF I4 STATION
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     X2RTOSTN(BUFFER)      ;FORMAT BUFFER, NO RELAY HEADER
C                           ;MESSAGE TO STATION
C     IN:
C     BUFFER   -     PROCOM BUFFER (NOT BUFFER #)
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
	SUBROUTINE X2RTOSTN(BUFFER)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:X2FEMES.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*4  INPTAB2
	PARAMETER (INPTAB2=INPTAB*2-1)
C									! V01
	INTEGER*4  LINENO2						! V01
	PARAMETER (LINENO2=LINENO*2-1)					! V01
C
	INTEGER*2 BUFFER(*)
	INTEGER*4 OFFSET, TO_MOVE, STN					! V02
C
	CALL MOV2TOI4(TO_MOVE,BUFFER(INPTAB2),X2STMES_RELAY_MSG_LEN-1)
	BUFFER(OUTLEN)=TO_MOVE
	CALL ILBYTE(OFFSET,BUFFER(INPTAB2),X2STMES_RELAY_MSG_OFF-1)
	IF (TO_MOVE.NE.0)
     *	CALL MOVBYT(BUFFER(INPTAB2),OFFSET+1,BUFFER(INPTAB2),1,TO_MOVE)

C	***** Start V01 changes *****

        IF (X2X_I4_STATION) THEN
           STN = BUFFER(LINENO2)                                        ! V02
	   CALL I2TOBUF2(BUFFER(LINENO2),BUFFER(INPTAB2),
     *	   X2STMES_STATION_NO-1)
        ELSE
           STN = BUFFER(LINENO)                                         ! V02
	   CALL I2TOBUF2(BUFFER(LINENO),BUFFER(INPTAB2),
     *	   X2STMES_STATION_NO-1)
	ENDIF

C	***** End V01 changes *****

	BUFFER(X2X_CONNCTL_OVR)=256*
     *	      (X2X_UNSOLICIT_FE_CONNECT*16+X2X_UNSOLICIT_FE_DISCONNECT)
C V02 *	      +(X2X_UNSOLICIT_STATION_CONNECT*16+
C V02 *	       X2X_UNSOLICIT_STATION_DISCONNEC )
     *        +(X2XC_UNSO_STN_CON(X2XS_STNCLS(STN))*16+                 ! V02
     *         X2XC_UNSO_STN_DIS(X2XS_STNCLS(STN)) )                    ! V02
	BUFFER(X2X_DELIVER_OVR)=X2FEMES_FLAGS_ER+X2FEMES_FLAGS_DA
D	TYPE *,'X2RTOSTN ',TO_MOVE,OFFSET
	RETURN
	END
