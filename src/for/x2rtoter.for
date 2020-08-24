C
C SUBROUTINE X2RTOTER
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RTOTER.FOV                                 $
C  $Date::   17 Apr 1996 16:32:56                                         $
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
C X2XRSUBS.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     X2RTOTER(BUFFER)      ;FORMAT BUFFER, NO RELAY HEADER
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
	SUBROUTINE X2RTOTER(BUFFER)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:X2FEMES.DEF'
C
	INTEGER*4  INPTAB2
	PARAMETER (INPTAB2=INPTAB*2-1)
C
	INTEGER*2 BUFFER(*)
	INTEGER*4 OFFSET, TO_MOVE
C
	CALL MOV2TOI4(TO_MOVE,BUFFER(INPTAB2),X2STMES_RELAY_MSG_LEN-1)
	BUFFER(OUTLEN)=TO_MOVE
	CALL ILBYTE(OFFSET,BUFFER(INPTAB2),X2STMES_RELAY_MSG_OFF-1)
	IF (TO_MOVE.NE.0)
     *	CALL MOVBYT(BUFFER(INPTAB2),OFFSET+1,BUFFER(INPTAB2),1,TO_MOVE)
	BUFFER(X2X_DEST)=0
	BUFFER(X2X_DELIVER_OVR)=X2FEMES_FLAGS_ER+X2FEMES_FLAGS_DA
	BUFFER(TRCODE)=TYPBRO
	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XRAPP).NE.0)
     *	TYPE *,'X2RTOTER   TO_MOVE,OFFSET,',TO_MOVE,OFFSET
	RETURN
	END
