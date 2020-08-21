C
C SUBROUTINE GNREAD
C $Log:   GXAFXT:[GOLS]GNREAD.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:26:58   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:32:14   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - gndecode.for **
C
* GNDECODE.FOR
*
* V01 01-AUG-90 XXX RELEASED FOR VAX
*
* V01 04-JUN-90 MRM INITIAL RELEASE.
*
* This subroutine is utilized to decode the upline GVIS
* header from the PC.
*
* Input parameters:
*
*     MESS        Int*4       Input message buffer
*
* Output parameters:
*
*     MESTYP      Int*4       Upline requested message type
*     SUBTYP      Int*4       Upline requested message subtype
*     MSGID       Int*4       PC message identifier
*     TYP         Int*4(4)    Type of qualification data
*     CNT         Int*4(4)    Number of qualification parameters
*     QUADTA      Int*4(20,4) Qualification parameters
*     VALID       Int*4       -1 = invalid message
*
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* This item is the property of GTECH Corporation, Providence, Rhode
* Island, and contains confidential and trade secret information. It
* may not be transferred from the custody or control of GTECH except
* as authorized in writing by an officer of GTECH. Neither this item
* nor the information it contains may be used, transferred,
* reproduced, published, or disclosed, in whole or in part, and
* directly or indirectly, except as expressly authorized by an
* officer of GTECH, pursuant to written agreement.
*
* Copyright 1990 GTECH Corporation. All rights reserved.
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GNREAD  (MESS,MESTYP,SUBTYP,MSGID,
     *	                    TYP,CNT,QUADTA,VALID)
	IMPLICIT NONE
*
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GNSMES.DEF'
*
	INTEGER*4   MESS(400)                   !Message buffer
	INTEGER*4   PROTID                      !Protocol id
	INTEGER*4   MESTYP                      !Message type
	INTEGER*4   SUBTYP                      !Message subtype
	INTEGER*4   MESLEN                      !Message length
	INTEGER*4   MSGID                       !PC message number
	INTEGER*4   OFF(4)                      !Offset to qualification
	INTEGER*4   CNT(4)                      !Number of parameters
	INTEGER*4   TYP(4)                      !Type of data
	INTEGER*4   QUADTA(GNHDRMES_MAXCNT,4)   !Qualification data
	INTEGER*4   DTALEN                      !Data length
	INTEGER*4   OFFSET                      !Offset of data
	INTEGER*4   I,J                         !Work variables
	INTEGER*4   VALID                       !Valid message
*
* CLEAR OUT SELECTION TABLES.
*
	CALL FASTSET(0,QUADTA,GNHDRMES_MAXCNT*4)
	CALL FASTSET(0,OFF,4)
	CALL FASTSET(0,CNT,4)
	CALL FASTSET(0,TYP,4)
	VALID=0
*
* EXTRACT INFORMATION FROM BUFFER.
*
	CALL ILBYTE(PROTID,MESS,GNHDRMES_PROTID-1)
	CALL MOV2TOI4(MESTYP,MESS,GNHDRMES_MESTYP-1)
	CALL ILBYTE(SUBTYP,MESS,GNHDRMES_SUBTYP-1)
	CALL MOV2TOI4(MESLEN,MESS,GNHDRMES_MESLEN-1)
	CALL MOV2TOI4(MSGID,MESS,GNHDRMES_MSGID-1)
	CALL MOV2TOI4(OFF(1),MESS,GNHDRMES_OFF1-1)
	CALL ILBYTE(CNT(1),MESS,GNHDRMES_CNT1-1)
	CALL ILBYTE(TYP(1),MESS,GNHDRMES_TYP1-1)
	CALL MOV2TOI4(OFF(2),MESS,GNHDRMES_OFF2-1)
	CALL ILBYTE(CNT(2),MESS,GNHDRMES_CNT2-1)
	CALL ILBYTE(TYP(2),MESS,GNHDRMES_TYP2-1)
	CALL MOV2TOI4(OFF(3),MESS,GNHDRMES_OFF3-1)
	CALL ILBYTE(CNT(3),MESS,GNHDRMES_CNT3-1)
	CALL ILBYTE(TYP(3),MESS,GNHDRMES_TYP3-1)
	CALL MOV2TOI4(OFF(4),MESS,GNHDRMES_OFF4-1)
	CALL ILBYTE(CNT(4),MESS,GNHDRMES_CNT4-1)
	CALL ILBYTE(TYP(4),MESS,GNHDRMES_TYP4-1)
*
* VERIFY PROTOCOL ID.
*
	IF(PROTID.NE.GNHDRMES_PROTID_VAL) THEN
	  CALL OPS('GNREAD  :ILLEGAL PROTOCOL ',PROTID,0)
	  VALID=-1
	  GOTO 8000
	ENDIF
*
* VERIFY MESSAGE TYPE.
*
	IF(MESTYP.LE.0 .OR. MESTYP.GT.GNHDRMES_MESTYP_LAN) THEN
	  CALL OPS('GNREAD  :ILLEGAL MESTYP ',MESTYP,0)
	  VALID=-1
	  GOTO 8000
	ENDIF
*
* VERIFY MESSAGE SUBTYPE.
*
	IF(MESTYP.LE.0 .OR. MESTYP.EQ.GNHDRMES_MESTYP_GRPSTS) THEN
	  IF (SUBTYP.LE.0 .OR.
     *	      SUBTYP.GT.GNGRPSTS_STSTYP_NUMRET) THEN
	    CALL OPS('GNREAD  :ILLEGAL SUBTYP ',MESTYP,SUBTYP)
	    VALID=-1
	    GOTO 8000
	  ENDIF
	ELSE IF(SUBTYP.NE.0) THEN
	  CALL OPS('GNREAD  :ILLEGAL SUBTYP ',MESTYP,SUBTYP)
	  VALID=-1
	  GOTO 8000
	ENDIF
*
* VERIFY QUALIFICATION PARAMETERS.
*
	DO 50 I=1,4
	  IF(TYP(I).EQ.GNHDRMES_TYP_RANGE .AND.
     *	     CNT(I).GT.2) THEN
	    CALL OPS('GNREAD  :ILLEGAL TYPE',TYP(I),CNT(I))
	    VALID=-1
	    GOTO 8000
	  ELSEIF(TYP(I).EQ.GNHDRMES_TYP_SPECIFIC .AND.
     *	     CNT(I).EQ.0) THEN
	    CALL OPS('GNREAD  :ILLEGAL TYPE',TYP(I),CNT(I))
	    VALID=-1
	    GOTO 8000
	  ELSEIF(TYP(I).EQ.GNHDRMES_TYP_ALL .AND.
     *	     CNT(I).NE.0) THEN
	    CALL OPS('GNREAD  :ILLEGAL TYPE',TYP(I),CNT(I))
	    VALID=-1
	    GOTO 8000
	  ENDIF
50	CONTINUE
*
* VERIFY THE MESSAGE DATA LENGTH.
*
	DTALEN=(CNT(1)+CNT(2)+CNT(3)+CNT(4))*4
	IF(MESLEN.NE.GNHDRMES_TYP4+DTALEN) THEN
	  CALL OPS('GNREAD  :ILLEGAL LEN',MESLEN,GNHDRMES_TYP4+
     *	                                         DTALEN)
	  VALID=-1
	  GOTO 8000
	ENDIF
*
* EXTRACT SELECTION QUALIFIERS.
*
	DO 100 I=1,4
	  IF(OFF(I).NE.0) THEN
	    DO 110 J=1,MIN0(CNT(I),GNHDRMES_MAXCNT)
	      OFFSET=OFF(I)+((J-1)*4)
	      CALL MOV4TOI4(QUADTA(J,I),MESS,OFFSET)
110	    CONTINUE
	  ENDIF
100	CONTINUE
*
* PROGRAM EXIT.
*
8000	CONTINUE
	RETURN
	END
