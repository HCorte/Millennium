C
C SUBROUTINE FRMCLEAR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]FRMCLEAR.FOV                                 $
C  $Date::   17 Apr 1996 13:13:22                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - frmclear.for;1 **
C
C FRMCLEAR.FOR
C
C V01 28-NOV-90 XXX RELEASED FOR VAX
C
C CALL FRMCLEAR(SSAP,DSAP,BUF)
C
C IN:
C SSAP     - INT*4 FRAME'S ORIGINATOR
C DSAP     - INT*4 FRAME'S DESTINATION
C BUF      - INT*4 WHERE THE INFO IS
C
C OUT:
C BUF      - INT*4 BUFFER NUMBER WHERE THE FRAME IS
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
	SUBROUTINE FRMCLEAR(SSAP,DSAP,BUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:LANFRLEN.DEF'
C
	INTEGER*4 STATUS, LAN, CONN, BUF, DSAP, SSAP
C
C SSAP AND DSAP COME FROM THE BUFFER
C IN TEST MODE VERIFY IT
C
	IF(LANTEST.NE.0) THEN
	   CALL OPS('**** CLEAR ****',SSAP,DSAP)
	   IF(SSAP.LE.0.OR.SSAP.GT.MAXSAP.OR.
     *	      DSAP.LE.0.OR.DSAP.GT.MAXSAP) THEN
	      TYPE*,'**** FRMCLEAR:... ILLEGAL SAP ****[',SSAP,DSAP,']'
	      GOTO 200
	   ENDIF
	   CONN=CONNECTION(SSAP,DSAP)
	   IF(CONN.LE.0.OR.CONN.GT.MAXCON) THEN
            TYPE*,'**** FRMCLEAR:... ILLEGAL CONNECTION ****[',CONN,']'
	      GOTO 200
	   ENDIF
	ENDIF
C
C SSAP HAS TO BE UP HERE
C
	IF(LANSAPSTS(SSAP).EQ.SAPDOWN) THEN
	   TYPE*,'**** INVALID SSAP STATUS ****[',SSAP,DSAP,']'
	   GOTO 200
	ENDIF
C
	DO 100 LAN=1,MAXLAN
	IF(LOCLAN(SSAP,LAN).EQ.LANUP) THEN
	   IF(BUF.LE.0) THEN
	      CALL LANGETX(BUF,STATUS)
	      IF(STATUS.EQ.2) THEN
	         BUF=0
	         GOTO 100
	      ENDIF
	   ENDIF
	   CALL MOVBYT(FLANADR(1,LAN,DSAP),1,LANBUF(1,BUF),DABEG,6)
	   CALL MOVBYT(FLANHOME(1,LAN),1,LANBUF(1,BUF),SABEG,6)
	   CALL I4TOBUF2(FRMLEN(FRCLEAR),LANBUF(1,BUF),TYPEBEG-1)
	   CALL ISBYTE(DSAP,LANBUF(1,BUF),DSAPBEG-1)
	   CALL ISBYTE(SSAP,LANBUF(1,BUF),SSAPBEG-1)
	   CALL ISBYTE(CTRLUI,LANBUF(1,BUF),CTRLBEG-1)
	   CALL ISBYTE(FRCLEAR,LANBUF(1,BUF),FRTYPBEG-1)
	   CALL ISBYTE(LAN,LANBUF(1,BUF),TLANBEG-1)
	   CALL GOWRITE(BUF)
	   BUF=0
	ENDIF
100	CONTINUE
C
200	CONTINUE
	IF(BUF.NE.0) CALL LANRELB(BUF)
	RETURN
	END
