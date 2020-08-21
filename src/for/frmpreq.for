C
C SUBROUTINE FRMPREQ
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]FRMPREQ.FOV                                  $
C  $Date::   17 Apr 1996 13:13:54                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - frmpreq.for;1 **
C
C FRMPREQ.FOR
C
C V01 28-NOV-90 XXX RELEASED FOR VAX
C
C CALL FRMPREQ(SSAP,DSAP,BUF)
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
	SUBROUTINE FRMPREQ(SSAP,DSAP,BUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:LANFRLEN.DEF'
C
C
	INTEGER*4 STATUS, LAN, CONN, BUF, DSAP, SSAP
	INTEGER*4 LANBROADR(2) /'FFFFFFFF'X,'FFFFFFFF'X/
C
	CONN=CONNECTION(SSAP,DSAP)
	LAN=CURLAN(CONN)
C
C SSAP AND DSAP ETC. COME FROM THE BUFFER
C IN TEST MODE VERIFY IT
C
	IF(LANTEST.NE.0) THEN
	   CALL OPS('**** POLL REQ ****',CONN,LAN)
	   IF(SSAP.LE.0.OR.SSAP.GT.MAXSAP.OR.
     *	      DSAP.LE.0.OR.DSAP.GT.MAXSAP) THEN
	      TYPE*,'**** FRMPREQ:... ILLEGAL SAP ****[',SSAP,DSAP,']'
	      GOTO 200
	   ENDIF
	   IF(CONN.LE.0.OR.CONN.GT.MAXCON) THEN
            TYPE*,'**** FRMPREQ:... ILLEGAL CONNECTION ****[',CONN,']'
	      GOTO 200
	   ENDIF
	   IF(LAN.LE.0.OR.LAN.GT.MAXLAN) THEN
           TYPE*,'**** FRMPREQ:... ILLEGAL LAN ****[',LAN,SSAP,DSAP,']'
	     GOTO 200
	   ENDIF
	ENDIF
C
C SSAP HAS TO BE UP HERE
C
	IF(LANSAPSTS(SSAP).EQ.SAPDOWN) THEN
	   TYPE*,'**** INVALID SSAP STATUS ****[',SSAP,DSAP,LAN,']'
	   GOTO 200
	ENDIF
C
C IF BUF GT 0 DONT GET THE BUF
C
	IF(LOCLAN(SSAP,LAN).EQ.LANUP) THEN
	   IF(BUF.LE.0) THEN
	      CALL LANGETX(BUF,STATUS)
	      IF(STATUS.EQ.2) THEN
	         BUF=0
	         RETURN
	      ENDIF
	   ENDIF
	   CALL MOVBYT(FLANADR(1,LAN,DSAP),1,LANBUF(1,BUF),DABEG,6)
	   CALL MOVBYT(FLANHOME(1,LAN),1,LANBUF(1,BUF),SABEG,6)
	   CALL I4TOBUF2(FRMLEN(FRPREQ),LANBUF(1,BUF),TYPEBEG-1)
	   CALL ISBYTE(DSAP,LANBUF(1,BUF),DSAPBEG-1)
	   CALL ISBYTE(SSAP,LANBUF(1,BUF),SSAPBEG-1)
	   CALL ISBYTE(CTRLUI,LANBUF(1,BUF),CTRLBEG-1)
	   CALL ISBYTE(FRPREQ,LANBUF(1,BUF),FRTYPBEG-1)
	   CALL ISBYTE(LAN,LANBUF(1,BUF),TLANBEG-1)
	   CALL MOVBYT(FLANHOME(1,LAN),1,LANBUF(1,BUF),LANDATAB,6)
	   CALL GOWRITE(BUF)
	   BUF=0
	ENDIF
C
200	CONTINUE
	IF(BUF.NE.0) CALL LANRELB(BUF)
	RETURN
	END
