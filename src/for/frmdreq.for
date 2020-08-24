C
C SUBROUTINE FRMDREQ
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]FRMDREQ.FOV                                  $
C  $Date::   17 Apr 1996 13:13:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - frmdreq.for;1 **
C
C FRMDREQ.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 10-JUN-89 MBK ORIGINAL RELEASE
C
C CALL FRMDREQ(SSAP,DSAP,BUF)
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
	SUBROUTINE FRMDREQ(SSAP,DSAP,BUF)
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
	CONN=CONNECTION(SSAP,DSAP)
	LAN=CURLAN(CONN)
C
C SSAP AND DSAP ETC. COME FROM THE BUFFER
C IN TEST MODE VERIFY IT
C
	IF(LANTEST.NE.0) THEN
	   CALL OPS('**** DIS REQ ****',CONN,LAN)
	   IF(SSAP.LE.0.OR.SSAP.GT.MAXSAP.OR.
     *	      DSAP.LE.0.OR.DSAP.GT.MAXSAP) THEN
	      TYPE*,'**** FRMDREQ:... ILLEGAL SAP ****[',SSAP,DSAP,']'
	      GOTO 200
	   ENDIF
	   IF(CONN.LE.0.OR.CONN.GT.MAXCON) THEN
            TYPE*,'**** FRMDREQ:... ILLEGAL CONNECTION ****[',CONN,']'
	      GOTO 200
	   ENDIF
	   IF(BUF.LE.0.OR.BUF.GT.LANBNUM) THEN
           TYPE*,'**** FRMDREQ:... ILLEGAL BUF ****[',BUF,SSAP,DSAP,']'
	      GOTO 200
	   ENDIF
	   IF(LAN.LE.0.OR.LAN.GT.MAXLAN) THEN
           TYPE*,'**** FRMDREQ:... ILLEGAL LAN ****[',LAN,SSAP,DSAP,']'
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
	   CALL I4TOBUF2(FRMLEN(FRDREQ),LANBUF(1,BUF),TYPEBEG-1)
	   CALL ISBYTE(DSAP,LANBUF(1,BUF),DSAPBEG-1)
	   CALL ISBYTE(SSAP,LANBUF(1,BUF),SSAPBEG-1)
	   CALL ISBYTE(CTRLUI,LANBUF(1,BUF),CTRLBEG-1)
	   CALL ISBYTE(FRDREQ,LANBUF(1,BUF),FRTYPBEG-1)
	   CALL ISBYTE(LAN,LANBUF(1,BUF),TLANBEG-1)
	   CALL GOWRITE(BUF)
	   BUF=0
	ENDIF
C
200	CONTINUE
	IF(BUF.NE.0) CALL LANRELB(BUF)
	RETURN
	END
