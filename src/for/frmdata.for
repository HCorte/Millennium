C
C SUBROUTINE FRMDATA
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]FRMDATA.FOV                                  $
C  $Date::   17 Apr 1996 13:13:30                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - frmdata.for;1 **
C
C FRMDATA.FOR
C
C V01 28-NOV-90 XXX RELEASED FOR VAX
C
C CALL FRMDATA(SSAP,DSAP,BUF)
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
	SUBROUTINE FRMDATA(SSAP,DSAP,BUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4 STA, LAN, CONN, BUF, DSAP, SSAP
C
	CONN=CONNECTION(SSAP,DSAP)
	LAN=CURLAN(CONN)
C
C SSAP AND DSAP ETC. COME FROM THE BUFFER
C IN TEST MODE VERIFY IT
C
	IF(LANTEST.NE.0) THEN
C
	   CALL OPS('**** DATA ****',CONN,LAN)
D	   TYPE*,'**** FRMDATA ****[',SSAP,DSAP,LAN,BUF,']'
C
	   IF(SSAP.LE.0.OR.SSAP.GT.MAXSAP.OR.
     *	      DSAP.LE.0.OR.DSAP.GT.MAXSAP) THEN
	      TYPE*,'**** FRMDATA:... ILLEGAL SAP ****[',SSAP,DSAP,']'
	      GOTO 200
	   ENDIF
	   IF(CONN.LE.0.OR.CONN.GT.MAXCON) THEN
            TYPE*,'**** FRMDATA:... ILLEGAL CONNECTION ****[',CONN,']'
	      GOTO 200
	   ENDIF
	   IF(BUF.LE.0.OR.BUF.GT.LANBNUM) THEN
           TYPE*,'**** FRMDATA:... ILLEGAL BUF ****[',BUF,SSAP,DSAP,']'
	     GOTO 200
	   ENDIF
	   IF(LAN.LE.0.OR.LAN.GT.MAXLAN) THEN
           TYPE*,'**** FRMDATA:... ILLEGAL LAN ****[',LAN,SSAP,DSAP,']'
	     GOTO 200
	   ENDIF
	ENDIF
C
C SSAP HAS TO BE UP HERE
C
	IF(LANSAPSTS(SSAP).EQ.SAPDOWN) THEN
D	   TYPE*,'**** INVALID SSAP STATUS ****[',SSAP,DSAP,LAN,']'
	   STA=LANSAPSTS(SSAP)
	   CALL OPS('**** INVALID SSAP STATE TO SEND ****',SSAP,STA)
	   GOTO 200
	ENDIF
C
C SET UP FOR SEND
C
	IF(LOCLAN(SSAP,LAN).EQ.LANUP) THEN
	   CALL MOVBYT(FLANADR(1,LAN,DSAP),1,LANBUF(1,BUF),DABEG,6)
	   CALL MOVBYT(FLANHOME(1,LAN),1,LANBUF(1,BUF),SABEG,6)
	   CALL ISBYTE(DSAP,LANBUF(1,BUF),DSAPBEG-1)
	   CALL ISBYTE(SSAP,LANBUF(1,BUF),SSAPBEG-1)
	   CALL ISBYTE(CTRLUI,LANBUF(1,BUF),CTRLBEG-1)
	   CALL ISBYTE(FRDATA,LANBUF(1,BUF),FRTYPBEG-1)
	   CALL ISBYTE(LAN,LANBUF(1,BUF),TLANBEG-1)
	   CALL GOWRITE(BUF)
	   BUF=0
	ENDIF
C
200	CONTINUE
	IF(BUF.NE.0) CALL LANRELB(BUF)
	RETURN
	END
