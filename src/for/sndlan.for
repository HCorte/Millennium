C
C SUBROUTINE SNDLAN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]SNDLAN.FOV                                   $
C  $Date::   17 Apr 1996 15:09:44                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - sndlan.for **
C
C SNDLAN.FOR
C
C V01 10-SEP-90 MRM RELEASED FOR VAX
C
C CALL SNDLAN(SSAP,DSAP,BUF,LENGTH)
C
C      BUF     - LAN BUFFER
C      DSAP    - DESTINATION SERVICE ACCESS POINT
C      SSAP    - SOURCE SERVICE ACCESS POINT (MY APPLICATION)
C      LENGTH  - LENGTH IN BYTES STARTING FROM LANDATA (F H B)
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
	SUBROUTINE SNDLAN(SSAP,DSAP,BUF,LENGTH)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4 QUE /0/
	INTEGER*4 BTYPE, BUF, INFOLEN, LENGTH
	INTEGER*4 DSAP, SSAP, STATUS, ST
C
D	TYPE *,'*** SNDLAN SSAP/DSAP,BUF,LENGTH ',SSAP,DSAP,BUF,LENGTH
C
C SWITCH ON BUFFER TYPE
C
	BTYPE=LANBUF(LANBTYP,BUF)
C
	IF(BTYPE.EQ.LTYPDATA) THEN
C
C       FILL IN DLL HEADER
C
	    INFOLEN=LENGTH+(HDRTOT-MACHDLEN)+3
C
	    IF(LENGTH.GT.ETHLENDT) THEN
	       INFOLEN=MIN0(INFOLEN,ETHLENDT)
	       CALL OPS('**** SNDLAN: ILLEGAL LENGTH ****',LENGTH,DSAP)
	    ENDIF
C
	    CALL I4TOBUF2(INFOLEN,LANBUF(1,BUF),TYPEBEG-1)
	    CALL ISBYTE(SSAP,LANBUF(1,BUF),SSAPBEG-1)
	    CALL ISBYTE(DSAP,LANBUF(1,BUF),DSAPBEG-1)
C
	ELSEIF (BTYPE.EQ.LTYPCMD) THEN
C
C         JUST PASS IT TO LANPRO
C
	ELSE
	    CALL OPS('**** SNDLAN:... BTYPE ****',BTYPE,0)
D	    TYPE*,'ILLEGAL TYP SNDLAN...: ',BUF,BTYPE,DSAP,SSAP,LENGTH
D	    PAUSE
	ENDIF
C
	LANBUF(LANOWN,BUF)=OWNEXEC
	CALL ABL(BUF,LANEXEC,STATUS)
D	IF(STATUS.NE.0) PAUSE 'SNDLAN QUE OVF'
C	IF(QUECNT(LANEXEC).LE.1) THEN!KICK ONLY IF EMPTY QUEUE
	   IF(LANTRPTSK(QUE).EQ.TRAPYES) THEN
	   CALL QUEUE(LANTASKS(QUE),BUF,ST)!CONTEXT DECIDES IF THERE IS REPLY
	   IF(ST.NE.0) THEN
	      CALL OPS('**** SNDLAN:... QUEUE ****',QUE,ST)
	   ENDIF
	   ENDIF
C	ENDIF
C
	RETURN
	END
