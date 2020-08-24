C
C SUBROUTINE X2OPEN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2OPEN.FOV                                   $
C  $Date::   17 Apr 1996 16:26:02                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2xmgr.for;1 **
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     CALL X2OPEN(SAP,QUE,MODE) ;OPEN SAP
C
C     IN:
C     SAP   - SAP TO BE OPENED
C     QUE   - QUE # USED BY LANPRO
C     MODE  - PRIMAY/BACKUP/SPARE MODE
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
	SUBROUTINE X2OPEN(SAP,QUE,MODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
C
	INTEGER*4 DUMMY, ST, BUF, MODE, QUE, SAP, MAXFAIR
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	    TYPE *,IAM(),'X2OPEN ',SAP,QUE,MODE
20	CONTINUE
	CALL LANGETB(BUF,ST)
	IF (ST.EQ.2) THEN
	   CALL XWAIT(1,2,ST)    !WAIT IF COULD NOT GET BUFFER
	   CALL X2ERROR(X2ERR_LANBUF,LTYPCMD,COPEN,DUMMY)
	   GOTO 20
	ENDIF
C
	IF (LANPOOL(QUE).EQ.0) LANPOOL(QUE)=MAXSAP*2
	MAXFAIR=(LANBNUM-NUMEXTRA)/2
	IF(LANPOOL(QUE).GT.MAXFAIR) LANPOOL(QUE)=MAXFAIR
	LANBUF(LANBTYP,BUF)=LTYPCMD
	LANBUF(LANDATAF,BUF)=COPEN
	LANBUF(LANDATAF+1,BUF)=CCOMMAND
	LANBUF(LANDATAF+2,BUF)=SAP
	LANBUF(LANDATAF+3,BUF)=QUE
	LANBUF(LANDATAF+4,BUF)=LANPOOL(QUE)
	LANBUF(LANDATAF+5,BUF)=MODE
	LANBUF(LANDATAF+6,BUF)=LANTBEND
C
	CALL X2SNDLAN(DUMMY,DUMMY,BUF,ST)
C
C     WAIT FOR RESPONCE NOW, (SHOULD BE SOME)
C
C
C     AND FLUSH APPLICATION QUEUE, ITS CRUDE, RELIES ON TIME
C     TO GET INFORMATION BACK, BUT EVEN IF IT NOT RIGHT THE REST
C     OF THE LOGIC WILL RECOVER FROM IT
C
30	CONTINUE
	CALL X2GETAPP(BUF,QUE,ST)
	IF (ST.NE.2) THEN
C
C        CHECK IF OPEN SAP RETURN
C        IF SO, RETURN
C
	   IF (LANBUF(LANBTYP,BUF).NE.LTYPCMD.OR.
     *	       LANBUF(LANDATAF,BUF).NE.COPEN.OR.
     *	       LANBUF(LANDATAF+1,BUF).NE.CREPLYOK.OR.
     *	       LANBUF(LANDATAF+2,BUF).NE.X2X_GAME_SAP) THEN
	      CALL XWAIT(200,1,ST)
	      CALL X2ERROR(X2ERR_OPEN,LANBUF(LANDATAF,BUF),
     *	           LANBUF(LANDATAF+1,BUF),LANBUF(LANDATAF+2,BUF))
	      CALL LANRELB(BUF)
	      GOTO 30
	   ENDIF
	   CALL X2ERROR(X2ERR_OPEN,COPEN,CREPLYOK,X2X_GAME_SAP)
	   IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	     TYPE *,IAM(),'RETURN X2OPEN '
	   CALL LANRELB(BUF)
	   RETURN
	ELSE
	   CALL XWAIT(200,1,ST)
	   GOTO 30
	ENDIF
	END
