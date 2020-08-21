C
C SUBROUTINE ADDMSG
C $Log:   GXAFXT:[GOLS]ADDMSG.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:08:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993  9:06:22   EBD
C  Initial revision.
C
C ** Source - msgsub.for **
C
C MSGSUB.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C SET OF ROUTINES TO ADD, DELETE MESSAGES FROM MSGCOM
C
C
C
C
C *** ADDMSG: ADD MESSAGE TO LIST
C
C IF MESSAGE ALREADY EXISTS, DELETE OLD ONE FIRST
C
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE ADDMSG(MSGNUM,MSGBUF,LEN,ERR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:MSGCOM.DEF'
C
	INTEGER*4 NXTLNK, FSTLEN, K, ZLEN, MOFF, LINK, XLEN
	INTEGER*4 TOTLNK, ERR, LEN, MSGNUM
	CHARACTER*1 MSGBUF(LEN)
C
C
	CHARACTER*1 C1MSG(MSGSZH*2,MSGSIZ)
	EQUIVALENCE (C1MSG,I4MSGTAB)
C
C
C
	ERR=-1
	TOTLNK=(LEN-1)/MSGLST + 1
	IF(TOTLNK.GT.I4MSGTAB(MFRCNT,MSGCTL))THEN
	  TYPE *,' TOTLNK = ',TOTLNK
	  TYPE *,' I4MSGTAB(MFRCNT,MSGCTL)= ',I4MSGTAB(MFRCNT,MSGCTL)
	  GOTO 700
	ENDIF
C
	CALL DELMSG(MSGNUM)
C
C
	XLEN=LEN
	LINK=MSGNUM
	MOFF=1
C
C
100	CONTINUE
	ZLEN=MIN(XLEN,MSGLST)
	DO 200 K=1,ZLEN
	  C1MSG(K,LINK)=MSGBUF(MOFF)
	  MOFF=MOFF+1
200	CONTINUE
	IF(LINK.EQ.MSGNUM)THEN
	  FSTLEN=ZLEN                 !FOR 1ST BUFFER, SAVE LENGTH
	ELSE
	  I4MSGTAB(SEGLEN,LINK)=ZLEN
	ENDIF
C
	XLEN=XLEN-ZLEN
	IF(XLEN.LE.0)GO TO 500
	CALL GETFRE(NXTLNK)
	IF(NXTLNK.LE.0)THEN
	  CALL DELMSG(MSGNUM)
	  TYPE *,' NXTLNK = ',NXTLNK
	  GO TO 700
	ENDIF
	I4MSGTAB(MSGLNK,LINK)=NXTLNK
	LINK=NXTLNK
	GO TO 100
C
C
C
C WAIT UNTIL NOW TO SET LENGTH OF FIRST SEGMENT IN ORDER TO LOCK ***
C MESSAGE UNTIL IT IS COMPLETELY WRITTEN, TO MAINTAIN INTEGRITY: ***
C
500	CONTINUE
	I4MSGTAB(SEGLEN,MSGNUM)=FSTLEN
	ERR=0
C
C
700	CONTINUE
	RETURN
	END
