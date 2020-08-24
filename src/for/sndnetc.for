C
C SUBROUTINE SNDNETC
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]SNDNETC.FOV                                  $
C  $Date::   17 Apr 1996 15:10:00                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - net_netsub2.for;1 **
C
C
C     SEND TO NETLOG, DO NOT SET BUFFER LENGTH (SIMILAR TO SNDNET)
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
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
	SUBROUTINE SNDNETC(BUF,WAY)
	IMPLICIT NONE
C
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DCNEVN.DEF'
	INTEGER*4 ID/1000000/
	INTEGER*4 ST, DEST, TOMASTER, WAY, BUF
	INTEGER*2   ERROR_LENGTH
	CHARACTER   ERROR_TEXT*256
C
	NETBUF(1,BUF)=IAND(RESBUF,NETBUF(1,BUF))  !FOR DRIVER
	NETBUF(PPORG,BUF)=NODEID
	NETBUF(WAYNR,BUF)=WAY
	NETBUF(USEID,BUF)=ID+30000000*NODEID
	ID=ID+1
C
C     BUFFERS WITH TRNS IN FILE FORMAT OR RESYNC COMMANDS  TO FILE
C     QUEUE OTHERWISE TO EXEC QUEUE
C
	TOMASTER=0
	DEST=NETBUF(PDEST,BUF)
	IF (DEST.NE.0) THEN
	  IF (NETSTAT(DEST,WAY).EQ.NSTASEC) TOMASTER=-1
	ENDIF
	IF (NETBUF(MODE,BUF).EQ.FILMD.OR.NETBUF(MODE,BUF).EQ.CMDMD
     *	    .AND.NETBUF(HDRSIZ+1,BUF).EQ.RESYNC
     *	    .OR.TOMASTER.NE.0) THEN
	   CALL ABL(BUF,NETFIL(1,WAY),ST)
	ELSE
	   CALL ABL(BUF,NETEXEC(1,WAY),ST)           !ADD BUFFER TO LIST
	ENDIF
C**	CALL QUEUE(8HNETLOG  ,BUF,ST)
	ST=SYS$SETEF(%VAL(NET_EVENT))
	IF(.NOT.ST)THEN	!Coundn't wake NETLOG
	    CALL SYS$GETMSG(%VAL(ST),ERROR_LENGTH,
	1	ERROR_TEXT,,)
	    CALL OPS(CHAR(7)//ERROR_TEXT//CHAR(7),ST,7)
	ENDIF
C
	RETURN
	END
