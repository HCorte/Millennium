C
C SUBROUTINE ASKSYNC
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]ASKSYNC.FOV                                  $
C  $Date::   17 Apr 1996 12:13:40                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - net_netsub2.for;1 **
C
C
C     ASKSYNC(SER,WAY)  GENERATE RESYNCHRONISETION REQUEST FROM SERIAL
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*
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
	SUBROUTINE ASKSYNC(SERIAL,WAY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
C
	INTEGER*4 SERIAL,WAY, ST, BUF, SYSUP
C
C
	SYSUP=NODEMASTER(WAY)
C
	   IF (NETROUT(SYSUP,WAY).EQ.ROUACT.AND.
     *	       NETSTAT(SYSUP,WAY).EQ.NSTASEC)  THEN
10	      CONTINUE
	      CALL EXTRABUF(BUF,WAY,ST)     !TRY TO GET A BUFFER
	      IF (ST.EQ.2) THEN
	         CALL XWAIT(10,1,ST)
	         GOTO 10
	      ENDIF
C
	      NETBUF(MODE,BUF)=CMDMD         !COMMAND
	      NETBUF(PDEST,BUF)=SYSUP          !DESTINATION NODE
	      NETBUF(FDEST,BUF)=SYSUP          !DESTINATION NODE
	      NETBUF(HDRSIZ+1,BUF)=RESYNC
	      NETBUF(HDRSIZ+2,BUF)=NODEID    !RESYNC YOUR NODE
	      NETBUF(HDRSIZ+3,BUF)=SERIAL
	      NETBUF(HDRSIZ+4,BUF)=WAY
C
	      NETBUF(WAYNR,BUF)=WAY
	      CALL SNDNET(BUF,WAY)
C
	   ENDIF
C
	RETURN
	END
