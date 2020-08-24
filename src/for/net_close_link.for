C
C SUBROUTINE NET_CLOSE_LINK
C $Log:   GXAFXT:[GOLS]NET_CLOSE_LINK.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:11:10   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:06:24   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - net_close_link.for;1 **
C
C NET_CLOSE_LINK
C
C V01 06-FEB-91 MRM INITIAL RELEASE.
C
C This routine will build a command to NETMGR informing it to
C drop the link to the input node.
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
	SUBROUTINE NET_CLOSE_LINK(NODE)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:DESNET.DEF'
C
	INTEGER*4   NODE		!ID OF SYSTEM TO REMOVE
	INTEGER*4   BUF			!BUFFER NUMBER
	INTEGER*4   ST			!RETURN ROUTINE STATUS
	INTEGER*4   MAXCNT		!MAX RETRY TIMES
C
C GET A NETWORK BUFFER.
C
	IF(NODE.EQ.0) RETURN
	MAXCNT=0
100	CONTINUE
	CALL EXTRABUF(BUF,WAYINP,ST)
        IF(ST.EQ.2) THEN
	   MAXCNT=MAXCNT+1
           CALLXWAIT(20,1,ST)
           IF(MAXCNT.LT.10) THEN
             GOTO 100
	   ELSE
	     CALL OPS('DN_CLOSE_LINK:ERROR GETTING BUFFER',0,0)
	     GOTO 9000
	   ENDIF
        ENDIF
D	TYPE *,'NET_CLOSE_LINK: GO BUFFER ',BUF
C
C BUILD THE REMOVE LINK COMMAND.
C
	NETBUF(NEXT,BUF)=HDRSIZ+5
	NETBUF(MODE,BUF)=DRVMD
	NETBUF(WAYNR,BUF)=WAYINP
	NETBUF(HDRSIZ+1,BUF)=REMLINK
	NETBUF(HDRSIZ+2,BUF)=NODE
	NETBUF(HDRSIZ+3,BUF)=0
	NETBUF(HDRSIZ+4,BUF)=WAYINP
	NETSTAT(NODE,WAYINP)=-IABS(NETSTAT(NODE,WAYINP))
	NETROUT(NODE,WAYINP)=ROUIDLE
C
C NOTE:!!!! HACK TO DEMO ONLY - FIX LATER !!!!
C
	NETSTAT(3,WAYINP)=-IABS(NETSTAT(3,WAYINP))
	NETROUT(3,WAYINP)=ROUIDLE
 
	IF(NODE.EQ.NETBACKUP(WAYINP)) NETBACKUP(WAYINP)=0
D	TYPE *,'NET_CLOSE_LINK: BUILT REMOVE LINK COMMAND '
C
C SEND THE BUFFER TO NETMGR.
C
 	CALL TSNDNET(BUF,WAYINP)
C
9000	CONTINUE
	RETURN
	END
