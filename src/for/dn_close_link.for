C
C SUBROUTINE DN_CLOSE_LINK
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DN_CLOSE_LINK.FOV                            $
C  $Date::   17 Apr 1996 12:57:10                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - dn_close_link.for ***
C
C V01 06-FEB-91 MRM INITIAL RELEASE.
C  
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1993 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose: This routine will build a command to NETMGR informing it to
C          drop the link to the input node.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE DN_CLOSE_LINK(NODE)
C
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:DESNET.DEF'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	BUF,				! BUFFER NUMBER
     *			MAXCNT,				! MAX RETRY TIMES
     *			NODE,				! ID OF SYSTEM TO REMOVE
     *			ST				! RETURN ROUTINE STATUS
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C GET A NETWORK BUFFER.
C
	IF (NODE .EQ. 0) GOTO 9999
C
	MAXCNT = 0
C
100	CONTINUE
	CALL EXTRABUF(BUF, WAYINP, ST)
	IF (ST .EQ. 2) THEN
	  MAXCNT = MAXCNT + 1
	  CALL XWAIT(20, 1, ST)
	  IF (MAXCNT .LT. 10) THEN
	    GOTO 100
	  ELSE
	    GOTO 9999
	  ENDIF
	ENDIF
C
C BUILD THE REMOVE LINK COMMAND.
C
	NETBUF(NEXT,     BUF) = HDRSIZ + 5
	NETBUF(MODE,     BUF) = DRVMD
	NETBUF(WAYNR,    BUF) = WAYINP
	NETBUF(HDRSIZ+1, BUF) = REMLINK
	NETBUF(HDRSIZ+2, BUF) = NODE
	NETBUF(HDRSIZ+3, BUF) = 0
	NETBUF(HDRSIZ+4, BUF) = WAYINP
C
	IF (NODEID .NE. NETBACKUP(WAYINP)) THEN
	  NETSTAT(NODE, WAYINP) = -NETSTAT(NODE, WAYINP)
	  NETROUT(NODE, WAYINP) = ROUIDLE
	ENDIF
C
	IF (NODE .EQ. NETBACKUP(WAYINP)) NETBACKUP(WAYINP) = 0
C
C SEND THE BUFFER TO NETLOG.
C
 	CALL TSNDNET(BUF, WAYINP)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
