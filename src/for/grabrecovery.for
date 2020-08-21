C
C *** SUBROUTINE GRABRECOVERY ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]GRABRECOVERY.FOV                             $
C  $Date::   17 Apr 1996 13:27:58                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
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
C Purpose:
C   Get a buffer.
C
C Calling Sequence:
C   CALL GRABRECOVERY(BUF, SYSTEM, WAY, ST)
C
C Input:
C   WAY - CURRENT WAY
C
C Output:
C   BUF - BUFFER #
C   ST  - SAME AS RTL AND RBL
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE GRABRECOVERY(BUF, SYSTEM, WAY, ST)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:DESNET.DEF'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	BUF,
     *			ST,
     *			SYSTEM,
     *			WAY
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	CALL RTL(BUF, NETFREE_RECOVERY(1, SYSTEM), ST)
C
	IF (ST .EQ. 2) GOTO 9999
C
	IF (BUF .LE. 0 .OR. BUF .GT. NETNUM) THEN
	  TYPE 9000, IAM(), CHAR(7), BUF
	  ST = 2
	  GOTO 9999
	ENDIF
C
	CALL FASTSET(0, NETBUF(NCNLEN + 1, BUF), HDRSIZ - NCNLEN)
C
	NETBUF(PPORG, BUF) = NODEID
	NETBUF(NEXT,  BUF) = HDRSIZ + 1
	NETBUF(WAYNR, BUF) = WAY
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FORMAT STATEMENTS.
C
9000	FORMAT(X, A, A, 'INVALID BUFFER # ', I4)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
