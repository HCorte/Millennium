C
C SUBROUTINE FREEBUF
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]FREEBUF.FOV                                  $
C  $Date::   17 Apr 1996 13:13:08                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - net_netsub2.for ***
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
C Purpose: Release buffer to free queue
C
C Input:   BUF - Buffer number
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE FREEBUF(BUF)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:DESNET.DEF'
C
C PARAMETER DECLARATIONS
C
	INTEGER*4	FIRST_RECOVERY_BUFFER,
     *			NWAY,
     *			WAYPOOL
C
	PARAMETER	(NWAY = 1)			! # OF ACTIVE WAYS
C
	PARAMETER	(FIRST_RECOVERY_BUFFER =
     *                   NETNUM - NETSYS * NUM_RECOVERY_BUFFERS + 1)
C
	PARAMETER	(WAYPOOL =
     *                  (NETNUM - NETSYS * NUM_RECOVERY_BUFFERS) / NWAY)
C
C LOCAL DECLARATIONS
C
	INTEGER*4	ACTWAY,
     *			BUF,
     *			FINAL,
     *			ST,
     *			SYSTEM,
     *			TIMES,
     *			WAY
C
	LOGICAL*4	EXTRA				! QUEUE TO EXTRA BUF ?
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C EACH BUFFER COULD BE USED FOR MORE THAN 1 I/O AT THE SAME TIME.
C RELEASE BUFFER ONLY FOR LAST I/O.
C
	CALL TSTCHG2(BUF, -1, FINAL)
	IF (FINAL .GT. 0) GOTO 9999
C
C CHECK IF THE BUFFER BELONGS TO RECOVERY QUEUE, IF SO, RELEASE AND EXIT
C
	IF (BUF .GE. FIRST_RECOVERY_BUFFER) THEN
	  SYSTEM = MOD(BUF - FIRST_RECOVERY_BUFFER, NETSYS) + 1
	  CALL ABL(BUF, NETFREE_RECOVERY(1, SYSTEM), ST)	    
	  GOTO 9999
	ENDIF
C
	WAY   = (BUF - 1) / WAYPOOL + 1
	EXTRA = MOD(BUF - 1, WAYPOOL) .LE. (NETSYS * 2 - 1)
C						   
C THIS IS FOR CALIFORNIA ONLY
C
	IF (WAY .EQ. 1) THEN
	  WAY = WAYINP
	ELSE
	  WAY = WAY + 1
	ENDIF
C
C QUEUE TO FREE QUEUE FOR BASIC POOL OF BUFFERS OR TO EXTRA QUEUE
C
	IF (.NOT. EXTRA) THEN
	  IF (NETCMDFRZ .NE. 0) THEN
	    ACTWAY = NETBUF(WAYNR, BUF)
	    IF (ACTWAY .EQ. RECOVWAY) THEN
	      CALL ABL(BUF, RECOVQUE, ST)
	    ELSE
	      CALL ABL(BUF, NETFREE(1, WAY), ST)	! RELEASE BUFFER
	    ENDIF
C
C CHECK IF ON FREE LIST
C
	  ELSE
	    CALL NET_CHKQUEUE(BUF, NETFREE(1, WAY), TIMES)
	    CALL ABL(BUF, NETFREE(1, WAY), ST)		! RELEASE BUFFER
	  ENDIF
C
C QUEUE TO EXTRA QUEUE
C
	ELSE
	  CALL ABL(BUF, NETEXTRA(1, WAY), ST)
	ENDIF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
