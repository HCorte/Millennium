C
C V01 13-NOV-2000 UXN Initial release.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS/CHECK=NOOVERFLOW
	PROGRAM GUIWORK
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:GUIMCOM.DEF'
C
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4 BUF,ST
C
	CALL COPYRITE
	CALL SNIF_AND_WRKSET
C
	CALL GUIOPN()
C
C
C CREATE THE COMMON EVENT FLAG CLUSTER.
C
        ST=SYS$ASCEFC(%VAL(GUI_EVNT_START),GUI_EVNNAME,0,0)
        IF(.NOT.ST) CALL LIB$SIGNAL(%VAL(ST))
C
10	CONTINUE
C
	IF(GUI_WORKER_DIE.NE.0) THEN
	   CALL GUICLS()
	   CALL GSTOP(GEXIT_SUCCESS)
	ENDIF
C
	CALL RTL(BUF,GUI_TO_WORKER_QUEUE,ST)
	IF(ST.EQ.2) THEN
	   CALL XWAIT(20,1,ST)
	   GOTO 10
	ENDIF
C
C Process assembled messages
C
        CALL GUICMSG(BUF)
C
        CALL GUIQUE(GUI_WRITE, ST)
        IF(ST.NE.0) CALL OPS('Error setting event flag for GUILINK',ST,ST)
C
	GOTO 10
	END
