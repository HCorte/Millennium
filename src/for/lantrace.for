C
C SUBROUTINE LANTRACE
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]LANTRACE.FOV                                 $
C  $Date::   17 Apr 1996 13:47:40                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lantrace.for;1 **
C
C LANTRACE.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 28-AUG-89 MBK ORIGINAL
C
C DSAP    - INT*4
C SSAP    - INT*4
C STATE   - INT*4
C EVENT   - INT*4 IN
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
	SUBROUTINE LANTRACE(DSAP,SSAP,STATE,EVENT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4 ENTRY,FLUSH, ST, EVENT, STATE, SSAP, DSAP
C
	CALL ISBYTE(DSAP,ENTRY,0)
	CALL ISBYTE(SSAP,ENTRY,1)
	CALL ISBYTE(STATE,ENTRY,2)
	CALL ISBYTE(EVENT,ENTRY,3)
C
100	CONTINUE
	CALL ABL(ENTRY,LANTLIST,ST)
	IF(ST.NE.0) THEN
	   CALL RTL(FLUSH,LANTLIST,ST)
	   GOTO 100
	ENDIF
C
	RETURN
	END
