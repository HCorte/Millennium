C
C SUBROUTINE CHKUMENU
C $Log:   GXAFXT:[GOLS]CHKUMENU.FOV  $
C  
C     Rev 1.1   19 May 1996 17:45:10   HXK
C  Wojtek's security stuff added
C  
C     Rev 1.0   21 Jan 1993 15:51:50   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_chkumenu.for **
C
C CHKUMENU.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 06-NOV-90 DSL  INITIAL RELEASE MASS
C
C
C Subroutine to determine ID menus available for USERPASS
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
	SUBROUTINE CHKUMENU(ACT,SECL,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
	INCLUDE 'INCLIB:RECUSE.DEF'
C
	INTEGER*4 ACT,ST,I, SECL
C
CWXW	CALL SECSGNON(UINDEX,ST)
CWXW	IF(ST.NE.0) RETURN
C
	ST=-1
	DO 10 I=1,168
	IF(USERS(I,SECL).EQ.ACT) GOTO 20
10	CONTINUE
	RETURN
C
 20	CONTINUE
	ST=0
	RETURN
	END
