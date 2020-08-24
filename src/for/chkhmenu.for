C
C SUBROUTINE CHKHMENU
C $Log:   GXAFXT:[GOLS]CHKHMENU.FOV  $
C  
C     Rev 1.1   19 May 1996 17:45:06   HXK
C  Wojtek's security stuff added
C  
C     Rev 1.0   21 Jan 1993 15:49:36   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_chkhmenu.for **
C
C CHKHMENU.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C
C Subroutine to determine ID menus available for HASF
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
	SUBROUTINE CHKHMENU(ACT,SECL,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:PRMHSF.DEF'
C
	INTEGER*4 ACT,SECLEV,ST,I, SECL
	ST=-1
	DO 10 I=1,168
	IF(HMENUS(I,SECL).EQ.ACT) GOTO 20
C**     IF(HMENUS(I,SECL).EQ.0) RETURN
10	CONTINUE
	RETURN
C
 20	CONTINUE
	ST=0
	RETURN
	END
