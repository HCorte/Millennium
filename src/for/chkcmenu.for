C  GXSRC:CHKCMENU.FOR
C  
C  $Log:   GXAFXT:[GOLS]CHKCMENU.FOV  $
C  
C     Rev 1.1   19 May 1996 17:53:10   HXK
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    19 May 1996 17:48:58   HXK
C  Initial revision.
C  
C
C SUBROUTINE CHKCMENU
C  
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - pas_chkumenu.for **
C
C CHKCMENU.FOR
C
C Subroutine to determine ID menus available for CHGLVL
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
	SUBROUTINE CHKCMENU(ACT,SECL,ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:PRMLVL.DEF'
	INCLUDE 'INCLIB:RECUSE.DEF'
        INCLUDE 'INCLIB:DESLVL.DEF'
C
	INTEGER*4 ACT,ST,I, SECL
C
	ST=-1
	DO 10 I=1,168
	IF(LEVMEN(I,SECL).EQ.ACT) GOTO 20
10	CONTINUE
	RETURN
C
 20	CONTINUE
	ST=0
	RETURN
	END
