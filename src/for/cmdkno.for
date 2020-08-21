C
C SUBROUTINE CMDKNO
C $Log:   GXAFXT:[GOLS]CMDKNO.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:38:16   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:55:50   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - cmdkno.for **
C
C CMDKNO.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS KENO GAME COMMANDS
C
C (KENO GAME NOT CURRENTLY DEFINED)
C
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
	SUBROUTINE CMDKNO(TRABUF,MESS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INTEGER*4 MESS(EDLEN)
	INTEGER*4 CMDNUM
C
C
C
	CMDNUM=TRABUF(TCMNUM)
	GOTO (10,20,30,40,50,60,70,80,90,100) CMDNUM
	GOTO 1000
C
C
10	CONTINUE
20	CONTINUE
30	CONTINUE
40	CONTINUE
50	CONTINUE
60	CONTINUE
70	CONTINUE
80	CONTINUE
90	CONTINUE
100	CONTINUE
C
C INVALID COMMAND NUMBER
C
1000	CONTINUE
	TRABUF(TSTAT)=REJT
	TRABUF(TERR)=INVL
	MESS(2)=TECMD
	MESS(3)=1
	MESS(4)=TRABUF(TCMTYP)
	MESS(5)=TRABUF(TCMNUM)
	RETURN
	END
