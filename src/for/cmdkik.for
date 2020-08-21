C
C SUBROUTINE CMDKIK
C $Log:   GXAFXT:[GOLS]CMDKIK.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:38:12   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:55:46   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - cmdkik.for **
C
C CMDKIK.FOR
C
C V02 12-NOV-91 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS KICKER GAME COMMANDS
C
C
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
	SUBROUTINE CMDKIK(TRABUF,MESS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INTEGER*4 MESS(EDLEN), GIND, CMDNUM, TEMP
	INTEGER*2 I2TEMP(2)
	EQUIVALENCE (TEMP,I2TEMP)
C
C
C
	CMDNUM=TRABUF(TCMNUM)
	GOTO (10,20,30,40,50,60,70,80,90,100) CMDNUM
	GOTO 1000
C
C CHANGE KICKER GAME STATUS
C
10	CONTINUE
	GIND=TRABUF(TCMDT1)
	TRABUF(TCMOLD)=KIKSTS(GIND)
	KIKSTS(GIND)=TRABUF(TCMNEW)
	IF(TRABUF(TCMNEW).EQ.GAMBFD) THEN
	  KIKCTM(GIND)=TRABUF(TTIM)
	  CALL BSET(KIKTIM(GIND),1)
	  CALL CLRSUM
	  TEMP=KIKREV(GIND)
	  I2TEMP(1)=I2TEMP(1)+1
	  KIKREV(GIND)=TEMP
	ENDIF
	MESS(2)=TECMD
	MESS(3)=3
	MESS(6)=GIND
	MESS(9)=TRABUF(TCMOLD)
	MESS(10)=TRABUF(TCMNEW)
	RETURN
C
C SET KICKER WINNING NUMBERS
C
20	CONTINUE
	GIND=TRABUF(TCMDT1)
	TRABUF(TCMOLD)=KIKWIN(GIND)
	KIKWIN(GIND)=TRABUF(TCMNEW)
	MESS(2)=TECMD
	MESS(3)=4
	MESS(6)=GIND
	RETURN
C
C PUT NEXT KICKER COMMAND HERE
C
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
