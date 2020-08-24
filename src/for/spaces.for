C
C SUBROUTINE SPACES
C
C V02 07-OCT-1997 UXN LOOPS REPLACED WITH LIB$MOVC5 CALLS.
C V01 01-AUG-1990 XXX RELEASED FOR VAX
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
	SUBROUTINE SPACES
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	CHARACTER*9 TYPE(4)
	CHARACTER*1 NAME(6)
	INTEGER*2 DATE(LDATE_LEN)
	INTEGER*4 T, N, K
C
	DATA TYPE/'?????????','  Primary','   Backup','    Spare'/
	DATA NAME/'?','A','B','C','D','E'/
C
C INITIALIZE OLD/NEW WITH SPACES.
C
	CALL LIB$MOVC5(0,0,ICHAR(' '),SIZEOF(NEW),NEW)
	CALL LIB$MOVC5(0,0,ICHAR(' '),SIZEOF(OLD),OLD)
C
	DATE(5)=DAYCDC
	CALL LCDATE(DATE)
C
	T=P(SYSTYP)+1
	IF(T.LT.1.OR.T.GT.4)T=1
	N=NODEID+1
	IF(N.LT.1.OR.N.GT.NETSYS+1) N=1
	WRITE(VISHEAD,9001)(DATE(K),K=7,13),TYPE(T),NAME(N)
9001	FORMAT(7A2,12X,A9,'-',A1)
	RETURN
	END
