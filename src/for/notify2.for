C
C SUBROUTINE NOTIFY2
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NOTIFY2.FOV                                  $
C  $Date::   17 Apr 1996 14:13:34                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - net_netsub2.for;1 **
C
C
C     SUBROUTINE TO NOTYFY THE OPERATOR IN A CASE OF SOME
C                SPECIAL EVENT IN THE NETWORK
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
	SUBROUTINE NOTIFY2(ADR,TYPE,VALUE,WAY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
C
C
	INTEGER*4  NETMBS
	PARAMETER (NETMBS=10)
C
	INTEGER*4 MESS(EDLEN), K, NUM, WAY, VALUE, TYPE, ADR
	REAL*8 NAME
	INTEGER*4 HNAME(2)
	INTEGER*4 HTSKNAM(2,NUMTSK)
	EQUIVALENCE (HNAME,NAME)
	EQUIVALENCE (HTSKNAM,TSKNAM)
C
C
C
	CALL GETNAM(NAME)
C
C
	NUM=0
	DO 1 K=1,NUMTSK
	IF(HNAME(2).NE.HTSKNAM(2,K)) GOTO 1
	IF(HNAME(1).NE.HTSKNAM(1,K)) GOTO 1
	NUM=K
	GOTO 2
1	CONTINUE
C     NOT FOUND
2	CONTINUE
 
	MESS(1)=NUM
	MESS(2)=TENET
	MESS(3)=NETMBS+TYPE           !NETWORK MESSAGE BASE
	MESS(4)=ADR
	MESS(5)=VALUE
	MESS(6)=WAY
C
C
	CALL QUEMES(MESS)
C
	RETURN
	END
