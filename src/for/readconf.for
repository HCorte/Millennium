C SUBROUTINE READCONF
C
C V04 13-JUN-2000 OXK LANREAD from LANCOM.DEF to here
C V02 30-MAR-1992 DAS ADDED LANREAD TIME
C V01 28-NOV-1990 XXX RELEASED FOR VAX
C
C CALL READCONF(REPLY)
C REPLY    - 0 = OK
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
	SUBROUTINE READCONF(REPLY)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
C
C**   INTEGER*4 NODEID /1/                   ;FOR TESTING @@@@@!!!!!
C
	INTEGER*4 LANREAD                     !LAN BUFS EXHAUSTED,TRY AGAIN

	INTEGER*4 NAME(2) /'LAN@','@.ID'/
	INTEGER*4 TASK, I, LAN, NUM, REPLY
	INTEGER*4 SAP
C
	REPLY=0
C
C**   NODEID=1                               ;FOR TEST ONLY !!!!
C
	CALL BINASC(NAME,4,2,NODEID)
C
	CALL OPENW(WORKLU,NAME,4,0,0,REPLY)
	IF(REPLY.NE.0) RETURN
C
	READ(WORKLU,900,END=1000) NUMLAN
	READ(WORKLU,900,END=1000) LANSTEP,LANDEL1,LANDEL2,LANREAD
900	FORMAT(8(I10,1X))
C
	DO 5 NUM=1,NUMLAN
	  READ(WORKLU,901,END=1000) LAN,(LANHOME(I,LAN),I=1,6),
     *	                                 LANLADDR(LAN)
901	  FORMAT(I2,1X,6Z2,1X,I1)
5	CONTINUE
C
	DO 10 NUM=1,NUMLAN
	  READ(WORKLU,902,END=1000) LAN,(LANDEV(I,1,LAN),I=1,5),
     1	                                (LANDEV(I,2,LAN),I=1,5)
902	  FORMAT(I2,1X,5A1,1X,5A1)
C
          DO 12 SAP=1,MAXSAP
            LOCLAN(SAP,LAN)=LANUP
            LANOPN(SAP,LAN)=LSOPN
12        CONTINUE
10	CONTINUE
C
C GET APPL TASKS NAMES
C
	DO 20 TASK=0,LANMAXTSK
	READ(WORKLU,903,END=1000) LANTASKS(TASK),LANTRPTSK(TASK)
903	FORMAT(A8,1X,I1)
20	CONTINUE
C
	GOTO 1100
C
1000	CONTINUE
	REPLY=-2
1100	CONTINUE
	CALL USRCLOS1(     WORKLU)
	RETURN
	END
