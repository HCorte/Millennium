C
C SUBROUTINE UPDAGTINF
C 
C V02 07-FEB-2001 UXN Replace 0 with ' '
C V01 22-SEP-2000 UXN Initial release.
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
C Copyright 2001 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE UPDAGTINF(FDB,TASK,TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:HASF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INTEGER*4 MESS(EDLEN),FDB(7)
	INTEGER*4 I, J, ST, TER, TASK
	INTEGER*4 LEN, CURLEN, ID, OFF, BYTES_TO_MOVE
	CHARACTER CZERO*1/Z0/
C
	INTEGER*4 CNT
C
	TER = TRABUF(TTER)                     ! terminal 
	CNT = TRABUF(TSNEW)                    ! # of fields to update
C
	CALL READW(FDB,TER,ASFREC,ST)
	IF(ST.NE.0) THEN
	  MESS(1)=TASK
	  MESS(2)=TEGEN
	  MESS(3)=4
	  CALL FASTMOV(SFNAMES(1,ASF),MESS(4),5)
	  MESS(9)=ST
	  MESS(10)=TER
	  CALL QUEMES(MESS)
	  TRABUF(TERR)=INVL
	  TRABUF(TSTAT)=REJT
	  RETURN
	ENDIF
C
	OFF = 1
	DO I=1,CNT
	   LEN = 0
	   ID  = 0
	   CALL MOVBYT(TRABUF(TSDT1),OFF,ID,1,1)    ! 1 bytes of ID
	   OFF = OFF + 1
	   CALL MOVBYT(TRABUF(TSDT1),OFF,LEN,1,1)   ! 1 bytes of lenght
	   OFF = OFF + 1

	   CURLEN = FLDEND(ID)-FLDBEG(ID)+1
	   BYTES_TO_MOVE = MIN(LEN,CURLEN)
	   CALL MOVBYT(TRABUF(TSDT1),OFF,ASFINF,FLDBEG(ID),BYTES_TO_MOVE)
	   OFF = OFF + LEN
	   IF(LEN.LT.CURLEN) THEN
	      DO J=LEN, CURLEN
	       ASFBYT(FLDBEG(ID)+J) = ' '
	      ENDDO
	   ENDIF
	ENDDO
C
	DO I=1,ALENGTH
	   IF(ASFBYT(I).EQ.CZERO) ASFBYT(I) = ' '
	ENDDO
C
	CALL WRITEW(FDB,TER,ASFREC,ST)
	IF(ST.NE.0) THEN
	  MESS(1)=TASK
	  MESS(2)=TEGEN
	  MESS(3)=5
	  CALL FASTMOV(SFNAMES(1,ASF),MESS(4),5)
	  MESS(9)=ST
	  MESS(10)=TER
	  CALL QUEMES(MESS)
	  TRABUF(TERR)=INVL
	  TRABUF(TSTAT)=REJT
	  RETURN
	ENDIF
	RETURN
	END
