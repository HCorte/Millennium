C
C SUBROUTINE QUEAGTINF
C QUEAGTINF.FOR
C
C V01 29-SEP-2000 UXN Initial release.
C
C SUBROUTINE TO QUEUE AGTINF TRANSACTIONS (ONLINE AGENT UPDATE)
C TO INPUT QUEUE.
C
C CALLING SEQUENCE:
C      CALL QUEAGTINF(TER,INPMSG,ST)
C INPUT
C      INPMSG
C          BYTE 1 = NUMBER OF ITEMS n
C          BYTE 2 = DATA IDENTIFICATION \
C          BYTE 3 = DATA LENGTH t        > repeated n times.
C          BYTE 4->4+t-1 = DATA         /
C
C OUTPUT
C      ST  STATUS
C          0 - NO ERROR
C          1 - BAD TERMINAL NUMBER
C          2 - NO SYSTEM BUFFERS AVAILABLE
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE QUEAGTINF(TER, INPMSG, ST)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:HASF.DEF'
C
	INTEGER*4 TER                   ! TERMINAL NUMBER
	BYTE      INPMSG(*)
	INTEGER*4 TOT_NR_ITEMS          ! TOTAL NUMBER OF ITEMS
	INTEGER*4 ST,OFF
C
	INTEGER*4 BUF, DATA_LEN
	INTEGER*4 I, NR_ITEMS, TOT_BYTES                     	
	ST=0
C
C CHECK IF VALID TERMINAL NUMBER
C
	IF(TER.LT.1.OR.TER.GT.NUMAGT) THEN
	  ST=1
	  RETURN
	ENDIF
C	
	I = 1
	OFF = 1
	TOT_NR_ITEMS = ZEXT(INPMSG(OFF))
	OFF = OFF + 1
10	CONTINUE
C
	IF(I.GT.TOT_NR_ITEMS) RETURN
C
C ALLOCATE PROCOM PROCESSING BUFFER
C IF NO BUFFERS AVAILABLE THEN RETURN
C
	CALL GETBUF(BUF)
	IF(BUF.EQ.0) THEN
	  ST=2
	  RETURN
	ENDIF

	HPRO(TERNUM,BUF) = TER
	HPRO(TRCODE,BUF) = TYPAGTINF

	NR_ITEMS  = 0                      ! NUMBER OF ITEMS PER PRO() BUFFER
	TOT_BYTES = 1                      ! PRO(INPTAB,BUF) LENGTH
C
20	CONTINUE
C
	DATA_LEN = ZEXT(INPMSG(OFF+1)) + 2 ! id->len->data
	CALL MOVBYT(INPMSG,OFF,BPRO(BINPTAB+1,BUF),TOT_BYTES, DATA_LEN)
	OFF = OFF + DATA_LEN
	TOT_BYTES = TOT_BYTES + DATA_LEN
	NR_ITEMS = NR_ITEMS + 1

	I = I + 1

	DATA_LEN = 0
       	IF(I.LE.TOT_NR_ITEMS) DATA_LEN = ZEXT(INPMSG(OFF+1)) + 2 ! id->len->data

	IF(TOT_BYTES + DATA_LEN .GT. 150 .OR. I .GT. TOT_NR_ITEMS) THEN
           BPRO(BINPTAB,BUF) = NR_ITEMS
    	   HPRO(INPLEN,BUF)  = TOT_BYTES

	   CALL QUEINP(BUF,ST)     !  QUEUE TRANSACTION TO INPUT QUEUE
	   IF(ST.NE.0) RETURN

	   GOTO 10	    
	ELSE
	   GOTO 20 ! NEXT ITEM FITS INTO RECORD.
	ENDIF
	RETURN
	END
