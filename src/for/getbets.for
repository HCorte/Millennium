C
C SUBROUTINE GETBETS
C $Log:   GXAFXT:[GOLS]GETBETS.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:18:28   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   04 Dec 1995 15:17:20   HXK
C  Made changes for LTPOOL_GAMENR not having MAXTYP as array size!
C  
C     Rev 1.0   21 Jan 1993 16:24:10   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - getbets.for **
C
C GETBETS.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C             CALLING SEQUENCE   Input :  BOARD - board selected
C
C                               Output :  NUM   -  number of bets
C                                         STAT  -  status .non.0. err
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
	SUBROUTINE GETBETS(BOARD,NUM,STAT,GAM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	 INTEGER*4 BUFFER(SECSIZE/4)
	 INTEGER*2 I2BUFFER(SECSIZE/2)
	 EQUIVALENCE (I2BUFFER,BUFFER)
	 INTEGER*4 FDB(7), ST, BASE_PAGE, IN_BLOCK, VALUE, OFF
	 INTEGER*4 I, INDEX, TYPE, GAM
	 INTEGER *4 NUM,STAT,BOARD(*)
	 INTEGER *4 TEMP,BLOCK,OFFSET/0/,NIBBLE,BYTE,NOVR,TIMES/0/
	 INTEGER*4 TAB_1X2(3) /1,2,4/   ! 1 - 1, 2 - 2 (2), 3 - 4 (4)
	 INTEGER*4 TAB2(16)
	 LOGICAL SORT
C
C
	DO 10, TYPE=1,LTPOOL_MAXTYP
	DO 10, INDEX=1,MAXIND
	   IF (LTPOOL_GAMENR(TYPE,INDEX).EQ.GAM) GOTO 20
10	CONTINUE
	RETURN
C
20	CONTINUE
C
	IF (TYPE.EQ.TLTO) THEN
C
C Sort lotto selection in ascending order
C
	  STAT=-1
30	  CONTINUE
C
	  SORT=.FALSE.
	  DO 40 I=1,LTPOOLBET(GAM)-1
	    IF(BOARD(I).LE.0) RETURN
	    IF(BOARD(I).EQ.BOARD(I+1)) RETURN
	    IF(BOARD(I).GT.BOARD(I+1)) THEN
	      TEMP=BOARD(I)
	      BOARD(I)=BOARD(I+1)
	      BOARD(I+1)=TEMP
	      SORT=.TRUE.
	    ENDIF
40	  CONTINUE
	  IF(SORT) GOTO 30
	  STAT=0
C
C Call routine to calculate selection offset
C
	  CALL CMBOFF(BOARD,OFFSET,LTPOOLBET(GAM))
C
	ELSE IF(TYPE.EQ.TSPT) THEN
	  DO 50, OFF=1,LTPOOLNR(GAM)
	    IF (BOARD(OFF).LE.0) RETURN
	    BOARD(OFF)=MOD(BOARD(OFF)-1,3)+1
	    VALUE=TAB_1X2(BOARD(OFF))
	    CALL SETNIBLE(VALUE,TAB2,OFF)
50	  CONTINUE
	  CALL SETNIBLE(0,TAB2,LTPOOLNR(GAM)+1)
C
	  CALL SPTOFF(1,TAB2,LTPOOLNR(GAM),OFFSET)
	ENDIF
C
C
C Once you have offset calculate  block, nibble, and byte
C
	IN_BLOCK=SECSIZE*2
	BASE_PAGE=LTPOOL_GAMPAG(GAM)
	IF (LTPOOL_INWORD(BASE_PAGE).EQ.4) IN_BLOCK=SECSIZE
	IF (LTPOOL_INWORD(BASE_PAGE).EQ.2) IN_BLOCK=SECSIZE/2
	BLOCK=((OFFSET-1)/IN_BLOCK)+1    !Block number
	BLOCK=BLOCK+(BASE_PAGE-1)*(PAGESIZE/(SECSIZE/4))
	IF (LTPOOL_INWORD(BASE_PAGE).EQ.8) THEN
	  NIBBLE=MOD(OFFSET,IN_BLOCK)
	  IF (NIBBLE.EQ.0) NIBBLE=IN_BLOCK
	  BYTE=(NIBBLE+1)/2
	ELSE
	  BYTE=MOD(OFFSET,IN_BLOCK)
	  IF (BYTE.EQ.0) BYTE=IN_BLOCK
	  NIBBLE=0           !JUST FOR WARNING
	ENDIF
C
C Open and read LTOPOOL.FIL with block number calculated
C
	CALL OPENW(1,SFNAMES(1,LPR),4,0,0,ST)
	IF(ST.NE.0) THEN
	  CALL FILERR(SFNAMES(1,LPR),1,ST,0)
	  CALL CLOSEFIL(FDB)
	  STAT=-2
	  RETURN
	ENDIF
	CALL IOINIT(FDB,1,1*SECSIZE)
C
C Read record with block number
C
	CALL READW(FDB,BLOCK,BUFFER,ST)
	IF (ST.NE.0) THEN
	  CALL FILERR(SFNAMES(1,LPR),2,ST,BLOCK)
	  CALL CLOSEFIL(FDB)
	  STAT=-3
	  RETURN
	ENDIF
C
C Get value of nibble which will equal number of times bet
C
C NOTE: POOLS ARE MAINTAINED IN LEFT TO RIGHT ORDER, SO
C 'NLBYTE' ROUTINE SHOULD BE USED FOR DEC MACHINES
C
	CALL NLBYTE(TEMP,BUFFER,BYTE-1)
	IF (LTPOOL_INWORD(BASE_PAGE).EQ.8) THEN
	  IF((MOD(NIBBLE,2)).EQ.1) THEN
	    TIMES=ISHFT(TEMP,-4)
	  ELSE
	    TIMES=IAND(TEMP,15)
	  ENDIF
	ELSE IF(LTPOOL_INWORD(BASE_PAGE).EQ.4) THEN
	  TIMES=TEMP
	ELSE IF(LTPOOL_INWORD(BASE_PAGE).EQ.2) THEN
C
C*** CONCURRENT:
C***	      TIMES=I2BUFFER(BYTE)
C*** DEC:
	  IF(IAND(BYTE,1).NE.0)THEN
	    TIMES = I2BUFFER(BYTE+1)
	  ELSE
	    TIMES = I2BUFFER(BYTE-1)
	  ENDIF
	ENDIF
C
C Call overflow routine to calculate number of bets and number of
C overflow
C
	CALL CLOSEFIL(FDB)
	CALL GETOVR(OFFSET,NOVR,GAM)
C
C Calculate total number of times bet
C
	NUM=TIMES+NOVR
	RETURN
	END
