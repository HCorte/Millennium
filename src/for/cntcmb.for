C
C CNTCMB.FOR
C
C This subroutine reads LOTTO pools (from LTOPOOL.FIL and POOLOVR1.FIL)
C and calculates total number of combinations played and most
C popular combinations.
C
C V03 22-FEB-1999 UXN DIM parameter added to ADD_LIST and BINARY_SEARCH
C V02 04-FEB-1999 UXN WHILE loop corrected.
C V01 05-SEP-1997 UXN Initial release.
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE CNTCMB(
     *             GAM,			  ! Game number
     *             TOTAL_BOARDS,	  ! Total # of boards played
     *             TOTAL_COMB_PLAYED,	  ! Total # of combinations played
     *             TOTAL_COMB,	          ! Total # of combinations available 
     *		   POP)   		  ! Most popular combinations
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:LTOPOL.DEF'
	INCLUDE 'INCLIB:LTOSTREP.DEF'
C
	INTEGER*4   GAM,TOTAL_BOARDS
	INTEGER*4   TOTAL_COMB_PLAYED,TOTAL_COMB
	RECORD /POPULAR_COMBINATION/ POP(MAX_POP)
	INTEGER*2   OFFSETS(MAX_COMB)
	STATIC      OFFSETS
C
C Local variables
C
	INTEGER*4   I,J,CMB
	INTEGER*4   FDB(7)
	INTEGER*4   COUNT,BLOCK,ST
	INTEGER*4   BUFFER(SECSIZE/4)
	INTEGER*4   MAXOFF
C
C External functions
C
	LOGICAL NEXT_COMBINATION
	EXTERNAL NEXT_COMBINATION
C
C Initialize variables.
C
	TOTAL_BOARDS = 0
	TOTAL_COMB_PLAYED = 0
	MAXOFF = 0
	J = 0
	CALL OTS$MOVE5(%VAL(0),0,%VAL(0),%VAL(SIZEOF(POP)),POP)
	BLOCK = (LTPOOL_GAMPAG(GAM)-1)*PAGESIZE/(SECSIZE/4)
	CALL QGETOVR(OFFSETS,GAM,MAXOFF)
C
C OPEN LTOPOOL.FIL
C
	CALL OPENW(1,SFNAMES(1,LPR),4,0,0,ST)
	IF(ST.NE.0) THEN
	    CALL FILERR(SFNAMES(1,LPR),1,ST,0)
	    CALL GSTOP(GEXIT_FATAL)
	ENDIF
	CALL IOINIT(FDB,1,SECSIZE)
C
C Main processing loop
C
	CMB = 0
	DO WHILE(.TRUE.)
	  J = J + 1
	  IF(MOD(J,SECSIZE/4).EQ.1) THEN
	    BLOCK = BLOCK + 1
	    CALL READW(FDB,BLOCK,BUFFER,ST)
	    IF(ST.NE.0) THEN
	      CALL FILERR(SFNAMES(1,LPR),2,ST,BLOCK)
	      CALL CLOSEFIL(FDB)
	      CALL GSTOP(GEXIT_FATAL)
	    ENDIF
	  ENDIF
	  DO I=7,0,-1
	    CMB = CMB + 1
	    IF(CMB.GT.TOTAL_COMB) GOTO 100
	    COUNT = IAND(ISHFT(BUFFER(MOD(J-1,SECSIZE/4)+1),-I*4),'0F'X)
	    OFFSETS(CMB) = OFFSETS(CMB) + COUNT
	    IF(COUNT.GT.0.AND.MAXOFF.LT.CMB) MAXOFF = CMB
	  ENDDO
	ENDDO
100	CONTINUE
	CALL CLOSEFIL(FDB)
C
C FIND MOST POPULAR COMBINATIONS.
C	
	DO I = 1,MAXOFF
	  IF(OFFSETS(I).GT.POP(MAX_POP).COUNT) 
     *          CALL ADD_LIST(POP,OFFSETS(I),I,MAX_POP)
	  IF(OFFSETS(I).GT.0) THEN
	    TOTAL_BOARDS = TOTAL_BOARDS + OFFSETS(I)
	    TOTAL_COMB_PLAYED = TOTAL_COMB_PLAYED + 1
	  ENDIF 
	ENDDO
	DO I = 1,MAX_POP
	  IF(POP(I).COUNT.GT.0) CALL OFFCMB(POP(I).BOARD,POP(I).OFFSET,
     *                               LTPOOLBET(GAM))
	ENDDO
	END
C
C Add another combination to the list.
C
	SUBROUTINE ADD_LIST(POP,COUNT,OFF,DIM)
	IMPLICIT NONE
	INCLUDE 'INCLIB:LTOSTREP.DEF'
C
	INTEGER*4   OFF,DIM
	RECORD /POPULAR_COMBINATION/ POP(DIM)
	INTEGER*2   COUNT
C
C Local variables
C
	INTEGER*4   I,POS
C
	CALL BINARY_SEARCH(POP,COUNT,POS,DIM)
	DO I = DIM,POS+1,-1
	   POP(I) = POP(I-1)
	ENDDO
	POP(POS).COUNT  = COUNT
	POP(POS).OFFSET = OFF
	END
C
C BINARY SEARCH
C	
	SUBROUTINE BINARY_SEARCH(POP,COUNT,POS,DIM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:LTOSTREP.DEF'
C
	INTEGER*4   POS,DIM
	RECORD /POPULAR_COMBINATION/ POP(DIM)
	INTEGER*2   COUNT
C
	INTEGER*4   SEARCH_START,SEARCH_END,SEARCH_AVG
C
	SEARCH_START = 1
	SEARCH_END   = DIM 
C
100	CONTINUE
	SEARCH_AVG = (SEARCH_START+SEARCH_END)/2
	IF(SEARCH_START.EQ.SEARCH_AVG) THEN
	   IF(POP(SEARCH_START).COUNT.GT.COUNT) THEN
	     POS = SEARCH_START + 1
	   ELSE
	     POS = SEARCH_START
	   ENDIF
	   RETURN
	ENDIF
	IF(POP(SEARCH_AVG).COUNT.LT.COUNT) THEN
	   SEARCH_END = SEARCH_AVG
	ELSE
	   SEARCH_START = SEARCH_AVG
	ENDIF
	GOTO 100
	END
