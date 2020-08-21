C BNGCNV_WINMAT.FOR
C
C $Log:   GXAFXT:[GOLS]BNGCNV_WINMAT.FOV  
C  
C     Rev 1.0   17 Apr 1996 12:20:28   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   29 Nov 1994 13:39:04   JXP
C  REPLACE WINCOM WITH BNGCOM
C  
C     Rev 1.1   23 Nov 1994 16:18:02   HXK
C  Changed logic slightly.
C  
C     Rev 1.0   27 Oct 1994 17:05:22   HXK
C  Initial revision.
C
C This subroutine will convert Bingo match table winners into division format.
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE BNGCNV_WINMAT(WINMAT,SHRDIV,SUBGAME,WIN,GIND)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:BNGCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'

C
	INTEGER*4 WINMAT(BGOMAXMAP+1+BGOCOL*BGOROW,BGONUM)
					    !# bitmaps won on by board.
	INTEGER*4 SHRDIV(BGODIV)	    !Shares won by Division (Out)
	INTEGER*4 SUBGAME		    !Subgame Number
	INTEGER*4 DIV			    !Division won
	INTEGER*4 I			    !Loop Variable
	INTEGER*4 BRD			    !Board Loop Variable
	INTEGER*4 WIN			    !# of wins.
        INTEGER*4 GIND                      !game index

        LOGICAL   PHASE_2_COMPLETE

C
C 
C CHECK WHAT SUBGAME WE ARE DOING AND PERFORM ACCORDINGLY
C
	WIN = 0
	GOTO (100,200,300) SUBGAME
	TYPE*,IAM(),'Invalid Bingo Sub Game Number passed ',SUBGAME
	RETURN
C
C BINGO AB 
C
100	CONTINUE
	DO 110 BRD = 1,BGONUM
	    DO I = BGOMAXMAP+1+(BGOCOL*BGOROW),1,-1
		DIV = BNGMAT(I,SUBGAME,GIND)
		IF(DIV.GT.0.AND.DIV.LE.BGODIV) THEN
		    IF(WINMAT(I,BRD).GT.0) THEN
			WIN = WIN + 1
			SHRDIV(DIV) = SHRDIV(DIV) + 1
                        GOTO 110
		    ENDIF
		ENDIF
	    ENDDO
110     CONTINUE
C
	RETURN
C
C FULL HOUSE
C
200	CONTINUE
	DO 210 BRD = 1,BGONUM
            PHASE_2_COMPLETE = .FALSE.
	    DO I = BGOMAXMAP+1+(BGOCOL*BGOROW),1,-1
                IF(I.EQ.BGOMAXMAP) PHASE_2_COMPLETE = .TRUE. 
		DIV = BNGMAT(I,SUBGAME,GIND)
		IF(DIV.GT.0.AND.DIV.LE.BGODIV) THEN
                    IF(.NOT.PHASE_2_COMPLETE) THEN
		       IF(WINMAT(I,BRD).EQ.2) THEN  !phase 2 winner
			  WIN = WIN + 1
			  SHRDIV(DIV) = SHRDIV(DIV) + 1
                          PHASE_2_COMPLETE = .TRUE.
		       ENDIF
                    ELSE
                       IF(WINMAT(I,BRD).EQ.1) THEN  !phase 1 winner
                          WIN = WIN + 1
                          SHRDIV(DIV) = SHRDIV(DIV) + 1
                          GOTO 210
                       ENDIF
                    ENDIF
		ENDIF
	    ENDDO
210	CONTINUE
C
	RETURN
		
C
C LUCKY NUMBER
C
300	CONTINUE
	RETURN
C
	END	
	
