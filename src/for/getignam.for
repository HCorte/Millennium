C GETIGNAM.FOR
C
C V02 31-JUL-2000 UXN OPS() added.
C V01 20-JUN-2000 OXK Initial revision.
C  
C SUBROUTINE TO GET INSTANT GAME NAME.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, W.Greenwich, Rhode
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
        SUBROUTINE GETIGNAM(IGUN)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:INSNAM.DEF'
C
        INTEGER*4 IGNBR
        CHARACTER*10 IGNAM
C
        INTEGER*4 IGUN,ST

        OPEN(UNIT=IGUN,FILE='FILE:INSNAM.FIL',STATUS='OLD',ERR=100,IOSTAT=ST)

	CALL OPSTXT('Loading Instant Game data')

10      READ(IGUN,1000,END=999)IGNBR,IGNAM
1000    FORMAT(I4,A10)
        IF(IGNBR.LT.1 .OR. IGNBR.GT.99) GOTO 10
        INSSNAM(IGNBR) = IGNAM
        GOTO 10
C
100     CONTINUE
	CALL OPS('GETIGNAM: Error opening INSNAM.FIL, status ',ST,ST)	
        RETURN
C
999     CONTINUE
	CALL OPSTXT('Instant Game Data loaded')
        CLOSE(IGUN)
C
        RETURN
        END
