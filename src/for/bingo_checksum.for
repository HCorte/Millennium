C
C Subroutine to calculate the checksum for
C BINGO transfer files...
C
C V01 11-AUG-97 UXN Initial release.
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
C
        SUBROUTINE BINGO_CHECKSUM(INPUT,OUTPUT,LEN)
        IMPLICIT NONE
        BYTE          INPUT(*)
        INTEGER*4     OUTPUT,LEN
C
        INTEGER*4   I
C
        DO I=1,LEN
          OUTPUT = OUTPUT + INPUT(I)
        ENDDO
        END 
	
