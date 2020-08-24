C
C V01 26-FEB-2001 UXN Initial release.
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

        SUBROUTINE EXTEND(LUN, BLKS, ST)
        IMPLICIT NONE
C
	INTEGER*4 LUN, BLKS,ST

        CALL EXTEND1( %VAL(FOR$RAB(LUN)), BLKS, ST)

        END
C
        SUBROUTINE EXTEND1(RAB, BLKS, ST)
        IMPLICIT NONE

        INCLUDE '($SYSSRVNAM)'
        INCLUDE '($FABDEF)'
        INCLUDE '($RABDEF)'
        INCLUDE '($RMSDEF)'
C
        RECORD /RABDEF/ RAB
	INTEGER*4 BLKS,ST

        CALL EXTEND2( %VAL(RAB.RAB$L_FAB), BLKS, ST)

        END
C
        SUBROUTINE EXTEND2(FAB, BLKS, ST)
        IMPLICIT NONE

        INCLUDE '($SYSSRVNAM)'
        INCLUDE '($FABDEF)'
        INCLUDE '($RABDEF)'
        INCLUDE '($RMSDEF)'
C
        RECORD /FABDEF/ FAB
	INTEGER*4 BLKS
        INTEGER*4 ST

        FAB.FAB$L_ALQ = BLKS       ! EXTEND # OF BLOCKS
        FAB.FAB$L_FOP = IBSET( FAB.FAB$L_FOP, FAB$V_CBT )

        ST = SYS$EXTEND( FAB,, )
C 
        END
