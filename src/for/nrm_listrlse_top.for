C
C LISTRLSE +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Release lock entire list (top and bottom)
C
C LISTRLSE ----------------------------------------------------------
C V01 01-AUG-1996 WS  Force a memory barrier by call to MB().  Split up
C                     LISTLOCK() and LISTRLSE() into separate routines for
C                     locking/releasing top and bottom of list array.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE LISTRLSE_TOP ( LIST_ARRAY )
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:GLIST.DEF'
C
        INTEGER*4       LIST_ARRAY(*)
C
C DECLARE LOCAL VARIABLES
C
        VOLATILE LIST_ARRAY
C
C Unlock the list
C
        CALL LIB$BBCCI(GLIST_LOCK_BIT, LIST_ARRAY(GLIST_TOP_LOCK))
C
        RETURN
        END
