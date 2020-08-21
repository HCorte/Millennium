C
C LISTLOCK ----------------------------------------------------------
C
C V01 01-AUG-1996 WS  Force a memory barrier by call to MB().  Split up
C                     LISTLOCK() and LISTRLSE() into separate routines for
C                     locking/releasing top and bottom of list array.
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
        SUBROUTINE LISTLOCK_TOP ( LIST_ARRAY )
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLIST.DEF'
C
        INTEGER*4       LIST_ARRAY(*)
C
C DECLARE LOCAL VARIABLES
C
        INTEGER*4       ST
C
        EXTERNAL    LIB$BBSSI
        INTEGER*4   LIB$BBSSI, K
C
        VOLATILE LIST_ARRAY
C
200     CONTINUE
        ST = LIB$BBSSI(GLIST_LOCK_BIT, LIST_ARRAY(GLIST_TOP_LOCK))
        IF (ST) THEN
          DO 210 K = 1, GLIST_LOCK_DELAY_COUNT
            CALL XWAIT( GLIST_LOCK_DELAY_VALUE, 1, ST)
            IF( .NOT. BTEST(LIST_ARRAY(GLIST_TOP_LOCK),
     *                                 GLIST_LOCK_BIT))GOTO 200
210       CONTINUE
          TYPE *,IAM(),'TOP LOCK AT ',%LOC(LIST_ARRAY(GLIST_TOP_LOCK)),
     *           ' LOCKED TOO LONG'
          GOTO 200
        ENDIF
C
        RETURN
        END
