C
C SUBROUTINE LISTSIZE
C V02 01-AUG-1996 WS  Force a memory barrier by call to MB().  Split up
C                     LISTLOCK() and LISTRLSE() into separate routines for
C                     locking/releasing top and bottom of list array.
C
C LISTSIZE +++++++++++++++++++++++++++++++++++++++++++++++++++++
C Gets # of entries in queue without locking
C
C LISTSIZE ----------------------------------------------------
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
	SUBROUTINE LISTSIZE( LIST_ARRAY, SIZE )
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:GLIST.DEF'
C
	INTEGER*4	LIST_ARRAY(*)
	INTEGER*4	SIZE
C
	VOLATILE LIST_ARRAY
C
C
C INITIALIZE GLIST HEADER VALUES
C
	SIZE = LIST_ARRAY(GLIST_ABL_CNT) +
     *         LIST_ARRAY(GLIST_ATL_CNT) -
     *	       LIST_ARRAY(GLIST_RBL_CNT) -
     *         LIST_ARRAY(GLIST_RTL_CNT)
C
	RETURN
	END
