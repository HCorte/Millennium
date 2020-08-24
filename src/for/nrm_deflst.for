C
C SUBROUTINE DEFLST
C  
C GLIST.FOR
C
C V05 01-AUG-1996 WS  Force a memory barrier by call to MB().  Split up
C                     LISTLOCK() and LISTRLSE() into separate routines for
C                     locking/releasing top and bottom of list array.
C V04 03-APR-1991 TKO Fix bug in LISTTOP which would not get end of queue
C V03 28-JAN-1991 TKO Lock in the queue itself - not in lock table
C V02 20-DEC-1990 TKO MAKE ABL, RTL, ETC SUBROUTINES - NOT FUNCTIONS
C V01 05-JUL-1990 MP  INITIAL RELEASE
C		      TARGETED FOR PORT OF CONCURRENT FORTRAN
C		      TO VAX/VMS ENVIRONMENT
C
C This routines will allow definition and maintenance of
C GLISTs similar (but not identically) to Concurrent
C FORTRAN GLIST instructions. The differences are:
C	1. implementation is NOT with uninterruptable instructions
C	2. GLIST header structure is totally different to provide
C	   some additional features in the future
C
C GLIST HEADER IS CURRENTLY 16 4-BYTE WORDS AND MAY BE INCREASED
C QHEDSZ IN SYSDEFINE.DEF DEFINES THE ACTUAL LENGTH
C
C This module contains the following routines:
C
C	DEFLST (LIST_ARRAY, count)
C		DEFINE LIST
C
C	NOLOKTOP (LIST_ARRAY)
C		TURN OFF LOCK FOR TOP OF LIST (ATL, RTL)
C
C	NOLOKBOT (LIST_ARRAY)
C		TURN OFF LOCK FOR BOTTOM OF LIST (ABL, RBL)
C
C	ABL (buf_number, LIST_ARRAY, status)
C		ADD TO THE BOTTOM OF THE LIST
C
C	ATL (buf_number, LIST_ARRAY, status)
C		ADD TO THE TOP OF THE LIST
C
C	RTL (buf_number, LIST_ARRAY, status)
C		REMOVE FROM THE TOP OF THE LIST
C
C	RBL (buf_number, LIST_ARRAY, status)
C		REMOVE FROM THE BOTTOM OF THE LIST
C
C	LISTLOCK (LIST_ARRAY)
C	        LOCK THE ENTIRE LIST (TOP AND BOTTOM)
C
C	LISTRLSE (LIST_ARRAY)
C	        RELEASE LIST FROM LOCK STATE
C
C	LISTTOP  (buf_number, LIST_ARRAY, status)
C		GET ELEMENT FROM THE TOP WITHOUT REMOVING
C
C	LISTSIZE (LIST_ARRAY, SIZE)
C		GET SIZE OF THE LIST
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C DEFLST +++++++++++++++++++++++++++++++++++++++++++++++++++++
C Defines new GLIST and builds GLISTs' header
C
C DEFLST ----------------------------------------------------
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE DEFLST(LIST_ARRAY, COUNT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:GLIST.DEF'
C
	INTEGER*4	LIST_ARRAY(*), COUNT
C
	INTEGER*4	K
	VOLATILE LIST_ARRAY
C
C
C
C INITIALIZE GLIST HEADER VALUES
C
	DO 110 K = 1, QHEDSZ
	  LIST_ARRAY(K) = 0
110	CONTINUE
C
	LIST_ARRAY (GLIST_MAX_OFFSET)   = GLIST_START + COUNT !1 EXTRA
C
	LIST_ARRAY (GLIST_TOP)	        = GLIST_START
	LIST_ARRAY (GLIST_TOP_LOCK)     =
     *        IOR(LIST_ARRAY(GLIST_TOP_LOCK), GLIST_LOCK_FLAG )
	LIST_ARRAY (GLIST_TOP_LOCK_CNT) = 0
C
	LIST_ARRAY (GLIST_BOT)	        = GLIST_START
	LIST_ARRAY (GLIST_BOT_LOCK)     =
     *        IOR(LIST_ARRAY(GLIST_BOT_LOCK), GLIST_LOCK_FLAG )
	LIST_ARRAY (GLIST_BOT_LOCK_CNT) = 0
C
 
	LIST_ARRAY (GLIST_ABL_CNT) = 0
	LIST_ARRAY (GLIST_ATL_CNT) = 0
	LIST_ARRAY (GLIST_RBL_CNT) = 0
	LIST_ARRAY (GLIST_RTL_CNT) = 0
C
	LIST_ARRAY (GLIST_EMPTY_CNT) = 0
	LIST_ARRAY (GLIST_FULL_CNT)  = 0
C
	RETURN
	END
