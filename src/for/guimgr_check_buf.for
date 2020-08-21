C GUIMGR_CHECK_BUF(BUF_NR, SUB_NAME)
C
C V02 31-OCT-2000 UXN GUI prefix added.
C V01 22-JUN-1993 MP  RELEASE FOR VAX 
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C INPUT:
C	BUF_NR   - buffer number from GUI_LINK_BUF pool
C	SUB_NAME - calling subroutine name
C OUTPUT:
C	error message
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUIMGR_CHECK_BUF(BUF_NR, SUB_NAME)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
C
	INTEGER*4   BUF_NR
	CHARACTER*(*) SUB_NAME
C
	INTEGER*4   BLANK
	DATA	    BLANK/'    '/
C
	IF(BUF_NR.NE.GUI_LINK_BUF(GUI_BUF_NUM_OFF,BUF_NR)) THEN
	    CALL FASTSET(BLANK,GUI_MES_BUF,33)
	    WRITE(GUI_MES_CBUF,9010) IAM(), SUB_NAME,
     *	      BUF_NR,GUI_LINK_BUF(GUI_BUF_NUM_OFF,BUF_NR)
9010	    FORMAT(A,A,': Bad BUF nr, dequeued:',I,' buffer header:',I)
	    CALL WRITEBRK(GUI_MES_CBUF)
	ENDIF
C
	RETURN
	END
