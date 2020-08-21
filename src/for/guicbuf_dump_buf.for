C GUICBUF_DUMP_BUF
C
C V02 31-OCT-2000 UXN GUI prefix added.
C V01 23-JUN-1993 MP  INITIAL RELEASE FOR VAX
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
C	DBG_UNIT - debugging logical unit nr
C	R_NAM	 - routine name
C	BBUF	 - byte buffer array
C	BUF_NR	 - buffer number
C OUTPUT:
C	none
C RESULT:
C	data dumped to the device (file) identified by DBG_UNIT
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUICBUF_DUMP_BUF(DBG_UNIT, R_NAM, BBUF, BUF_NR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GUIMCOM.DEF'
C
	INTEGER*4   DBG_UNIT	    ! DEBIGGING OUTPUT LOGICAL UNIT NR
	CHARACTER*(*) R_NAM	    ! ROUTINE NAME
	BYTE	    BBUF(*)
	INTEGER*4   BUF_NR
	INTEGER*4   BUF_BOFF
C
	WRITE(DBG_UNIT,*)
	WRITE(DBG_UNIT,*)IAM(),R_NAM//'Received buf_nr:', BUF_NR
C
	BUF_BOFF=1
	CALL GUICBUF_DUMP_BYTES
     *	      (DBG_UNIT,R_NAM//'Buf_hed buf_nr', BBUF, BUF_BOFF,4)
C
	CALL GUICBUF_DUMP_BYTES
     *	      (DBG_UNIT,R_NAM//'Buf_hed len', BBUF, BUF_BOFF,4)
C
	CALL GUICBUF_DUMP_BYTES
     *	      (DBG_UNIT,R_NAM//'Buf_hed io_inx', BBUF, BUF_BOFF,4)
C
	CALL GUICBUF_DUMP_BYTES
     *	      (DBG_UNIT,R_NAM//'Buf_hed io_sts', BBUF, BUF_BOFF,4)
C
	CALL GUICBUF_DUMP_BYTES
     *	      (DBG_UNIT,R_NAM//'Buf_hed err', BBUF, BUF_BOFF,4)
C
	CALL GUICBUF_DUMP_BYTES
     *	      (DBG_UNIT,R_NAM//'Buf_hed conn', BBUF, BUF_BOFF,4)
C
	CALL GUICBUF_DUMP_BYTES
     *	      (DBG_UNIT,R_NAM//'Link header', BBUF, BUF_BOFF,
     *		GUI_LH_SZ)
C
	CALL GUICBUF_DUMP_BYTES
     *	      (DBG_UNIT,R_NAM//'Request', BBUF, BUF_BOFF, 
     *	      ZEXT(BBUF(6))*256+ZEXT(BBUF(5))-GUI_LH_SZ)
C
	RETURN
	END
