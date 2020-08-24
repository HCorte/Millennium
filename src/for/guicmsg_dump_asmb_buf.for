C GUICMSG_DUMP_ASMB_BUF
C
C V02 31-OCT-2000 UXN GUI prefix added 
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
C	CONN_NR	 - connection number
C OUTPUT:
C	none
C RESULT:
C	data dumped to the device (file) identified by DBG_UNIT
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUICMSG_DUMP_ASMB_BUF(DBG_UNIT, R_NAM, BBUF, CONN_NR)
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
	INTEGER*4   CONN_NR
	INTEGER*4   BUF_BOFF
C
	WRITE(DBG_UNIT,*)
	WRITE(DBG_UNIT,*)IAM(),R_NAM//'conn_nr:', CONN_NR
C
	BUF_BOFF=1
C
	CALL GUICBUF_DUMP_BYTES
     *	      (DBG_UNIT,R_NAM//'Link header:', BBUF, BUF_BOFF,
     *		GUI_LH_SZ)
C
	CALL GUICBUF_DUMP_BYTES
     *	      (DBG_UNIT,R_NAM//'Path header:', BBUF, BUF_BOFF,
     *		GUI_PH_ADDR_OFF)
C
	CALL GUICBUF_DUMP_BYTES
     *	      (DBG_UNIT,R_NAM//'Data:', BBUF, BUF_BOFF, 
     *	      ZEXT(BBUF(1))*256+ZEXT(BBUF(2))-
     *		GUI_LH_SZ-GUI_PH_ADDR_OFF)
C
	RETURN
	END
