C
C SUBROUTINE XTOLOG
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]XTOLOG.FOV                                   $
C  $Date::   17 Apr 1996 16:47:28                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - xtolog.for;1 **
C
C V02 16-FEB-94 JWE Change DMPHEX parameters
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C ENCODE XLOG BUFFER
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE XTOLOG(TRABUF,MESSAGE,BUFFER,DIR,LENGTH)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2FCOM.DEF'
C
	INTEGER*4 MESSAGE(*)
	INTEGER*4 BUFFER(*)
	INTEGER*4 DIR
	INTEGER*4 OFF, MESOFF, L4
C
	INTEGER*2 LENGTH
C
	BUFFER(XLOG_IDX)=TRABUF(TXIDX)
	BUFFER(XLOG_SER)=TRABUF(TSER)
	BUFFER(XLOG_CDC)=TRABUF(TCDC)
	BUFFER(XLOG_TIM)=TRABUF(TTIM)
	BUFFER(XLOG_STN)=TRABUF(TXSTN)
	BUFFER(XLOG_TER)=TRABUF(TTER)
	BUFFER(XLOG_SAP)=TRABUF(TXSAP)
	BUFFER(XLOG_LAY)=TRABUF(TXLAY)
	BUFFER(XLOG_PTL)=TRABUF(TXPTL)
	BUFFER(XLOG_BAK)=TRABUF(TXBAK)
	BUFFER(XLOG_DIR)=DIR
C
	L4=MIN0(LENGTH,252-XLOG_MLEN*4)
	BUFFER(XLOG_MLEN)=L4                     !10 = CURRENT OFFSET
C
	MESOFF=XLOG_MLEN*4+1
	CALL MOVBYT(MESSAGE,1,BUFFER,MESOFF,L4)
C
	DO 100 OFF=L4+MESOFF,(X2FRSIZ-1)*4       !LAST FW = DIRECTION
	CALL ISBYTE(0,BUFFER,OFF-1)
100	CONTINUE
C
C**   TYPE*,'LENGTH ',L4
C**   CALL DMPHEX(BUFFER,X2FRSIZ*4)
	RETURN
	END
