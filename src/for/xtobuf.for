C
C SUBROUTINE XTOBUF
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]XTOBUF.FOV                                   $
C  $Date::   17 Apr 1996 16:47:22                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  DEC Baseline
C ** Source - xtobuf.for;1 **
C
C XTOBUF.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C This routine will convert X2X transactions from the
C stats buffer into TRABUF format, and also will extract
C the orginal X2X transport message from X2XMGR.
C
C Calling sequence:
C
C     CALL XTOBUF(TRABUF,MESSAGE,BUFFER,DIR,LENGTH)
C
C Input parameters:
C
C     BUFFER      Int*4(*)    X2X record buffer
C
C Output parameters:
C
C     TRABUF      Int*4(TRALEN)   Transaction buffer
C     MESSAGE     Int*4(*)        Original message from X2XMGR
C     DIR         Int*4           Inbound-Outbound flag
C     LENGTH      Int*2           Length of message (in bytes)
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
	SUBROUTINE XTOBUF(TRABUF,MESSAGE,BUFFER,DIR,LENGTH)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2FCOM.DEF'
C
	INTEGER*2 LENGTH            !Output message length
	INTEGER*4 MESSAGE(*)        !Output transport message
	INTEGER*4 BUFFER(*)         !Input buffer (disk record)
	INTEGER*4 MESOFF            !Start of X2XMGR message in buf
	INTEGER*4 DIR               !IN-OUT FLAG
	INTEGER*4 L4
C
C MOVE INFORMATION FROM BUFFER HEADER INTO TRABUF.
C
	TRABUF(TXIDX) = BUFFER(XLOG_IDX)
	TRABUF(TSER)  = BUFFER(XLOG_SER)
	TRABUF(TCDC)  = BUFFER(XLOG_CDC)
	TRABUF(TTIM)  = BUFFER(XLOG_TIM)
	TRABUF(TXSTN) = BUFFER(XLOG_STN)
	TRABUF(TTER)  = BUFFER(XLOG_TER)
	TRABUF(TXSAP) = BUFFER(XLOG_SAP)
	TRABUF(TXLAY) = BUFFER(XLOG_LAY)
	TRABUF(TXPTL) = BUFFER(XLOG_PTL)
	TRABUF(TXBAK) = BUFFER(XLOG_BAK)
	DIR           = BUFFER(XLOG_DIR)
C
C DETERMINE THE LENGTH (STORED IN BUFFER HEADER), AND
C MOVE THE TRANSPORT MESSAGE FROM THE BUFFER TO THE
C OUTPUT MESSAGE.
C
	LENGTH=BUFFER(XLOG_MLEN)
	L4=LENGTH
	MESOFF=XLOG_MLEN*4+1
	IF(L4.GT.0.AND.MESOFF.GT.0) THEN
	   CALL MOVBYT(BUFFER,MESOFF,MESSAGE,1,L4)
	ENDIF
C
	RETURN
	END
