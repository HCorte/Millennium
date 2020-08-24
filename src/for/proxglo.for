C
C SUBROUTINE PROXGLO
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]PROXGLO.FOV                                  $
C  $Date::   17 Apr 1996 14:32:44                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2exe.for;1 **
C
C
C ========================================================
C PROXGLO.FTN
C
C V02 16-FEB-94 JWE Add broadcast server & statistics support
C V01 24-OCT-89 MBK-MRM ORIGINAL RELEASE
C
C ENCODE GLOBAL MESSAGE TO GO OUT (LAYER UNRELATED)
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
	SUBROUTINE PROXGLO(TRABUF,PROBUF,ORIGINAL,RESPOND,
     *	                   LENGTH,BUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
C
	INTEGER*4   TER, BUF
	INTEGER*4   PROBUF      !Procom buffer number
	INTEGER*4   ORIGINAL(*) !Original input buffer
	INTEGER*2   LENGTH      !Length of buffer
	LOGICAL     RESPOND     !Response required flag
C
	RESPOND=.FALSE.
C
C PROCESS STATION RESET.
C
	IF(TRABUF(TXPTL).EQ.X2ERR_GLO_STN_RES_SOFT) THEN
C
	  CALL X2RESSTN(TRABUF,PRO(OUTTAB,PROBUF),
     *	                ORIGINAL,LENGTH)
	  RESPOND=.TRUE.
C
	ELSEIF(TRABUF(TXPTL).EQ.X2ERR_GLO_STN_RES_HARD) THEN
C
	  CALL X2RESSTN(TRABUF,PRO(OUTTAB,PROBUF),
     *	                ORIGINAL,LENGTH)
	  RESPOND=.TRUE.
C
C PROCESS WAKE UP MESSAGE
C
        ELSEIF(TRABUF(TXPTL).EQ.X2ERR_GLO_STN_WAKE_UP) THEN
C
          CALL X2WAKSTN(TRABUF,PRO(OUTTAB,PROBUF),
     *                  ORIGINAL,LENGTH)
          RESPOND=.TRUE.
C
C PROCESS DISABLE MESSAGE
C
        ELSEIF(TRABUF(TXPTL).EQ.X2ERR_GLO_STN_DISABLE) THEN
C
          CALL X2DISABL(TRABUF,PRO(OUTTAB,PROBUF),
     *                  ORIGINAL,LENGTH)
          RESPOND=.TRUE.
C
C STATION STATISTICS REQUEST.
C
	ELSEIF(TRABUF(TXPTL).EQ.X2ERR_GLO_STN_STATS .OR.
     *       TRABUF(TXPTL).EQ.X2ERR_GLO_STN_X25_STATS .OR.          ! V05
     *       TRABUF(TXPTL).EQ.X2ERR_GLO_STN_BCST_STATS .OR.         ! V05
     *	       TRABUF(TXPTL).EQ.X2ERR_GLO_STN_LAST_CALL) THEN
C
	  CALL X2REQSTS(TRABUF,PRO(OUTTAB,PROBUF),
     *	                ORIGINAL,LENGTH)
	  RESPOND=.TRUE.
C
C STATION CONNECTION STATUS REQUEST.
C
	ELSEIF(TRABUF(TXPTL).EQ.X2ERR_GLO_STN_CONN) THEN
C
	  CALL X2REQCON(TRABUF,PRO(OUTTAB,PROBUF),
     *	                ORIGINAL,LENGTH)
	  RESPOND=.TRUE.
C
	ELSEIF(TRABUF(TXPTL).EQ.X2ERR_GLO_FE_STATS) THEN
C
	ELSEIF(TRABUF(TXPTL).EQ.X2ERR_GLO_TRACE_TER_INP) THEN
	  RESPOND=.FALSE.
	ELSEIF(TRABUF(TXPTL).EQ.X2ERR_GLO_TRACE_TER_OUT) THEN
	  RESPOND=.FALSE.
	  TER=TRABUF(TTER)
	  TRABUF(TXBAK)=X2XT_TRACE_INDEX(TER)
	  X2XT_TRACE_INDEX(TER)=TRABUF(TXIDX)
	ENDIF
C
	RETURN
	END
