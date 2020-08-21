C
C SUBROUTINE DSTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DSTN.FOV                                     $
C  $Date::   17 Apr 1996 13:01:04                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2decode.for;1 **
C
C DSTN.FTN
C
C V01 24-OCT-89 MBK ORIGINAL RELEASE
C
C DECODE SOME STATION LAYER MESSAGE INTO TRABUF
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
	SUBROUTINE DSTN(MESSAGE,TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
C
	INTEGER*4 MESSAGE(*), CHKVAL, OFFMES
C
C DECODE THE FOLLOWING: TXSPID, TXSSDTU
C
C IF TXSSDTU=4    ALSO GET  TXSCC, TXSSNUM
C
C IF TXSSDTU=255  ALSO GET  TXSSTYP, TXSSPHID
C
	CALL ILBYTE(OFFMES,MESSAGE,X2PRO_OFFSET-1)
	IF(CHKVAL(OFFMES,11,90,' MESS OFF ').NE.0) THEN
	   TRABUF(TERR) =XERR
	   TRABUF(TSTAT)=REJT
	   TRABUF(TXPTL)=X2ERR_MOFF          !MESSAGE OFFSET
	   RETURN
	ENDIF
C
	CALL ILBYTE(TRABUF(TXSPID),MESSAGE,OFFMES+X2STMES_PROTID-2)
	CALL ILBYTE(TRABUF(TXSSDTU),MESSAGE,OFFMES+X2STMES_DATATYPE-2)
C
	IF(TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_CMD_UP) THEN
C
	   CALL ILBYTE(TRABUF(TXSCC),MESSAGE,OFFMES+X2STMES_CODE-2)
	   CALL MOV2TOI4(TRABUF(TXSSNUM),MESSAGE,
     *	                 OFFMES+X2STMES_STATION_NO-2)
C
        ELSEIF(TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_RESET .OR.
     *         TRABUF(TXSSDTU).EQ.X2STMES_DEF_CONF_REQ  .OR.
     *         TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_DEFAULT_CONF2 .OR.
     *         TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_RESET2) THEN
C
	   CALL ILBYTE(TRABUF(TXSSTYP),MESSAGE,OFFMES+X2STMES_STYPE-2)
	   CALL MOV4TOI4(TRABUF(TXSSPHID),MESSAGE,OFFMES+X2STMES_PHYSID
     *	                                          -2)
C
	ELSEIF(TRABUF(TXSSDTU).EQ.X2STMES_DATATYPE_RELAY_ACK) THEN
C
	ELSE
C
	   CALL OPS('**** ILLEGAL STATION DATA TYPE ****',
     *	            TRABUF(TSER),TRABUF(TXSSDTU))
	   TRABUF(TXPTL)=X2ERR_SDTU          !WRONG ST MES DATA TYPE
C
	ENDIF
C
	RETURN
	END
