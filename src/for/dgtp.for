C
C SUBROUTINE DGTP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]DGTP.FOV                                     $
C  $Date::   17 Apr 1996 12:52:18                                         $
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
C V01 24-OCT-89 MBK ORIGINAL RELEASE
C
C DGTP.FTN
C
C V01 24-OCT-89 MBK ORIGINAL RELEASE
C
C DECODE SOME GTP LAYER MESSAGE INTO TRABUF
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
	SUBROUTINE DGTP(MESSAGE,TRABUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2TDBH.DEF'
C
	INTEGER*4 MESSAGE(*), CHKVAL, OFFMES
C
C DECODE THE FOLLOWING: TXTDSAP, TXTFEID, TXTBTYP
C
	CALL ILBYTE(OFFMES,MESSAGE,X2PRO_OFFSET-1)
	IF(CHKVAL(OFFMES,11,90,' MESS OFF ').NE.0) THEN
	   TRABUF(TERR) =XERR
	   TRABUF(TSTAT)=REJT
	   TRABUF(TXPTL)=X2ERR_MOFF          !MESSAGE OFFSET
	   RETURN
	ENDIF
C
	CALL ILBYTE(TRABUF(TXTFEID),MESSAGE,OFFMES+X2TDBH_FE_ID-2)
	CALL ILBYTE(TRABUF(TXTDSAP),MESSAGE,OFFMES+X2TDBH_DSAP-2)
	CALL ILBYTE(TRABUF(TXTBTYP),MESSAGE,OFFMES+X2TDBH_BLKTYP-2)
C
	RETURN
	END
