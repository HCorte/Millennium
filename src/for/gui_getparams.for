C
C V01 08-NOV-2000 UXN Initial release.
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
C=======OPTIONS/CHECK=NOOVERFLOW
	SUBROUTINE GUI_GETPARAMS(MSG,ST)
	IMPLICIT NONE	
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:GUIMPRM.DEF'

	BYTE	  MSG(*)
	INTEGER*4 ST

	INTEGER*4 PARAM_CNT
	INTEGER*4 PARAM_ID
	INTEGER*4 PARAM_TYP

	INTEGER*4 OFF,I,LEN

	OFF = 10
	PARAM_CNT = ZEXT(MSG(OFF))
	OFF = OFF + 1
	IF(PARAM_CNT.GT.GUI_MAX_ARGS) THEN
	    CALL OPS('GUI_GETPARAMS: Invalid parameter count',PARAM_CNT,ST)
	    ST = -1
	    RETURN
        ENDIF
	CALL FASTSET(0,GUI_ARGVAL,2*GUI_MAX_ARGS)
	CALL FASTSET('    ',B_GUI_ARGCHAR, (GUI_ARGCHAR_MAXLEN*GUI_MAX_ARGS)/4)
	CALL FASTSET(0,GUI_ARGLEN,GUI_MAX_ARGS)
C
	DO I=1, PARAM_CNT
           PARAM_ID = ZEXT(MSG(OFF))
	   OFF = OFF + 1
	   IF(PARAM_ID.LT.1.OR.PARAM_ID.GT.GUI_MAX_ARGS) THEN
	      ST = -2
              CALL OPS('GUI_GETPARAMS: Invalid parameter id',PARAM_ID,ST)
	      RETURN
	   ENDIF
D	   CALL OPS('GUI_GETPARAMS: param_id ',PARAM_ID,0)
	   PARAM_TYP = ZEXT(MSG(OFF))
	   OFF = OFF + 1	
	   LEN = ZEXT(MSG(OFF))
	   OFF = OFF + 1
           GUI_ARGLEN( PARAM_ID ) = LEN
	   IF(PARAM_TYP.EQ.GUICHAR.OR.
     *        PARAM_TYP.EQ.GUIVARCHAR) THEN
	      IF(LEN.GT.GUI_ARGCHAR_MAXLEN) THEN
	         ST = -3
		 CALL OPS('GUI_GETPARAMS: invalid string length',LEN,ST)
		 RETURN
	      ENDIF
	      CALL MOVBYT(MSG,OFF,B_GUI_ARGCHAR(1, PARAM_ID ),1,LEN)
	      GUI_ARGLEN( PARAM_ID ) = LEN
	   ELSE
              IF(LEN.GT.8) THEN
	         ST = -3 ! INVALID LENGTH
	         CALL OPS('GUI_GETPARAMS: Invalid parameter length',LEN,ST)
	         RETURN
              ENDIF
	      CALL MOVBYT(MSG,OFF,GUI_ARGVAL( PARAM_ID ),1,LEN)
	   ENDIF
	   OFF = OFF + LEN
	ENDDO
	ST = 0
	RETURN
	END
