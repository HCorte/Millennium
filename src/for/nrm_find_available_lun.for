C NRM_FIND_AVAILABLE_LUN.FOR
C
C V01 29-DEC-00 CS  INITIAL RELEASE FOR PORTUGAL
C
C SUBROUTINE FIND_AVAILABLE_LUN(LUN,ST)
C
C FINDS AN AVAILABLE LOGICAL UNIT
C
C INPUTS:
C *NONE*
C 
C OUTPUTS:
C  LUN        LOGICAL UNIT                          INT*4
C  ST         STATUS (0 -> OK, ELSE NO LUN, ERROR)  INT*4
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
	SUBROUTINE FIND_AVAILABLE_LUN(LUN,ST)
	IMPLICIT NONE
C			  
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	LOGICAL OPNFLG
C
	INTEGER*4 IOS,MAXLUN,LUN
	PARAMETER(MAXLUN=500)  !MAXIMUM LUN FOR SEARCH
	INTEGER*4 ST
C
	LUN    = 7   !ASSUME LUN 7 IS AVAILABLE   
	ST     = 0    
	OPNFLG = .TRUE.
C
C KEEP INCREMENTING LUN UNTIL AVAILABLE OR ERROR OR MAXLUN
C
	DO WHILE((IOS.NE.0.OR.OPNFLG).AND.(LUN.LE.MAXLUN))
	  INQUIRE(UNIT=LUN,OPENED=OPNFLG,IOSTAT=IOS)
	  IF(IOS.NE.0.OR.OPNFLG) LUN = LUN+1
	END DO
C
C REPORT ERROR STATUS
C
	IF(LUN.GT.MAXLUN)THEN
	  TYPE*,IAM(),'NAO HA UNIDADE LOGICA (LUN) DISPONIVEL'
	  ST = -1
	ELSE IF(IOS.NE.0) THEN  !INQUIRE ERROR
	  TYPE*,IAM(),'ERRO DE ENTRADA DE DADOS'
	  ST = IOS
	ENDIF  
C
	RETURN 
	END
