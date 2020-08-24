C
C PROGRAM PRGTSK
C
C V02 14-APR-99 RXK Call of WIMG/YESNO replaced with call of PRMYESNO.
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C PRGTSK.FOR
C
C
C VALIDATION PURGE PROCEDURE CONTROL TASK
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM PRGTSK
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4    FILES(3), K, I, FLAG, ST
	DATA FILES  /VLC,CTP,UTP/
C
C
	CALL COPYRITE
C
C
	CALL PRMYESNO('Are you sure you want validation purge [Y/N]? ',FLAG)
	IF(FLAG.NE.1) CALL GSTOP(GEXIT_SUCCESS)
C
C CREATE NEW FILES
C
	DO 10 I=1,3
	CALL CRTFIL(SFNAMES(1,FILES(I)),SFSIZES(FILES(I)),ST)
	IF(ST.NE.0) THEN
	  WRITE(5,900) IAM(),(SFNAMES(K,FILES(I)),K=1,5)
	  CALL GPAUSE
	ENDIF
10	CONTINUE
C
C LOAD AND START VPURGE TASK
C
	WRITE(5,901) IAM(),'VPURGE  '
	CALL RUNTSK(8HVPURGE  )
C
C RENAME FILES
C
	CALL PRMYESNO('Did validation purge run ok [Y/N]? ',FLAG)
	IF(FLAG.NE.1) CALL GSTOP(GEXIT_SUCCESS)
	CALL FMAINT(VLF,VLC,ST)
	IF(ST.NE.0) THEN
	  TYPE*,IAM(),'Problem renaming files'
	  CALL GPAUSE
	ENDIF
C
C RUN PURGE REPORTS
C
	WRITE(5,901) IAM(),'PURWIN  '
	CALL RUNTSK(8HPURWIN  )
C
C
200	CONTINUE
	WRITE(5,902) IAM()
	CALL GSTOP(GEXIT_SUCCESS)
C
C FORMAT AREA
C
901	FORMAT(1X,A,' Begining execution of ',A8)
902	FORMAT(1X,A,' Validation purge complete ')
900	FORMAT(1X,A,' Error while allocating ',4A4)
	END
