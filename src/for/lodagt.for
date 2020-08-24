C LODAGT.FOR
C
C V02 01-SEP-1999 UXN Report added.
C V01 13-NOV-1997 UXN Initial release. 
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	PROGRAM LODAGT
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
C
	INTEGER*4 ERRORS(NUMAGT)/NUMAGT*0/
	COMMON    ERRORS
	INTEGER*4 ST, REPLUN, I, CNT
C
	CALL COPYRITE
	CALL SNIF_AND_WRKSET
C
	CALL GETAGT(1)
C
C CREATE A REPORT ABOUT THE TERMINALS, THAT ARE NOT DEFINED IN X2X DATABASE.
C	
	REPLUN = 8
	CALL ROPEN('LODAGT.REP', REPLUN, ST)
	IF(ST.NE.0) REPLUN = 6   
C
	CNT = 0
	DO I=1, NUMAGT
	   IF(ERRORS(I).EQ.1) THEN
	      CNT = CNT + 1
	      WRITE(REPLUN, *) IAM(),'Terminal ', I, 
     *                         ' not defined in X2X database !'
	   ENDIF
	ENDDO
	IF(REPLUN.NE.6) CLOSE(REPLUN)
	
	IF(CNT.GT.0) THEN
	   TYPE*,IAM()
	   TYPE*,IAM(),CNT,' terminal(s) not defined in X2X'
	   TYPE*,IAM(),'Read LODAGT.REP for details!'
	   TYPE*,IAM()
	ENDIF
C
	END	
