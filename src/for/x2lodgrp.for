C
C SUBROUTINE X2LODGRP.FOR
C 
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2LODGRP.FOV                                 $
C  $Date::   17 Apr 1996 16:21:30                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C V01 13-DEC-94 GPR RELEASED FOR UK
C
C This subroutine will load the GTECH Distributed Network
C common from the relay configuration file.
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
	SUBROUTINE X2LODGRP                                         
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XGRP.DEF'
C
	CHARACTER   X2FILNAM*20         !File name function
	INTEGER*4   GRPCNT /0/          !Counter
	INTEGER*4   ST, GRP
C
C OPEN THE GROUP DEFINITION FILE.
C
	CALL OPENX(1,X2FILNAM(XGRP),4,0,0,ST)
	CALL IOINIT(X2XGRP_FDB,1,X2XGRP_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OS32ER(5,X2FILNAM(XGRP),'OPENX',ST,0)
	  CALL GPAUSE
	ENDIF
C
C ATTEMPT TO LOAD ALL RELAY APPLICATIONS.
C
	DO 100 GRP=1,X2X_NUM_GROUPS
C
C READ RECORD ONE AS ALL DATA IS CONTAINED THERE.
C
	  CALL READW(X2XGRP_FDB,GRP,X2XGRP_REC,ST)
	  IF(ST.EQ.144) GOTO 8000
	  IF(ST.NE.0) THEN
	    CALL OS32ER(5,X2FILNAM(XGRP),'READW',ST,1)
	    CALL GPAUSE
	  ENDIF
	  IF(X2XGRP_REC(1).LE.0) GOTO 100
C
C STORE THE INFORMATION INTO COMMON.
C
	  X2XG_STATE(GRP)=X2XGRP_STATE
	  IF(X2XGRP_STATE.GE.X2XGS_ENABLED) GRPCNT=GRPCNT+1
100	CONTINUE
C
C CLOSE THE FILE AND RETURN.
C
8000	CONTINUE
	CALL CLOSEFIL(X2XGRP_FDB)
C
D	TYPE*,'Number of enabled relay groups: ',GRPCNT
C
	RETURN
C
	END
