C PROGRAM XXXDEBUG
C
C V04 15-JUN-2000 OXK CLEANUP W/ WARNINGS=ALL
C V03 08-JUN-2000 UXN IMPLICIT NONE ADDED.
C V02 18-JUL-1994 WS  MULTINETWORK CHANGES - Integrate UK changes into 
C	  	      X2X Baseline
C V01 01-AUG-1990 XXX RELEASED FOR VAX
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM X2DEBUG
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:MSGCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'			!V02
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'			!V02
	INCLUDE 'INCLIB:X2XREL.DEF'			!V02
	INCLUDE 'INCLIB:X2XPOST.DEF'
	INCLUDE 'INCLIB:X2FCOM.DEF'
	INCLUDE 'INCLIB:X2FEMES.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:REQCOM.DEF'
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:X2NETCOM.DEF'
	INCLUDE 'INCLIB:QUECOM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	
	INTEGER*4   VAL,ANS,ST
C
C
C
	CALL COPYRITE
C
10	CONTINUE
	TYPE*,'X2X_DEBUG VALUE IS: ',X2X_DEBUG
	TYPE*,' '
	TYPE*,' '
	TYPE*,'Do you want to change this value? (Y/N) '
	CALL YESNO(ANS)
	IF(ANS.EQ.2) STOP
	   CALL INPNUM('Enter NEW value: ',VAL,0,100,ST)
	   TYPE*,' '
	   TYPE*,' '
	   X2X_DEBUG = VAL
	   GOTO 10
	END
