C
C TCPDEBUG.FOR
C
C V01 20-APR-95 SMH RELEASED FOR Belgium M-GOLS
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM TCPDEBUG
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:CRSCOM.DEF'

        INTEGER*4  ANS
        INTEGER*4  VAL
        INTEGER*4  ST




C
C
C
	CALL COPYRITE
C
10	CONTINUE
	TYPE*,'TCP_DEBUG VALUE IS: ',TCP_DEBUG
	TYPE*,' '
	TYPE*,' '
	TYPE*,'Do you want to change this value? (Y/N/E) '
	CALL YESNO(ANS)
	IF(ANS.EQ.1) THEN 
	   CALL INPNUM('Enter NEW value: ',VAL,0,1,ST)
	   TYPE*,' '
	   TYPE*,' '
	   TCP_DEBUG = VAL
	   GOTO 10
	END IF

        END
