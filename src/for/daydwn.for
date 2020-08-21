C
C V02 24-JAN-2011 RXK Statement RETURN removed.  
C V01 21-MAR-2001 UXN Initial release.
C 
C This program is called only from KILSYS.COM and it changes
C the game status to DSKILL    
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
	PROGRAM DAYDWN
	IMPLICIT NONE
	INCLUDE '(LIB$ROUTINES)'
C
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	CHARACTER*255   KILSYS_SYMBOL, KILSYS_PRCNAM
        DATA            KILSYS_SYMBOL/'KILSYS_PRCNAM'/

	INTEGER*4 LEN,ST

        ST = LIB$GET_SYMBOL(KILSYS_SYMBOL,KILSYS_PRCNAM,LEN)
	IF(.NOT.ST) GOTO 100
	IF(KILSYS_PRCNAM(:LEN) .NE. 'KILSYS') GOTO 100
        
	CALL OPS('*** SYSTEM IS DOWN ***',DAYSTS,DSKILL)
	DAYSTS = DSKILL 

100     CONTINUE
	END
